#ifndef CHIHAYA_BINARY_COMPARISON_HPP
#define CHIHAYA_BINARY_COMPARISON_HPP

#include "H5Cpp.h"
#include <stdexcept>
#include <vector>
#include <algorithm>
#include "utils.hpp"
#include "unary_comparison.hpp"

/**
 * @file binary_comparison.hpp
 *
 * @brief Validation for delayed binary comparison operations.
 */

namespace chihaya {

/**
 * @cond
 */
inline ArrayDetails fetch_seed_for_comparison(const H5::Group& handle, const std::string& target, const std::string& name, const Version& version) {
    if (!handle.exists(target) || handle.childObjType(target) != H5O_TYPE_GROUP) {
        throw std::runtime_error(std::string("expected '") + target + "' group for a binary comparison operation");
    }
    return validate(handle.openGroup(target), name + "/" + target, version);
}
/**
 * @endcond
 */

/**
 * Validate delayed binary comparison operations in a HDF5 file.
 * This is "binary" in the sense that exactly two delayed objects are involved.
 *
 * @param handle An open handle on a HDF5 group representing a binary comparison operation.
 * @param name Name of the group inside the file.
 * @param version Version of the **chihaya** specification.
 *
 * @return Details of the object after applying the comparison operation.
 * Otherwise, if the validation failed, an error is raised.
 * 
 * A binary comparison operation is represented as a HDF5 group with the following attributes:
 *
 * - `delayed_type` should be a scalar string `"operation"`.
 * - `delayed_operation` should be a scalar string `"binary comparison"`.
 *
 * Inside the group, we expect:
 *
 * - A `left` group, containing a delayed object on the left of the comparison operation.
 *   The `left` group handle is passed to `validate()` to check its contents recursively and to retrieve the dimensions.
 * - A `right` group, containing a delayed object on the right of the operation.
 *   The `right` group handle is passed to `validate()` to check its contents recursively and to retrieve the dimensions.
 *   It should have exactly the same dimensions as the `left` object.
 *   If `left` contains strings, so should `right`; otherwise, both `left` and `right` should be any of boolean, integer or float.
 * - A `method` string scalar dataset, specifying the comparison method to use.
 *   This can be any one of `==`, `<`, `>`, `>=`, `<=`, or `!=`. 
 *   The exact string representation is left to the implementation.
 *
 * The type of the output object is always boolean.
 */
inline ArrayDetails validate_binary_comparison(const H5::Group& handle, const std::string& name, const Version& version) try {
    auto left_details = fetch_seed_for_comparison(handle, "left", name, version);
    auto right_details = fetch_seed_for_comparison(handle, "right", name, version);

    bool okay = are_dimensions_equal(left_details.dimensions, right_details.dimensions);
    if (!okay) {
        throw std::runtime_error("'left' and 'right' should have the same dimensions for a binary comparison operation");
    }

    if ((left_details.type == STRING) != (right_details.type == STRING)) {
        throw std::runtime_error("both or none of 'left' and 'right' should contain strings in a binary comparison operation");
    }

    // Checking the method.
    if (!handle.exists("method") || handle.childObjType("method") != H5O_TYPE_DATASET) {
        throw std::runtime_error("expected 'method' dataset for a binary comparison operation");
    }

    auto mhandle = handle.openDataSet("method");
    if (mhandle.getSpace().getSimpleExtentNdims() != 0 || mhandle.getTypeClass() != H5T_STRING) {
        throw std::runtime_error("'method' should be a scalar string for a binary comparison operation");
    }

    std::string method;
    mhandle.read(method, mhandle.getStrType());
    if (!valid_comparison(method)) {
        throw std::runtime_error(std::string("unrecognized 'method' (") + method + ") for a binary comparison operation");
    }

    left_details.type = BOOLEAN;
    return left_details;
} catch (std::exception& e) {
    throw std::runtime_error("failed to validate binary comparison operation at '" + name + "'\n- " + std::string(e.what()));
}

}

#endif
