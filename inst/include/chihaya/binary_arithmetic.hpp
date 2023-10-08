#ifndef CHIHAYA_BINARY_ARITHMETIC_HPP
#define CHIHAYA_BINARY_ARITHMETIC_HPP

#include "H5Cpp.h"
#include <stdexcept>
#include <vector>
#include <algorithm>
#include "utils.hpp"
#include "unary_arithmetic.hpp"

/**
 * @file binary_arithmetic.hpp
 *
 * @brief Validation for delayed binary arithmetic operations.
 */

namespace chihaya {

/**
 * @cond
 */
inline ArrayDetails fetch_seed_for_arithmetic(const H5::Group& handle, const std::string& target, const std::string& name, const Version& version) {
    if (!handle.exists(target) || handle.childObjType(target) != H5O_TYPE_GROUP) {
        throw std::runtime_error(std::string("expected '") + target + "' group for a binary arithmetic operation");
    }

    auto seed_details = validate(handle.openGroup(target), name + "/" + target, version);
    if (seed_details.type == STRING) {
        throw std::runtime_error(std::string("type of '") + target + "' should be numeric or boolean for a binary arithmetic operation");
    }

    return seed_details;
}
/**
 * @endcond
 */

/**
 * Validate delayed binary arithmetic operations in a HDF5 file.
 * This is "binary" in the sense that exactly two delayed objects are involved.
 *
 * @param handle An open handle on a HDF5 group representing a binary arithmetic operation.
 * @param name Name of the group inside the file.
 * @param version Version of the **chihaya** specification.
 *
 * @return Details of the object after applying the arithmetic operation.
 * Otherwise, if the validation failed, an error is raised.
 * 
 * A binary arithmetic operation is represented as a HDF5 group with the following attributes:
 *
 * - `delayed_type` should be a scalar string `"operation"`.
 * - `delayed_operation` should be a scalar string `"binary arithmetic"`.
 *
 * Inside the group, we expect:
 *
 * - A `left` group, containing a delayed object on the left of the arithmetic operation.
 *   The `left` group handle is passed to `validate()` to check its contents recursively and to retrieve the dimensions.
 *   This can be either boolean, integer or float.
 * - A `right` group, containing a delayed object on the right of the operation.
 *   The `right` group handle is passed to `validate()` to check its contents recursively and to retrieve the dimensions.
 *   This can be either boolean, integer or float.
 *   It should have exactly the same dimensions as the `left` object.
 * - A `method` string scalar dataset, specifying the arithmetic method to use.
 *   This can be any one of `+`, `-`, `/`, `*`, `^`, `%%` (modulo) or `%/%` (integer division).
 *   The exact string representation is left to the implementation.
 *
 * The type of the output object depends on the operation, the type of `left` and the type of `right`:
 *
 * - For simple division, the output type is always float.
 * - For integer division, the output type is always integer.
 * - For modulo where both `left` and `right` are integer, the output type is integer.
 * - In all other cases, the output type is the more advanced type of `left` and `right`. 
 *
 * Note that any boolean types in `left` and `right` are first promoted to integer before type determination.
 */
inline ArrayDetails validate_binary_arithmetic(const H5::Group& handle, const std::string& name, const Version& version) try {
    auto left_details = fetch_seed_for_arithmetic(handle, "left", name, version);
    auto right_details = fetch_seed_for_arithmetic(handle, "right", name, version);

    bool okay = are_dimensions_equal(left_details.dimensions, right_details.dimensions);
    if (!okay) {
        throw std::runtime_error("'left' and 'right' should have the same dimensions for a binary arithmetic operation");
    }

    // Checking the method.
    if (!handle.exists("method") || handle.childObjType("method") != H5O_TYPE_DATASET) {
        throw std::runtime_error("expected 'method' dataset for a binary arithmetic operation");
    }

    auto mhandle = handle.openDataSet("method");
    if (mhandle.getSpace().getSimpleExtentNdims() != 0 || mhandle.getTypeClass() != H5T_STRING) {
        throw std::runtime_error("'method' should be a scalar string for a binary arithmetic operation");
    }

    std::string method;
    mhandle.read(method, mhandle.getStrType());
    if (!valid_arithmetic(method)) {
        throw std::runtime_error(std::string("unrecognized 'method' (") + method + ") for a binary arithmetic operation");
    }

    left_details.type = determine_arithmetic_type(left_details.type, right_details.type, method);
    return left_details;
} catch (std::exception& e) {
    throw std::runtime_error("failed to validate binary arithmetic operation at '" + name + "'\n- " + std::string(e.what()));
}

}

#endif
