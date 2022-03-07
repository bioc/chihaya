#ifndef CHIHAYA_UNARY_COMPARISON_HPP
#define CHIHAYA_UNARY_COMPARISON_HPP

#include "H5Cpp.h"
#include <stdexcept>
#include <vector>
#include <algorithm>
#include "utils.hpp"

/**
 * @file unary_comparison.hpp
 *
 * @brief Validation for delayed unary comparison operations.
 */

namespace chihaya {

/**
 * @cond
 */
inline ArrayDetails validate(const H5::Group& handle, const std::string&);

inline bool valid_comparison(const std::string& method) {
    return (
        method == "==" ||
        method == ">" ||
        method == "<" ||
        method == ">=" ||
        method == "<=" ||
        method == "!="
    );
}
/**
 * @endcond
 */

/**
 * Validate delayed unary comparison operations in a HDF5 file.
 * This is "unary" in the sense that only one delayed object is involved.
 *
 * @param handle An open handle on a HDF5 group representing an unary comparison operation.
 * @param name Name of the group inside the file.
 *
 * @return Details of the object after applying the comparison operation.
 * Otherwise, if the validation failed, an error is raised.
 * 
 * A delayed unary comparison operation is represented as a HDF5 group with the following attributes:
 *
 * - `delayed_type` should be a scalar string `"operation"`.
 * - `delayed_operation` should be a scalar string `"unary comparison"`.
 *
 * Inside the group, we expect:
 *
 * - A `seed` group, containing a delayed object for which comparison is to be applied.
 *   The `seed` group handle is passed to `validate()` to check its contents recursively and to retrieve the dimensions.
 * - A `method` string scalar dataset, specifying the comparison method to use.
 *   This can be any one of `==`, `<`, `>`, `>=`, `<=`, or `!=`. 
 *   The exact string representation is left to the implementation.
 * - A `side` string scalar dataset, describing the side of the `seed` object to apply the operation.
 *   This can be `"left"`, when `value` is applied to the left of `seed`, e.g., `value > seed`;
 *   or `"right"`, when `value` is applied to the right of `seed`, e.g., `seed > value`.
 *   The exact string representation is left to the implementation.
 * - A `value` dataset.
 *   This may be scalar or 1-dimensional.
 *   If 1-dimensional, it should have length equal to the extent specified in `along`.
 *   If `seed` contains strings, so should `value`; otherwise, both `seed` and `value` should be any of boolean, integer or float.
 *   The exact type representation is left to the implementation.
 * - An `along` integer scalar dataset, specifying the dimension on which to apply the operation with `value`.
 *   The exact integer representation is left to the implementation.
 *
 * The type of the output object is always boolean.
 */
inline ArrayDetails validate_unary_comparison(const H5::Group& handle, const std::string& name) {
    if (!handle.exists("seed") || handle.childObjType("seed") != H5O_TYPE_GROUP) {
        throw std::runtime_error("expected 'seed' group for an unary comparison operation");
    }

    auto seed_details = validate(handle.openGroup("seed"), name + "/seed");

    // Checking the method.
    if (!handle.exists("method") || handle.childObjType("method") != H5O_TYPE_DATASET) {
        throw std::runtime_error("expected 'method' dataset for an unary comparison operation");
    }

    auto mhandle = handle.openDataSet("method");
    if (mhandle.getSpace().getSimpleExtentNdims() != 0 || mhandle.getTypeClass() != H5T_STRING) {
        throw std::runtime_error("'method' should be a scalar string for an unary comparison operation");
    }

    std::string method;
    mhandle.read(method, mhandle.getStrType());
    if (!valid_comparison(method)) {
        throw std::runtime_error(std::string("unrecognized 'method' (") + method + ") for an unary comparison operation");
    }

    // Checking the sidedness.
    if (!handle.exists("side") || handle.childObjType("side") != H5O_TYPE_DATASET) {
        throw std::runtime_error("expected 'side' dataset for an unary comparison operation");
    }

    auto shandle = handle.openDataSet("side");
    if (shandle.getSpace().getSimpleExtentNdims() != 0 || shandle.getTypeClass() != H5T_STRING) {
        throw std::runtime_error("'side' should be a scalar string for an unary comparison operation");
    }

    std::string side;
    shandle.read(side, shandle.getStrType());
    if (side != "left" && side != "right") {
        throw std::runtime_error(std::string("unrecognized 'side' (") + side + ") for an unary comparison operation");
    }

    // Checking the value.
    if (!handle.exists("value") || handle.childObjType("value") != H5O_TYPE_DATASET) {
        throw std::runtime_error("expected 'value' dataset for an unary comparison operation");
    }

    auto vhandle = handle.openDataSet("value");
    if ((seed_details.type == STRING) != (vhandle.getTypeClass() == H5T_STRING)) {
        throw std::runtime_error("both or none of 'seed' and 'value' should contain strings in an unary comparison operation");
    }

    size_t ndims = vhandle.getSpace().getSimpleExtentNdims();
    if (ndims == 0) {
        // scalar operation.
    } else if (ndims == 1) {
        hsize_t extent;
        vhandle.getSpace().getSimpleExtentDims(&extent);

        // Checking 'along'.
        if (!handle.exists("along") || handle.childObjType("along") != H5O_TYPE_DATASET) {
            throw std::runtime_error("expected 'along' dataset for an unary comparison operation");
        }

        auto ahandle = handle.openDataSet("along");
        if (ahandle.getSpace().getSimpleExtentNdims() != 0 || ahandle.getTypeClass() != H5T_INTEGER) {
            throw std::runtime_error("'along' should be a scalar integer for an unary comparison operation");
        }

        int along;
        ahandle.read(&along, H5::PredType::NATIVE_INT);
        if (along < 0 || static_cast<size_t>(along) >= seed_details.dimensions.size()) {
            throw std::runtime_error("'along' should be non-negative and less than the dimensionality for an unary comparison operation");
        }

        if (extent != seed_details.dimensions[along]) {
            throw std::runtime_error("length of 'value' dataset should be equal to the dimension specified in 'along'");
        }
    } else { 
        throw std::runtime_error("'value' dataset should be scalar or 1-dimensional for an unary comparison operation");
    }

    seed_details.type = BOOLEAN;
    return seed_details;
}

}

#endif
