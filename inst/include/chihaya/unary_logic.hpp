#ifndef CHIHAYA_UNARY_LOGIC_HPP
#define CHIHAYA_UNARY_LOGIC_HPP

#include "H5Cpp.h"
#include <stdexcept>
#include <vector>
#include <algorithm>
#include "utils.hpp"

/**
 * @file unary_logic.hpp
 *
 * @brief Validation for delayed unary logic operations.
 */

namespace chihaya {

/**
 * @cond
 */
inline ArrayDetails validate(const H5::Group& handle, const std::string&, const Version&);
/**
 * @endcond
 */

/**
 * Validate delayed unary logic operations in a HDF5 file.
 * This is "unary" in the sense that only one delayed object is involved.
 *
 * @param handle An open handle on a HDF5 group representing an unary logic operation.
 * @param name Name of the group inside the file.
 * @param version Version of the **chihaya** specification.
 *
 * @return Details of the object after applying the logical operation.
 * Otherwise, if the validation failed, an error is raised.
 * 
 * A delayed unary logic operation is represented as a HDF5 group with the following attributes:
 *
 * - `delayed_type` should be a scalar string `"operation"`.
 * - `delayed_operation` should be a scalar string `"unary logic"`.
 *
 * Inside the group, we expect:
 *
 * - A `seed` group, containing a delayed object for which logic is to be applied.
 *   The `seed` group handle is passed to `validate()` to check its contents recursively and to retrieve the dimensions.
 *   This should be boolean; integer or floats are implicitly converted to booleans with the usual methods.
 * - A `method` string scalar dataset, specifying the logic method to use.
 *   This can be any one of `!`, `&&`, `||` .
 *   The exact string representation is left to the implementation.
 *
 * For `method != "!"`, we also expect:
 *
 * - A `side` dataset, describing the side of the `seed` object to apply the operation.
 *   This can be `"left"`, when `value` is applied to the left of `seed`, e.g., `value > seed`;
 *   or `"right"`, when `value` is applied to the right of `seed`, e.g., `seed > value`.
 *   The exact string representation is left to the implementation.
 * - A `value` dataset.
 *   This may be scalar or 1-dimensional.
 *   This should be integer or float, which are implicitly converted to booleans with the usual methods.
 *   The exact type representation is left to the implementation.
 *
 * If `value` is 1-dimensional, we also expect:
 *
 * - An `along` integer scalar dataset, specifying the dimension on which to apply the operation with `value`.
 *   The exact integer representation is left to the implementation.
 *   The length of `value` should be equal to the extent of the dimension specified in `along`.
 *
 * `value` may contain a `missing_placeholder` attribute.
 * This should be a scalar dataset of the same type class as `value`, specifying the placeholder value used for all missing elements,
 * i.e., any elements in `value` with the same value as the placeholder should be treated as missing.
 * (Note that, for floating-point datasets, the placeholder itself may be NaN, so byte-wise comparison should be used when checking for missingness.)
 *
 * The type of the output object is always boolean.
 */
inline ArrayDetails validate_unary_logic(const H5::Group& handle, const std::string& name, const Version& version) try {
    if (!handle.exists("seed") || handle.childObjType("seed") != H5O_TYPE_GROUP) {
        throw std::runtime_error("expected 'seed' group for an unary logic operation");
    }

    auto seed_details = validate(handle.openGroup("seed"), name + "/seed", version);
    if (seed_details.type == STRING) {
        throw std::runtime_error("'seed' should contain numeric or boolean values for an unary logic operation");
    }

    // Checking the method.
    if (!handle.exists("method") || handle.childObjType("method") != H5O_TYPE_DATASET) {
        throw std::runtime_error("expected 'method' dataset for an unary logic operation");
    }

    auto mhandle = handle.openDataSet("method");
    if (mhandle.getSpace().getSimpleExtentNdims() != 0 || mhandle.getTypeClass() != H5T_STRING) {
        throw std::runtime_error("'method' should be a scalar string for an unary logic operation");
    }

    std::string method;
    mhandle.read(method, mhandle.getStrType());

    if (method != "!" &&
        method != "&&" && 
        method != "||")
    {
        throw std::runtime_error(std::string("unrecognized 'method' (") + method + ") for an unary logic operation");
    }

    // Checking the sidedness.
    if (method != "!") {
        if (!handle.exists("side") || handle.childObjType("side") != H5O_TYPE_DATASET) {
            throw std::runtime_error("expected 'side' dataset for an unary logic operation");
        }

        auto shandle = handle.openDataSet("side");
        if (shandle.getSpace().getSimpleExtentNdims() != 0 || shandle.getTypeClass() != H5T_STRING) {
            throw std::runtime_error("'side' should be a scalar string for an unary logic operation");
        }

        std::string side;
        shandle.read(side, shandle.getStrType());
        if (side != "left" && side != "right") {
            throw std::runtime_error(std::string("unrecognized 'side' (") + side + ") for an unary logic operation");
        }

        // Checking the value.
        if (!handle.exists("value") || handle.childObjType("value") != H5O_TYPE_DATASET) {
            throw std::runtime_error("expected 'value' dataset for an unary logic operation");
        }

        auto vhandle = handle.openDataSet("value");
        if (vhandle.getTypeClass() == H5T_STRING) {
            throw std::runtime_error("'value' should contain numeric or boolean values for an unary logic operation");
        }

        validate_missing_placeholder(vhandle, version);

        size_t ndims = vhandle.getSpace().getSimpleExtentNdims();
        if (ndims == 0) {
            // scalar operation.
        } else if (ndims == 1) {
            hsize_t extent;
            vhandle.getSpace().getSimpleExtentDims(&extent);

            // Checking 'along'.
            if (!handle.exists("along") || handle.childObjType("along") != H5O_TYPE_DATASET) {
                throw std::runtime_error("expected 'along' dataset for an unary logic operation");
            }

            auto ahandle = handle.openDataSet("along");
            if (ahandle.getSpace().getSimpleExtentNdims() != 0 || ahandle.getTypeClass() != H5T_INTEGER) {
                throw std::runtime_error("'along' should be a scalar integer for an unary logic operation");
            }

            int along;
            ahandle.read(&along, H5::PredType::NATIVE_INT);
            if (along < 0 || static_cast<size_t>(along) >= seed_details.dimensions.size()) {
                throw std::runtime_error("'along' should be non-negative and less than the dimensionality for an unary logic operation");
            }

            if (extent != seed_details.dimensions[along]) {
                throw std::runtime_error("length of 'value' dataset should be equal to the dimension specified in 'along'");
            }
        } else { 
            throw std::runtime_error("'value' dataset should be scalar or 1-dimensional for an unary logic operation");
        }
    }

    seed_details.type = BOOLEAN;
    return seed_details;
} catch (std::exception& e) {
    throw std::runtime_error("failed to validate unary logic operation at '" + name + "'\n- " + std::string(e.what()));
}

}

#endif
