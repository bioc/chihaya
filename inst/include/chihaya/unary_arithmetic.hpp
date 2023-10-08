#ifndef CHIHAYA_UNARY_ARITHMETIC_HPP
#define CHIHAYA_UNARY_ARITHMETIC_HPP

#include "H5Cpp.h"
#include <stdexcept>
#include <vector>
#include <algorithm>
#include "utils.hpp"

/**
 * @file unary_arithmetic.hpp
 *
 * @brief Validation for delayed unary arithmetic operations.
 */

namespace chihaya {

/**
 * @cond
 */
inline ArrayDetails validate(const H5::Group& handle, const std::string&, const Version&);

inline bool valid_arithmetic(const std::string& method) {
    return (method == "+" ||
        method == "-" ||
        method == "/" ||
        method == "*" || 
        method == "%/%" ||
        method == "^" ||
        method == "%%");
}

inline ArrayType determine_arithmetic_type(const ArrayType& first, const ArrayType& second, const std::string& method) {
    if (method == "/") {
        return FLOAT;
    } else if (method == "%/%") {
        return INTEGER;
    } else if (method == "%%") {
        if (first <= INTEGER && second <= INTEGER) {
            return INTEGER;
        } else {
            return FLOAT;
        }
    } else if (first == BOOLEAN && second == BOOLEAN) {
        return INTEGER;
    } else {
        return std::max(first, second);
    }
}
/**
 * @endcond
 */

/**
 * Validate delayed unary arithmetic operations in a HDF5 file.
 * This is "unary" in the sense that only one delayed object is involved.
 *
 * @param handle An open handle on a HDF5 group representing an unary arithmetic operation.
 * @param name Name of the group inside the file.
 * @param version Version of the **chihaya** specification.
 *
 * @return Details of the object after applying the arithmetic operation.
 * Otherwise, if the validation failed, an error is raised.
 * 
 * An unary arithmetic operation is represented as a HDF5 group with the following attributes:
 *
 * - `delayed_type` should be a scalar string `"operation"`.
 * - `delayed_operation` should be a scalar string `"unary arithmetic"`.
 *
 * Inside the group, we expect:
 *
 * - A `seed` group, containing a delayed object for which arithmetic is to be applied.
 *   The `seed` group handle is passed to `validate()` to check its contents recursively and to retrieve the dimensions.
 *   This can be either boolean, integer or float.
 * - A `method` string scalar dataset, specifying the arithmetic method to use.
 *   This can be any one of `+`, `-`, `/`, `*`, `^`, `%%` (modulo) or `%/%` (integer division).
 *   The exact string representation is left to the implementation.
 * - A `side` string scalar dataset, describing the side of the `seed` object to apply the operation.
 *   This can be `"left"`, when `value` is applied to the left of `seed`, e.g., `value - seed`;
 *   or `"right"`, when `value` is applied to the right of `seed`, e.g., `seed - value`.
 *   For `+` and `-` as pure unary methods, this may also be `"none"`.
 *   The exact string representation is left to the implementation.
 *
 * For `side != "none"`, we also expect:
 *
 * - A `value` dataset.
 *   This can be either boolean, integer or float, and may be scalar or 1-dimensional.
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
 * The type of the output object depends on the operation, the type of `seed` and the type of `value`:
 *
 * - For simple division, the output type is always float.
 * - For integer division, the output type is always integer.
 * - For modulo where both `seed` and `value` are integer, the output type is integer.
 * - In all other cases, the output type is the more advanced type of `seed` and `value`. 
 *
 * Note that any boolean types in `seed` and `value` are first promoted to integer before type determination.
 */
inline ArrayDetails validate_unary_arithmetic(const H5::Group& handle, const std::string& name, const Version& version) try {
    if (!handle.exists("seed") || handle.childObjType("seed") != H5O_TYPE_GROUP) {
        throw std::runtime_error("expected 'seed' group for an unary arithmetic operation");
    }

    auto seed_details = validate(handle.openGroup("seed"), name + "/seed", version);
    if (seed_details.type == STRING) {
        throw std::runtime_error("type of 'seed' should be numeric or boolean for an unary arithmetic operation");
    }

    // Checking the method.
    if (!handle.exists("method") || handle.childObjType("method") != H5O_TYPE_DATASET) {
        throw std::runtime_error("expected 'method' dataset for an unary arithmetic operation");
    }

    auto mhandle = handle.openDataSet("method");
    if (mhandle.getSpace().getSimpleExtentNdims() != 0 || mhandle.getTypeClass() != H5T_STRING) {
        throw std::runtime_error("'method' should be a scalar string for an unary arithmetic operation");
    }

    std::string method;
    mhandle.read(method, mhandle.getStrType());
    if (!valid_arithmetic(method)) {
        throw std::runtime_error(std::string("unrecognized 'method' (") + method + ") for an unary arithmetic operation");
    }

    // Checking the sidedness.
    if (!handle.exists("side") || handle.childObjType("side") != H5O_TYPE_DATASET) {
        throw std::runtime_error("expected 'side' dataset for an unary arithmetic operation");
    }

    auto shandle = handle.openDataSet("side");
    if (shandle.getSpace().getSimpleExtentNdims() != 0 || shandle.getTypeClass() != H5T_STRING) {
        throw std::runtime_error("'side' should be a scalar string for an unary arithmetic operation");
    }

    std::string side;
    shandle.read(side, shandle.getStrType());
    if (side == "none") {
        if (method != "+" && method != "-") {
            throw std::runtime_error(std::string("'side' cannot be 'none' for method '") + method + "' for an unary arithmetic operation");
        } 
    } else if (side != "left" && side != "right") {
        throw std::runtime_error(std::string("unrecognized 'side' (") + side + ") for an unary arithmetic operation");
    }

    ArrayType min_type = INTEGER;
    if (side != "none") {
        // Checking the value.
        if (!handle.exists("value") || handle.childObjType("value") != H5O_TYPE_DATASET) {
            throw std::runtime_error("expected 'value' dataset for an unary arithmetic operation");
        }

        auto vhandle = handle.openDataSet("value");
        if (vhandle.getTypeClass() == H5T_STRING) {
            throw std::runtime_error("'value' dataset should be numeric or boolean for an unary arithmetic operation");
        } else if (vhandle.getTypeClass() == H5T_FLOAT) {
            min_type = FLOAT;
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
                throw std::runtime_error("expected 'along' dataset for an unary arithmetic operation");
            }

            auto ahandle = handle.openDataSet("along");
            if (ahandle.getSpace().getSimpleExtentNdims() != 0 || ahandle.getTypeClass() != H5T_INTEGER) {
                throw std::runtime_error("'along' should be a scalar integer for an unary arithmetic operation");
            }

            int along;
            ahandle.read(&along, H5::PredType::NATIVE_INT);
            if (along < 0 || static_cast<size_t>(along) >= seed_details.dimensions.size()) {
                throw std::runtime_error("'along' should be non-negative and less than the dimensionality for an unary arithmetic operation");
            }

            if (extent != seed_details.dimensions[along]) {
                throw std::runtime_error("length of 'value' dataset should be equal to the dimension specified in 'along'");
            }
        } else { 
            throw std::runtime_error("'value' dataset should be scalar or 1-dimensional for an unary arithmetic operation");
        }
    }

    // Determining type promotion rules.
    seed_details.type = determine_arithmetic_type(min_type, seed_details.type, method);

    return seed_details;
} catch (std::exception& e) {
    throw std::runtime_error("failed to validate unary arithmetic operation at '" + name + "'\n- " + std::string(e.what()));
}

}

#endif
