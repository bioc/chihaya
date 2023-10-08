#ifndef CHIHAYA_UNARY_MATH_HPP
#define CHIHAYA_UNARY_MATH_HPP

#include "H5Cpp.h"
#include <stdexcept>
#include <vector>
#include <algorithm>
#include "utils.hpp"

/**
 * @file unary_math.hpp
 *
 * @brief Validation for delayed unary math operations.
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
 * Validate delayed unary math operations in a HDF5 file.
 * This is "unary" in the sense that only one delayed object is involved.
 *
 * @param handle An open handle on a HDF5 group representing an unary math operation.
 * @param name Name of the group inside the file.
 * @param version Version of the **chihaya** specification.
 *
 * @return Details of the object after applying the mathal operation.
 * Otherwise, if the validation failed, an error is raised.
 * 
 * A delayed unary math operation is represented as a HDF5 group with the following attributes:
 *
 * - `delayed_type` should be a scalar string `"operation"`.
 * - `delayed_operation` should be a scalar string `"unary math"`.
 *
 * Inside the group, we expect:
 *
 * - A `seed` group, containing a delayed object for which the mathematical operation is to be applied.
 *   The `seed` group handle is passed to `validate()` to check its contents recursively and to retrieve the dimensions.
 *   This can be boolean (coerced to integer), integer or float.
 * - A `method` string scalar dataset, specifying the mathematical operation to perform.
 *   This can be any one of:
 *   - `abs`, absolute value.
 *   - `log1p`, log-transformation after adding 1.
 *   - `sqrt`, square root.
 *   - `exp`, exponential.
 *   - `expm1`, exponential followed by subtraction of 1.
 *   - `sign`, sign of the value (-1, 0, or 1).
 *   - `ceiling`, ceiling.
 *   - `floor`, floor.
 *   - `trunc`, truncate to zero.
 *   - `cos`, `sin`, `tan`, `acos`, `asin`, `atan`; the usual trigonometric functions (angles in radians).
 *   - `cosh`, `sinh`, `tanh`, `acosh`, `asinh`, `atanh`; the usual hyperbolic functions.
 *   - `log`, a log-transformation.
 *   - `round`, rounding to a certain number of decimal places.
 *   - `signif`, rounding to a certain number of significant digits.
 *   The exact string representation is left to the implementation.
 *
 * If `method = "log"`, we may contain:
 *
 * - `base`, a scalar float dataset containing the base of the log-transformation.
 *   If this is absent, it is assumed that the natural base will be used.
 *   The exact float representation is left to the implementation.
 *
 * If `method = "round"` or `"signif"`, we expect:
 *
 * - `digits`, a scalar integer dataset specifying the number of digits to report.
 *   The exact integer representation is left to the implementation.
 *
 * The type of the output object is usually float.
 * The only exceptions are for `abs`, which is either integer or float depending on the input (booleans are promoted to integer);
 * and `sign`, which is always integer.
 */
inline ArrayDetails validate_unary_math(const H5::Group& handle, const std::string& name, const Version& version) try {
    if (!handle.exists("seed") || handle.childObjType("seed") != H5O_TYPE_GROUP) {
        throw std::runtime_error("expected 'seed' group for an unary math operation");
    }

    auto seed_details = validate(handle.openGroup("seed"), name + "/seed", version);

    if (seed_details.type == STRING) {
        throw std::runtime_error("'seed' should contain numeric or boolean values for an unary math operation");
    }

    // Checking the method.
    if (!handle.exists("method") || handle.childObjType("method") != H5O_TYPE_DATASET) {
        throw std::runtime_error("expected 'method' dataset for an unary math operation");
    }

    auto mhandle = handle.openDataSet("method");
    if (mhandle.getSpace().getSimpleExtentNdims() != 0 || mhandle.getTypeClass() != H5T_STRING) {
        throw std::runtime_error("'method' should be a scalar string for an unary math operation");
    }

    std::string method;
    mhandle.read(method, mhandle.getStrType());

    if (method == "sign") {
        seed_details.type = INTEGER;

    } else if (method == "abs") {
        seed_details.type = std::max(seed_details.type, INTEGER);

    } else if (
        method == "log1p" ||
        method == "sqrt" ||
        method == "exp" ||
        method == "expm1" ||
        method == "ceiling" ||
        method == "floor" || 
        method == "trunc" ||
        method == "sin" ||
        method == "cos" ||
        method == "tan" ||
        method == "acos" ||
        method == "asin" ||
        method == "atan" ||
        method == "sinh" ||
        method == "cosh" ||
        method == "tanh" ||
        method == "acosh" ||
        method == "asinh" ||
        method == "atanh")
    {
        seed_details.type = FLOAT;

    } else if (method == "log") {
        if (handle.exists("base")) {
            if (handle.childObjType("base") != H5O_TYPE_DATASET) {
                throw std::runtime_error("expected 'base' to be a dataset for a log transformation");
            }

            auto vhandle = handle.openDataSet("base");
            if (vhandle.getSpace().getSimpleExtentNdims() != 0 || vhandle.getTypeClass() != H5T_FLOAT) {
                throw std::runtime_error("'base' should be a scalar float for a log transformation");
            }
        }
        seed_details.type = FLOAT;

    } else if (method == "round" || method == "signif") {
        if (!handle.exists("digits") || handle.childObjType("digits") != H5O_TYPE_DATASET) {
            throw std::runtime_error("expected 'digits' dataset for a rounding operation");
        }
        auto vhandle = handle.openDataSet("digits");
        if (vhandle.getSpace().getSimpleExtentNdims() != 0 || vhandle.getTypeClass() != H5T_INTEGER) {
            throw std::runtime_error("'digits' should be a scalar integer for a log transformation");
        }
        seed_details.type = FLOAT;

    } else {
        throw std::runtime_error(std::string("unrecognized 'method' (") + method + ") for an unary math operation");
    }

    return seed_details;
} catch (std::exception& e) {
    throw std::runtime_error("failed to validate unary math operation at '" + name + "'\n- " + std::string(e.what()));
}

}

#endif
