#ifndef CHIHAYA_VALIDATE_HPP
#define CHIHAYA_VALIDATE_HPP

#include "subset.hpp"
#include "combine.hpp"
#include "transpose.hpp"

#include "dense_array.hpp"
#include "sparse_matrix.hpp"
#include "external_hdf5.hpp"
#include "custom_array.hpp"
#include "constant_array.hpp"

#include "dimnames.hpp"
#include "subset_assignment.hpp"

#include "unary_arithmetic.hpp"
#include "unary_comparison.hpp"
#include "unary_logic.hpp"
#include "unary_math.hpp"
#include "unary_special_check.hpp"

#include "binary_arithmetic.hpp"
#include "binary_comparison.hpp"
#include "binary_logic.hpp"

#include "matrix_product.hpp"
#include "utils.hpp"

/**
 * @file validate.hpp
 *
 * @brief Main validation function.
 */

namespace chihaya {

/**
 * Validate a delayed operation/array at the specified HDF5 group.
 * 
 * @param handle Open handle to a HDF5 group corresponding to a delayed operation or array.
 * @param name Name of the group inside the file, to be used for meaningful error messages.
 * @param version Version of the **chihaya** specification.
 *
 * @return Details of the array after all delayed operations in `handle` (and its children) have been applied.
 *
 * A "delayed object" is stored in the file as a HDF5 group with the `delayed_type` string attribute.
 * The value of this attribute can either be:
 *
 * - `"operation"`, in which case the object represents a delayed operation that is applied to a simpler (nested) "seed" object.
 *   This requires an additional `delayed_operation` string attribute to specify the type of operation.
 * - `"array"`, in which case the object represents an array prior to any application of operations.
 *   This requires an additional `delayed_array` string attribute to specify the type of array.
 *
 * In all cases, the exact string representation is left to the implementation.
 */
inline ArrayDetails validate(const H5::Group& handle, const std::string& name, const Version& version) {
    auto dtype = load_string_attribute(handle, "delayed_type", " for a delayed object");
    ArrayDetails output;

    if (dtype == "array") {
        try {
            auto atype = load_string_attribute(handle, "delayed_array", " for an array");

            // Checking external.
            if (atype == "dense array") {
                output = validate_dense_array(handle, name, version);
            } else if (atype == "sparse matrix") {
                output = validate_sparse_matrix(handle, name, version);
            } else if (atype.rfind("custom ", 0) != std::string::npos) {
                output = validate_custom_array(handle, name, version);
            } else if (atype.rfind("external hdf5 ", 0) != std::string::npos) {
                output = validate_external_hdf5(handle, name, version);
            } else if (atype == "constant array") {
                output = validate_constant_array(handle, name, version);
            } else {
                throw std::runtime_error(std::string("unknown array type '") + atype + "' at '" + name + "'");
            }

        } catch (std::exception& e) {
            throw std::runtime_error(std::string("failed to validate delayed array at '") + name + "':\n  " + e.what());
        }

    } else if (dtype == "operation") {
        try {
            auto otype = load_string_attribute(handle, "delayed_operation", " for an operation");

            // Checking subset.
            if (otype == "subset") {
                output = validate_subset(handle, name, version);
            } else if (otype == "combine") {
                output = validate_combine(handle, name, version);
            } else if (otype == "transpose") {
                output = validate_transpose(handle, name, version);
            } else if (otype == "dimnames") {
                output = validate_dimnames(handle, name, version);
            } else if (otype == "subset assignment") {
                output = validate_subset_assignment(handle, name, version);
            } else if (otype == "unary arithmetic") {
                output = validate_unary_arithmetic(handle, name, version);
            } else if (otype == "unary comparison") {
                output = validate_unary_comparison(handle, name, version);
            } else if (otype == "unary logic") {
                output = validate_unary_logic(handle, name, version);
            } else if (otype == "unary math") {
                output = validate_unary_math(handle, name, version);
            } else if (otype == "unary special check") {
                output = validate_unary_special_check(handle, name, version);
            } else if (otype == "binary arithmetic") {
                output = validate_binary_arithmetic(handle, name, version);
            } else if (otype == "binary comparison") {
                output = validate_binary_comparison(handle, name, version);
            } else if (otype == "binary logic") {
                output = validate_binary_logic(handle, name, version);
            } else if (otype == "matrix product") {
                output = validate_matrix_product(handle, name, version);
            } else {
                throw std::runtime_error(std::string("unknown operation type '") + otype + "' at '" + name + "'");
            }

        } catch (std::exception& e) {
            throw std::runtime_error(std::string("failed to validate delayed operation at '") + name + "':\n  " + e.what());
        }

    } else {
        throw std::runtime_error(std::string("unknown type '") + dtype + "' at '" + name + "'");
    }

    return output;
}

/**
 * Validate a delayed operation/array at the specified HDF5 group,
 * using a version string in the `delayed_version` attribute of the `handle`.
 * This should be a string of the form `<MAJOR>.<MINOR>.<PATCH>`;
 * if missing, this defaults to `0.0.0`.
 * 
 * @param handle Open handle to a HDF5 group corresponding to a delayed operation or array.
 * @param name Name of the group inside the file, to be used for meaningful error messages.
 *
 * @return Details of the array after all delayed operations in `handle` (and its children) have been applied.
 */
inline ArrayDetails validate(const H5::Group& handle, const std::string& name) {
    Version version;
    if (handle.attrExists("delayed_version")) {
        auto ahandle = handle.openAttribute("delayed_version");
        auto vstring = load_string_attribute(ahandle, "delayed_version");
        version = parse_version_string(vstring);
    }
    return validate(handle, name, version);
}

/**
 * Validate a delayed operation/array at the specified HDF5 group.
 * This simply calls the `validate()` overload for a `H5::Group`; the specification version is taken from the `delayed_version` attribute.
 * 
 * @param path Path to a HDF5 file.
 * @param name Name of the group inside the file, to be used for meaningful error messages.
 *
 * @return Details of the array after all delayed operations in `handle` (and its children) have been applied.
 */
inline ArrayDetails validate(const std::string& path, std::string name) {
    H5::H5File handle(path, H5F_ACC_RDONLY);
    auto ghandle = handle.openGroup(name);
    return validate(ghandle, name);
}

}

#endif
