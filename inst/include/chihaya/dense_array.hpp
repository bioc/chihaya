#ifndef CHIHAYA_DENSE_ARRAY_HPP
#define CHIHAYA_DENSE_ARRAY_HPP

#include "H5Cpp.h"
#include <vector>
#include "utils.hpp"
#include "dimnames.hpp"

/**
 * @file dense_array.hpp
 *
 * @brief Dense array, stored inside the file.
 */

namespace chihaya {

/**
 * Validate a dense array in a HDF5 file.
 *
 * @param handle An open handle on a HDF5 group representing a dense array.
 * @param name Name of the group inside the file.
 * @param version Version of the **chihaya** specification.
 *
 * @return Details of the dense array.
 * Otherwise, if the validation failed, an error is raised.
 * 
 * A dense array is represented as a HDF5 group with the following attributes:
 *
 * - `delayed_type` should be a scalar string `"array"`.
 * - `delayed_array` should be a scalar string `"dense array"`.
 *
 * Inside the group, we expect:
 *
 * - A `data` dataset, containing the array data.
 *   This should have a non-zero number of dimensions (i.e., not scalar) and contain integers, floats or strings.
 *   The exact type representation is left to the implementation.
 * - A `native` scalar integer dataset, to be interpreted as a boolean.
 *   This specifies whether the dimensions of the dense array are sorted from slowest-changing (first) to fastest-changing (last).
 *   If true, the dimensions of the `data` dataset are in the same order as the dimensions of the dense array.
 *   If false, the dimensions are saved in reverse order, i.e., the first dimension of the dense array is the last dimension of the `data` dataset.
 *
 * Setting `native = 0` is frequently done for efficiency when the in-memory dense array has a different layout than the on-disk HDF5 dataset.
 * For example, Fortran, R and Julia use column-major order for their matrices, while C code (and HDF5) would typically use row-major order.
 * By setting `native = 0`, we avoid the need to reorganize the data when reading/writing from file;
 * however, this means that the dimensions reported by HDF5 need to be reversed to obtain the dimensions of the delayed object.
 *
 * The group may also contain:
 *
 * - A `dimnames` group, representing a list (see `ListDetails`) of length equal to the number of dimensions in the dense array.
 *   Each child entry corresponds to a dimension of the dense array and contains the names along that dimension.
 *   The absence of a child entry indicates that no names are attached to the corresponding dimension.
 *   Each (non-absent) entry should be a 1-dimensional string dataset of length equal to the extent of its dimension.
 *   The exact string representation is left to the implementation.
 *
 * Note that the ordering of `dimnames` is unrelated to the setting of `native`.
 * For example, entry 0 always corresponds to the first dimension of the dense array, regardless of how it is saved in `data`.
 *
 * If `data` is an integer dataset, it may contain an `is_boolean` attribute.
 * This should be an integer scalar; if non-zero, it indicates that the contents of `data` should be treated as booleans where zeros are falsey and non-zeros are truthy.
 *
 * `data` may contain a `missing_placeholder` attribute.
 * This should be a scalar dataset of the same type class as `data`, specifying the placeholder value used for all missing elements,
 * i.e., any elements in `data` with the same value as the placeholder should be treated as missing.
 * (Note that, for floating-point datasets, the placeholder itself may be NaN, so byte-wise comparison should be used when checking for missingness.)
 */
inline ArrayDetails validate_dense_array(const H5::Group& handle, const std::string& name, const Version& version) try {
    // Check for a 'data' group.
    if (!handle.exists("data") || handle.childObjType("data") != H5O_TYPE_DATASET) {
        throw std::runtime_error("'data' should be a dataset for a dense array");
    }

    auto dhandle = handle.openDataSet("data");
    auto ndims = dhandle.getSpace().getSimpleExtentNdims();
    if (ndims == 0) {
        throw std::runtime_error("'data' should have non-zero dimensions for a dense array");
    }
    validate_missing_placeholder(dhandle, version);

    ArrayDetails output;
    std::vector<hsize_t> dims(ndims);
    dhandle.getSpace().getSimpleExtentDims(dims.data());
    output.dimensions.insert(output.dimensions.end(), dims.begin(), dims.end());

    auto cls = dhandle.getTypeClass();
    if (cls == H5T_INTEGER) {
        output.type = INTEGER;
    } else if (cls == H5T_FLOAT) {
        output.type = FLOAT;
    } else if (cls == H5T_STRING) {
        output.type = STRING;
    } else {
        throw std::runtime_error("unrecognized type of 'data' for a dense array");
    }

    // Check for native.
    if (!handle.exists("native") || handle.childObjType("native") != H5O_TYPE_DATASET) {
        throw std::runtime_error("'native' should be a dataset for a dense array");
    }

    auto nhandle = handle.openDataSet("native");
    if (nhandle.getSpace().getSimpleExtentNdims() != 0 || nhandle.getTypeClass() != H5T_INTEGER) {
        throw std::runtime_error("'native' should be an integer scalar for a dense array");
    }

    int native;
    nhandle.read(&native, H5::PredType::NATIVE_INT);
    if (!native) {
        std::reverse(output.dimensions.begin(), output.dimensions.end());
    }

    // Validating dimnames.
    if (handle.exists("dimnames")) {
        validate_dimnames(handle, output.dimensions, "dense array", version);
    }

    // Check if it's boolean.
    if (is_boolean(dhandle)) {
        output.type = BOOLEAN;
    }

    return output;
} catch (std::exception& e) {
    throw std::runtime_error("failed to validate dense array at '" + name + "'\n- " + std::string(e.what()));
}

}

#endif
