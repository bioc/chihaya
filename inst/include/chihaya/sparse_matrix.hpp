#ifndef CHIHAYA_SPARSE_MATRIX_HPP
#define CHIHAYA_SPARSE_MATRIX_HPP

#include "H5Cpp.h"
#include <vector>
#include "utils.hpp"
#include "dimnames.hpp"

/**
 * @file sparse_matrix.hpp
 *
 * @brief Validation for compressed sparse column matrices. 
 */

namespace chihaya {

/**
 * Validate a sparse matrix in a HDF5 file.
 * This is stored in the compressed sparse column layout, using the same notation as the [10X Genomics feature barcode matrices](https://support.10xgenomics.com/single-cell-gene-expression/software/pipelines/latest/advanced/h5_matrices).
 *
 * @param handle An open handle on a HDF5 group representing a sparse matrix.
 * @param name Name of the group inside the file.
 * @param version Version of the **chihaya** specification.
 * 
 * A sparse matrix is represented as a HDF5 group with the following attributes:
 *
 * - `delayed_type` should be a scalar string `"array"`.
 * - `delayed_array` should be a scalar string `"sparse matrix"`.
 *
 * Inside the group, we expect:
 *
 * - A `shape` 1-dimensional integer dataset, containing the dimensions of the matrix.
 *   The first entry represents the number of rows and the second entry represents the number of columns.
 *   The exact integer representation is left to the implementation.
 * - A `data` 1-dimensional dataset, containing the values of the non-zero elements.
 *   This should contain integers or floats, though the exact type representation is left to the implementation.
 * - An `indices` 1-dimensional dataset, containing the row indices for the non-zero elements.
 *   This should have the same length as `data` and should contain integers in `[0, NR)` where `NR` is the number of rows from `shape`.
 *   Entries should be strictly increasing within each column, based on the ranges defined by `indptr`.
 *   The exact integer representation is left to the implementation.
 * - An `indptr` 1-dimensional dataset, containing column pointers into the `indices` vector.
 *   This should have length equal to `NC + 1` where `NC` is the number of columns from `shape`.
 *   The first element should be equal to zero and the last element should be equal to the length of `data`.
 *   The exact integer representation is left to the implementation.
 *
 * The group may also contain:
 *
 * - A `dimnames` group, representing a list (see `ListDetails`) of length equal to the number of dimensions in the `seed`.
 *   Each child entry corresponds to a dimension of `seed` and contains the names along that dimension.
 *   Missing entries indicate that no names are attached to its dimension.
 *   Each (non-missing) entry should be a 1-dimensional string dataset of length equal to the extent of its dimension.
 *   The exact string representation is left to the implementation.
 *
 * If `data` is an integer dataset, it may contain an `is_boolean` attribute.
 * This should be an integer scalar; if non-zero, it indicates that the contents of `data` should be treated as booleans where zeros are falsey and non-zeros are truthy.
 *
 * `data` may contain a `missing_placeholder` attribute.
 * This should be a scalar dataset of the same type class as `data`, specifying the placeholder value used for all missing elements,
 * i.e., any elements in `data` with the same value as the placeholder should be treated as missing.
 * (Note that, for floating-point datasets, the placeholder itself may be NaN, so byte-wise comparison should be used when checking for missingness.)
 */
inline ArrayDetails validate_sparse_matrix(const H5::Group& handle, const std::string& name, const Version& version) try {
    std::vector<int> dims(2);
    {
        auto shandle = check_vector(handle, "shape", "sparse_matrix");
        if (shandle.getTypeClass() != H5T_INTEGER) {
            throw std::runtime_error("'shape' should be integer for a sparse matrix");
        }
        if (vector_length(shandle) != 2) {
            throw std::runtime_error("'shape' should have length 2 for a sparse matrix");
        }
        shandle.read(dims.data(), H5::PredType::NATIVE_INT);

        if (dims[0] < 0 || dims[1] < 0) {
            throw std::runtime_error("'shape' should contain non-negative values for a sparse matrix");
        }
    }

    auto dhandle = check_vector(handle, "data", "sparse matrix");
    size_t ntriplets = vector_length(dhandle);
    ArrayType type;
    if (dhandle.getTypeClass() == H5T_INTEGER) {
        type = INTEGER;
    } else if (dhandle.getTypeClass() == H5T_FLOAT) {
        type = FLOAT;
    } else {
        throw std::runtime_error("unknown type of 'data' for a sparse matrix");
    }
    validate_missing_placeholder(dhandle, version);

    // Checking the properties of the indices.
    auto ihandle = check_vector(handle, "indices", "sparse matrix");
    if (ihandle.getTypeClass() != H5T_INTEGER) {
        throw std::runtime_error("'indices' should be integer for a sparse matrix");
    }
    if (ntriplets != vector_length(ihandle)) {
        throw std::runtime_error("'indices' and 'data' should have the same length for a sparse matrix");
    }

    auto iphandle = check_vector(handle, "indptr", "sparse matrix");
    if (iphandle.getTypeClass() != H5T_INTEGER) {
        throw std::runtime_error("'indptr' should be integer for a sparse matrix");
    }
    if (vector_length(iphandle) != static_cast<size_t>(dims[1] + 1)) {
        throw std::runtime_error("'indptr' should have length equal to the number of columns plus 1 for a sparse matrix");
    }
    std::vector<hsize_t> indptrs(dims[1] + 1);
    iphandle.read(indptrs.data(), H5::PredType::NATIVE_HSIZE);
    if (indptrs[0] != 0) {
        throw std::runtime_error("first entry of 'indptr' should be 0 for a sparse matrix");
    }
    if (indptrs.back() != static_cast<hsize_t>(ntriplets)) {
        throw std::runtime_error("last entry of 'indptr' should be equal to the length of 'data' for a sparse matrix");
    }

    // Validating the indices.
    int nrows = dims[0];
    size_t ncols = dims[1];
    std::vector<int> indices;
    hsize_t full_len = ntriplets;
    H5::DataSpace colspace(1, &full_len);

    for (size_t c = 0; c < ncols; ++c) {
        auto start = indptrs[c];
        auto end = indptrs[c + 1];
        if (start > end) {
            throw std::runtime_error("entries of 'indptr' must be sorted for a sparse matrix");
        }

        hsize_t len = end - start;
        H5::DataSpace memspace(1, &len);
        indices.resize(len);

        hsize_t offset = start;
        colspace.selectHyperslab(H5S_SELECT_SET, &len, &offset);
        ihandle.read(indices.data(), H5::PredType::NATIVE_INT, memspace, colspace);

        // Checking for sortedness and good things.
        int previous = -1;
        for (auto i : indices) {
            if (i < 0) {
                throw std::runtime_error("entries of 'indices' should be non-negative for a sparse matrix");
            }
            if (i <= previous) {
                throw std::runtime_error("'indices' should be strictly increasing within each column for a sparse matrix");
            }
            if (i >= nrows) {
                throw std::runtime_error("entries of 'indices' should be less than the number of rows for a sparse matrix");
            }
            previous = i;
        }
    }

    // Validating dimnames.
    if (handle.exists("dimnames")) {
        validate_dimnames(handle, dims, "sparse matrix", version);
    }

    // Check if it's boolean.
    if (is_boolean(dhandle)) {
        type = BOOLEAN;
    }

    return ArrayDetails(type, std::vector<size_t>(dims.begin(), dims.end()));
} catch (std::exception& e) {
    throw std::runtime_error("failed to validate sparse matrix at '" + name + "'\n- " + std::string(e.what()));
}

}

#endif
