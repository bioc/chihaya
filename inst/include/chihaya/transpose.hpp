#ifndef CHIHAYA_TRANSPOSE_HPP
#define CHIHAYA_TRANSPOSE_HPP

#include "H5Cpp.h"
#include <stdexcept>
#include <vector>
#include "utils.hpp"

/**
 * @file transpose.hpp
 *
 * @brief Validation for delayed transposition.
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
 * Validate a delayed transposition in a HDF5 file.
 *
 * @param handle An open handle on a HDF5 group representing a transposition.
 * @param name Name of the group inside the file.
 * @param version Version of the **chihaya** specification.
 *
 * @return Details of the transposed object.
 * Otherwise, if the validation failed, an error is raised.
 * 
 * A delayed transposition is represented as a HDF5 group with the following attributes:
 *
 * - `delayed_type` should be a scalar string `"operation"`.
 * - `delayed_operation` should be a scalar string `"transpose"`.
 *
 * Inside the group, we expect:
 *
 * - A `seed` group, containing the object on which the transposition is to be applied.
 *   The `seed` group handle is passed to `validate()` to check its contents recursively and to retrieve the dimensions.
 * - A `permutation` integer 1-dimensional dataset.
 *   Assuming that `seed` has dimensionality `d`, this dataset should have length `d` and contain all integers in `[0, d)`.
 *   This dataset describes the permutation to be applied to the dimensions to create the transposed array.
 *   For example, a `permutation` of `[1, 0]` will transpose a matrix, as the second dimension becomes the first and the first becomes the second.
 *   The exact integer representation is left to the implementation.
 *
 * The type of the output is the same as that of `seed`; only the dimensions are altered.
 */
inline ArrayDetails validate_transpose(const H5::Group& handle, const std::string& name, const Version& version) try {
    if (!handle.exists("seed") || handle.childObjType("seed") != H5O_TYPE_GROUP) {
        throw std::runtime_error("expected 'seed' group for a transpose operation");
    }
    auto seed_details = validate(handle.openGroup("seed"), name + "/seed", version);

    if (!handle.exists("permutation") || handle.childObjType("permutation") != H5O_TYPE_DATASET) {
        throw std::runtime_error("expected 'permutation' dataset for a transpose operation"); 
    }

    auto phandle = handle.openDataSet("permutation");
    if (phandle.getSpace().getSimpleExtentNdims() != 1 || phandle.getTypeClass() != H5T_INTEGER) {
        throw std::runtime_error("'permutation' should be a 1-dimensional integer dataset for a transpose operation");
    }

    hsize_t ndims;
    phandle.getSpace().getSimpleExtentDims(&ndims);
    if (static_cast<size_t>(ndims) != seed_details.dimensions.size()) {
        throw std::runtime_error("length of 'permutation' should match dimensionality of 'seed' for a transpose operation");
    }

    std::vector<int> permutation(ndims);
    phandle.read(permutation.data(), H5::PredType::NATIVE_INT);

    std::vector<size_t> new_dimensions(ndims);
    for (size_t p = 0; p < permutation.size(); ++p) {
        auto current = permutation[p];
        if (current < 0 || static_cast<hsize_t>(current) >= ndims) {
            throw std::runtime_error("'permutation' contains out-of-bounds indices for a transpose operation");
        }
        new_dimensions[p] = seed_details.dimensions[permutation[p]];
    }

    std::sort(permutation.begin(), permutation.end());
    for (size_t p = 0; p < permutation.size(); ++p) {
        if (p != static_cast<size_t>(permutation[p])) {
            throw std::runtime_error("indices in 'permutation' should be unique for a transpose operation");
        }
    }

    seed_details.dimensions = new_dimensions;
    return seed_details;
} catch (std::exception& e) {
    throw std::runtime_error("failed to validate transposition at '" + name + "'\n- " + std::string(e.what()));
}

}

#endif
