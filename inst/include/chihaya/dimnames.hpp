#ifndef CHIHAYA_DIMNAMES_HPP
#define CHIHAYA_DIMNAMES_HPP

#include "H5Cpp.h"
#include <stdexcept>
#include <vector>
#include "list.hpp"
#include "utils.hpp"

/**
 * @file dimnames.hpp
 *
 * @brief Validation for delayed dimnames assignment.
 */

namespace chihaya {

/**
 * @cond
 */
inline ArrayDetails validate(const H5::Group& handle, const std::string&, const Version&);

template<class V>
void validate_dimnames(const H5::Group& handle, const V& dimensions, const std::string& message, const Version& version) {
    if (!handle.exists("dimnames") || handle.childObjType("dimnames") != H5O_TYPE_GROUP) {
        throw std::runtime_error(std::string("expected 'dimnames' group for a ") + message);
    }

    auto dhandle = handle.openGroup("dimnames");
    ListDetails list_params;
    
    try {
        list_params = validate_list(dhandle, version);
    } catch (std::exception& e) {
        throw std::runtime_error(std::string("failed to load 'dimnames' list for a ") + message + std::string(":\n  ") + e.what());
    }

    if (list_params.length != dimensions.size()) {
        throw std::runtime_error(std::string("length of 'dimnames' list should be equal to seed dimensionality for a ") + message);
    }

    for (const auto& p : list_params.present) {
        if (dhandle.childObjType(p.second) != H5O_TYPE_DATASET) {
            throw std::runtime_error(std::string("each entry of 'dimnames' should be a dataset for a ") + message);
        }

        auto current = dhandle.openDataSet(p.second);
        if (current.getSpace().getSimpleExtentNdims() != 1 || current.getTypeClass() != H5T_STRING) {
            throw std::runtime_error(std::string("each entry of 'dimnames' should be a 1-dimensional string dataset for a ") + message);
        }

        hsize_t dim;
        current.getSpace().getSimpleExtentDims(&dim);
        if (dim != static_cast<hsize_t>(dimensions[p.first])) {
            throw std::runtime_error(std::string("each entry of 'dimnames' should have length equal to the extent of its corresponding dimension for a ") + message);
        }
    }
}
/**
 * @endcond
 */

/**
 * Validate delayed dimnames assignment in a HDF5 file.
 *
 * @param handle An open handle on a HDF5 group representing a dimnames assignment operation.
 * @param name Name of the group inside the file.
 * @param version Version of the **chihaya** specification.
 *
 * @return Details of the object after assigning dimnames.
 * Otherwise, if the validation failed, an error is raised.
 * 
 * A delayed subsetting operation is represented as a HDF5 group with the following attributes:
 *
 * - `delayed_type` should be a scalar string `"operation"`.
 * - `delayed_operation` should be a scalar string `"dimnames"`.
 *
 * Inside the group, we expect:
 *
 * - A `seed` group, containing a delayed object for which dimnames are to be assigned.
 *   The `seed` group handle is passed to `validate()` to check its contents recursively and to retrieve the dimensions.
 * - A `dimnames` group, representing a list (see `ListDetails`) of length equal to the number of dimensions in the `seed`.
 *   Each child entry corresponds to a dimension of `seed` and contains the names along that dimension.
 *   Each entry should be a 1-dimensional string dataset of length equal to the extent of its dimension.
 *   The exact string representation and encoding is left to the implementation.
 *   If a dataset is absent, no names are attached to the corresponding dimension.
 *   It is assumed that each string in each dataset is not missing (i.e., the placeholders described in `validate_dense_array()` should not be used here).
 */
inline ArrayDetails validate_dimnames(const H5::Group& handle, const std::string& name, const Version& version) try {
    if (!handle.exists("seed") || handle.childObjType("seed") != H5O_TYPE_GROUP) {
        throw std::runtime_error("expected 'seed' group for a dimnames assignment");
    }
    auto seed_details = validate(handle.openGroup("seed"), name + "/seed", version);
    validate_dimnames(handle, seed_details.dimensions, "dimnames assignment", version);
    return seed_details;
} catch (std::exception& e) {
    throw std::runtime_error("failed to validate dimnames operation at '" + name + "'\n- " + std::string(e.what()));
}


}

#endif
