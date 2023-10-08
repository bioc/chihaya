#ifndef CHIHAYA_SUBSET_HPP
#define CHIHAYA_SUBSET_HPP

#include "H5Cpp.h"
#include <stdexcept>
#include <vector>
#include "list.hpp"
#include "utils.hpp"

/**
 * @file subset.hpp
 *
 * @brief Validation for delayed subsetting operations.
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
 * Validate a delayed subsetting operation in a HDF5 file.
 *
 * @param handle An open handle on a HDF5 group representing a subset operation.
 * @param name Name of the group inside the file.
 *
 * @return Details of the subsetted object.
 * Otherwise, if the validation failed, an error is raised.
 * 
 * A delayed subsetting operation is represented as a HDF5 group with the following attributes:
 *
 * - `delayed_type` should be a scalar string `"operation"`.
 * - `delayed_operation` should be a scalar string `"subset"`.
 *
 * Inside the group, we expect:
 *
 * - A `seed` group, containing the delayed object to be subsetted. 
 *   This can be an array or anothed delayed operation.
 *   The `seed` group handle is passed to `validate()` to check its contents recursively and to retrieve the dimensions.
 * - An `index` group, representing a list (see `ListDetails`) of length equal to the number of dimensions in the `seed`.
 *   Each child entry is named after a dimension of `seed` and contains the indices of interest along that dimension.
 *   Each entry should be a 1-dimensional integer dataset containing 0-based indices that are less than the extent of its dimension.
 *   The exact integer representation is left to the implementation.
 *   The absence of an entry for a dimension indicates that no subsetting is to be performed, i.e., the full extent of that dimension is present in the result object.
 *
 * The type of the output object is the same as that of the `seed`; only the dimensions are changed.
 */
inline ArrayDetails validate_subset(const H5::Group& handle, const std::string& name, const Version& version) try {
    if (!handle.exists("seed") || handle.childObjType("seed") != H5O_TYPE_GROUP) {
        throw std::runtime_error("expected 'seed' group for a subset operation");
    }
    auto seed_details = validate(handle.openGroup("seed"), name + "/seed", version);

    if (!handle.exists("index") || handle.childObjType("index") != H5O_TYPE_GROUP) {
        throw std::runtime_error("expected 'index' group for a subset operation"); 
    }

    auto ihandle = handle.openGroup("index");
    ListDetails list_params;
    
    try {
        list_params = validate_list(ihandle, version);
    } catch (std::exception& e) {
        throw std::runtime_error(std::string("failed to load 'index' list for a subset operation:\n  ") + e.what());
    }

    auto& seed_dims = seed_details.dimensions;
    if (list_params.length != seed_dims.size()) {
        throw std::runtime_error("length of 'index' should be equal to number of dimensions in 'seed' for a subset operation");
    }

    for (const auto& p : list_params.present) {
        if (ihandle.childObjType(p.second) != H5O_TYPE_DATASET) {
            throw std::runtime_error("each child of 'index' should be a dataset for a subset operation");
        }

        auto dhandle = ihandle.openDataSet(p.second);
        if (dhandle.getTypeClass() != H5T_INTEGER || dhandle.getSpace().getSimpleExtentNdims() != 1) {
            throw std::runtime_error("each child of 'index' should be a 1-dimensional integer dataset for a subset operation");
        }
        hsize_t len;
        dhandle.getSpace().getSimpleExtentDims(&len);

        std::vector<int> buffer(len);
        dhandle.read(buffer.data(), H5::PredType::NATIVE_INT); 

        int upper_limit = seed_dims[p.first];
        for (auto b : buffer) {
            if (b < 0 || b >= upper_limit) {
                throw std::runtime_error(std::string("indices out of range for element '") + p.second + "' in 'index' for a subset operation");
            }
        }

        seed_dims[p.first] = buffer.size();
    }

    return seed_details;
} catch (std::exception& e) {
    throw std::runtime_error("failed to validate subset operation at '" + name + "'\n- " + std::string(e.what()));
}

}

#endif
