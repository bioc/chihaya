#ifndef CHIHAYA_SUBSET_ASSIGNMENT_HPP
#define CHIHAYA_SUBSET_ASSIGNMENT_HPP

#include "H5Cpp.h"
#include <stdexcept>
#include <vector>
#include "list.hpp"
#include "utils.hpp"

/**
 * @file subset_assignment.hpp
 *
 * @brief Validation for delayed subset assignment.
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
 * Validate a delayed subset assignment operation in a HDF5 file.
 *
 * @param handle An open handle on a HDF5 group representing a subset assignment.
 * @param name Name of the group inside the file.
 * @param version Version of the **chihaya** specification.
 *
 * @return Details of the object after subset assignment.
 * Otherwise, if the validation failed, an error is raised.
 * 
 * A delayed subset assignment is represented as a HDF5 group with the following attributes:
 *
 * - `delayed_type` should be a scalar string `"operation"`.
 * - `delayed_operation` should be a scalar string `"subset assignment"`.
 *
 * Inside the group, we expect:
 *
 * - A `seed` group, containing the object to be subjected to subset assignment.
 *   The `seed` group handle is passed to `validate()` to check its contents recursively and to retrieve the dimensions.
 * - A `value` group, containing the object supplying the replacement values for subset assignment.
 *   The `value` group handle is passed to `validate()` to check its contents recursively and to retrieve the dimensions.
 * - An `index` group, representing a list (see `ListDetails`) of length equal to the number of dimensions in the `seed`.
 *   Each child entry is named after a dimension of `seed` and contains the indices to be replaced along that dimension.
 *   Each entry should be a 1-dimensional integer dataset containing 0-based indices less than the extent of the corresponding dimension of `seed`;
 *   the length of this dataset should be equal to the extent of the corresponding dimension of `value`.
 *   The exact integer representation is left to the implementation.
 *   The absence of an entry for a dimension indicates that the full extent of that dimension is to be replaced.
 *
 * The type of the object is defined as the more advanced type of `seed` and `value`.
 * For example, if `seed` is `INTEGER` and `value` is `FLOAT`, the output object will be promoted to `FLOAT`.
 */
inline ArrayDetails validate_subset_assignment(const H5::Group& handle, const std::string& name, const Version& version) try {
    if (!handle.exists("seed") || handle.childObjType("seed") != H5O_TYPE_GROUP) {
        throw std::runtime_error("expected 'seed' group for a subset assignment");
    }
    auto seed_details = validate(handle.openGroup("seed"), name + "/seed", version);

    if (!handle.exists("value") || handle.childObjType("value") != H5O_TYPE_GROUP) {
        throw std::runtime_error("expected 'value' group for a subset assignment");
    }
    auto value_details = validate(handle.openGroup("value"), name + "/value", version);

    if (seed_details.dimensions.size() != value_details.dimensions.size()) {
        throw std::runtime_error("'seed' and 'value' arrays should have the same dimensionalities");
    }

    if (!handle.exists("index") || handle.childObjType("index") != H5O_TYPE_GROUP) {
        throw std::runtime_error("expected 'index' group for a subset assignment"); 
    }

    auto ihandle = handle.openGroup("index");
    ListDetails list_params;
    
    try {
        list_params = validate_list(ihandle, version);
    } catch (std::exception& e) {
        throw std::runtime_error(std::string("failed to load 'index' list for a subset assignment:\n  ") + e.what());
    }

    const auto& seed_dims = seed_details.dimensions;
    if (list_params.length != seed_dims.size()) {
        throw std::runtime_error("length of 'index' should be equal to number of dimensions in 'seed' for a subset assignment");
    }

    for (const auto& p : list_params.present) {
        if (ihandle.childObjType(p.second) != H5O_TYPE_DATASET) {
            throw std::runtime_error("each child of 'index' should be a dataset for a subset assignment");
        }

        auto dhandle = ihandle.openDataSet(p.second);
        if (dhandle.getTypeClass() != H5T_INTEGER || dhandle.getSpace().getSimpleExtentNdims() != 1) {
            throw std::runtime_error("each child of 'index' should be a 1-dimensional integer dataset for a subset assignment");
        }

        hsize_t len;
        dhandle.getSpace().getSimpleExtentDims(&len);
        if (static_cast<size_t>(len) != value_details.dimensions[p.first]) {
            throw std::runtime_error("length of an entry of 'index' should be equal to the extent of the corresponding dimension of 'value' for a subset assignment");
        }

        std::vector<int> buffer(len);
        dhandle.read(buffer.data(), H5::PredType::NATIVE_INT); 

        int upper_limit = seed_details.dimensions[p.first];
        for (auto b : buffer) {
            if (b < 0 || b >= upper_limit) {
                throw std::runtime_error(std::string("indices out of range for element '") + p.second + "' in 'index' for a subset assignment");
            }
        }
    }

    // Promotion.
    seed_details.type = std::max(seed_details.type, value_details.type);
    return seed_details;
} catch (std::exception& e) {
    throw std::runtime_error("failed to validate subset assignment at '" + name + "'\n- " + std::string(e.what()));
}

}

#endif
