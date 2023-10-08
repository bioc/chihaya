#ifndef CHIHAYA_COMBINE_HPP
#define CHIHAYA_COMBINE_HPP

#include "H5Cpp.h"
#include <stdexcept>
#include <vector>
#include "list.hpp"
#include "utils.hpp"

/**
 * @file combine.hpp
 *
 * @brief Validation for delayed combining operations.
 */

namespace chihaya {

/**
 * @cond
 */
inline ArrayDetails validate(const H5::Group& handle, const std::string&, const Version& version);
/**
 * @endcond
 */

/**
 * Validate a delayed combining operation in a HDF5 file.
 *
 * @param handle An open handle on a HDF5 group representing a combining operation.
 * @param name Name of the group inside the file.
 * @param version Version of the **chihaya** specification.
 * 
 * @return Details of the combined object.
 * Otherwise, if the validation failed, an error is raised.
 *
 * A delayed combining operation is represented as a HDF5 group with the following attributes:
 *
 * - `delayed_type` should be a scalar string `"operation"`.
 * - `delayed_operation` should be a scalar string `"combine"`.
 *
 * Inside the group, we expect:
 *
 * - A `seeds` group.
 *   This is expected to be a list (see `ListDetails`) of length equal to the number of seeds to be combined.
 *   Each seed should be a group describing a delayed object, i.e., another delayed operation or array. 
 *   All seeds should have the same dimensionality and extents, with the exception of the `along` dimension.
 * - An `along` scalar integer dataset.
 *   This is a 0-based index that specifies the dimensions on which to combine objects in `seeds`.
 *   It should be a non-negative value that is less than the dimensionality of each object.
 *   The exact integer representation is left to the implementation.
 *
 * If all `seeds` have the same type, the output object will also be of that type.
 * Otherwise, the type of the output object is set to the most advanced `ArrayType` among all `seeds` objects.
 * For example, a mixture of `INTEGER` and `FLOAT` objects will result in a `FLOAT` output.
 */
inline ArrayDetails validate_combine(const H5::Group& handle, const std::string& name, const Version& version) try {
    if (!handle.exists("along") || handle.childObjType("along") != H5O_TYPE_DATASET) {
        throw std::runtime_error("expected 'along' dataset for a combine operation");
    }

    auto ahandle = handle.openDataSet("along");
    if (ahandle.getSpace().getSimpleExtentNdims() != 0 || ahandle.getTypeClass() != H5T_INTEGER) {
        throw std::runtime_error("'along' should be an integer scalar for a combine operation");
    }

    int along = 0;
    ahandle.read(&along, H5::PredType::NATIVE_INT);
    if (along < 0) {
        throw std::runtime_error("'along' should contain a non-negative value for a combine operation");
    }

    if (!handle.exists("seeds") || handle.childObjType("seeds") != H5O_TYPE_GROUP) {
        throw std::runtime_error("expected 'seeds' group for a combine operation");
    }

    auto shandle = handle.openGroup("seeds");
    ListDetails list_params;

    try {
        list_params = validate_list(shandle, version);
    } catch (std::exception& e) {
        throw std::runtime_error(std::string("failed to load 'seeds' list for a combine operation:\n  ") + e.what());
    }

    if (list_params.present.size() != list_params.length) {
        throw std::runtime_error("missing elements in the 'seeds' list for a combine operation");
    }

    std::vector<size_t> dimensions;
    ArrayType type = BOOLEAN;
    bool first = true;

    for (const auto& p : list_params.present) {
        auto current = shandle.openGroup(p.second);
        auto cur_seed = validate(current, name + "/seeds/" + p.second, version);

        if (first) {
            type = cur_seed.type;
            dimensions = cur_seed.dimensions;
            if (static_cast<size_t>(along) >= dimensions.size()) {
                throw std::runtime_error("'along' should be less than the seed dimensionality for a combine operation");
            }
            first = false;
        } else {
            if (type < cur_seed.type) {
                type = cur_seed.type;
            }
            if (dimensions.size() != cur_seed.dimensions.size()) {
                throw std::runtime_error("dimensionality mismatch between seeds for a combine operation");
            }
            for (size_t d = 0; d < dimensions.size(); ++d) {
                if (d == static_cast<size_t>(along)) {
                    dimensions[d] += cur_seed.dimensions[d];
                } else if (dimensions[d] != cur_seed.dimensions[d]) {
                    throw std::runtime_error("inconsistent dimension extents between seeds for a combine operation");
                }
            }
        }
    }

    return ArrayDetails(type, std::move(dimensions));
} catch (std::exception& e) {
    throw std::runtime_error("failed to validate combine operation at '" + name + "'\n- " + std::string(e.what()));
}

}

#endif
