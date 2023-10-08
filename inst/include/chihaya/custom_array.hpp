#ifndef CHIHAYA_CUSTOM_ARRAY_HPP
#define CHIHAYA_CUSTOM_ARRAY_HPP

#include "H5Cpp.h"
#include <vector>
#include "utils.hpp"

/**
 * @file custom_array.hpp
 *
 * @brief Validation for custom third-party arrays.
 */

namespace chihaya {

/**
 * Validate a custom array, where the only provided information is its dimensions and type.
 *
 * @param handle An open handle on a HDF5 group representing an external array.
 * @param name Name of the group inside the file.
 * @param version Version of the **chihaya** specification.
 *
 * @return Details of the custom array.
 * Otherwise, if the validation failed, an error is raised.
 *
 * Each custom array is represented as a HDF5 group with the following attributes:
 *
 * - `delayed_type` should be a scalar string `"array"`.
 * - `delayed_array` should be any scalar string that starts with `"custom "` (note the space).
 *   Implementations are expected to add their own descriptors to define specific interprations.
 *
 * Inside the group, we expect at least the following children:
 *
 * - `dimensions`, a 1-dimensional integer dataset. 
 *   This stores the dimensionality of the external array and should only contain non-negative values.
 * - `type`, a scalar string specifying the type.
 *   This should be one of `BOOLEAN`, `INTEGER`, `FLOAT` or `STRING`.
 * 
 * The custom array concept allows third-party developers to extend the chihaya specification with their own array definitions.
 * This is most commonly used to embed references to external resources, e.g., arrays stored in custom databases,
 * thus avoiding a redundant copy of a large arrays when we only want to preserve the delayed operations.
 * Of course, it is assumed that clients will know how to retrieve specific resources from the remotes.
 */
inline ArrayDetails validate_custom_array(const H5::Group& handle, const std::string& name, const Version& version) try {
    return validate_minimal(handle, []() -> std::string { return std::string("a custom array"); }, version);
} catch (std::exception& e) {
    throw std::runtime_error("failed to validate custom array at '" + name + "'\n- " + std::string(e.what()));
}

}

#endif
