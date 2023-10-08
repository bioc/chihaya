#ifndef CHIHAYA_MATRIX_PRODUCT_HPP
#define CHIHAYA_MATRIX_PRODUCT_HPP

/**
 * @file matrix_product.hpp
 *
 * @brief Validation for delayed matrix products.
 */

namespace chihaya {

/**
 * @cond
 */
inline std::pair<ArrayDetails, bool> fetch_seed_for_product(
    const H5::Group& handle, 
    const std::string& target, 
    const std::string& orientation, 
    const std::string& name,
    const Version& version)
{
    // Checking the seed.
    if (!handle.exists(target) || handle.childObjType(target) != H5O_TYPE_GROUP) {
        throw std::runtime_error(std::string("expected '") + target + "' group for a matrix product");
    }

    auto seed_details = validate(handle.openGroup(target), name + "/" + target, version);
    if (seed_details.dimensions.size() != 2) {
        throw std::runtime_error("expected '" + target + "' to be a 2-dimensional array for a matrix product");
    }

    if (seed_details.type == STRING) {
        throw std::runtime_error(std::string("type of '") + target + "' should be numeric or boolean for a matrix product");
    }
    
    // Checking the orientation.
    if (!handle.exists(orientation) || handle.childObjType(orientation) != H5O_TYPE_DATASET) {
        throw std::runtime_error(std::string("expected '") + orientation + "' dataset for a matrix product");
    }

    auto ahandle = handle.openDataSet(orientation);
    if (ahandle.getSpace().getSimpleExtentNdims() != 0 || ahandle.getTypeClass() != H5T_STRING) {
        throw std::runtime_error("'" + orientation + "' should be a string for a matrix product");
    }

    std::string oristr;
    ahandle.read(oristr, ahandle.getStrType());
    if (oristr != "N" && oristr != "T") {
        throw std::runtime_error("'" + orientation + "' should be either 'N' or 'T' for a matrix product");
    }

    return std::pair<ArrayDetails, bool>(seed_details, oristr == "T");
}
/**
 * @endcond
 */


/**
 * Validate a delayed matrix product in a HDF5 file.
 *
 * @param handle An open handle on a HDF5 group representing a matrix product.
 * @param name Name of the group inside the file.
 * @param version Version of the **chihaya** specification.
 *
 * @return Details of the matrix product.
 * Otherwise, if the validation failed, an error is raised.
 *
 * A delayed matrix product is represented as a HDF5 group with the following attributes:
 *
 * - `delayed_type` should be a scalar string `"operation"`.
 * - `delayed_operation` should be a scalar string `"matrix product"`.
 *
 * Inside the group, we expect:
 *
 * - A `left_seed` group describing a delayed object, i.e., another delayed operation or array.
 *   This is used as the left hand side of the matrix product.
 * - A `left_orientation` scalar string dataset, specifying whether the `left_seed` array should be transposed (`"T"`) or not (`"N"`).
 * - A `right_seed` group describing a delayed object, i.e., another delayed operation or array.
 *   This is used as the right hand side of the matrix product.
 * - A `right_orientation` scalar string dataset, specifying whether the `right_seed` should be transposed (`"T"`) or not (`"N"`).
 *
 * For example, setting `left orientation` to `"T"` and `right orientation` to `"N"` would be equivalent to `t(left_seed) * right_seed`.
 * This enables optimizations during the multiplication by avoiding the need to explicitly realize the transposition.
 *
 * If either `left_seed` or `right_seed` are floating-point, the output type will also be `FLOAT`.
 * Otherwise, the output type will be `INTEGER`.
 */
inline ArrayDetails validate_matrix_product(const H5::Group& handle, const std::string& name, const Version& version) try {
    auto left_details = fetch_seed_for_product(handle, "left_seed", "left_orientation", name, version);
    auto right_details = fetch_seed_for_product(handle, "right_seed", "right_orientation", name, version);

    ArrayDetails output;
    output.dimensions.resize(2);
    auto& nrow = output.dimensions[0];
    auto& ncol = output.dimensions[1];
    size_t common, common2;

    if (left_details.second) {
        nrow = left_details.first.dimensions[1];
        common = left_details.first.dimensions[0];
    } else {
        nrow = left_details.first.dimensions[0];
        common = left_details.first.dimensions[1];
    }

    if (right_details.second) {
        ncol = right_details.first.dimensions[0];
        common2 = right_details.first.dimensions[1];
    } else {
        ncol = right_details.first.dimensions[1];
        common2 = right_details.first.dimensions[0];
    }

    if (common != common2) {
        throw std::runtime_error("inconsistent common dimensions (" + std::to_string(common) + " vs " + std::to_string(common2) + ") in the matrix product");
    }

    if (left_details.first.type == FLOAT || right_details.first.type == FLOAT) {
        output.type = FLOAT;
    } else {
        output.type = INTEGER;
    }

    return output;
} catch (std::exception& e) {
    throw std::runtime_error("failed to validate matrix product at '" + name + "'\n- " + std::string(e.what()));
}

}

#endif
