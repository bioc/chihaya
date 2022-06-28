#ifndef CHIHAYA_CONSTANT_ARRAY_HPP
#define CHIHAYA_CONSTANT_ARRAY_HPP

/**
 * @file constant_array.hpp
 *
 * @brief Constant array, stored inside the file.
 */

namespace chihaya {

/**
 * Validate a constant array in a HDF5 file.
 *
 * @param handle An open handle on a HDF5 group representing a dense array.
 * @param name Name of the group inside the file.
 *
 * @return Details of the constant array.
 * Otherwise, if the validation failed, an error is raised.
 *
 * A constant array is represented as a HDF5 group with the following attributes:
 *
 * - `delayed_type` should be a scalar string `"array"`.
 * - `delayed_operation` should be a scalar string `"constant array"`.
 *
 * Inside the group, we expect:
 *
 * - A `dimensions` dataset, specifying the dimensions of the constant array.
 *   This should be a 1-dimensional dataset of non-zero length equal to the number of dimensions, containing only non-negative integers.
 * - A `value` scalar dataset, containing the value of the constant array. 
 */
inline ArrayDetails validate_constant_array(const H5::Group& handle, const std::string& name) {
    std::vector<int> dims;
    {
        auto shandle = check_vector(handle, "dimensions", "constant_array");
        if (shandle.getTypeClass() != H5T_INTEGER) {
            throw std::runtime_error("'dimensions' should be integer for a constant array");
        }

        size_t size = vector_length(shandle);
        if (size == 0) {
            throw std::runtime_error("'dimensions' should have non-zero length for a constant array");
        }

        dims.resize(size);
        shandle.read(dims.data(), H5::PredType::NATIVE_INT);

        for (auto d : dims) {
            if (d < 0) {
                throw std::runtime_error("'dimensions' should contain non-negative values for a constant array");
            }
        }
    }
 
    if (!handle.exists("value") || handle.childObjType("value") != H5O_TYPE_DATASET) {
        throw std::runtime_error("'value' should be a dataset for a constant array");
    }

    auto dhandle = handle.openDataSet("value");
    auto ndims = dhandle.getSpace().getSimpleExtentNdims();
    if (ndims != 0) {
        throw std::runtime_error("'value' should be a scalar for a constant array");
    }

    ArrayDetails output;
    output.dimensions.insert(output.dimensions.end(), dims.begin(), dims.end());

    auto cls = dhandle.getTypeClass();
    if (cls == H5T_INTEGER) {
        output.type = INTEGER;
    } else if (cls == H5T_FLOAT) {
        output.type = FLOAT;
    } else if (cls == H5T_STRING) {
        output.type = STRING;
    } else {
        throw std::runtime_error("unrecognized type of 'value' for a constant array");
    }

    return output;
}

}

#endif
