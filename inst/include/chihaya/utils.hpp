#ifndef CHIHAYA_UTILS_HPP
#define CHIHAYA_UTILS_HPP

#include <vector>
#include <string>

#include "H5Cpp.h"

/**
 * @file utils.hpp
 *
 * @brief Various utilities.
 */

namespace chihaya {

/**
 * Type of the array.
 * Operations involving mixed types will generally result in promotion to the more advanced types,
 * e.g., an `INTEGER` and `FLOAT` addition will result in promotion to `FLOAT`.
 * Note that operations involving the same types are not guaranteed to preserve type,
 * e.g., `INTEGER` division is assumed to produce a `FLOAT`.
 */
enum ArrayType { BOOLEAN = 0, INTEGER = 1, FLOAT = 2, STRING = 3 }; // giving explicit values for comparisons to work.

/**
 * @brief Details about an array.
 *
 * This contains the type and dimensionality of the array.
 * The exact type representation of the array is left to the implementation;
 * we do not make any guarantees about precision, width or signedness.
 */
struct ArrayDetails {
    /**
     * @cond
     */
    ArrayDetails() {}

    ArrayDetails(ArrayType t, std::vector<size_t> d) : type(t), dimensions(std::move(d)) {}
    /**
     * @endcond
     */

    /**
     * Type of the array.
     */
    ArrayType type;

    /** 
     * Dimensions of the array.
     * Values should be non-negative.
     */
    std::vector<size_t> dimensions;
};

/**
 * @brief Version of the **chihaya** specification.
 */
struct Version {
    /**
     * @cond
     */
    Version() = default;
    Version(int ma, int mi, int pa) : major(ma), minor(mi), patch(pa) {}
    /**
     * @endcond
     */

    /**
     * Major version number.
     */
    int major = 0;

    /**
     * Minor version number.
     */
    int minor = 0;

    /**
     * Patch version number.
     */
    int patch = 0;
};

/**
 * @cond
 */
inline std::string load_string_attribute(const H5::Attribute& attr, const std::string field) {
    if (attr.getTypeClass() != H5T_STRING || attr.getSpace().getSimpleExtentNdims() != 0) {
        throw std::runtime_error(std::string("'") + field + "' attribute should be a scalar string");
    }

    std::string output;
    attr.read(attr.getStrType(), output);

    return output;
}

inline std::string load_string_attribute(const H5::Group& handle, const std::string& field, const std::string& extra) {
    if (!handle.attrExists(field)) {
        throw std::runtime_error(std::string("expected a '") + field + "' attribute" + extra);
    }
    return load_string_attribute(handle.openAttribute(field), field);
}

template<class V>
bool are_dimensions_equal(const V& left, const V& right) {
    if (left.size() != right.size()) {
        return false;
    } else {
        for (size_t i = 0; i < left.size(); ++i) {
            if (left[i] != right[i]) {
                return false;
            }
        }
    }
    return true;
}

inline H5::DataSet check_vector(const H5::Group& handle, const std::string& name, const std::string& message) {
    if (!handle.exists(name) || handle.childObjType(name) != H5O_TYPE_DATASET) {
        throw std::runtime_error(std::string("expected '") + name + "' to be a dataset for a " + message);
    }

    auto dhandle = handle.openDataSet(name);
    if (dhandle.getSpace().getSimpleExtentNdims() != 1) {
        throw std::runtime_error(std::string("'") + name + "' should be a 1-dimensional dataset for a " + message);
    }

    return dhandle;
}

inline size_t vector_length(const H5::DataSet& handle) {
    hsize_t len;
    handle.getSpace().getSimpleExtentDims(&len);
    return len;
}

inline bool is_boolean(const H5::DataSet& handle) {
    int is_bool = 0;
    if (handle.attrExists("is_boolean")) {
        if (handle.getDataType().getClass() != H5T_INTEGER) {
            throw std::runtime_error("'is_boolean' attribute should only exist for integer datasets");
        }

        auto ahandle = handle.openAttribute("is_boolean");
        if (ahandle.getSpace().getSimpleExtentNdims() != 0 || ahandle.getTypeClass() != H5T_INTEGER) {
            throw std::runtime_error("'is_boolean' attribute should be an integer scalar");
        }

        ahandle.read(H5::PredType::NATIVE_INT, &is_bool);
    }
    return is_bool;
}

inline void validate_missing_placeholder(const H5::DataSet& handle, const Version& version) {
    if (version.major == 0) {
        return;
    }

    if (!handle.attrExists("missing_placeholder")) {
        return;
    }

    auto ahandle = handle.openAttribute("missing_placeholder");
    if (ahandle.getSpace().getSimpleExtentNdims() != 0) {
        throw std::runtime_error("missing placeholder attribute should be a scalar");
    }

    if (handle.getTypeClass() != ahandle.getTypeClass()) {
        throw std::runtime_error("missing placeholder attribute should be of the same type as the dataset");
    }
}

inline Version parse_version_string(const std::string& version_string) {
    int major = 0, minor = 0, patch = 0;
    size_t i = 0, end = version_string.size();

    // MAJOR VERSION.
    if (version_string.empty()) {
        throw std::runtime_error("version string is empty");
    }
    if (version_string[i] == '0') {
        throw std::runtime_error("invalid version string '" + version_string + "' has leading zeros in its major version");
    }
    while (i < end && version_string[i] != '.') {
        if (!std::isdigit(version_string[i])) {
            throw std::runtime_error("invalid version string '" + version_string + "' contains non-digit characters");
        }
        major *= 10;
        major += version_string[i] - '0';
        ++i;
    }

    // MINOR VERSION.
    if (i == end) {
        throw std::runtime_error("version string '" + version_string + "' is missing a minor version");
    }
    ++i; // get past the period and check again.
    if (i == end) {
        throw std::runtime_error("version string '" + version_string + "' is missing a minor version");
    }

    if (version_string[i] != '0') {
        while (i < end && version_string[i] != '.') {
            if (!std::isdigit(version_string[i])) {
                throw std::runtime_error("invalid version string '" + version_string + "' contains non-digit characters");
            }
            minor *= 10;
            minor += version_string[i] - '0';
            ++i;
        }

    } else {
        ++i;
        if (i < end && version_string[i] != '.') {
            throw std::runtime_error("version string '" + version_string + "' has leading zeros in its minor version");
        }
    }

    // PATCH VERSION.
    if (i == end) {
        throw std::runtime_error("version string '" + version_string + "' is missing a patch version");
    }
    ++i; // get past the period and check again.
    if (i == end) {
        throw std::runtime_error("version string '" + version_string + "' is missing a patch version");
    }

    if (version_string[i] == '0' && i + 1 < end) {
        throw std::runtime_error("invalid version string '" + version_string + "' has leading zeros in its patch version");
    }
    while (i < end) {
        if (!std::isdigit(version_string[i])) {
            throw std::runtime_error("invalid version string '" + version_string + "' contains non-digit characters");
        }
        patch *= 10;
        patch += version_string[i] - '0';
        ++i;
    }

    return Version(major, minor, patch);
}
/**
 * @endcond
 */

}

#endif
