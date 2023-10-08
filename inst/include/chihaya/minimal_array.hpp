#ifndef CHIHAYA_MINIMAL_ARRAY_HPP
#define CHIHAYA_MINIMAL_ARRAY_HPP

#include <vector>
#include <stdexcept>
#include <cstdint>
#include "H5Cpp.h"
#include "utils.hpp"

namespace chihaya {

template<class Function>
ArrayDetails validate_minimal(const H5::Group& handle, Function fun, const Version&) {
    if (!handle.exists("dimensions") || handle.childObjType("dimensions") != H5O_TYPE_DATASET) {
        throw std::runtime_error("expected 'dimensions' dataset for " + fun());
    }

    auto dhandle = handle.openDataSet("dimensions");
    auto dspace = dhandle.getSpace();
    if (dspace.getSimpleExtentNdims() != 1 || dhandle.getTypeClass() != H5T_INTEGER) {
        throw std::runtime_error("'dimensions' should be a 1-dimensional integer dataset for " + fun());
    }

    hsize_t len;
    dspace.getSimpleExtentDims(&len);

    std::vector<int64_t> dimensions(len);
    dhandle.read(dimensions.data(), H5::PredType::NATIVE_INT64);
    for (auto d : dimensions) {
        if (d < 0) {
            throw std::runtime_error("elements in 'dimensions' should be non-negative for " + fun());
        }
    } 

    ArrayType atype;
    {
        if (!handle.exists("type") || handle.childObjType("type") != H5O_TYPE_DATASET) {
            throw std::runtime_error("expected 'type' dataset for " + fun());
        }

        auto thandle = handle.openDataSet("type");
        if (thandle.getSpace().getSimpleExtentNdims() != 0 || thandle.getTypeClass() != H5T_STRING) {
            throw std::runtime_error("'type' should be a string scalar for " + fun());
        }

        std::string type;
        thandle.read(type, thandle.getStrType());

        if (type == "BOOLEAN") {
            atype = BOOLEAN;
        } else if (type == "INTEGER") {
            atype = INTEGER;
        } else if (type == "FLOAT") {
            atype = FLOAT;
        } else if (type == "STRING") {
            atype = STRING;
        } else {
            throw std::runtime_error(std::string("unknown 'type' (") + type + ") for " + fun());
        }
    }

    return ArrayDetails(atype, std::vector<size_t>(dimensions.begin(), dimensions.end()));
}

}

#endif
