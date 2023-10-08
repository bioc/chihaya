#ifndef CHIHAYA_LIST_HPP
#define CHIHAYA_LIST_HPP

#include <map>
#include <string>

#include "H5Cpp.h"

#include "utils.hpp"

/**
 * @file list.hpp
 *
 * @brief Definition of a list in HDF5.
 */

namespace chihaya {

/**
 * @brief Details of a list object.
 *
 * A list is represented in HDF5 as a group with the following attributes:
 *
 * - `delayed_type` should be a scalar string `"list"`.
 * - `delayed_length` should be a scalar integer specifying the list length.
 * 
 * Children of this group represent the list elements and are named by their positional index.
 * List elements may be absent, e.g., a group with `delayed_length` of 3 and the children `0` and `2` corresponds to a list that is missing an element at index 1.
 * The intepretation of the absence of an element is context-dependent.
 */
struct ListDetails {
    /**
     * Length of the list.
     */
    size_t length;

    /**
     * Elements present in the list.
     * Keys represent the list index while values represent the name of the HDF5 group (a string representation of the index).
     * Keys will lie in `[0, length)`; 
     * each key will appear no more than once, and possibly zero times if the element at that index is missing.
     */
    std::map<int, std::string> present;
};

/**
 * @cond
 */
inline ListDetails validate_list(const H5::Group& handle, const Version&) {
    ListDetails output;

    auto dtype = load_string_attribute(handle, "delayed_type", " for a list");
    if (dtype != "list") {
        throw std::runtime_error("expected 'delayed_type = \"list\"' for a list");
    }

    if (!handle.attrExists("delayed_length")) {
        throw std::runtime_error("expected a 'delayed_length' attribute for a list");
    }
    auto len = handle.openAttribute("delayed_length");
    if (len.getTypeClass() != H5T_INTEGER || len.getSpace().getSimpleExtentNdims() != 0) {
        throw std::runtime_error("expected a 'delayed_length' integer scalar for a list");
    } else {
        int l = 0;
        len.read(H5::PredType::NATIVE_INT, &l);
        output.length = l;
    }

    size_t n = handle.getNumObjs();
    if (n > output.length) {
        throw std::runtime_error("more objects in the list than are specified by 'delayed_length'");
    }
    for (size_t i = 0; i < n; ++i) {
        std::string name = handle.getObjnameByIdx(i);

        // Aaron's cheap and dirty atoi!
        int sofar = 0;
        for (auto c : name) {
            if (c < '0' || c > '9') {
                throw std::runtime_error(std::string("'") + name + "' is not a valid name for a list index");
            }
            sofar *= 10;
            sofar += (c - '0'); 
        }

        if (static_cast<size_t>(sofar) >= output.length) {
            throw std::runtime_error(std::string("'") + name + "' is out of range for a list"); 
        }
        output.present[sofar] = name;
    }

    return output;
}

inline ListDetails validate_list(const H5::Group& handle) {
    return validate_list(handle, Version());
}
/**
 * @endcond
 */

}

#endif
