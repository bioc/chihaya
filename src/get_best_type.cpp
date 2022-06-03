#include "Rcpp.h"
#include <string>
#include <limits>
#include <algorithm>
#include <cmath>

//[[Rcpp::export(rng=false)]]
std::string get_best_type(Rcpp::NumericVector values) {
    if (values.size() == 0) {
        return "H5T_NATIVE_USHORT";
    }

    for (auto v : values) {
        if (v != std::trunc(v)) {
            return "H5T_NATIVE_DOUBLE";
        } 
    }

    double min = *std::min_element(values.begin(), values.end());
    double max = *std::max_element(values.begin(), values.end());

    if (min < 0) {
        if (min >= static_cast<double>(std::numeric_limits<short>::min()) && max <= static_cast<double>(std::numeric_limits<short>::max())) {
            return "H5T_NATIVE_SHORT";
        } else if (min >= static_cast<double>(std::numeric_limits<int>::min()) && max <= static_cast<double>(std::numeric_limits<int>::max())) {
            return "H5T_NATIVE_INT";
        } else if (min >= static_cast<double>(std::numeric_limits<long>::min()) && max <= static_cast<double>(std::numeric_limits<long>::max())) {
            return "H5T_NATIVE_LONG";
        }
    } else {
        if (max <= static_cast<double>(std::numeric_limits<unsigned short>::max())) {
            return "H5T_NATIVE_USHORT";
        } else if (max <= static_cast<double>(std::numeric_limits<unsigned int>::max())) {
            return "H5T_NATIVE_UINT";
        } else if (max <= static_cast<double>(std::numeric_limits<unsigned long>::max())) {
            return "H5T_NATIVE_ULONG";
        }
    }

    return "H5T_NATIVE_DOUBLE";
}
