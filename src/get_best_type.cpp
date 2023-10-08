#include "Rcpp.h"
#include <string>
#include <limits>
#include <algorithm>
#include <cmath>
#include <cstdint>

template<typename T>
std::string find_best_integer(T min, T max) {
    if (min < 0) {
        if (min >= static_cast<T>(std::numeric_limits<int8_t>::min()) && max <= static_cast<T>(std::numeric_limits<int8_t>::max())) {
            return "H5T_NATIVE_INT8";
        } else if (min >= static_cast<T>(std::numeric_limits<int16_t>::min()) && max <= static_cast<T>(std::numeric_limits<int16_t>::max())) {
            return "H5T_NATIVE_INT16";
        } else if (min >= static_cast<T>(std::numeric_limits<int32_t>::min()) && max <= static_cast<T>(std::numeric_limits<int32_t>::max())) {
            return "H5T_NATIVE_INT32";
        }
    } else {
        if (max <= static_cast<T>(std::numeric_limits<uint8_t>::max())) {
            return "H5T_NATIVE_UINT8";
        } else if (max <= static_cast<T>(std::numeric_limits<uint16_t>::max())) {
            return "H5T_NATIVE_UINT16";
        } else if (max <= static_cast<T>(std::numeric_limits<int32_t>::max())) {
            // Signedness is deliberate; we never want to store integers as
            // integers > 2^31-1, as this wouldn't be representable in R.
            return "H5T_NATIVE_INT32";
        }
    }

    return "H5T_NATIVE_DOUBLE";
}


//[[Rcpp::export(rng=false)]]
std::string get_best_type_double(Rcpp::NumericVector values) {
    if (values.size() == 0) {
        return "H5T_NATIVE_DOUBLE";
    }

    for (auto v : values) {
        if (v != std::trunc(v) || ISNA(v)) {
            return "H5T_NATIVE_DOUBLE";
        }
    }

    double min = *std::min_element(values.begin(), values.end());
    double max = *std::max_element(values.begin(), values.end());
    return find_best_integer(min, max);
}

//[[Rcpp::export(rng=false)]]
std::string get_best_type_int(Rcpp::IntegerVector values) {
    if (values.size() == 0) {
        return "H5T_NATIVE_INT32";
    }

    for (auto v : values) {
        if (v == NA_INTEGER) {
            return "H5T_NATIVE_INT32";
        }
    }

    // R's ints better be 32-bit, otherwise many things will fail everywhere.
    int min = *std::min_element(values.begin(), values.end());
    int max = *std::max_element(values.begin(), values.end());
    return find_best_integer(min, max);
}
