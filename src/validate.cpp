#include "Rcpp.h"
#include "chihaya/chihaya.hpp"

// [[Rcpp::export(rng=false)]]
SEXP validate_(std::string path, std::string name) {
    chihaya::validate(path, name);
    return R_NilValue;
}

