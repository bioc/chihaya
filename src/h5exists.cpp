#include "H5Cpp.h"
#include "Rcpp.h"

// [[Rcpp::export(rng=false)]]
bool h5exists(std::string path, std::string host, std::string name) {
    H5::H5File fhandle(path, H5F_ACC_RDONLY);
    auto ghandle = fhandle.openGroup(host);
    return ghandle.exists(name);
}
