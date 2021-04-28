#' @export
#' @importFrom HDF5Array writeHDF5Array
setMethod("saveLayer", "array", function(x, file, name) {
    writeHDF5Array(x, file, name)
})
