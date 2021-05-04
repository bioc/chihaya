#' @export
#' @importFrom HDF5Array writeHDF5Array
setMethod("saveLayer", "array", function(x, file, name) {
    writeHDF5Array(x, file, name)
})

#' @export
#' @importFrom rhdf5 h5write h5writeAttribute H5Fopen H5Fclose H5Dopen H5Dclose
setMethod("saveLayer", "vector", function(x, file, name) {
    h5write(x, file, name) 
    fhandle <- H5Fopen(file)
    on.exit(H5Fclose(fhandle))
    dhandle <- H5Dopen(fhandle, name)
    on.exit(H5Dclose(dhandle), add=TRUE, after=FALSE)
    h5writeAttribute("vector", dhandle, "_class")
})

#' @export
#' @importFrom rhdf5 h5createGroup h5write
setMethod("saveLayer", "list", function(x, file, name) {
    h5createGroup(file, name)

    for (i in seq_along(x)) {
        if (!is.null(x[[i]])) {
            saveLayer(x[[i]], file, file.path(name, i))
        }
    }

    .label_group(file, name, "_length", length(x))
    nms <- names(x)
    if (!is.null(nms)) {
        .label_group(file, name, "_names", nms)
    }

    invisible(NULL)
})

.label_group_class <- function(file, name, type) {
    .label_group(file, name, "_class", type)
}

#' @importFrom rhdf5 h5writeAttribute H5Fopen H5Fclose H5Gopen H5Gclose
.label_group <- function(file, name, field, value) {
    fhandle <- H5Fopen(file)
    on.exit(H5Fclose(fhandle))
    ghandle <- H5Gopen(fhandle, name)
    on.exit(H5Gclose(ghandle), add=TRUE, after=FALSE)
    h5writeAttribute(value, ghandle, field)
}
