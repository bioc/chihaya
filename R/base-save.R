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
setMethod("saveLayer", "DelayedArray", function(x, file, name) {
    saveLayer(x@seed, file, name)
})

#' @export
#' @importFrom rhdf5 h5createGroup
#' @importClassesFrom Matrix CsparseMatrix
setMethod("saveLayer", "CsparseMatrix", function(x, file, name) {
    if (name!="") {
        h5createGroup(file, name)
    }
    .label_group_class(file, name, c("seed", "csparse matrix"))

    # TODO: convert this into a UTF-8 string. Note that we are deliberately
    # using CSR here, as everything is transposed in HDF5.
    .label_group(file, name, "h5sparse_format", "csr-matrix")
    .label_group(file, name, "h5sparse_shape", rev(dim(x)))

    saveLayer(x@p, file, file.path(name, "indptr"))
    saveLayer(x@i, file, file.path(name, "indices"))
    saveLayer(x@x, file, file.path(name, "data"))
    saveLayer(rev(dimnames(x)), file, file.path(name, "dimnames"))
})

#' @importFrom Matrix sparseMatrix
#' @importFrom rhdf5 h5readAttributes
.load_csparse_matrix <- function(file, name, contents) {
    p <- .dispatch_loader(file, file.path(name, "indptr"), contents[["indptr"]])
    i <- .dispatch_loader(file, file.path(name, "indices"), contents[["indices"]])
    x <- .dispatch_loader(file, file.path(name, "data"), contents[["data"]])

    attrs <- h5readAttributes(file, name)
    dims <- as.integer(attrs$h5sparse_shape)
    dims <- rev(dims)
    dimnames <- .dispatch_loader(file, file.path(name, "dimnames"), contents[["dimnames"]])
    dimnames <- rev(dimnames)

    # Avoid inefficiency of sparseMatrix() constructor.
    if (is.double(x)) {
        cls <- "dgCMatrix"
    } else {
        cls <- "lgCMatrix"
    }
    out <- new(cls, i=i, p=p, x=x, Dim=dims, Dimnames=dimnames)

    DelayedArray(out)
}

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
