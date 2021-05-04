#' @export
#' @importFrom HDF5Array writeHDF5Array 
setMethod("saveLayer", "array", function(x, file, name) {
    if (name!="") {
        h5createGroup(file, name)
    }
    .label_group_class(file, name, "array")

    writeHDF5Array(x, file, file.path(name, 'data'))

    if (!is.null(dimnames(x))) {
        # Creating a list and then filling it.
        .save_list(dimnames(x), file, file.path(name, 'dimnames'), vectors.only=TRUE)
    }

    invisible(NULL)
})

#' @importFrom HDF5Array HDF5Array
#' @importFrom DelayedArray DelayedArray
.load_array <- function(file, name, contents) {
    vals <- h5read(file, file.path(name, "data"))
    if ("dimnames" %in% names(contents)) {
        dimnames(vals) <- .load_list(file, file.path(name, "dimnames"), contents[["dimnames"]], vectors.only=TRUE)
    }
    DelayedArray(vals)
}

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

    h5write(x@p, file, file.path(name, "indptr"))
    h5write(x@i, file, file.path(name, "indices"))
    h5write(x@x, file, file.path(name, "data"))
    .save_list(rev(dimnames(x)), file, file.path(name, "dimnames"), vectors.only=TRUE)
})

#' @importFrom Matrix sparseMatrix
#' @importFrom rhdf5 h5readAttributes
.load_csparse_matrix <- function(file, name, contents) {
    p <- .load_simple_vector(file, file.path(name, "indptr")) 
    i <- .load_simple_vector(file, file.path(name, "indices"))
    x <- .load_simple_vector(file, file.path(name, "data"))

    attrs <- h5readAttributes(file, name)
    dims <- as.integer(attrs$h5sparse_shape)
    dims <- rev(dims)
    dimnames <- .load_list(file, file.path(name, "dimnames"), contents[["dimnames"]], vectors.only=TRUE)
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


