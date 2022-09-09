#' Saving simple seed classes
#'
#' Methods to save simple seed classes - namely, ordinary matrices or sparse \pkg{Matrix} objects - into the delayed operation file.
#' See \dQuote{Dense arrays} and \dQuote{Sparse matrices} at \url{https://ltla.github.io/chihaya} for more details.
#'
#' @param x An R object of the indicated class.
#' @inheritParams saveDelayedObject
#' 
#' @return A \code{NULL}, invisibly.
#' A group is created at \code{name} containing the contents of \code{x}.
#'
#' @author Aaron Lun
#'
#' @examples
#' # Saving an ordinary matrix.
#' X <- matrix(rpois(100, 2), 5, 20)
#' Y <- DelayedArray(X)
#' temp <- tempfile(fileext=".h5")
#' saveDelayed(Y, temp)
#' rhdf5::h5ls(temp)
#' loadDelayed(temp)
#'
#' # Saving a sparse matrix.
#' X <- rsparsematrix(100, 20, 0.1)
#' Y <- DelayedArray(X)
#' temp <- tempfile(fileext=".h5")
#' saveDelayed(Y, temp)
#' rhdf5::h5ls(temp)
#' loadDelayed(temp)
#'
#' @export
#' @rdname simple
#' @importFrom HDF5Array writeHDF5Array 
#' @importFrom Matrix t
setMethod("saveDelayedObject", "array", function(x, file, name) {
    h5createGroup(file, name)

    .labelArrayGroup(file, name, "dense array")
    writeHDF5Array(x, file, file.path(name, 'data'))
    write_integer_scalar(file, name, "native", 0L)

    if (!is.null(dimnames(x))) {
        .saveList(file, 'dimnames', dimnames(x), parent=name, vectors.only=TRUE)
    }

    invisible(NULL)
})

#' @importFrom Matrix t
#' @importFrom HDF5Array HDF5Array
#' @importFrom DelayedArray DelayedArray
.load_array <- function(file, name, contents) {
    vals <- h5read(file, file.path(name, "data"))

    # If it's native, we need to undo rhdf5's transposition.
    if (h5read(file, file.path(name, "native"))) { 
        vals <- aperm(vals, dim(vals):1)
    }

    if (h5exists(file, name, "dimnames")) {
        dimnames(vals) <- .loadList(file, "dimnames", parent=name, vectors.only=TRUE)
    }

    vals
}

#' @export
#' @rdname saveDelayedObject 
#' @importFrom DelayedArray DelayedArray
setMethod("saveDelayedObject", "DelayedArray", function(x, file, name) {
    saveDelayedObject(x@seed, file, name)
})

#' @export
#' @rdname simple
#' @importFrom rhdf5 h5createGroup h5createDataset h5write H5Dopen H5Dclose
#' @importClassesFrom Matrix CsparseMatrix
setMethod("saveDelayedObject", "CsparseMatrix", function(x, file, name) {
    h5createGroup(file, name)
    .labelArrayGroup(file, name, "sparse matrix")

    # Choosing the most efficient representation where possible.
    if (!is.logical(x@x)) {
        xstore <- get_best_type(x@x)
    } else {
        xstore <- "H5T_NATIVE_UCHAR"
    }
    dname <- file.path(name, "data")
    h5createDataset(file, dname, dims=length(x@x), H5type=xstore, storage.mode=typeof(x@x), chunk=min(length(x@x), 200000));
    h5write(x@x, file, dname)

    if (nrow(x) < 2^16) {
        istore <- "H5T_NATIVE_USHORT"
    } else {
        istore <- "H5T_NATIVE_UINT"
    }
    iname <- file.path(name, "indices")
    h5createDataset(file, iname, dims=length(x@i), H5type=istore, chunk=min(length(x@i), 200000));
    h5write(x@i, file, iname)

    # Also chunking the indptrs, in case you just want to fetch specific columns.
    pname <- file.path(name, "indptr")
    h5createDataset(file, pname, dims=length(x@p), H5type="H5T_NATIVE_ULONG", chunk=min(length(x@p), 5000));
    h5write(x@p, file, pname)

    h5write(dim(x), file, file.path(name, "shape"))

    .saveList(file, "dimnames", dimnames(x), parent=name, vectors.only=TRUE)
})

#' @importFrom Matrix sparseMatrix
#' @importFrom rhdf5 h5readAttributes
.load_csparse_matrix <- function(file, name) {
    p <- .load_simple_vector(file, file.path(name, "indptr")) 
    i <- .load_simple_vector(file, file.path(name, "indices"))
    x <- .load_simple_vector(file, file.path(name, "data"))

    dims <- .load_simple_vector(file, file.path(name, "shape"))
    dimnames <- .loadList(file, "dimnames", parent=name, vectors.only=TRUE)

    # Avoid inefficiency of sparseMatrix() constructor.
    if (is.logical(x)) {
        cls <- "lgCMatrix"
    } else {
        cls <- "dgCMatrix"
        x <- as.double(x)
    }

    new(cls, i=i, p=p, x=x, Dim=dims, Dimnames=dimnames)
}
