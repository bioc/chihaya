#' Saving simple seed classes
#'
#' Methods to save simple seed classes - namely, ordinary matrices or sparse \pkg{Matrix} objects - into the delayed operation file.
#' See \dQuote{Dense arrays} and \dQuote{Sparse matrices} at \url{https://ltla.github.io/chihaya} for more details.
#'
#' @param x An R object of the indicated class.
#' @inheritParams saveDelayedObject
#'
#' @details
#' For string arrays, missing values are handled by the \code{"missing-value-placeholder"} attribute on the \code{data} dataset.
#' All \code{NA} values in the array are replaced by the placeholder value in the attribute when they are saved inside the HDF5 file.
#' If this attribute is not present, it can be assumed that all strings are non-missing.
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
#' @importFrom DelayedArray type
setMethod("saveDelayedObject", "array", function(x, file, name) {
    h5createGroup(file, name)
    .labelArrayGroup(file, name, "dense array")

    .saveDataset(file, "data", x, parent=name, optimize.type=TRUE)
    write_integer_scalar(file, name, "native", 0L)

    if (!is.null(dimnames(x))) {
        .saveList(file, 'dimnames', dimnames(x), parent=name, vectors.only=TRUE)
    }

    invisible(NULL)
})

#' @importFrom Matrix t
#' @importFrom HDF5Array HDF5Array
#' @importFrom DelayedArray DelayedArray
#' @importFrom rhdf5 h5readAttributes
.load_array <- function(file, name, contents) {
    dname <- paste0(name, "/data")
    vals <- .load_dataset_with_attributes(file, dname)

    # If it's native, we need to undo rhdf5's transposition.
    if (h5read(file, file.path(name, "native"), drop=TRUE)) { 
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

    chunkdim <- min(length(x@x), 200000)
    .saveDataset(file, "data", parent=name, x=x@x, optimize.type=TRUE, chunks=chunkdim)

    if (nrow(x) < 2^16) {
        istore <- "H5T_NATIVE_USHORT"
    } else {
        istore <- "H5T_NATIVE_UINT"
    }
    iname <- paste0(name, "/indices")
    h5createDataset(file, iname, dims=length(x@i), H5type=istore, chunk=chunkdim)
    h5write(x@i, file, iname)

    # Also chunking the indptrs, in case you just want to fetch specific columns.
    pname <- file.path(name, "indptr")
    h5createDataset(file, pname, dims=length(x@p), H5type="H5T_NATIVE_ULONG", chunk=min(length(x@p), 5000));
    h5write(x@p, file, pname)

    h5write(dim(x), file, file.path(name, "shape"))

    .saveList(file, "dimnames", dimnames(x), parent=name, vectors.only=TRUE)
})

#' @importFrom Matrix sparseMatrix
.load_csparse_matrix <- function(file, name) {
    p <- h5read(file, paste0(name, "/indptr"), drop=TRUE)
    i <- h5read(file, paste0(name, "/indices"), drop=TRUE)
    dname <- paste0(name, "/data")
    x <- .load_vector_with_attributes(file, dname)

    dims <- h5read(file, paste0(name, "/shape"), drop=TRUE)
    dimnames <- .loadList(file, "dimnames", parent=name, vectors.only=TRUE)

    if (is.logical(x)) {
        cls <- "lgCMatrix"
    } else {
        cls <- "dgCMatrix"
        x <- as.double(x)
    }

    new(cls, i=i, p=p, x=x, Dim=dims, Dimnames=dimnames)
}
