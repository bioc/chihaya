#' Saving simple seed classes
#'
#' Methods to save simple seed classes into the delayed operation file.
#' See the \dQuote{Specification} vignette for details on the layout.
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
#' library(HDF5Array)
#' X <- rsparsematrix(100, 20, 0.1)
#' Y <- DelayedArray(X)
#' temp <- tempfile(fileext=".h5")
#' saveDelayed(Y, temp)
#' rhdf5::h5ls(temp)
#' loadDelayed(temp)
#'
#' @export
#' @rdname base-save
#' @importFrom HDF5Array writeHDF5Array 
#' @importFrom Matrix t
setMethod("saveDelayedObject", "array", function(x, file, name) {
    h5createGroup(file, name)

    .label_group_seed(file, name, "dense array")
    writeHDF5Array(x, file, file.path(name, 'data'))
    write_integer_scalar(file, name, "native", 0L)

    if (!is.null(dimnames(x))) {
        .save_list(dimnames(x), file, file.path(name, 'dimnames'), vectors.only=TRUE)
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
        dimnames(vals) <- .load_list(file, file.path(name, "dimnames"), vectors.only=TRUE)
    }

    DelayedArray(vals)
}

#' @export
#' @rdname base-save
#' @importFrom DelayedArray DelayedArray
setMethod("saveDelayedObject", "DelayedArray", function(x, file, name) {
    saveDelayedObject(x@seed, file, name)
})

#' @export
#' @rdname base-save
#' @importFrom rhdf5 h5createGroup
#' @importClassesFrom Matrix CsparseMatrix
setMethod("saveDelayedObject", "CsparseMatrix", function(x, file, name) {
    h5createGroup(file, name)
    .label_group_seed(file, name, "sparse matrix")

    h5write(x@p, file, file.path(name, "indptr"))
    h5write(x@i, file, file.path(name, "indices"))
    h5write(x@x, file, file.path(name, "data"))
    h5write(dim(x), file, file.path(name, "shape"))

    .save_list(dimnames(x), file, file.path(name, "dimnames"), vectors.only=TRUE)
})

#' @importFrom Matrix sparseMatrix
#' @importFrom rhdf5 h5readAttributes
.load_csparse_matrix <- function(file, name) {
    p <- .load_simple_vector(file, file.path(name, "indptr")) 
    i <- .load_simple_vector(file, file.path(name, "indices"))
    x <- .load_simple_vector(file, file.path(name, "data"))

    dims <- .load_simple_vector(file, file.path(name, "shape"))
    dimnames <- .load_list(file, file.path(name, "dimnames"), vectors.only=TRUE)

    # Avoid inefficiency of sparseMatrix() constructor.
    if (is.double(x)) {
        cls <- "dgCMatrix"
    } else {
        cls <- "lgCMatrix"
    }
    out <- new(cls, i=i, p=p, x=x, Dim=dims, Dimnames=dimnames)

    DelayedArray(out)
}
