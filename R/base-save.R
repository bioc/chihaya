#' Saving simple seed classes
#'
#' Methods to save simple seed classes into the delayed operation file.
#' See the \dQuote{Specification} vignette for details on the layout.
#'
#' @param x An R object of the indicated class.
#' @param file String containing the path to a HDF5 file.
#' @param name String containing the name of the group to save into.
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
setMethod("saveLayer", "array", function(x, file, name) {
    if (name!="") {
        h5createGroup(file, name)
    }
    .label_group_class(file, name, "array")

    to.save <- .flip_first_and_second(DelayedArray(x))
    writeHDF5Array(to.save, file, file.path(name, 'data'))

    if (!is.null(dimnames(x))) {
        # Creating a list and then filling it.
        .save_list(dimnames(x), file, file.path(name, 'dimnames'), vectors.only=TRUE)
    }

    invisible(NULL)
})

# Transposing it to get the right orientation, because rhdf5 will store R's
# columns as HDF5 rows, which is pretty damn confusing. This ensures that
# R columns are also stored as HDF5 columns, which makes life easier.
.flip_first_and_second <- function(x) {
    perm <- seq_along(dim(x))
    if (length(perm) > 1) {
        perm <- c(2L, setdiff(perm, 2L))
        x <- aperm(x, perm)
    }
    x
}

#' @importFrom Matrix t
#' @importFrom HDF5Array HDF5Array
#' @importFrom DelayedArray DelayedArray
.load_array <- function(file, name, contents) {
    vals <- h5read(file, file.path(name, "data"))
    vals <- .flip_first_and_second(vals)
    if ("dimnames" %in% names(contents)) {
        dimnames(vals) <- .load_list(file, file.path(name, "dimnames"), contents[["dimnames"]], vectors.only=TRUE)
    }
    DelayedArray(vals)
}

#' @export
#' @rdname base-save
#' @importFrom DelayedArray DelayedArray
setMethod("saveLayer", "DelayedArray", function(x, file, name) {
    saveLayer(x@seed, file, name)
})

#' @export
#' @rdname base-save
#' @importFrom rhdf5 h5createGroup
#' @importClassesFrom Matrix CsparseMatrix
setMethod("saveLayer", "CsparseMatrix", function(x, file, name) {
    if (name!="") {
        h5createGroup(file, name)
    }
    .label_group_class(file, name, c("seed", "csparse matrix"))

    # TODO: convert this into a UTF-8 string. 
    .label_group(file, name, "h5sparse_format", "csc-matrix")
    .label_group(file, name, "h5sparse_shape", dim(x))

    h5write(x@p, file, file.path(name, "indptr"))
    h5write(x@i, file, file.path(name, "indices"))
    h5write(x@x, file, file.path(name, "data"))
    .save_list(dimnames(x), file, file.path(name, "dimnames"), vectors.only=TRUE)
})

#' @importFrom Matrix sparseMatrix
#' @importFrom rhdf5 h5readAttributes
.load_csparse_matrix <- function(file, name, contents) {
    p <- .load_simple_vector(file, file.path(name, "indptr")) 
    i <- .load_simple_vector(file, file.path(name, "indices"))
    x <- .load_simple_vector(file, file.path(name, "data"))

    attrs <- h5readAttributes(file, name)
    dims <- as.integer(attrs$h5sparse_shape)
    dimnames <- .load_list(file, file.path(name, "dimnames"), contents[["dimnames"]], vectors.only=TRUE)

    # Avoid inefficiency of sparseMatrix() constructor.
    if (is.double(x)) {
        cls <- "dgCMatrix"
    } else {
        cls <- "lgCMatrix"
    }
    out <- new(cls, i=i, p=p, x=x, Dim=dims, Dimnames=dimnames)

    DelayedArray(out)
}
