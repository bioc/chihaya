#' Get or set loaders for operations/arrays
#'
#' Get or set loading functions for operations or arrays that were saved into the HDF5 file.
#' This enables third-party packages to modify the \pkg{chihaya} framework for their own purposes.
#'
#' @param operations Named list of loading functions for operations.
#' Each function should accept the same arguments as \code{\link{loadDelayed}} and return a matrix-like object.
#' Names should match the \code{delayed_operation} string used to save the operation to file.
#' @param arrays Named list of loading functions for arrays.
#' Each function should accept the same arguments as \code{\link{loadDelayed}} and return a matrix-like object.
#' Names should match the \code{delayed_array} string used to save the array to file.
#'
#' @return 
#' If \code{operations} is missing, \code{customLoadOperations} will return a list of the current custom operations that have been registered with \pkg{chihaya}.
#' If \code{operations} is provided, it is used to define the set of custom operations, and the \emph{previous} set of operations is returned.
#' The same approach is used for \code{arrays} in \code{customLoadArrays}.
#'
#' @details
#' This function can be used to modify the loading procedure for existing operations/arrays or to add new loaders for new arrays.
#'
#' Custom arrays should use a \code{"custom "} prefix in the name to ensure that they do not clash with future additions to the \pkg{chihaya} specification.
#' If an instance of a custom array contains an \pkg{r_package} scalar string dataset inside its HDF5 group, the string is assumed to hold the name of the package that implements its loading handler;
#' if this package is installed, it will be automatically loaded and used by \code{\link{loadDelayed}}.
#'
#' Custom operations can be added, but they are not currently supported via \code{\link{validate}}, so it is assumed that such operations will be created outside of \code{\link{saveDelayed}}.
#'
#' @author Aaron Lun
#' @examples
#' library(HDF5Array)
#' X <- rsparsematrix(100, 20, 0.1)
#' Y <- DelayedArray(X)
#' Z <- log2(Y + 1)
#'
#' temp <- tempfile(fileext=".h5")
#' saveDelayed(Z, temp)
#'
#' # Overriding an existing operation:
#' ops <- knownOperations()
#' old_unary <- ops[["unary math"]]
#' ops[["unary math"]] <- function(file, path) {
#'     cat("WHEE!\n")
#'     old_unary(file, path)
#' }
#' old <- knownOperations(ops)
#' 
#' # Prints our little message:
#' loadDelayed(temp)
#'
#' # Setting it back.
#' knownOperations(old)
#' 
#' @export
knownOperations <- function(operations) {
    prev <- known.env$operations
    if (missing(operations)) {
        prev
    } else {
        known.env$operations <- operations
        invisible(prev)
    }
}

#' @export
#' @rdname knownOperations
knownArrays <- function(arrays) {
    prev <- known.env$arrays
    if (missing(arrays)) {
        prev
    } else {
        known.env$arrays <- arrays
        invisible(prev)
    }
}
