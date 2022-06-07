#' Load a DelayedMatrix
#'
#' Load a \linkS4class{DelayedMatrix} object from a location within a HDF5 file.
#'
#' @param file String containing a path to a HDF5 file.
#' @param path String containing a path inside a HDF5 file containing the DelayedMatrix.
#' 
#' @return A \linkS4class{DelayedMatrix} containing the contents at \code{path}.
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
#' loadDelayed(temp)
#'
#' @seealso
#' \code{\link{knownOperations}} and \code{\link{knownArrays}}, to modify the loading procedure.
#'
#' @export
loadDelayed <- function(file, path="delayed") {
    .dispatch_loader(file, path)
}

#' @importFrom rhdf5 h5readAttributes h5read
.dispatch_loader <- function(file, path) {
    attrs <- h5readAttributes(file, path)

    if (is.null(attrs$delayed_type)) {
        vals <- .load_list(file, path)

    } else if (identical(attrs$delayed_type, "operation")) {
        if (startsWith(attrs$delayed_operation, "custom ") && h5exists(file, path, "r_package")) {
            candidate <- h5read(file, paste0(path, "/r_package"))
            if (!isNamespaceLoaded(candidate)) {
                loadNamespace(candidate)
            }
        }

        key <- attrs$delayed_operation

        # Check if there's a R type hint that we can use.
        if (h5exists(file, path, "r_type_hint")) {
            altkey <- h5read(file, paste0(path, "/r_type_hint"))
            if (altkey %in% names(known.env$operations) && known.env$operations[[altkey]]()) {
                key <- altkey
            }
        }

        FUN <- known.env$operations[[key]]
        if (is.null(FUN)) {
            stop("unknown operation type '", attrs$delayed_operation, "'")
        }

        vals <- FUN(file, path)

    } else if (identical(attrs$delayed_type, "array")) {
        if (startsWith(attrs$delayed_array, "custom ") && h5exists(file, path, "r_package")) {
            candidate <- h5read(file, paste0(path, "/r_package"))
            if (!isNamespaceLoaded(candidate)) {
                loadNamespace(candidate)
            }
        }

        FUN <- known.env$arrays[[attrs$delayed_array]]
        if (is.null(FUN)) {
            stop("unknown array type '", attrs$delayed_array, "'")
        }

        vals <- FUN(file, path)

    } else {
        stop("unsupported type '", attrs$delayed_type[1], "'")
    }

    vals
}

known.env <- new.env()

known.env$operations <- list(
    subset=.load_delayed_subset,
    transpose=.load_delayed_aperm,
    combine=.load_delayed_combine,
    `binary arithmetic`=.load_delayed_nary_iso,
    `binary comparison`=.load_delayed_nary_iso,
    `binary logic`=.load_delayed_nary_iso,
    dimnames=.load_delayed_dimnames,
    `subset assignment`=.load_delayed_subassign,
    `unary arithmetic`=.load_delayed_unary_iso,
    `unary comparison`=.load_delayed_unary_iso,
    `unary logic`=.load_delayed_unary_iso,
    `unary math`=.load_delayed_unary_iso,
    `unary special check`=.load_delayed_unary_iso,
    `matrix product`=.load_matrix_product,
    `residual matrix`=.load_residual_matrix
)

known.env$arrays <- list(
    `dense array`=.load_array,
    `sparse matrix`=.load_csparse_matrix,
    `external hdf5 dense array`=.load_dense_hdf5_array,
    `external hdf5 sparse matrix`=.load_sparse_hdf5_matrix
)

#' Get or set loaders for operations/arrays
#'
#' Get or set loading functions for operations or arrays that were saved into the HDF5 file.
#' This enables third-party packages to modify the \pkg{chihaya} framework for their own purposes.
#'
#' @param operations Named list of loading functions for operations.
#' Each function should accept the same arguments as \code{\link{loadDelayed}} and return a \linkS4class{DelayedArray} of some kind.
#' Names should match the \code{delayed_operation} string used to save the operation to file.
#' @param arrays Named list of loading functions for arrays.
#' Each function should accept the same arguments as \code{\link{loadDelayed}} and return an array of some kind.
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
