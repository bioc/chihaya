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
        FUN <- switch(attrs$delayed_operation,
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
            `unary special check`=.load_delayed_unary_iso
        )
        vals <- FUN(file, path)

    } else if (identical(attrs$delayed_type, "array")) {
        FUN <- switch(attrs$delayed_array,
            `dense array`=.load_array,
            `sparse matrix`=.load_csparse_matrix,
            `external hdf5 dense array`=.load_dense_hdf5_array,
            `external hdf5 sparse matrix`=.load_sparse_hdf5_matrix
        ) 
        vals <- FUN(file, path)

    } else {
        stop("unsupported type '", attrs$delayed_array[1], "'")
    }

    vals
}
