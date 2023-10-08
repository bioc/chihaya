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
    out <- .dispatch_loader(file, path)
    DelayedArray(out)
}

#' @importFrom rhdf5 h5readAttributes h5read
.dispatch_loader <- function(file, path) {
    attrs <- h5readAttributes(file, path)

    if (is.null(attrs$delayed_type)) {
        vals <- .loadList(file, path)

    } else if (identical(attrs$delayed_type, "operation")) {
        if (startsWith(attrs$delayed_operation, "custom ") && h5exists(file, path, "r_package")) {
            candidate <- h5read(file, paste0(path, "/r_package"), drop=TRUE)
            if (!isNamespaceLoaded(candidate)) {
                loadNamespace(candidate)
            }
        }

        key <- attrs$delayed_operation

        # Check if there's a R type hint that we can use.
        if (h5exists(file, path, "r_type_hint")) {
            altkey <- h5read(file, paste0(path, "/r_type_hint"), drop=TRUE)
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
            candidate <- h5read(file, paste0(path, "/r_package"), drop=TRUE)
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
