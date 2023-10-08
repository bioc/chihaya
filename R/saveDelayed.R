#' Save a DelayedMatrix
#'
#' Save a \linkS4class{DelayedMatrix} object to a location within a HDF5 file.
#'
#' @param x A \linkS4class{DelayedArray} object.
#' @param file String containing a path to a HDF5 file.
#' This will be created if it does not yet exist.
#' @param path String containing a path inside a HDF5 file.
#' This should not already exist, though any parent groups should already be constructed.
#' 
#' @return The contents of \code{x} are written to file and a \code{NULL} is invisibly returned.
#'
#' @details
#' See the various \code{\link{saveDelayedObject}} methods for how each suite of delayed operations is handled.
#' Also see \url{https://ltla.github.io/chihaya} for more details on the data layout inside the HDF5 file.
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
#' rhdf5::h5ls(temp)
#'
#' @export
#' @importFrom rhdf5 h5createFile
saveDelayed <- function(x, file, path="delayed") {
    if (!is(x, "DelayedArray")) {
        stop("'x' should be a DelayedArray")
    }
    if (!file.exists(file)) {
        h5createFile(file)
    }
    saveDelayedObject(x@seed, file, path)
    .label_group(file, path, c(delayed_version="1.0.0"))
    validate(file, path)
    invisible(NULL)
}
