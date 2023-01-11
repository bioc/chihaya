#' Validate an artifact
#'
#' Validate the delayed objects inside a HDF5 file.
#' This is automatically run at the end of every \code{\link{saveDelayed}} call to check the integrity of the saved files.
#' See \url{https://ltla.github.io/chihaya} for more details.
#'
#' @param path String containing the path to the HDF5 file.
#' @param name String containing the name of the delayed object inside the file.
#'
#' @return \code{NULL} if there are no problems, otherwise an error is raised.
#'
#' @author Aaron Lun
#'
#' @seealso
#' See \url{https://ltla.github.io/chihaya} for the specification.
#' @examples
#' X <- DelayedArray(matrix(runif(100), ncol=20))
#' Y <- X[1:2,3:5]
#' temp <- tempfile(fileext=".h5")
#' saveDelayed(Y, temp)
#' validate(temp, "delayed")
#'
#' @export
#' @importFrom Rcpp sourceCpp
#' @useDynLib chihaya, .registration=TRUE
validate <- function(path, name) {
    stopifnot(length(path)==1, is.character(path), !is.na(path))
    stopifnot(length(name)==1, is.character(name), !is.na(name))
    path <- normalizePath(path, mustWork=TRUE)
    validate_(path, name) 
}

