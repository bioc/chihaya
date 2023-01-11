#' Save a delayed object 
#'
#' Saves a delayed object recursively.
#'
#' @param x An R object containing a delayed operation or seed class.
#' @param file String containing the path to a HDF5 file.
#' @param name String containing the name of the group to save into.
#'
#' @author Aaron Lun
#' @return A \code{NULL} is returned invisibly.
#' A group is created at \code{name} inside \code{file} and the delayed operation is saved within.
#'
#' @details
#' The \code{\link{saveDelayedObject}} generic is intended for developers to create methods for new operations.
#' End-users should use the \code{\link{saveDelayed}} function instead.
#'
#' The \linkS4class{DelayedArray} method will simply extract the seed and use it to call \code{\link{saveDelayedObject}} again.
#'
#' @examples
#' library(HDF5Array)
#' X <- rsparsematrix(100, 20, 0.1)
#' Y <- DelayedArray(X)[1:10,1:5]
#'
#' temp <- tempfile(fileext=".h5")
#' rhdf5::h5createFile(temp)
#' saveDelayedObject(Y, temp, "FOO")
#' rhdf5::h5ls(temp)
#' @export
#' @import methods
#' @name saveDelayedObject 
setGeneric("saveDelayedObject", function(x, file, name) standardGeneric("saveDelayedObject"))
