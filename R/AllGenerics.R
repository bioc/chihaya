#' Save a delayed layer
#'
#' Saves a delayed layer, either an operation or a seed array.
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
#' The \code{\link{saveLayer}} generic is intended for developers to create methods for new operations.
#' End-users should use the \code{\link{saveDelayed}} function instead.
#'
#' @export
#' @import methods
#' @name saveLayer
setGeneric("saveLayer", function(x, file, name) standardGeneric("saveLayer"))
