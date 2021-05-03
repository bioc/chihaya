#' Saving a DelayedAperm
#'
#' Save a \linkS4class{DelayedAperm} object.
#'
#' @param x A \linkS4class{DelayedAperm} object.
#' @param file String containing the path to a HDF5 file.
#' @param name String containing the name of the group to save into.
#'
#' @return A \code{NULL}, invisibly.
#' A group is created at \code{name} containing the contents of the DelayedAperm.
#'
#' @details
#' The DelayedAperm class is represented on file by several elements within the \code{name}d group.
#' \itemize{
#' \item \code{type}, a character dataset with two elements.
#' The first is set to \code{"OPERATION"} and the second is set to \code{"TRANSPOSE"}.
#' \item \code{along}, a dataset specifying the dimension to combine along.
#' This should be of length equal to the number of dimensions and contain unique values in [1, number of dimensions].
#' \item \code{seed}, a group or dataset containing the seed to be permuted.
#' }
#'
#' @author Aaron Lun
#' 
#' @examples
#' X <- DelayedArray(matrix(runif(100), ncol=20))
#' Y <- t(X)
#' temp <- tempfile(fileext=".h5")
#' saveDelayedOps(Y, temp)
#' rhdf5::h5ls(temp)
#' 
#' @export
#' @importFrom rhdf5 h5createGroup h5write
setMethod("saveLayer", "DelayedAperm", function(x, file, name) {
    if (name!="") {
        h5createGroup(file, name)
    }
    h5write(c("OPERATION", "TRANSPOSE"), file, file.path(name, "type"))
    h5write(x@perm, file, file.path(name, "permutation"))
    saveLayer(x@seed, file, file.path(name, "seed"))
    invisible(NULL)
})
