#' Saving a DelayedSubassign
#'
#' Save a \linkS4class{DelayedSubassign} object into a HDF5 file.
#'
#' @param x A \linkS4class{DelayedSubassign} object.
#' @param file String containing the path to a HDF5 file.
#' @param name String containing the name of the group to save into.
#' 
#' @return A \code{NULL}, invisibly.
#' A group is created at \code{name} containing the contents of the DelayedSubassign.
#'
#' @details
#' The DelayedSubassign class is represented on file by several elements within the \code{name}d group.
#' \itemize{
#' \item \code{type}, a character dataset with two elements.
#' The first is set to \code{"OPERATION"} and the second is set to \code{"SUBSET"}.
#' \item \code{index}, a group containing \code{number}, the number of dimensions,
#' and any number of integer datasets with names in [1, number].
#' Each dataset contains 1-based indices to subset the named dimension of the array.
#' \item \code{value}, a group or dataset containing the value to use for replacement.
#' This may potentially be another seed-like structure.
#' \item \code{seed}, a group or dataset containing the seed to assign the replacement value to.
#' }
#'
#' @author Aaron Lun
#'
#' @examples
#' X <- DelayedArray(matrix(runif(100), ncol=20))
#' X[1:2,3:5] <- matrix(-runif(6), ncol=3)
#' temp <- tempfile(fileext=".h5")
#' saveDelayedOps(X, temp)
#' rhdf5::h5ls(temp)
#' 
#' @export
#' @importFrom rhdf5 h5createGroup h5write
setMethod("saveLayer", "DelayedSubassign", function(x, file, name) {
    if (name!="") {
        h5createGroup(file, name)
    }
    h5write(c("OPERATION", "SUBASSIGN"), file, file.path(name, "type"))

    h5createGroup(file, file.path(name, "index"))
    indices <- x@Lindex
    h5write(length(indices), file, file.path(name, "index", "number"))
    for (i in seq_along(indices)) {
        h5write(indices[[i]], file, file.path(name, "index", i))
    }

    saveLayer(x@Rvalue, file, file.path(name, "value"))
    saveLayer(x@seed, file, file.path(name, "seed"))
    invisible(NULL)
})
