#' Saving a DelayedSubset
#'
#' Save a \linkS4class{DelayedSubset} object into a HDF5 file.
#'
#' @param x A \linkS4class{DelayedSubset} object.
#' @param file String containing the path to a HDF5 file.
#' @param name String containing the name of the group to save into.
#' 
#' @return A \code{NULL}, invisibly.
#' A group is created at \code{name} containing the contents of the DelayedSubset.
#'
#' @details
#' The DelayedSubset class is represented on file by several elements within the \code{name}d group.
#' \itemize{
#' \item \code{type}, a character dataset with two elements.
#' The first is set to \code{"OPERATION"} and the second is set to \code{"SUBSET"}.
#' \item \code{index}, a group containing \code{number}, the number of dimensions,
#' and any number of integer datasets with names in [1, number].
#' Each dataset contains 1-based indices to subset the named dimension of the array.
#' \item \code{seed}, a group or dataset containing the seed to be subsetted.
#' }
#'
#' @author Aaron Lun
#'
#' @examples
#' X <- DelayedArray(matrix(runif(100), ncol=20))
#' Y <- X[1:2,3:5]
#' temp <- tempfile(fileext=".h5")
#' saveDelayedOps(Y, temp)
#' rhdf5::h5ls(temp)
#' 
#' @export
#' @importFrom rhdf5 h5createGroup h5write
setMethod("saveLayer", "DelayedSubset", function(x, file, name) {
    if (name!="") {
        h5createGroup(file, name)
    }
    h5write(c("OPERATION", "SUBSET"), file, file.path(name, "type"))

    h5createGroup(file, file.path(name, "index"))
    indices <- x@index
    h5write(length(indices), file, file.path(name, "index", "number"))
    for (i in seq_along(indices)) {
        h5write(indices[[i]], file, file.path(name, "index", i))
    }

    saveLayer(x@seed, file, file.path(name, "seed"))
    invisible(NULL)
})
