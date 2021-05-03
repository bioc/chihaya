#' Saving a DelayedAbind
#'
#' Save a \linkS4class{DelayedAbind} object.
#'
#' @param x A \linkS4class{DelayedAbind} object.
#' @param file String containing the path to a HDF5 file.
#' @param name String containing the name of the group to save into.
#'
#' @return A \code{NULL}, invisibly.
#' A group is created at \code{name} containing the contents of the DelayedUnaryIsoOpStack.
#'
#' @details
#' The DelayedAbind class is represented on file by several elements within the \code{name}d group.
#' \itemize{
#' \item \code{type}, a character dataset with two elements.
#' The first is set to \code{"OPERATION"} and the second is set to \code{"COMBINE"}.
#' \item \code{along}, a dataset specifying the dimension to combine along.
#' This should be integer and of length 1.
#' \item \code{seeds}, a group describing the seeds to combine.
#' This contains additional groups or datasets, named by the order in which they are to be combined.
#' }
#'
#' @author Aaron Lun
#' 
#' @examples
#' X <- DelayedArray(matrix(runif(100), ncol=20))
#' Y <- cbind(X, X)
#' temp <- tempfile(fileext=".h5")
#' saveDelayedOps(Y, temp)
#' rhdf5::h5ls(temp)
#' 
#' @export
#' @importFrom rhdf5 h5createGroup h5write
setMethod("saveLayer", "DelayedAbind", function(x, file, name) {
    if (name!="") {
        h5createGroup(file, name)
    }
    h5write(c("OPERATION", "COMBINE"), file, file.path(name, "type"))

    path <- file.path(name, "along")
    h5write(x@along, file, path)

    seeds <- x@seeds
    spath <- file.path(name, "seeds")
    h5createGroup(file, spath)

    for (i in seq_along(seeds)) {
        saveLayer(x@seeds[[i]], file, file.path(spath, i))
    }

    invisible(NULL)
})
