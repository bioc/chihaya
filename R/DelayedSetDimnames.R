#' Saving a DelayedSetDimnames
#'
#' Save a \linkS4class{DelayedSetDimnames} object.
#'
#' @param x A \linkS4class{DelayedSetDimnames} object.
#' @param file String containing the path to a HDF5 file.
#' @param name String containing the name of the group to save into.
#'
#' @return A \code{NULL}, invisibly.
#' A group is created at \code{name} containing the contents of the DelayedSetDimnames.
#'
#' @details
#' The DelayedSetDimnames class is represented on file by several elements within the \code{name}d group.
#' \itemize{
#' \item \code{type}, a character dataset with two elements.
#' The first is set to \code{"OPERATION"} and the second is set to \code{"DIMNAMES"}.
#' \item \code{dimnames}, a group containing \code{number}, the number of dimensions,
#' and any number of string datasets with names in [1, number].
#' Each dataset contains names along the named dimension of the array.
#' \item \code{seed}, a group or dataset containing the seed to be named.
#' }
#'
#' @author Aaron Lun
#' 
#' @examples
#' X <- DelayedArray(matrix(runif(100), ncol=20))
#' colnames(X) <- LETTERS[1:20]
#' temp <- tempfile(fileext=".h5")
#' saveDelayedOps(X, temp)
#' rhdf5::h5ls(temp)
#' 
#' @export
#' @importFrom rhdf5 h5createGroup h5write
setMethod("saveLayer", "DelayedSetDimnames", function(x, file, name) {
    if (name!="") {
        h5createGroup(file, name)
    }
    h5write(c("OPERATION", "DIMNAMES"), file, file.path(name, "type"))

    h5createGroup(file, file.path(name, "dimnames"))
    dimnames <- x@dimnames
    h5write(length(dimnames), file, file.path(name, "dimnames", "number"))

    for (i in seq_along(dimnames)) {
        if (is.character(dimnames[[i]])) { # avoid NULLs, -1's.
            h5write(dimnames[[i]], file, file.path(name, "dimnames", i))
        }
    }

    saveLayer(x@seed, file, file.path(name, "seed"))

    invisible(NULL)
})
