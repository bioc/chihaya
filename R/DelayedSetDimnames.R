#' Saving a DelayedSetDimnames
#'
#' Save a \linkS4class{DelayedSetDimnames} object.
#' See the \dQuote{Specification} vignette for details on the layout.
#'
#' @param x A \linkS4class{DelayedSetDimnames} object.
#' @param file String containing the path to a HDF5 file.
#' @param name String containing the name of the group to save into.
#'
#' @return A \code{NULL}, invisibly.
#' A group is created at \code{name} containing the contents of the DelayedSetDimnames.
#'
#' @author Aaron Lun
#' 
#' @examples
#' X <- DelayedArray(matrix(runif(100), ncol=20))
#' colnames(X) <- LETTERS[1:20]
#' temp <- tempfile(fileext=".h5")
#' saveDelayed(X, temp)
#' rhdf5::h5ls(temp)
#' loadDelayed(temp)
#' 
#' @export
#' @importFrom rhdf5 h5createGroup h5write
setMethod("saveLayer", "DelayedSetDimnames", function(x, file, name) {
    if (name!="") {
        h5createGroup(file, name)
    }
    .label_group_class(file, name, c('operation', 'dimnames'))

    dimnames <- x@dimnames
    for (i in seq_along(dimnames)) {
        if (!is.character(dimnames[[i]])) { # avoid -1's.
            dimnames[i] <- list(NULL)
        }
    }

    saveLayer(dimnames, file, file.path(name, "dimnames"))
    saveLayer(x@seed, file, file.path(name, "seed"))
    invisible(NULL)
})

.load_delayed_dimnames <- function(file, name, contents) {
    x <- .dispatch_loader(file, file.path(name, "seed"), contents[["seed"]])
    if (!is(x, "DelayedArray")) x <- DelayedArray(x)
    dnames <- .dispatch_loader(file, file.path(name, "dimnames"), contents[["dimnames"]])
    dimnames(x) <- dnames
    x
}
