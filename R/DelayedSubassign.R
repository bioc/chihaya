#' Saving a DelayedSubassign
#'
#' Save a \linkS4class{DelayedSubassign} object into a HDF5 file.
#' See the \dQuote{Specification} vignette for details on the layout.
#'
#' @param x A \linkS4class{DelayedSubassign} object.
#' @param file String containing the path to a HDF5 file.
#' @param name String containing the name of the group to save into.
#' 
#' @return A \code{NULL}, invisibly.
#' A group is created at \code{name} containing the contents of the DelayedSubassign.
#'
#' @author Aaron Lun
#'
#' @examples
#' X <- DelayedArray(matrix(runif(100), ncol=20))
#' X[1:2,3:5] <- matrix(-runif(6), ncol=3)
#' temp <- tempfile(fileext=".h5")
#' saveDelayed(X, temp)
#' rhdf5::h5ls(temp)
#' loadDelayed(temp)
#' 
#' @export
#' @importFrom rhdf5 h5createGroup h5write
setMethod("saveLayer", "DelayedSubassign", function(x, file, name) {
    if (name!="") {
        h5createGroup(file, name)
    }
    .label_group_class(file, name, c('operation', 'subassign'))
    saveLayer(x@Lindex, file, file.path(name, "index"))
    saveLayer(x@Rvalue, file, file.path(name, "value"))
    saveLayer(x@seed, file, file.path(name, "seed"))
    invisible(NULL)
})

.load_delayed_subassign <- function(file, path, contents) {
    x <- .dispatch_loader(file, file.path(path, "seed"), contents[["seed"]])
    if (!is(x, "DelayedArray")) x <- DelayedArray(x)

    index <- .dispatch_loader(file, file.path(path, "index"), contents[["index"]])
    value <- .dispatch_loader(file, file.path(path, "value"), contents[["value"]])

    do.call(`[<-`, c(list(x=x), index, list(value=value)))
} 
