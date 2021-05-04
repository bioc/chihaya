#' Saving a DelayedSubset
#'
#' Save a \linkS4class{DelayedSubset} object into a HDF5 file.
#' See the \dQuote{Specification} vignette for details on the layout.
#'
#' @param x A \linkS4class{DelayedSubset} object.
#' @param file String containing the path to a HDF5 file.
#' @param name String containing the name of the group to save into.
#' 
#' @return A \code{NULL}, invisibly.
#' A group is created at \code{name} containing the contents of the DelayedSubset.
#'
#' @author Aaron Lun
#'
#' @examples
#' X <- DelayedArray(matrix(runif(100), ncol=20))
#' Y <- X[1:2,3:5]
#' temp <- tempfile(fileext=".h5")
#' saveDelayed(Y, temp)
#' rhdf5::h5ls(temp)
#' loadDelayed(temp)
#' 
#' @export
#' @importFrom rhdf5 h5createGroup h5write
setMethod("saveLayer", "DelayedSubset", function(x, file, name) {
    if (name!="") {
        h5createGroup(file, name)
    }
    .label_group_class(file, name, c('operation', 'subset'))
    saveLayer(x@index, file, file.path(name, "index"))
    saveLayer(x@seed, file, file.path(name, "seed"))
    invisible(NULL)
})

.load_delayed_subset <- function(file, name, contents) {
    x <- .dispatch_loader(file, file.path(name, "seed"), contents[["seed"]])
    indices <- .dispatch_loader(file, file.path(name, "index"), contents[["index"]])
    if (!is(x, "DelayedArray")) x <- DelayedArray(x)
    do.call(`[`, c(list(x), indices, list(drop=FALSE)))
}
