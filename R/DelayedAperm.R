#' Saving a DelayedAperm
#'
#' Save a \linkS4class{DelayedAperm} object.
#' See the \dQuote{Specification} vignette for details on the layout.
#'
#' @param x A \linkS4class{DelayedAperm} object.
#' @param file String containing the path to a HDF5 file.
#' @param name String containing the name of the group to save into.
#'
#' @return A \code{NULL}, invisibly.
#' A group is created at \code{name} containing the contents of the DelayedAperm.
#'
#' @author Aaron Lun
#' 
#' @examples
#' X <- DelayedArray(matrix(runif(100), ncol=20))
#' Y <- t(X)
#' temp <- tempfile(fileext=".h5")
#' saveDelayed(Y, temp)
#' rhdf5::h5ls(temp)
#' loadDelayed(temp)
#' 
#' @export
#' @importFrom rhdf5 h5createGroup h5write
setMethod("saveLayer", "DelayedAperm", function(x, file, name) {
    if (name!="") {
        h5createGroup(file, name)
    }
    .label_group_class(file, name, c('operation', 'transpose'))
    saveLayer(x@perm, file, file.path(name, "permutation"))
    saveLayer(x@seed, file, file.path(name, "seed"))
    invisible(NULL)
})

.load_delayed_aperm <- function(file, name, contents) {
    x <- .dispatch_loader(file, file.path(name, "seed"), contents[["seed"]])
    perm <- .dispatch_loader(file, file.path(name, "permutation"), contents[["permutation"]])
    if (!is(x, "DelayedArray")) x <- DelayedArray(x)
    aperm(x, perm)
}
