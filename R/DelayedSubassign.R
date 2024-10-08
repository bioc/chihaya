#' Saving a DelayedSubassign
#'
#' Save a \linkS4class{DelayedSubassign} object into a HDF5 file.
#' See the \dQuote{Subset assignment} operation at \url{https://artifactdb.github.io/chihaya/} for more details.
#'
#' @param x A \linkS4class{DelayedSubassign} object.
#' @inheritParams saveDelayedObject
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
#' @rdname DelayedSubassign
#' @importFrom rhdf5 h5createGroup h5write
setMethod("saveDelayedObject", "DelayedSubassign", function(x, file, name) {
    h5createGroup(file, name)
    .labelOperationGroup(file, name, 'subset assignment')

    zerobased <- .zero_indices(x@Lindex)
    .saveList(file, "index", zerobased, parent=name, vectors.only=TRUE)

    saveDelayedObject(x@seed, file, file.path(name, "seed"))
    saveDelayedObject(x@Rvalue, file, file.path(name, "value"))
    invisible(NULL)
})

#' @import DelayedArray
.load_delayed_subassign <- function(file, path, contents) {
    x <- .dispatch_loader(file, file.path(path, "seed"))
    if (!is(x, "DelayedArray")) {
        x <- DelayedArray(x)
    }

    value <- .dispatch_loader(file, file.path(path, "value"))

    index <- .loadList(file, "index", parent=path, vectors.only=TRUE)
    index <- .restore_indices(index)

    do.call(`[<-`, c(list(x=x), index, list(value=value)))
} 
