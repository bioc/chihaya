#' Saving a DelayedSetDimnames
#'
#' Save a \linkS4class{DelayedSetDimnames} object.
#' See the \dQuote{Dimnames assignment} operation at \url{https://ltla.github.io/chihaya} for more details.
#'
#' @param x A \linkS4class{DelayedSetDimnames} object.
#' @inheritParams saveDelayedObject
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
#' @rdname DelayedSetDimnames
#' @importFrom rhdf5 h5createGroup h5write
setMethod("saveDelayedObject", "DelayedSetDimnames", function(x, file, name) {
    h5createGroup(file, name)
    .labelOperationGroup(file, name, 'dimnames')

    dimnames <- x@dimnames
    for (i in seq_along(dimnames)) {
        if (!is.character(dimnames[[i]])) { # avoid -1's.
            dimnames[i] <- list(NULL)
        }
    }

    .saveList(file, "dimnames", dimnames, parent=name, vectors.only=TRUE)
    saveDelayedObject(x@seed, file, file.path(name, "seed"))
    invisible(NULL)
})

#' @import DelayedArray
.load_delayed_dimnames <- function(file, name, contents) {
    x <- .dispatch_loader(file, file.path(name, "seed"))
    if (!is(x, "DelayedArray")) {
        x <- DelayedArray(x)
    }

    dnames <- .loadList(file, "dimnames", parent=name, vectors.only=TRUE)
    dimnames(x) <- dnames
    x
}
