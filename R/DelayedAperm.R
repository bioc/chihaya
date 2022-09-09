#' Saving a DelayedAperm
#'
#' Save a \linkS4class{DelayedAperm} object.
#' See the \dQuote{Transposition} operation at \url{https://ltla.github.io/chihaya} for more details.
#'
#' @param x A \linkS4class{DelayedAperm} object.
#' @inheritParams saveDelayedObject
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
#' @rdname DelayedAperm
#' @importFrom rhdf5 h5createGroup h5write
setMethod("saveDelayedObject", "DelayedAperm", function(x, file, name) {
    h5createGroup(file, name)
    .labelOperationGroup(file, name, 'transpose')
    h5write(x@perm - 1L, file, file.path(name, "permutation"))
    saveDelayedObject(x@seed, file, file.path(name, "seed"))
    invisible(NULL)
})

#' @import DelayedArray
.load_delayed_aperm <- function(file, name, contents) {
    x <- .dispatch_loader(file, file.path(name, "seed"))
    if (!is(x, "DelayedArray")) {
        x <- DelayedArray(x)
    }

    perm <- .load_simple_vector(file, file.path(name, "permutation"))
    aperm(x, perm + 1L)
}
