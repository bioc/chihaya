#' Saving a DelayedAbind
#'
#' Save a \linkS4class{DelayedAbind} object.
#' See the \dQuote{Combining} operation at \url{https://ltla.github.io/chihaya} for more details.
#'
#' @param x A \linkS4class{DelayedAbind} object.
#' @param file String containing the path to a HDF5 file.
#' @param name String containing the name of the group to save into.
#'
#' @return A \code{NULL}, invisibly.
#' A group is created at \code{name} containing the contents of the DelayedAbind.
#'
#' @author Aaron Lun
#' 
#' @examples
#' X <- DelayedArray(matrix(runif(100), ncol=20))
#' Y <- cbind(X, X)
#' temp <- tempfile(fileext=".h5")
#' saveDelayed(Y, temp)
#' rhdf5::h5ls(temp)
#' loadDelayed(temp)
#' 
#' @export
#' @rdname DelayedAbind
#' @importFrom rhdf5 h5createGroup h5write
setMethod("saveDelayedObject", "DelayedAbind", function(x, file, name) {
    h5createGroup(file, name)
    .labelOperationGroup(file, name, 'combine')
    write_integer_scalar(file, name, "along", x@along - 1L)
    .saveList(file, "seeds", x@seeds, parent=name)
    invisible(NULL)
})

#' @import DelayedArray
.load_delayed_combine <- function(file, name, contents) {
    along <- .load_simple_vector(file, file.path(name, "along"))
    seeds <- .loadList(file, "seeds", parent=name)

    for (i in seq_along(seeds)) {
        if (!is(seeds[[i]], "DelayedArray")) {
            seeds[[i]] <- DelayedArray(seeds[[i]])
        }
    }

    if (along==0L) {
        do.call(arbind, seeds)
    } else {
        do.call(acbind, seeds)
    }
}
