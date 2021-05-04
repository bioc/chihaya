#' Saving a DelayedAbind
#'
#' Save a \linkS4class{DelayedAbind} object.
#' See the \dQuote{Specification} vignette for details on the layout.
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
setMethod("saveLayer", "DelayedAbind", function(x, file, name) {
    if (name!="") {
        h5createGroup(file, name)
    }
    .label_group_class(file, name, c('operation', 'combine'))
    h5write(x@along, file, file.path(name, "along"))
    .save_list(x@seeds, file, file.path(name, "seeds"))
    invisible(NULL)
})

.load_delayed_combine <- function(file, name, contents) {
    along <- .load_simple_vector(file, file.path(name, "along"))
    seeds <- .load_list(file, file.path(name, "seeds"), contents[["seeds"]])

    for (i in seq_along(seeds)) {
        if (!is(seeds[[i]], "DelayedArray")) {
            seeds[[i]] <- DelayedArray(seeds[[i]])
        }
    }

    if (along==1L) {
        do.call(arbind, seeds)
    } else {
        do.call(acbind, seeds)
    }
}
