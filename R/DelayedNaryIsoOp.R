#' Saving a DelayedNaryIsoOp
#'
#' Save a \linkS4class{DelayedNaryIsoOp} object into a HDF5 file.
#' See the \dQuote{Specification} vignette for details on the layout.
#'
#' @param x A \linkS4class{DelayedNaryIsoOp} object.
#' @inheritParams saveLayer
#' 
#' @return A \code{NULL}, invisibly.
#' A group is created at \code{name} containing the contents of the DelayedNaryIsoOp.
#'
#' @author Aaron Lun
#'
#' @examples
#' X <- DelayedArray(matrix(runif(100), ncol=5))
#' Y <- DelayedArray(matrix(runif(100), ncol=5))
#' Z <- X * Y
#' temp <- tempfile(fileext=".h5")
#' saveDelayed(Z, temp)
#' rhdf5::h5ls(temp)
#' loadDelayed(temp)
#' 
#' @export
#' @rdname DelayedNaryIsoOp
#' @importFrom rhdf5 h5createGroup h5write
setMethod("saveLayer", "DelayedNaryIsoOp", function(x, file, name) {
    if (name!="") {
        h5createGroup(file, name)
    }
    .label_group_class(file, name, c('operation', 'n-ary isometric'))

    # Figuring out the identity of the operation.
    chosen <- NULL
    possibilities <- list(`+`=`+`, `/`=`/`, `*`=`*`, `-`=`-`, `^`=`^`)
    for (p in names(possibilities)) {
        if (identical(x@OP, possibilities[[p]])) {
            chosen <- p
            break
        }
    }
    if (is.null(chosen)) {
        stop("unknown operation in ", class(x))
    }
    h5write(chosen, file, file.path(name, "operation"))

    .save_list(x@Rargs, file, file.path(name, "right_arguments"))
    .save_list(x@seeds, file, file.path(name, "seeds"))
    invisible(NULL)
})

.load_delayed_nary_iso <- function(file, name, contents) {
    seeds <- .load_list(file, file.path(name, "seeds"), contents[["seeds"]])
    for (i in seq_along(seeds)) {
        if (!is(seeds[[i]], "DelayedArray")) {
            seeds[[i]] <- DelayedArray(seeds[[i]])
        }
    }
    op <- .load_simple_vector(file, file.path(name, "operation"))
    Rargs <- .load_list(file, file.path(name, "right_arguments"), contents[["dimnames"]])
    do.call(get(op), c(seeds, Rargs))
}
