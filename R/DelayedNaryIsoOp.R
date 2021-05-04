#' Saving a DelayedNaryIsoOp
#'
#' Save a \linkS4class{DelayedNaryIsoOp} object into a HDF5 file.
#' See the \dQuote{Specification} vignette for details on the layout.
#'
#' @param x A \linkS4class{DelayedNaryIsoOp} object.
#' @param file String containing the path to a HDF5 file.
#' @param name String containing the name of the group to save into.
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
    saveLayer(chosen, file, file.path(name, "operation"))

    saveLayer(x@Rargs, file, file.path(name, "right_arguments"))
    saveLayer(x@seeds, file, file.path(name, "seeds"))
    invisible(NULL)
})

.load_delayed_nary_iso <- function(file, name, contents) {
    seeds <- .dispatch_loader(file, file.path(name, "seeds"), contents[["seeds"]])
    for (i in seq_along(seeds)) {
        if (!is(seeds[[i]], "DelayedArray")) {
            seeds[[i]] <- DelayedArray(seeds[[i]])
        }
    }
    op <- .dispatch_loader(file, file.path(name, "operation"), contents[["operation"]])
    Rargs <- .dispatch_loader(file, file.path(name, "right_arguments"), contents[["right_arguments"]])
    do.call(get(op), c(seeds, Rargs))
}
