#' Saving a DelayedUnaryIsoOpWithArgs
#'
#' Save a \linkS4class{DelayedUnaryIsoOpWithArgs} object into a HDF5 file.
#' See the \dQuote{Specification} vignette for details on the layout.
#'
#' @param x A \linkS4class{DelayedUnaryIsoOpWithArgs} object.
#' @inheritParams saveLayer
#' 
#' @return A \code{NULL}, invisibly.
#' A group is created at \code{name} containing the contents of the DelayedUnaryIsoOpWithArgs.
#'
#' @author Aaron Lun
#'
#' @examples
#' X <- DelayedArray(matrix(runif(100), ncol=5))
#' Y <- (1:20 + X) / runif(5)
#' temp <- tempfile(fileext=".h5")
#' saveDelayed(Y, temp)
#' rhdf5::h5ls(temp)
#' loadDelayed(temp)
#' 
#' @export
#' @rdname DelayedUnaryIsoOpWithArgs
#' @importFrom rhdf5 h5createGroup h5write
setMethod("saveLayer", "DelayedUnaryIsoOpWithArgs", function(x, file, name) {
    if (name!="") {
        h5createGroup(file, name)
    }
    .label_group_class(file, name, c('operation', 'unary isometric'))

    # Figuring out the identity of the operation.
    chosen <- NULL
    for (p in supported.Ops) {
        if (identical(x@OP, get(p, envir=baseenv()))) {
            chosen <- p
            break
        }
    }
    if (is.null(chosen)) {
        stop("unknown operation in ", class(x))
    }
    h5write(chosen, file, file.path(name, "operation"))

    # Saving the left and right args. There should only be one or the other.
    # as the presence of both is not commutative.
    if (length(x@Rargs) + length(x@Largs) !=1) {
        stop("'DelayedUnaryIsoApWithArgs' should operate on exactly one argument")
    }

    left <- length(x@Largs) > 0
    if (left) {
        args <- x@Largs[[1]]
        along <- x@Lalong[1]
    } else {
        args <- x@Rargs[[1]]
        along <- x@Ralong[1]
    }

    h5createGroup(file, file.path(name, "parameters"))
    h5write(if (left) "left" else "right", file, file.path(name, "parameters/side"))
    h5write(along, file, file.path(name, "parameters/along"))
    h5write(args, file, file.path(name, "parameters/value"))

    saveLayer(x@seed, file, file.path(name, "seed"))
    invisible(NULL)
})
