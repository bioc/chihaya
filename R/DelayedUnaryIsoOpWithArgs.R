#' Saving a DelayedUnaryIsoOpWithArgs
#'
#' Save a \linkS4class{DelayedUnaryIsoOpWithArgs} object into a HDF5 file.
#' See the \dQuote{Unary ...} operation at \url{https://ltla.github.io/chihaya} for more details.
#'
#' @param x A \linkS4class{DelayedUnaryIsoOpWithArgs} object.
#' @inheritParams saveDelayedObject
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
setMethod("saveDelayedObject", "DelayedUnaryIsoOpWithArgs", function(x, file, name) {
    if (name!="") {
        h5createGroup(file, name)
    }

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
    if (chosen %in% supported.Logic) {
        .labelOperationGroup(file, name, 'unary logic')
        chosen <- .save_Ops(chosen)
    } else if (chosen %in% supported.Compare) {
        .labelOperationGroup(file, name, 'unary comparison')
    } else if (chosen %in% supported.Arith) {
        .labelOperationGroup(file, name, 'unary arithmetic')
    }
    write_string_scalar(file, name, "method", chosen)

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

    write_string_scalar(file, name, "side", if (left) "left" else "right")
    write_integer_scalar(file, name, "along", along - 1L)

    if (length(args) == 1L) {
        .saveDataset(file, "value", args, parent=name, scalar=TRUE)
    } else if (is.null(dim(args)) || length(dim(args)) == 1L) {
        .saveDataset(file, "value", args, parent=name)
    } else {
        stop("multi-dimensional 'value' not supported in 'DelayedUnaryIsoOpWithArgs'")
    }

    saveDelayedObject(x@seed, file, file.path(name, "seed"))
    invisible(NULL)
})
