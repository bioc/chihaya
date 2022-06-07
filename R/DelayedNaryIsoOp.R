#' Saving a DelayedNaryIsoOp
#'
#' Save a \linkS4class{DelayedNaryIsoOp} object into a HDF5 file.
#' See the \dQuote{Binary ...} operations at \url{https://ltla.github.io/chihaya} for more details.
#'
#' @param x A \linkS4class{DelayedNaryIsoOp} object.
#' @inheritParams saveDelayedObject
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
setMethod("saveDelayedObject", "DelayedNaryIsoOp", function(x, file, name) {
    h5createGroup(file, name)

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

    if (chosen %in% supported.Arith) {
        .label_group_operation(file, name, 'binary arithmetic')
    } else if (chosen %in% supported.Compare) {
        .label_group_operation(file, name, 'binary comparison')
    } else if (chosen %in% supported.Compare) {
        .label_group_operation(file, name, 'binary logic')
        chosen <- .save_Ops(chosen)
    }
    write_string_scalar(file, name, "method", chosen)

    if (length(x@seeds) != 2) {
        stop("expected exactly two seeds for 'DelayedNaryIsoOp'")
    }
    if (length(x@Rargs)) {
        stop("expected no additional right arguments for 'DelayedNaryIsoOp'")
    }

    saveDelayedObject(x@seeds[[1]], file, file.path(name, "left"))
    saveDelayedObject(x@seeds[[2]], file, file.path(name, "right"))
    invisible(NULL)
})

#' @import DelayedArray
.load_delayed_nary_iso <- function(file, name, contents) {
    left <- .dispatch_loader(file, file.path(name, "left"))
    if (!is(left, "DelayedArray")) {
        left <- DelayedArray(left)
    }

    right <- .dispatch_loader(file, file.path(name, "right"))
    if (!is(right, "DelayedArray")) {
        right <- DelayedArray(right)
    }

    op <- .load_simple_vector(file, file.path(name, "method"))
    op <- .load_Ops(op)
    get(op, envir=baseenv())(left, right)
}
