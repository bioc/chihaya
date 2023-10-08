#' Saving a DelayedUnaryIsoOpStack
#'
#' Save a \linkS4class{DelayedUnaryIsoOpStack} object into a HDF5 file.
#' See the \dQuote{Unary ...} operations at \url{https://ltla.github.io/chihaya} for more details.
#'
#' @param x A \linkS4class{DelayedUnaryIsoOpStack} object.
#' @inheritParams saveDelayedObject
#' 
#' @return A \code{NULL}, invisibly.
#' A group is created at \code{name} containing the contents of the DelayedUnaryIsoOpStack.
#'
#' @author Aaron Lun
#'
#' @examples
#' X <- DelayedArray(matrix(runif(100), ncol=20))
#' Y <- log2(X + 10)
#' temp <- tempfile(fileext=".h5")
#' saveDelayed(Y, temp)
#' rhdf5::h5ls(temp)
#' loadDelayed(temp)
#' 
#' @export
#' @rdname DelayedUnaryIsoOpStack
#' @importFrom rhdf5 h5createGroup h5write
setMethod("saveDelayedObject", "DelayedUnaryIsoOpStack", function(x, file, name) {
    for (i in rev(seq_along(x@OPS))) { # reverse order, as first operation is first applied (and thus needs to be closer to the leaf of the delayed tree).
        OP <- x@OPS[[i]]
        h5createGroup(file, name)

        status <- FALSE 
        if (!status) {
            info <- .unary_Math(file, name, OP)
            if (status <- !is.null(info)) {
                name <- info
            }
        } 

        if (!status) {
            info <- .unary_Math2(file, name, OP)
            if (status <- !is.null(info)) {
                name <- info
            }
        } 

        if (!status) {
            info <- .unary_Ops(file, name, OP)
            if (status <- !is.null(info)) {
                name <- info
            }
        } 

        if (!status) {
            info <- .unary_other(file, name, OP)
            if (status <- !is.null(info)) {
                name <- info
            }
        } 

        if (!status) {
            stop("unknown OPS[[", i, "]] function in ", class(x))
        }
    }

    saveDelayedObject(x@seed, file, name)

    invisible(NULL)
})

.unary_Math <- function(file, name, OP) {
    envir <- environment(OP)
    generic <- envir$`.Generic`
    seed.name <- file.path(name, "seed")

    if (!is.null(generic)) {
        direct.support <- c("abs", "sign", "sqrt", "ceiling", "floor", "trunc",
                            "exp", "expm1", "log1p", "cos", "cosh", "sin",
                            "sinh", "tan", "tanh", "acos", "acosh", "asin",
                            "asinh", "atan", "atanh", "gamma", "lgamma",
                            "digamma", "trigamma")

        if (generic %in% direct.support) {
            .labelOperationGroup(file, name, 'unary math')
            write_string_scalar(file, name, "method", generic)
            return(seed.name)
        }

        log.base.support <- c(log2=2, log10=10)
        if (generic %in% names(log.base.support)) {
            .labelOperationGroup(file, name, 'unary math')
            write_string_scalar(file, name, "method", "log")
            write_double_scalar(file, name, "base", log.base.support[[generic]])
            return(seed.name)
        }
    }

    # Special case for the general case log.
    if (isTRUE(all.equal(as.character(body(OP)), c("log", "a", "base")))) {
        .labelOperationGroup(file, name, 'unary math')
        write_string_scalar(file, name, "method", "log")

        base <- envir$base
        if (base != exp(1)) {
            write_double_scalar(file, name, "base", base)
        }
        return(seed.name)
    }
}

.unary_Math2 <- function(file, name, OP) {
    envir <- environment(OP)
    generic <- envir$`.Generic`
    seed.name <- file.path(name, "seed")

    if (generic %in% getGroupMembers("Math2")) {
        .labelOperationGroup(file, name, 'unary math')
        write_string_scalar(file, name, "method", generic)
        write_integer_scalar(file, name, "digits", envir$digits)
        return(seed.name)
    }
}

supported.Arith <- c("+", "-", "*", "^", "/", "%%", "%/%")
supported.Compare <- c("==", ">", "<", "!=", "<=", ">=")
supported.Logic <- c("&", "|")
supported.Ops <- c(supported.Arith, supported.Compare, supported.Logic)

.save_Ops <- function(method) {
    if (method == "&") {
        "&&"
    } else if (method == "|") {
        "||"
    } else {
        method
    }
}

.load_Ops <- function(method) {
    if (method == "&&") {
        "&"
    } else if (method == "||") {
        "|"
    } else {
        method
    }
}

.unary_Ops <- function(file, name, OP) {
    envir <- environment(OP)
    generic <- envir$`.Generic`
    seed.name <- file.path(name, "seed")

    if (generic %in% supported.Ops) {
        delayed_op <- NULL
        if (generic %in% supported.Arith) {
            delayed_op <- "unary arithmetic"
        } else if (generic %in% supported.Compare) {
            delayed_op <- "unary comparison"
        } else if (generic %in% supported.Logic) {
            delayed_op <- "unary logic"
            generic <- .save_Ops(generic)
        }
        .labelOperationGroup(file, name, delayed_op)
        write_string_scalar(file, name, "method", generic)

        e1 <- envir$e1
        e2 <- envir$e2

        if (missing(e2)) {
            if (!generic %in% c("+", "-")) {
                stop("second argument can only be missing for unary '+' or '-'")
            }
            write_string_scalar(file, name, "side", "none")
        } else {
            right <- is(e1, "DelayedArray") # i.e., is the operation applied to the left of the seed?
            left <- is(e2, "DelayedArray") # i.e., is the operation applied to the left of the seed?

            write_string_scalar(file, name, "side", if (left) "left" else "right")
            val <- if (left) e1 else e2
            if (length(val) == 1) {
                .saveDataset(file, "value", val, parent=name, scalar=TRUE)
            } else {
                # Don't think this ever gets called, because otherwise
                # we'd be dealing with a DelayedIsoOpWithArgs.
                # Nonetheless, we'll throw in the necessary code.
                write_integer_scalar(file, name, "along", 0)
                .saveDataset(file, "value", val, parent=name)
            }
        }

        return(seed.name)
    }
}

unary.logic.Ops <- c(is.infinite="is_infinite", is.nan="is_nan", is.finite="is_finite")

.unary_other <- function(file, name, OP) {
    envir <- environment(OP)
    generic <- envir$`.Generic`
    seed.name <- file.path(name, "seed")

    if (generic == "!") {
        .labelOperationGroup(file, name, "unary logic")
        write_string_scalar(file, name, "method", "!")
        return(seed.name)
    } else if (generic %in% names(unary.logic.Ops)) {
        .labelOperationGroup(file, name, "unary special check")
        write_string_scalar(file, name, "method", unary.logic.Ops[[generic]])
        return(seed.name)
    }
}

#' @import DelayedArray
.load_delayed_unary_iso <- function(file, name, contents) {
    x <- .dispatch_loader(file, file.path(name, "seed"))
    if (!is(x, "DelayedArray")) {
        x <- DelayedArray(x)
    }

    op.name <- h5read(file, file.path(name, "method"), drop=TRUE)
    op.name <- .load_Ops(op.name)

    if (op.name %in% getGroupMembers("Math")) {
        FUN <- get(op.name, envir=baseenv())
        if (op.name=="log") {
            if (h5exists(file, name, "base")){
                base <- h5read(file, file.path(name, "base"), drop=TRUE)
                x <- log(x, base=base)
            } else {
                x <- log(x)
            }
        } else {
            x <- FUN(x)
        }

    } else if (op.name %in% getGroupMembers("Math2")) {
        FUN <- get(op.name, envir=baseenv())
        digits <- h5read(file, file.path(name, "digits"), drop=TRUE)
        x <- FUN(x, digits=digits)

    } else if (op.name %in% unary.logic.Ops) {
        actual.op <- names(unary.logic.Ops)[op.name == unary.logic.Ops]
        FUN <- get(actual.op, envir=baseenv())
        x <- FUN(x)

    } else if (op.name == "!") {
        x <- !x

    } else {
        FUN <- get(op.name, envir=baseenv())
        side <- h5read(file, file.path(name, "side"), drop=TRUE)

        if (side == "none") {
            x <- FUN(x)
        } else {
            value <- .load_vector_with_attributes(file, file.path(name, "value"))
            if (op.name == "&" || op.name == "|") {
                value <- as.logical(value)
            }

            simple <- FALSE
            if (!h5exists(file, name, "along"))  {
                simple <- TRUE # i.e., scalar.
            } else {
                along <- h5read(file, file.path(name, "along"), drop=TRUE)
                simple <- along == 0
            }

            if (simple) {
                if (side == "left") {
                    x <- FUN(value, x)
                } else if (side == "right") {
                    x <- FUN(x, value)
                } else {
                    stop("unrecognized side '", side, "'")
                }

            } else {
                # Stolen from base::sweep.
                along <- along + 1L
                perm <- c(along, seq_along(dim(x))[-along])
                tmp <- aperm(x, perm)

                if (side == "left") {
                    tmp <- FUN(value, tmp)
                } else if (side == "right") { 
                    tmp <- FUN(tmp, value)
                } else {
                    stop("unrecognized side '", side, "'")
                }

                x <- aperm(tmp, order(perm))
            }
        }
    }

    x
}
