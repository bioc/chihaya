#' Saving a DelayedUnaryIsoOpStack
#'
#' Save a \linkS4class{DelayedUnaryIsoOpStack} object into a HDF5 file.
#' See the \dQuote{Specification} vignette for details on the layout.
#'
#' @param x A \linkS4class{DelayedUnaryIsoOpStack} object.
#' @inheritParams saveLayer
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
setMethod("saveLayer", "DelayedUnaryIsoOpStack", function(x, file, name) {
    for (i in rev(seq_along(x@OPS))) { # reverse order, as first operation is first applied (and thus needs to be closer to the leaf of the delayed tree).
        OP <- x@OPS[[i]]
        
        if (name!="") {
            h5createGroup(file, name)
        }
        .label_group_class(file, name, c('operation', 'unary isometric'))

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

    saveLayer(x@seed, file, name)

    invisible(NULL)
})

.unary_Math <- function(file, path, OP) {
    envir <- environment(OP)
    generic <- envir$`.Generic`
    seed.name <- file.path(path, "seed")

    if (!is.null(generic)) {
        direct.support <- c("abs", "sign", "sqrt", "ceiling", "floor", "trunc",
                            "exp", "expm1", "log1p", "cos", "cosh", "sin",
                            "sinh", "tan", "tanh", "acos", "acosh", "asin",
                            "asinh", "atan", "atanh", "gamma", "lgamma",
                            "digamma", "trigamma")

        if (generic %in% direct.support) {
            h5write(generic, file, file.path(path, "operation"))
            h5createGroup(file, seed.name)
            return(seed.name)
        }

        log.base.support <- c(log=exp(1), log2=2, log10=10)
        if (generic %in% names(log.base.support)) {
            h5write("log", file, file.path(path, "operation"))
            h5createGroup(file, file.path(path, "parameters"))
            h5write(log.base.support[[generic]], file, file.path(path, "parameters/base"))
            return(seed.name)
        }
    }

    # Special case for the general case log.
    base <- envir$base
    if (isTRUE(all.equal(OP, function(a) log(a, base=base)))) {
        h5write("log", file, file.path(path, "operation"))
        h5createGroup(file, file.path(path, "parameters"))
        h5write(base, file, file.path(path, "parameters/base"))
        return(seed.name)
    }
}

.unary_Math2 <- function(file, path, OP) {
    envir <- environment(OP)
    generic <- envir$`.Generic`
    seed.name <- file.path(path, "seed")

    if (generic %in% getGroupMembers("Math2")) {
        h5write(generic, file, file.path(path, "operation"))
        h5createGroup(file, file.path(path, "parameters"))
        h5write(envir$digits, file, file.path(path, "parameters/digits"))
        return(seed.name)
    }
}

supported.Ops <- c("+", "-", "*", "^", "/", "%%", "%/%", # Arith
                   "==", ">", "<", "!=", "<=", ">=", # Compare
                   "&", "|") # Logic

.unary_Ops <- function(file, path, OP) {
    envir <- environment(OP)
    generic <- envir$`.Generic`
    seed.name <- file.path(path, "seed")

    if (generic %in% supported.Ops) {
        e1 <- envir$e1
        e2 <- envir$e2
        h5write(generic, file, file.path(path, "operation"))

        if (missing(e2)) {
            if (!generic %in% c("+", "-")) {
                stop("second argument can only be missing for unary '+' or '-'")
            }
            h5createGroup(file, file.path(path, "parameters"))
            h5write("none", file, file.path(path, "parameters/side"))
        } else {
            right <- is(e1, "DelayedArray") # i.e., is the operation applied to the left of the seed?
            left <- is(e2, "DelayedArray") # i.e., is the operation applied to the left of the seed?

            h5createGroup(file, file.path(path, "parameters"))
            h5write(if (left) "left" else "right", file, file.path(path, "parameters/side"))
            h5write(0L, file, file.path(path, "parameters/along"))
            h5write(if (left) e1 else e2, file, file.path(path, "parameters/value"))
        }

        return(seed.name)
    }
}

unary.logic.Ops <- c(`!`="!", is.na="is_na", is.infinite="is_infinite", is.nan="is_nan", is.finite="is_finite")

.unary_other <- function(file, path, OP) {
    envir <- environment(OP)
    generic <- envir$`.Generic`
    seed.name <- file.path(path, "seed")

    if (generic %in% names(unary.logic.Ops)) {
        h5write(unary.logic.Ops[[generic]], file, file.path(path, "operation"))
        return(seed.name)
    }
}

.load_delayed_unary_iso <- function(file, path, contents) {
    x <- .dispatch_loader(file, file.path(path, "seed"), contents[["seed"]])
    if (!is(x, "DelayedArray")) {
        x <- DelayedArray(x)
    }

    op.name <- .load_simple_vector(file, file.path(path, "operation"))

    if (op.name %in% getGroupMembers("Math")) {
        FUN <- get(op.name, envir=baseenv())
        if (op.name=="log") {
            base <- .load_simple_vector(file, file.path(path, "parameters", "base"))
            x <- FUN(x, base=base)
        } else {
            x <- FUN(x)
        }

    } else if (op.name %in% getGroupMembers("Math2")) {
        FUN <- get(op.name, envir=baseenv())
        digits <- .load_simple_vector(file, file.path(path, "parameters", "digits"))
        x <- FUN(x, digits=digits)

    } else if (op.name %in% unary.logic.Ops) {
        actual.op <- names(unary.logic.Ops)[op.name == unary.logic.Ops]
        FUN <- get(actual.op, envir=baseenv())
        x <- FUN(x)

    } else {
        FUN <- get(op.name, envir=baseenv())
        side <- .load_simple_vector(file, file.path(path, "parameters", "side"))

        if (side == "none") {
            x <- FUN(x)
        } else {
            along <- .load_simple_vector(file, file.path(path, "parameters", "along"))
            value <- .load_simple_vector(file, file.path(path, "parameters", "value"))

            if (along <= 1L) {
                if (side == "left") {
                    x <- FUN(value, x)
                } else if (side == "right") {
                    x <- FUN(x, value)
                } else {
                    stop("unrecognized side '", side, "'")
                }

            } else {
                # Stolen from base::sweep.
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
