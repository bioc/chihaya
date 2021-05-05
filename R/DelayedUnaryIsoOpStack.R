#' Saving a DelayedUnaryIsoOpStack
#'
#' Save a \linkS4class{DelayedUnaryIsoOpStack} object into a HDF5 file.
#' See the \dQuote{Specification} vignette for details on the layout.
#'
#' @param x A \linkS4class{DelayedUnaryIsoOpStack} object.
#' @param file String containing the path to a HDF5 file.
#' @param name String containing the name of the group to save into.
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
    if (name!="") {
        h5createGroup(file, name)
    }
    .label_group_class(file, name, c('operation', 'unary isometric stack'))

    path <- file.path(name, "operations")
    h5createGroup(file, path)

    for (i in seq_along(x@OPS)) {
        OP <- x@OPS[[i]]
        ipath <- file.path(path, i)
        h5createGroup(file, ipath)

        info <- NULL
        if (is.null(info)) {
            info <- .unary_Math(file, ipath, OP)
        } 
        if (is.null(info)) {
            info <- .unary_Math2(file, ipath, OP)
        } 
        if (is.null(info)) {
            info <- .unary_Ops(file, ipath, OP)
        } 
        if (is.null(info)) {
            stop("unknown generic function '", envir$.Generic, "' in ", class(x))
        }
    }

    saveLayer(x@seed, file, file.path(name, "seed"))

    invisible(NULL)
})

.unary_Math <- function(file, path, OP) {
    envir <- environment(OP)
    generic <- envir$`.Generic`

    if (!is.null(generic)) {
        if (generic %in% getGroupMembers("Math")) {
            h5write(generic, file, file.path(path, "operation"))
            return(TRUE)
        }
    }

    # Special case for log.
    if (isTRUE(all.equal(OP, function(a) log(a, base=base)))) {
        h5write("log", file, file.path(path, "operation"))
        h5write(environment(OP)$base, file, file.path(path, "base"))
        return(TRUE)
    }
}

.unary_Math2 <- function(file, path, OP) {
    envir <- environment(OP)
    generic <- envir$`.Generic`
    if (generic %in% getGroupMembers("Math2")) {
        h5write(generic, file, file.path(path, "operation"))
        h5write(envir$digits, file, file.path(path, "digits"))
        TRUE
    }
}

.unary_Ops <- function(file, path, OP) {
    envir <- environment(OP)
    generic <- envir$`.Generic`
    if (generic %in% getGroupMembers("Arith") || 
        generic %in% getGroupMembers("Compare") || 
        generic %in% getGroupMembers("Logic")) 
    {
        e1 <- envir$e1
        e2 <- envir$e2
        left <- is(e2, "DelayedArray") # i.e., is the operation applied to the left of the seed?
        h5write(generic, file, file.path(path, "operation"))
        h5write(if (left) "left" else "right", file, file.path(path, "side"))
        h5write(if (left) e1 else e2, file, file.path(path, "value"))
        TRUE
    }
}

.load_delayed_unary_iso_stack <- function(file, path, contents) {
    x <- .dispatch_loader(file, file.path(path, "seed"), contents[["seed"]])
    if (!is(x, "DelayedArray")) x <- DelayedArray(x)

    OPS <- names(contents[["operations"]])
    OPS <- OPS[order(as.integer(OPS))]

    for (o in OPS) {
        op.name <- .load_simple_vector(file, file.path(path, "operations", o, "operation"))
        FUN <- get(op.name)

        if (op.name %in% getGroupMembers("Math")) {
            if (op.name=="log") {
                base <- .load_simple_vector(file, file.path(path, "operations", o, "base"))
                x <- FUN(x, base=base)
            } else {
                x <- FUN(x)
            }
        } else if (op.name %in% getGroupMembers("Math2")) {
            digits <- .load_simple_vector(file, file.path(path, "operations", o, "digits"))
            x <- FUN(x, digits=digits)
        } else {
            side <- .load_simple_vector(file, file.path(path, "operations", o, "side"))
            value <- .load_simple_vector(file, file.path(path, "operations", o, "value"))
            if (identical(side, "left")) {
                x <- FUN(value, x)
            } else if (identical(side, "right")) {
                x <- FUN(x, value)
            }
        }
    }

    x
}
