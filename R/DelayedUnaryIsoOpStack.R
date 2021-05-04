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

    OPS <- x@OPS
    for (i in seq_along(OPS)) {
        envir <- environment(OPS[[i]])
        ipath <- file.path(path, i)
        h5createGroup(file, ipath)

        info <- NULL
        if (is.null(info)) {
            info <- .unary_Math(file, ipath, envir)        
        } 
        if (is.null(info)) {
            info <- .unary_Math2(file, ipath, envir)
        } 
        if (is.null(info)) {
            info <- .unary_Ops(file, ipath, envir)
        } 
        if (is.null(info)) {
            stop("unknown generic function '", envir$.Generic, "' in ", class(x))
        }
    }

    saveLayer(x@seed, file, file.path(name, "seed"))

    invisible(NULL)
})

.unary_Math <- function(file, path, envir) {
    generic <- envir$`.Generic`
    if (generic %in% getGroupMembers("Math")) {
        saveLayer(generic, file, file.path(path, "operation"))
    }
}

.unary_Math2 <- function(file, path, envir) {
    generic <- envir$`.Generic`
    if (generic %in% getGroupMembers("Math2")) {
        saveLayer(generic, file, file.path(path, "operation"))
        saveLayer(envir$digits, file, file.path(path, "digits"))
    }
}

.unary_Ops <- function(file, path, envir) {
    generic <- envir$`.Generic`
    if (generic %in% getGroupMembers("Arith") || 
        generic %in% getGroupMembers("Compare") || 
        generic %in% getGroupMembers("Logic")) 
    {
        e1 <- envir$e1
        e2 <- envir$e2
        left <- is(e2, "DelayedArray") # i.e., is the operation applied to the left of the seed?
        saveLayer(generic, file, file.path(path, "operation"))
        saveLayer(if (left) "left" else "right", file, file.path(path, "side"))
        saveLayer(if (left) e2 else e1, file, file.path(path, "value"))
    }
}

.load_delayed_unary_iso_stack <- function(file, path, contents) {
    x <- .dispatch_loader(file, file.path(path, "seed"), contents[["seed"]])
    if (!is(x, "DelayedArray")) x <- DelayedArray(x)

    OPS <- names(contents[["operations"]])
    OPS <- OPS[order(as.integer(OPS))]

    for (o in OPS) {
        current.view <- contents[["operations"]][[o]]
        op.name <- .dispatch_loader(file, file.path(path, "operations", o, "operation"), current.view[["operation"]])
        FUN <- get(op.name)

        if (op.name %in% getGroupMembers("Math")) {
            x <- FUN(x)
        } else if (op.name %in% getGroupMembers("Math2")) {
            digits <- .dispatch_loader(file, file.path(path, "operations", o, "digits"), current.view[["digits"]])
            x <- FUN(x, digits=digits)
        } else {
            side <- .dispatch_loader(file, file.path(path, "operations", o, "side"), current.view[["side"]])
            value <- .dispatch_loader(file, file.path(path, "operations", o, "value"), current.view[["value"]])
            if (identical(side, "left")) {
                x <- FUN(value, x)
            } else if (identical(side, "right")) {
                x <- FUN(x, value)
            }
        }
    }

    x
}
