#' @examples
#' X <- DelayedArray(matrix(runif(100), ncol=20))
#' Y <- log2(X + 10)
#' temp <- tempfile(fileext=".h5")
#' saveDelayedOps(Y, temp)
#' 
#' @export
#' @importFrom rhdf5 h5createGroup h5write
setMethod("saveLayer", "DelayedUnaryIsoOpStack", function(x, file, name) {
    if (name!="") {
        h5createGroup(file, name)
    }
    h5write(c("operation", "UNARY"), file, file.path(name, "type"))

    path <- file.path(name, "operations")
    h5createGroup(file, path)

    OPS <- x@OPS
    for (i in seq_along(OPS)) {
        envir <- environment(OPS[[i]])

        info <- NULL
        if (is.null(info)) {
            info <- .unary_Math(envir)        
        } 
        if (is.null(info)) {
            info <- .unary_Math2(envir)        
        } 
        if (is.null(info)) {
            info <- .unary_Ops(envir)
        } 
        if (is.null(info)) {
            stop("unknown generic function '", envir$.Generic, "' in ", class(x))
        }

        ipath <- file.path(path, i)
        h5createGroup(file, ipath)
        h5write(info$op, file, file.path(ipath, "operation"))

        if (!is.null(info$args)) {
            apath <- file.path(ipath, "arguments")
            h5createGroup(file, apath)
            for (a in names(info$args)) {
                h5write(info$args[[a]], file, file.path(apath, a))
            }
        }
    }

    saveLayer(x@seed, file, file.path(name, "seed"))
})

.unary_Math <- function(envir) {
    generic <- envir$`.Generic`
    if (generic %in% getGroupMembers("Math")) {
        list(op=generic)
    }
}

.unary_Math2 <- function(envir) {
    generic <- envir$`.Generic`
    if (generic %in% getGroupMembers("Math2")) {
        list(op=generic, args=list(digits=envir$digits))
    }
}

.unary_Ops <- function(envir) {
    generic <- envir$`.Generic`
    if (generic %in% getGroupMembers("Arith") || 
        generic %in% getGroupMembers("Ops") || 
        generic %in% getGroupMembers("Logic")) 
    {
        e1 <- envir$e1
        e2 <- envir$e2
        left <- is(e2, "DelayedArray") # i.e., is the other argument on the left?
        list(
            op=generic,
            args=list(
                side=if (left) "left" else "right",
                values=if (left) e1 else e2
            )
        )
    }
}
