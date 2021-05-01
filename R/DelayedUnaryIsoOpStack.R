#' @examples
#' X <- DelayedArray(matrix(runif(100), ncol=20))
#' Y <- log(X)
#' temp <- tempfile(fileext=".h5")
#' saveDelayedOps(Y, temp)
#' 
#' @export
#' @importFrom rhdf5 h5createGroup h5write
setMethod("saveLayer", "DelayedUnaryIsoOpStack", function(x, file, name) {
    if (name!="") {
        h5createGroup(file, name)
    }
    h5write(c("operation", "isometric"), file, file.path(name, "type"))

    path <- file.path(name, "operations")
    h5createGroup(file, path)

    OPS <- x@OPS
    for (i in seq_along(OPS)) {
        current.op <- environment(OPS[[i]])$`.Generic`

        if (is.null(info)) {
            info <- .unary_Math(generic, envir)        
        } 
        if (is.null(info)) {
            info <- .unary_Math2(generic, envir)        
        } 
        if (is.null(info)) {
            info <- .unary_Arith(generic, envir)        
        } 

        ipath <- file.path(path, i)
        h5createGroup(file, ipath)
        h5write(instruction$op, file, file.path(ipath, "operation"))

        apath <- file.path(ipath, "arguments")
        h5createGroup(file, apath)
        args <- instruction$args
        for (a in names(args)) {
            h5write(args[[a]], file, file.path(apath, a))
        }
    }

    saveLayer(x@seed, file, file.path(name, "seed"))
})

.unary_Math <- function(generic, envir) {
    if (generic %in% getGroupMembers("Math")) {
        list(op=generic)
    }
}

.unary_Math2 <- function(generic, envir) {
    if (generic %in% getGroupMembers("Math2")) {
        list(op=generic, args=list(digits=envir$digits))
    }
}

.unary_Ops <- function(generic, envir) {
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


