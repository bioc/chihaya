#' Saving a DelayedUnaryIsoOpStack
#'
#' Save a \linkS4class{DelayedUnaryIsoOpStack} object into a HDF5 file.
#'
#' @param x A \linkS4class{DelayedUnaryIsoOpStack} object.
#' @param file String containing the path to a HDF5 file.
#' @param name String containing the name of the group to save into.
#' 
#' @return A \code{NULL}, invisibly.
#' A group is created at \code{name} containing the contents of the DelayedUnaryIsoOpStack.
#'
#' @details
#' The DelayedUnaryIsoOpStack class is represented on file by several elements within the \code{name}d group.
#' \itemize{
#' \item \code{type}, a character dataset with two elements.
#' The first is set to \code{"OPERATION"} and the second is set to \code{"UNARY"}.
#' \item \code{operations}, a group containing information about the operations.
#' Each operation is represented by a group that is named by the execution order, i.e., \code{"1"} is performed first, then \code{"2"}, and so on.
#' \item \code{seed}, a group or dataset containing the seed to be operated on.
#' }
#'
#' Each group in \code{operations} contains:
#' \itemize{
#' \item \code{name}, a character dataset with a single element naming the unary operation to be performed.
#' The seed is assumed to be the first argument in this operation.
#' Note that the unary-ness refers to the fact that we only need to consider one seed; there may still be multiple arguments.
#' \item (optional) \code{arguments}, a group containing additional arguments to use in the operation, in addition to the \code{seed}.
#' Each argument is a dataset inside the group where the name of the dataset is the name of the argument.
#' If this group is not present, no further arguments are necessary.
#' }
#' 
#' Valid operation names in \code{name} are those listed for \code{\link{Ops}}, \code{\link{Math}} and \code{\link{Math2}}.
#' The format of \code{arguments} depends on the nature of \code{name}.
#' \itemize{
#' \item For operations listed in \code{\link{Ops}}, \code{arguments} will contain a \code{side} dataset of length 1.
#' This will either say \code{"left"} or \code{"right"}, indicating whether the operation is applied to the left or the right side of the seed.
#' \code{arguments} will also contain \code{value} containing the value of the non-seed operand.
#' \item For operations listed in \code{\link{Math2}}, \code{arguments} will contain a \code{digits} dataset.
#' This contains the digits to be used in the various rounding operations.
#' \item For operations listed in \code{\link{Math}}, \code{arguments} is ignored and should contain no arguments.
#' }
#'
#' @author Aaron Lun
#'
#' @examples
#' X <- DelayedArray(matrix(runif(100), ncol=20))
#' Y <- log2(X + 10)
#' temp <- tempfile(fileext=".h5")
#' saveDelayedOps(Y, temp)
#' rhdf5::h5ls(temp)
#' 
#' @export
#' @importFrom rhdf5 h5createGroup h5write
setMethod("saveLayer", "DelayedUnaryIsoOpStack", function(x, file, name) {
    if (name!="") {
        h5createGroup(file, name)
    }
    h5write(c("OPERATION", "UNARY"), file, file.path(name, "type"))

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
        h5write(info$op, file, file.path(ipath, "name"))

        if (!is.null(info$args)) {
            apath <- file.path(ipath, "arguments")
            h5createGroup(file, apath)
            for (a in names(info$args)) {
                h5write(info$args[[a]], file, file.path(apath, a))
            }
        }
    }

    saveLayer(x@seed, file, file.path(name, "seed"))

    invisible(NULL)
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
