#' Saving a DelayedUnaryIsoOpWithArgs
#'
#' Save a \linkS4class{DelayedUnaryIsoOpWithArgs} object into a HDF5 file.
#'
#' @param x A \linkS4class{DelayedUnaryIsoOpWithArgs} object.
#' @param file String containing the path to a HDF5 file.
#' @param name String containing the name of the group to save into.
#' 
#' @return A \code{NULL}, invisibly.
#' A group is created at \code{name} containing the contents of the DelayedUnaryIsoOpWithArgs.
#'
#' @details
#' The DelayedUnaryIsoOpWithArgs class is represented on file by several elements within the \code{name}d group.
#' \itemize{
#' \item \code{type}, a character dataset with two elements.
#' The first is set to \code{"OPERATION"} and the second is set to \code{"UNARY_WITH_ARGS"}.
#' \item \code{left_arguments}, a group containing information about the arguments to apply to the seed on the left.
#' \item \code{left_along}, an integer dataset of length equal to the number of left arguments.
#' \item \code{right_arguments}, a group containing information about the arguments to apply to the seed on the right.
#' \item \code{right_along}, an integer dataset of length equal to the number of right arguments.
#' }
#'
#' Each argument in \code{left_arguments} is represented by a 1-dimensional dataset of the relevant type, named according to the positional index of that argument.
#' This dataset has number of elements equal to the length of the seed dimension referenced in the corresponding element of \code{left_along}.
#' The idea is to apply the operation to the seed along the referenced dimension, recycling the argument's values across the entire array.
#' The same logic applies for \code{right_arguments} and \code{right_along}.
#'
#' We strongly recommend reading the documentation for \linkS4class{DelayedUnaryIsoOpWithArgs} to understand what's going on here.
#' 
#' @author Aaron Lun
#'
#' @examples
#' X <- DelayedArray(matrix(runif(100), ncol=5))
#' Y <- (1:20 + X) / runif(5)
#' temp <- tempfile(fileext=".h5")
#' saveDelayedOps(Y, temp)
#' rhdf5::h5ls(temp)
#' 
#' @export
#' @importFrom rhdf5 h5createGroup h5write
setMethod("saveLayer", "DelayedUnaryIsoOpWithArgs", function(x, file, name) {
    if (name!="") {
        h5createGroup(file, name)
    }
    h5write(c("operation", "UNARY_WITH_ARGS"), file, file.path(name, "type"))

    # Figuring out the identity of the operation.
    OP <- x@OP
    chosen <- NULL
    possibilities <- list(`+`=`+`, `/`=`/`, `*`=`*`, `-`=`-`, `^`=`^`)
    for (p in names(possibilities)) {
        if (identical(OP, possibilities[[p]])) {
            chosen <- p
            break
        }
    }
    if (is.null(chosen)) {
        stop("unknown operation in ", class(x))
    }
    h5write(chosen, file, file.path(name, "operation"))

    # Saving the left and right args.
    for (side in c("left", "right")) {
        if (side == "left") { 
            args <- x@Largs
            along <- x@Lalong
        } else {
            args <- x@Rargs
            along <- x@Ralong
        }

        argpath <- file.path(name, paste0(side, "_arguments"))
        h5createGroup(file, argpath)
        for (i in seq_along(args)) {
            h5write(args[[i]], file, file.path(argpath, i))
        }

        alongpath <- file.path(name, paste0(side, "_along"))
        h5write(along, file, alongpath)
    }

    saveLayer(x@seed, file, file.path(name, "seed"))
    invisible(NULL)
})

