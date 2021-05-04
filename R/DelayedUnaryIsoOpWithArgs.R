#' Saving a DelayedUnaryIsoOpWithArgs
#'
#' Save a \linkS4class{DelayedUnaryIsoOpWithArgs} object into a HDF5 file.
#' See the \dQuote{Specification} vignette for details on the layout.
#'
#' @param x A \linkS4class{DelayedUnaryIsoOpWithArgs} object.
#' @param file String containing the path to a HDF5 file.
#' @param name String containing the name of the group to save into.
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
setMethod("saveLayer", "DelayedUnaryIsoOpWithArgs", function(x, file, name) {
    if (name!="") {
        h5createGroup(file, name)
    }
    .label_group_class(file, name, c('operation', 'unary isometric with arguments'))

    # Figuring out the identity of the operation.
    chosen <- NULL
    possibilities <- list(`+`=`+`, `/`=`/`, `*`=`*`, `-`=`-`, `^`=`^`)
    for (p in names(possibilities)) {
        if (identical(x@OP, possibilities[[p]])) {
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
        .save_list(args, file, argpath, vectors.only=TRUE) # drops vector attributes, but I don't care.
        alongpath <- file.path(name, paste0(side, "_along"))
        h5write(along, file, alongpath)
    }

    saveLayer(x@seed, file, file.path(name, "seed"))
    invisible(NULL)
})

#' @importFrom DelayedArray sweep aperm
.load_delayed_unary_iso_with_args <- function(file, path, contents) {
    x <- .dispatch_loader(file, file.path(path, "seed"), contents[["seed"]])
    if (!is(x, "DelayedArray")) x <- DelayedArray(x)

    OP <- .load_simple_vector(file, file.path(path, "operation"))
    FUN <- get(OP)

    Lalong <- .load_simple_vector(file, file.path(path, "left_along"))
    if (length(Lalong) > 1) {
        stop("multiple left-hand-side operations not supported yet")
    }
    Largs <- .load_list(file, file.path(path, "left_arguments"), contents[["left_arguments"]], vectors.only=TRUE)
    for (i in seq_along(Largs)) {
        MARGIN <- Lalong[i]
        if (MARGIN==1) {
            x <- FUN(Largs[[i]], x)
        } else {
            # Stolen from base::sweep.
            perm <- c(MARGIN, seq_along(dim(x))[-MARGIN])
            tmp <- aperm(x, perm)
            tmp <- FUN(Largs[[i]], tmp)
            x <- aperm(x, order(perm))
        }
    }

    Rargs <- .load_list(file, file.path(path, "right_arguments"), contents[["right_arguments"]], vectors.only=TRUE)
    Ralong <- .load_simple_vector(file, file.path(path, "right_along"))
    for (i in seq_along(Rargs)) {
        x <- sweep(x, MARGIN=Ralong[i], STATS=Rargs[[i]], FUN=FUN)
    }

    x
}
