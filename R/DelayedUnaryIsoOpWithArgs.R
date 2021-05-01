#' @examples
#' X <- DelayedArray(matrix(runif(100), ncol=5))
#' Y <- (1:20 + X) / runif(5)
#' temp <- tempfile(fileext=".h5")
#' saveDelayedOps(Y, temp)
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

        xpath <- file.path(name, side)
        h5createGroup(file, xpath)

        argpath <- file.path(xpath, "arguments") 
        h5createGroup(file, argpath)
        for (i in seq_along(args)) {
            h5write(args[[i]], file, file.path(argpath, i))
        }

        alongpath <- file.path(xpath, "along") 
        h5createGroup(file, alongpath)
        for (i in seq_along(along)) {
            h5write(along[[i]], file, file.path(alongpath, i))
        }
    }

    saveLayer(x@seed, file, file.path(name, "seed"))
})

