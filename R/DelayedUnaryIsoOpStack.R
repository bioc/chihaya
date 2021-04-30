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
        instruction <- switch(current.op,
            log=list(op="log"),
            log2=list(op="log", args=list(base=2)),
            log10=list(op="log", args=list(base=10))
        )

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
