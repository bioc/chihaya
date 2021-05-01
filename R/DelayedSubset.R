#' @examples
#' X <- DelayedArray(matrix(runif(100), ncol=20))
#' Y <- X[1:2,3:5]
#' temp <- tempfile(fileext=".h5")
#' saveDelayedOps(Y, temp)
#' 
#' @export
#' @importFrom rhdf5 h5createGroup h5write
setMethod("saveLayer", "DelayedSubset", function(x, file, name) {
    if (name!="") {
        h5createGroup(file, name)
    }
    h5write(c("operation", "SUBSET"), file, file.path(name, "type"))

    h5createGroup(file, file.path(name, "index"))
    indices <- x@index
    h5write(length(indices), file, file.path(name, "index", "number"))
    for (i in seq_along(indices)) {
        h5write(indices[[i]], file, file.path(name, "index", i))
    }

    saveLayer(x@seed, file, file.path(name, "seed"))
})
