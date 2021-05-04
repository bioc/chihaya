#' @export
#' @importFrom rhdf5 h5createFile
saveDelayed <- function(x, file, path="delayed") {
    if (!is(x, "DelayedArray")) {
        stop("'x' should be a DelayedArray")
    }
    h5createFile(file)
    saveLayer(x@seed, file, path)
}
