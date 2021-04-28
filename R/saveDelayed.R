#' @export
saveDelayedOps <- function(x, file, path="") {
    if (!is(x, "DelayedArray")) {
        stop("'x' should be a DelayedArray")
    }
    saveLayer(x@seed, file, path)
}
