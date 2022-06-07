#' Saving a DelayedSubset
#'
#' Save a \linkS4class{DelayedSubset} object into a HDF5 file.
#' See the \dQuote{Subsetting} operation at \url{https://ltla.github.io/chihaya} for more details.
#'
#' @param x A \linkS4class{DelayedSubset} object.
#' @inheritParams saveDelayedObject
#' 
#' @return A \code{NULL}, invisibly.
#' A group is created at \code{name} containing the contents of the DelayedSubset.
#'
#' @author Aaron Lun
#'
#' @examples
#' X <- DelayedArray(matrix(runif(100), ncol=20))
#' Y <- X[1:2,3:5]
#' temp <- tempfile(fileext=".h5")
#' saveDelayed(Y, temp)
#' rhdf5::h5ls(temp)
#' loadDelayed(temp)
#' 
#' @export
#' @rdname DelayedSubset
#' @importFrom rhdf5 h5createGroup h5write
setMethod("saveDelayedObject", "DelayedSubset", function(x, file, name) {
    h5createGroup(file, name)
    .label_group_operation(file, name, 'subset')

    zerobased <- .zero_indices(x@index)
    .save_list(zerobased, file, file.path(name, "index"), vectors.only=TRUE)

    saveDelayedObject(x@seed, file, file.path(name, "seed"))
    invisible(NULL)
})

.zero_indices <- function(indices) {
    for (i in seq_along(indices)) {
        if (!is.null(indices[[i]])) {
            indices[[i]] <- indices[[i]] - 1L
        }
    }
    indices
}

.load_delayed_subset <- function(file, name, contents) {
    x <- .dispatch_loader(file, file.path(name, "seed"))
    if (!is(x, "DelayedArray")) {
        x <- DelayedArray(x)
    }

    indices <- .load_list(file, file.path(name, "index"), vectors.only=TRUE)
    indices <- .restore_indices(indices)

    do.call(`[`, c(list(x), indices, list(drop=FALSE)))
}

.restore_indices <- function(indices) {
    for (i in seq_along(indices)) {
        if (is.null(indices[[i]])) {
            indices[[i]] <- substitute()
        } else {
            indices[[i]] <- indices[[i]] + 1L
        }
    }
    indices
}

