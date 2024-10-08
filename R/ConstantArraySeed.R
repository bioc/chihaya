#' Saving a ConstantArraySeed
#'
#' Save a \linkS4class{ConstantArraySeed} object.
#' See the \dQuote{Constant array} section at \url{https://artifactdb.github.io/chihaya/} for more details.
#'
#' @param x A \linkS4class{ConstantArraySeed} object.
#' @param file String containing the path to a HDF5 file.
#' @param name String containing the name of the group to save into.
#'
#' @return A \code{NULL}, invisibly.
#' A group is created at \code{name} containing the contents of the ConstantArraySeed.
#'
#' @author Aaron Lun
#' 
#' @examples
#' X <- ConstantArray(value=NA_real_, dim=c(11, 25))
#' temp <- tempfile(fileext=".h5")
#' saveDelayed(X, temp)
#' rhdf5::h5ls(temp)
#' loadDelayed(temp)
#' 
#' @export
#' @rdname ConstantArraySeed 
#' @importFrom rhdf5 h5createGroup h5write
setMethod("saveDelayedObject", "ConstantArraySeed", function(x, file, name) {
    h5createGroup(file, name)
    .labelArrayGroup(file, name, 'constant array')
    h5write(dim(x), file, file.path(name, "dimensions"));
    .saveDataset(file, "value", x@value, parent=name, scalar=TRUE)
    invisible(NULL)
})

#' @import DelayedArray
.load_constant_array <- function(file, name, contents) {
    dim <- h5read(file, file.path(name, "dimensions"), drop=TRUE)
    val <- .load_vector_with_attributes(file, file.path(name, "value")) 
    ConstantArray(dim, value=val)
}
