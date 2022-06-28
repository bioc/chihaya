#' Saving a ConstantArraySeed
#'
#' Save a \linkS4class{ConstantArraySeed} object.
#' See the \dQuote{Constant array} section at \url{https://ltla.github.io/chihaya} for more details.
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
    .label_group_seed(file, name, 'constant array')
    h5write(dim(x), file, file.path(name, "dimensions"));

    if (is.logical(x@value)) {
        write_integer_scalar(file, name, "value", as.integer(x@value))

        # Decorating it with the same stuff as rhdf5 uses.
        fhandle <- H5Fopen(file)
        on.exit(H5Fclose(fhandle))
        dhandle <- H5Dopen(fhandle, file.path(name, "value"))
        on.exit(H5Dclose(dhandle), add=TRUE, after=FALSE)
        h5writeAttribute("logical", dhandle, "storage.mode", asScalar = TRUE, encoding = "UTF-8")

    } else if (is.numeric(x@value)) {
        write_number_scalar(file, name, "value", x@value)
    } else if (is.character(x@value)) {
        write_string_scalar(file, name, "value", x@value)
    } else {
        stop("unsupported constant type '", typeof(x@value), "'")
    }

    invisible(NULL)
})

#' @import DelayedArray
.load_constant_array <- function(file, name, contents) {
    dim <- .load_simple_vector(file, file.path(name, "dimensions"))
    val <- .load_simple_vector(file, file.path(name, "value")) 
    ConstantArray(dim, value=val)
}
