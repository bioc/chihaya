#' Save HDF5-based seeds
#'
#' Save \linkS4class{HDF5ArraySeed} or \linkS4class{H5SparseMatrix} objects or their subclasses.
#' See \dQuote{External HDF5 arrays} at \url{https://ltla.github.io/chihaya} for more details.
#' 
#' @param x A \linkS4class{HDF5ArraySeed} or \linkS4class{H5SparseMatrix} object or subclass thereof.
#' @inheritParams saveDelayedObject
#'
#' @return A \code{NULL}, invisibly.
#' A group is created at \code{name} containing the contents of the HDF5-based seed.
#'
#' @author Aaron Lun
#' 
#' @examples
#' library(HDF5Array)
#' X <- writeHDF5Array(matrix(runif(100), ncol=20))
#' Y <- X + 1
#' temp <- tempfile(fileext=".h5")
#' saveDelayed(Y, temp)
#' rhdf5::h5ls(temp)
#' loadDelayed(temp)
#'
#' @export
#' @rdname HDF5ArraySeed
#' @importClassesFrom HDF5Array HDF5ArraySeed
setMethod("saveDelayedObject", "HDF5ArraySeed", function(x, file, name) {
    if (!allowExternalSeeds()) {
        stop("external reference to '", x@filepath, "' is not allowed")
    }

    h5createGroup(file, name)
    .labelArrayGroup(file, name, 'external hdf5 dense array')

    h5write(dim(x), file, file.path(name, "dimensions"))
    write_string_scalar(file, name, "type", .pickArrayType(x))

    write_string_scalar(file, name, "file", x@filepath)
    write_string_scalar(file, name, "name", x@name)
    write_integer_scalar(file, name, "sparse", x@as_sparse)

    invisible(NULL)
})

#' @importFrom HDF5Array HDF5Array
.load_dense_hdf5_array <- function(file, name, contents) {
    path <- .load_simple_vector(file, file.path(name, "file"))
    gname <- .load_simple_vector(file, file.path(name, "name"))
    sparse <- .load_simple_vector(file, file.path(name, "sparse"))
    HDF5Array(path, gname, as.sparse=sparse > 0)
}

#' @export
#' @rdname HDF5ArraySeed
#' @importClassesFrom HDF5Array H5SparseMatrix
setMethod("saveDelayedObject", "H5SparseMatrixSeed", function(x, file, name) {
    if (!allowExternalSeeds()) {
        stop("external reference to '", x@filepath, "' is not allowed")
    }

    h5createGroup(file, name)
    .labelArrayGroup(file, name, 'external hdf5 sparse matrix')

    h5write(dim(x), file, file.path(name, "dimensions"))
    write_string_scalar(file, name, "type", .pickArrayType(x))

    write_string_scalar(file, name, "file", x@filepath)
    write_string_scalar(file, name, "name", x@group)

    if (!is.null(x@subdata)) {
        stop("non-NULL 'subdata' not supported yet")
    }

    invisible(NULL)
})

#' @importFrom HDF5Array H5SparseMatrix
.load_sparse_hdf5_matrix <- function(file, name, contents) {
    path <- .load_simple_vector(file, file.path(name, "file"))
    gname <- .load_simple_vector(file, file.path(name, "name"))
    H5SparseMatrix(path, gname)
}
