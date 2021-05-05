#' Save HDF5-based seeds
#'
#' Save \linkS4class{HDF5ArraySeed} or \linkS4class{H5SparseMatrix} objects or their subclasses.
#' See the \dQuote{Specification} vignette for details on the layout.
#' 
#' @param x A \linkS4class{HDF5ArraySeed} or \linkS4class{H5SparseMatrix} object or subclass thereof.
#' @inheritParams saveLayer
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
setMethod("saveLayer", "HDF5ArraySeed", function(x, file, name) {
    if (name!="") {
        h5createGroup(file, name)
    }
    .label_group_class(file, name, c('seed', 'external hdf5 array'))

    h5write(x@filepath, file, file.path(name, "path"))
    h5write(x@name, file, file.path(name, "name"))
    h5write(x@as_sparse, file, file.path(name, "sparse"))

    invisible(NULL)
})

#' @importFrom HDF5Array HDF5Array
.load_dense_hdf5_array <- function(file, name, contents) {
    path <- .load_simple_vector(file, file.path(name, "path"))
    gname <- .load_simple_vector(file, file.path(name, "name"))
    sparse <- .load_simple_vector(file, file.path(name, "sparse"))
    HDF5Array(path, gname, as.sparse=sparse)
}

#' @export
#' @rdname HDF5ArraySeed
#' @importClassesFrom HDF5Array H5SparseMatrix
setMethod("saveLayer", "H5SparseMatrixSeed", function(x, file, name) {
    if (name!="") {
        h5createGroup(file, name)
    }
    .label_group_class(file, name, c('seed', 'external sparse hdf5 matrix'))

    h5write(x@filepath, file, file.path(name, "path"))
    h5write(x@group, file, file.path(name, "name"))

    if (!is.null(x@subdata)) {
        stop("non-NULL 'subdata' not supported yet")
    }

    invisible(NULL)
})

#' @importFrom HDF5Array H5SparseMatrix
.load_sparse_hdf5_matrix <- function(file, name, contents) {
    path <- .load_simple_vector(file, file.path(name, "path"))
    gname <- .load_simple_vector(file, file.path(name, "name"))
    H5SparseMatrix(path, gname)
}

#' @export
#' @rdname HDF5ArraySeed
#' @importClassesFrom HDF5Array TENxMatrixSeed
setMethod("saveLayer", "TENxMatrixSeed", function(x, file, name) {
    if (name!="") {
        h5createGroup(file, name)
    }
    .label_group_class(file, name, c('seed', 'external tenx matrix'))
    h5write(x@filepath, file, file.path(name, "path"))
    h5write(x@group, file, file.path(name, "name"))
    invisible(NULL)
})

#' @importFrom HDF5Array TENxMatrix
.load_tenx_matrix <- function(file, name, contents) {
    path <- .load_simple_vector(file, file.path(name, "path"))
    gname <- .load_simple_vector(file, file.path(name, "name"))
    TENxMatrix(path, gname)
}

#' @export
#' @rdname HDF5ArraySeed
#' @importClassesFrom HDF5Array Dense_H5ADMatrixSeed
setMethod("saveLayer", "Dense_H5ADMatrixSeed", function(x, file, name) {
    if (name!="") {
        h5createGroup(file, name)
    }
    .label_group_class(file, name, c('seed', 'external h5ad matrix'))

    h5write(x@filepath, file, file.path(name, "path"))
    if (dirname(x@name)=="layers") {
        h5write(basename(x@name), file, file.path(name, "layer"))
    }

    invisible(NULL)
})

#' @export
#' @rdname HDF5ArraySeed
#' @importClassesFrom HDF5Array CSR_H5ADMatrixSeed
setMethod("saveLayer", "CSR_H5ADMatrixSeed", function(x, file, name) {
    if (name!="") {
        h5createGroup(file, name)
    }
    .label_group_class(file, name, c('seed', 'external h5ad matrix'))

    h5write(x@filepath, file, file.path(name, "path"))
    if (dirname(x@name)=="layers") {
        h5write(basename(x@name), file, file.path(name, "layer"))
    }

    invisible(NULL)
})

#' @export
#' @rdname HDF5ArraySeed
#' @importClassesFrom HDF5Array CSC_H5ADMatrixSeed
setMethod("saveLayer", "CSC_H5ADMatrixSeed", function(x, file, name) {
    if (name!="") {
        h5createGroup(file, name)
    }
    .label_group_class(file, name, c('seed', 'external h5ad matrix'))

    h5write(x@filepath, file, file.path(name, "path"))
    if (dirname(x@name)=="layers") {
        h5write(basename(x@name), file, file.path(name, "layer"))
    }

    invisible(NULL)
})

#' @importFrom HDF5Array H5ADMatrix
.load_h5ad_matrix <- function(file, name, contents) {
    path <- .load_simple_vector(file, file.path(name, "path"))
    if ("layer" %in% names(contents)) {
        layer <- .load_simple_vector(file, file.path(name, "layer"))
    } else {
        layer <- NULL
    }
    H5ADMatrix(path, layer=layer)
}
