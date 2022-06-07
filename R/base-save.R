#' Saving simple seed classes
#'
#' Methods to save simple seed classes into the delayed operation file.
#' See \dQuote{Dense arrays} and \dQuote{Sparse matrices} at \url{https://ltla.github.io/chihaya} for more details.
#'
#' @param x An R object of the indicated class.
#' @inheritParams saveDelayedObject
#' 
#' @return A \code{NULL}, invisibly.
#' A group is created at \code{name} containing the contents of \code{x}.
#'
#' @details
#' The ANY method will dispatch to classes that are implemented in other packages:
#' \itemize{
#' \item If \code{x} is a LowRankMatrixSeed from the \pkg{BiocSingular} package, it is handled as a delayed matrix product.
#' \item Otherwise, if \code{x} comes from package \pkg{Y}, we will try to load \pkg{chihaya.Y}.
#' This is assumed to define an appropriate \code{saveDelayedObject} method for \code{x}.
#' }
#' 
#' @author Aaron Lun
#'
#' @examples
#' # Saving a sparse matrix.
#' X <- rsparsematrix(100, 20, 0.1)
#' Y <- DelayedArray(X)
#' temp <- tempfile(fileext=".h5")
#' saveDelayed(Y, temp)
#' rhdf5::h5ls(temp)
#' loadDelayed(temp)
#'
#' # Saving a matrix product.
#' library(BiocSingular)
#' left <- matrix(rnorm(100000), ncol=20)
#' right <- matrix(rnorm(50000), ncol=20)
#' thing <- LowRankMatrix(left, right)
#' temp <- tempfile()
#' saveDelayed(thing, temp)
#' rhdf5::h5ls(temp)
#' loadDelayed(temp)
#' 
#' @export
#' @rdname base-save
#' @importFrom HDF5Array writeHDF5Array 
#' @importFrom Matrix t
setMethod("saveDelayedObject", "array", function(x, file, name) {
    h5createGroup(file, name)

    .label_group_seed(file, name, "dense array")
    writeHDF5Array(x, file, file.path(name, 'data'))
    write_integer_scalar(file, name, "native", 0L)

    if (!is.null(dimnames(x))) {
        .save_list(dimnames(x), file, file.path(name, 'dimnames'), vectors.only=TRUE)
    }

    invisible(NULL)
})

#' @importFrom Matrix t
#' @importFrom HDF5Array HDF5Array
#' @importFrom DelayedArray DelayedArray
.load_array <- function(file, name, contents) {
    vals <- h5read(file, file.path(name, "data"))

    # If it's native, we need to undo rhdf5's transposition.
    if (h5read(file, file.path(name, "native"))) { 
        vals <- aperm(vals, dim(vals):1)
    }

    if (h5exists(file, name, "dimnames")) {
        dimnames(vals) <- .load_list(file, file.path(name, "dimnames"), vectors.only=TRUE)
    }

    DelayedArray(vals)
}

#' @export
#' @rdname base-save
#' @importFrom DelayedArray DelayedArray
setMethod("saveDelayedObject", "DelayedArray", function(x, file, name) {
    saveDelayedObject(x@seed, file, name)
})

#' @export
#' @rdname base-save
#' @importFrom rhdf5 h5createGroup h5createDataset h5write
#' @importClassesFrom Matrix CsparseMatrix
setMethod("saveDelayedObject", "CsparseMatrix", function(x, file, name) {
    h5createGroup(file, name)
    .label_group_seed(file, name, "sparse matrix")

    # Choosing the most efficient representation where possible.
    if (!is.logical(x@x)) {
        xstore <- get_best_type(x@x)
    } else {
        xstore <- "H5T_NATIVE_UCHAR"
    }
    dname <- file.path(name, "data")
    h5createDataset(file, dname, dims=length(x@x), H5type=xstore, storage.mode=typeof(x@x), chunk=min(length(x@x), 200000));
    h5write(x@x, file, dname)

    if (nrow(x) < 2^16) {
        istore <- "H5T_NATIVE_USHORT"
    } else {
        istore <- "H5T_NATIVE_UINT"
    }
    iname <- file.path(name, "indices")
    h5createDataset(file, iname, dims=length(x@i), H5type=istore, chunk=min(length(x@i), 200000));
    h5write(x@i, file, iname)

    # Also chunking the indptrs, in case you just want to fetch specific columns.
    pname <- file.path(name, "indptr")
    h5createDataset(file, pname, dims=length(x@p), H5type="H5T_NATIVE_ULONG", chunk=min(length(x@p), 5000));
    h5write(x@p, file, pname)

    h5write(dim(x), file, file.path(name, "shape"))

    .save_list(dimnames(x), file, file.path(name, "dimnames"), vectors.only=TRUE)
})

#' @importFrom Matrix sparseMatrix
#' @importFrom rhdf5 h5readAttributes
.load_csparse_matrix <- function(file, name) {
    p <- .load_simple_vector(file, file.path(name, "indptr")) 
    i <- .load_simple_vector(file, file.path(name, "indices"))
    x <- .load_simple_vector(file, file.path(name, "data"))

    dims <- .load_simple_vector(file, file.path(name, "shape"))
    dimnames <- .load_list(file, file.path(name, "dimnames"), vectors.only=TRUE)

    # Avoid inefficiency of sparseMatrix() constructor.
    if (is.logical(x)) {
        cls <- "lgCMatrix"
    } else {
        cls <- "dgCMatrix"
        x <- as.double(x)
    }
    out <- new(cls, i=i, p=p, x=x, Dim=dims, Dimnames=dimnames)

    DelayedArray(out)
}

#' @export
#' @rdname base-save
#' @importFrom rhdf5 h5createGroup 
setMethod("saveDelayedObject", "ANY", function(x, file, name) {
    if (is(x, "LowRankMatrixSeed")) { # From BiocSingular.
        h5createGroup(file, name)
        .label_group_operation(file, name, 'matrix product')
        saveDelayedObject(x@rotation, file, paste0(name, "/left_seed"))
        write_string_scalar(file, name, "left_orientation", "N");
        saveDelayedObject(x@components, file, paste0(name, "/right_seed"))
        write_string_scalar(file, name, "right_orientation", "T");

    } else if (is(x, "ResidualMatrixSeed")) {
        h5createGroup(file, name)
        write_string_scalar(file, name, "r_type_hint", "residual matrix");

        # Mimic a transposition operation.
        if (x@transposed) {
            .label_group_operation(file, name, 'transpose')
            h5write(c(1L, 0L), file, file.path(name, "permutation"))
            name <- paste0(name, "/seed")
            h5createGroup(file, name)
        }

        # Mimic a binary subtraction.
        .label_group_operation(file, name, 'binary arithmetic')
        write_string_scalar(file, name, "method", "-")
        saveDelayedObject(x@.matrix, file, paste0(name, "/left"))

        # Mimic a matrix product.
        name <- paste0(name, "/right")
        h5createGroup(file, name)
        .label_group_operation(file, name, 'matrix product')
        saveDelayedObject(x@Q, file, paste0(name, "/left_seed"))
        write_string_scalar(file, name, "left_orientation", "N");
        saveDelayedObject(x@Qty, file, paste0(name, "/right_seed"))
        write_string_scalar(file, name, "right_orientation", "N");

    } else {
        pkg <- attr(class(x), "package")
        failed <- TRUE

        if (!is.null(pkg)) {
            # Trying again after loading the namespace of the likely
            # package that contains the saveDelayedObject definition.
            candidate <- paste0('chihaya.', pkg)
            if (!isNamespaceLoaded(candidate)) {
                err <- try(loadNamespace(candidate))
                if (!is(err, "try-error")) {
                    saveDelayedObject(x, file, name)
                    failed <- FALSE
                }
            }
        }

        if (failed) {
            stop("no saveDelayed handler defined for class '", class(x), "'")
        }
    }
    invisible(NULL)
})

#' @importFrom rhdf5 h5read
#' @importFrom Matrix t
#' @importClassesFrom Matrix Matrix
.load_matrix_product <- function(file, name) {
    L <- .dispatch_loader(file, paste0(name, "/left_seed"))
    Lori <- h5read(file, paste0(name, "/left_orientation"))
    if (length(Lori) == 1 && as.character(Lori) == "T") {
        L <- t(L)
    } else if (is.matrix(L@seed) || is(L@seed, "Matrix")) {
        L <- L@seed
    }

    R <- .dispatch_loader(file, paste0(name, "/right_seed"))
    Rori <- h5read(file, paste0(name, "/right_orientation"))
    if (length(Rori) == 1 && as.character(Rori) == "N") {
        R <- t(R)
    } else if (is.matrix(R@seed) || is(R@seed, "Matrix")) {
        R <- R@seed
    }

    BiocSingular::LowRankMatrix(L, R)
}

#' @importFrom rhdf5 h5readAttributes
.load_residual_matrix <- function(file, name) {
    if (missing(file)) {
        return(length(find.package("ResidualMatrix")) > 0);
    }

    attrs <- h5readAttributes(file, name)
    transposed <- FALSE
    if (attrs$delayed_operation == "transpose") {
        transposed <- TRUE
        name <- paste0(name, "/seed")
    }

    .matrix <- .dispatch_loader(file, paste0(name, "/left"))
    .matrix <- .matrix@seed # TODO: fix this in ResidualMatrix so that subtraction works for arbitrary DelayedMatrix objects.

    Q <- .dispatch_loader(file, paste0(name, "/right/left_seed"))
    Qty <- .dispatch_loader(file, paste0(name, "/right/right_seed"))

    if (!isNamespaceLoaded("ResidualMatrix")) {
        loadNamespace("ResidualMatrix")
    }
    seed <- new("ResidualMatrixSeed", .matrix = .matrix, Q = as.matrix(Q), Qty = as.matrix(Qty), transposed = transposed)
    DelayedArray(seed)
}
