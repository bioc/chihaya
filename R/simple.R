#' Saving simple seed classes
#'
#' Methods to save simple seed classes - namely, ordinary matrices or sparse \pkg{Matrix} objects - into the delayed operation file.
#' See \dQuote{Dense arrays} and \dQuote{Sparse matrices} at \url{https://ltla.github.io/chihaya} for more details.
#'
#' @param x An R object of the indicated class.
#' @inheritParams saveDelayedObject
#'
#' @details
#' For string arrays, missing values are handled by the \code{"missing-value-placeholder"} attribute on the \code{data} dataset.
#' All \code{NA} values in the array are replaced by the placeholder value in the attribute when they are saved inside the HDF5 file.
#' If this attribute is not present, it can be assumed that all strings are non-missing.
#' 
#' @return A \code{NULL}, invisibly.
#' A group is created at \code{name} containing the contents of \code{x}.
#'
#' @author Aaron Lun
#'
#' @examples
#' # Saving an ordinary matrix.
#' X <- matrix(rpois(100, 2), 5, 20)
#' Y <- DelayedArray(X)
#' temp <- tempfile(fileext=".h5")
#' saveDelayed(Y, temp)
#' rhdf5::h5ls(temp)
#' loadDelayed(temp)
#'
#' # Saving a sparse matrix.
#' X <- rsparsematrix(100, 20, 0.1)
#' Y <- DelayedArray(X)
#' temp <- tempfile(fileext=".h5")
#' saveDelayed(Y, temp)
#' rhdf5::h5ls(temp)
#' loadDelayed(temp)
#'
#' @export
#' @rdname simple
#' @importFrom HDF5Array writeHDF5Array 
#' @importFrom DelayedArray type
setMethod("saveDelayedObject", "array", function(x, file, name) {
    h5createGroup(file, name)
    .labelArrayGroup(file, name, "dense array")

    missing.placeholder <- NULL
    if (type(x) == "character" && anyNA(x)) {
        missing.placeholder <- "NA"
        search <- unique(x)
        while (missing.placeholder %in% search) {
            missing.placeholder <- paste0("_", missing.placeholder)
        }
        x[is.na(x)] <- missing.placeholder
    }

    dname <- paste0(name, "/data")
    writeHDF5Array(x, file, dname)
    write_integer_scalar(file, name, "native", 0L)

    if (!is.null(dimnames(x))) {
        .saveList(file, 'dimnames', dimnames(x), parent=name, vectors.only=TRUE)
    }

    if (type(x) == "logical") {
        .add_logical_attribute(file, dname)
    } else if (!is.null(missing.placeholder)) {
        .add_missing_string_placeholder(file, dname, missing.placeholder)
    }

    invisible(NULL)
})

#' @importFrom rhdf5 H5Fopen H5Dopen H5Fclose H5Dclose h5writeAttribute H5Aexists H5Adelete
.swap_attribute <- function(file, dname, aname, value, remove) { 
    fhandle <- H5Fopen(file)
    on.exit(H5Fclose(fhandle), add=TRUE)
    dhandle <- H5Dopen(fhandle, dname)
    on.exit(H5Dclose(dhandle), add=TRUE)

    # Removing the older attribute, usually added by rhdf5. This avoids
    # confusion with two attributes that kind-of say the same thing. It also
    # ensures that our tests don't just work because rhdf5 is detecting its
    # own attributes, but rather, our own flags are doing the work.
    for (r in remove) {
        if (H5Aexists(h5obj = dhandle, name = r)) {
            H5Adelete(h5obj = dhandle, name = r)
        }
    }

    h5writeAttribute(value, h5obj=dhandle, name=aname, asScalar=TRUE)
}

.add_logical_attribute <- function(file, name) {
    .swap_attribute(file, name, "is_boolean", 1L, "storage.mode") 
}

#' @importFrom rhdf5 h5readAttributes
.is_logical <- function(file, dname) {
    isTRUE(h5readAttributes(file, dname)$is_boolean == 1L)
}

.add_missing_string_placeholder <- function(file, dname, placeholder) {
    .swap_attribute(file, dname, "missing-value-placeholder", placeholder, character(0))
}

#' @importFrom rhdf5 h5readAttributes
.extract_missing_string_placeholder <- function(file, dname) {
    h5readAttributes(file, dname)[["missing-value-placeholder"]]
}

#' @importFrom Matrix t
#' @importFrom HDF5Array HDF5Array
#' @importFrom DelayedArray DelayedArray
#' @importFrom rhdf5 h5readAttributes
.load_array <- function(file, name, contents) {
    dname <- paste0(name, "/data")
    vals <- h5read(file, dname)

    # If it's native, we need to undo rhdf5's transposition.
    if (h5read(file, file.path(name, "native"))) { 
        vals <- aperm(vals, dim(vals):1)
    }

    if (h5exists(file, name, "dimnames")) {
        dimnames(vals) <- .loadList(file, "dimnames", parent=name, vectors.only=TRUE)
    }

    if (is.integer(vals)) {
        if (.is_logical(file, dname)) {
            storage.mode(vals) <- "logical"
        }
    } else if (is.character(vals)) {
        missing.placeholder <- .extract_missing_string_placeholder(file, dname)
        if (!is.null(missing.placeholder)) {
            vals[vals== missing.placeholder] <- NA_character_
        }
    }

    vals
}

#' @export
#' @rdname saveDelayedObject 
#' @importFrom DelayedArray DelayedArray
setMethod("saveDelayedObject", "DelayedArray", function(x, file, name) {
    saveDelayedObject(x@seed, file, name)
})

#' @export
#' @rdname simple
#' @importFrom rhdf5 h5createGroup h5createDataset h5write H5Dopen H5Dclose
#' @importClassesFrom Matrix CsparseMatrix
setMethod("saveDelayedObject", "CsparseMatrix", function(x, file, name) {
    h5createGroup(file, name)
    .labelArrayGroup(file, name, "sparse matrix")

    # Choosing the most efficient representation where possible.
    if (!is.logical(x@x)) {
        xstore <- get_best_type(x@x)
    } else {
        xstore <- "H5T_NATIVE_UCHAR"
    }
    dname <- paste0(name, "/data")
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

    if (type(x) == "logical") {
        .add_logical_attribute(file, dname)
    } 

    .saveList(file, "dimnames", dimnames(x), parent=name, vectors.only=TRUE)
})

#' @importFrom Matrix sparseMatrix
.load_csparse_matrix <- function(file, name) {
    p <- .load_simple_vector(file, file.path(name, "indptr")) 
    i <- .load_simple_vector(file, file.path(name, "indices"))
    dname <- paste0(name, "/data")
    x <- .load_simple_vector(file, dname)

    dims <- .load_simple_vector(file, file.path(name, "shape"))
    dimnames <- .loadList(file, "dimnames", parent=name, vectors.only=TRUE)

    if (.is_logical(file, dname)) {
        cls <- "lgCMatrix"
        x <- as.logical(x)
    } else {
        cls <- "dgCMatrix"
        x <- as.double(x)
    }

    new(cls, i=i, p=p, x=x, Dim=dims, Dimnames=dimnames)
}
