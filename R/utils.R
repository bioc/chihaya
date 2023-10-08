#' Developer utilities for custom extensions
#'
#' Convenience utilities for extending the \pkg{chihaya} format with \dQuote{custom} seeds or operations.
#' These should only be used by package developers. 
#'
#' @param file String containing a path to a file.
#' @param name String containing the name of the object inside the file.
#' This should be a full path from the root of the file, unless \code{parent} is provided, in which case it may be the name of the child.
#' @param x The object to save.
#' \itemize{ 
#' \item For \code{.pickArrayType}, this should be an array-like object.
#' \item For \code{.saveList}, this should be a list.
#' \item For \code{.saveDataset}, this should be a integer, logical, character or double vector or array.
#' }
#' @param parent String containing the name of the parent containing the child \code{name}.
#' @param vectors.only Logical scalar indicating whether elements of \code{x} should be saved and loaded as 1-d arrays rather than seeds.
#' @param op String containing the name of the delayed operation to use to label the group.
#' @param arr String containing the name of the delayed array to use to label the group.
#' @param scalar Logical scalar indicating whether length-1 \code{x} should be saved to file as a scalar.
#' @param optimize.type Logical scalar indicating whether to optimize the HDF5 storage type for non-scalar, non-string \code{x}.
#' @param h5type String specifying the HDF5 storage type to use for non-scalar, non-string \code{x}, see \code{\link{h5const}("H5T")} for possible choices.
#' This is ignored if \code{optimize.type=TRUE}.
#' @param chunks Integer vector of length equal to the number of dimensions of non-scalar \code{x}, specifying the chunk dimensions to use.
#' If \code{NULL}, this is set to the length of \code{x} (if \code{x} is a vector) or chosen by \pkg{HDF5Array} (if \code{x} is an array).
#' 
#' @return 
#' \code{.saveList} and \code{.saveScalar} will write \code{x} to file, returning \code{NULL} invisibly.
#'
#' \code{.labelArrayGroup} and \code{.labelOperationGroup} will apply the label to the specified group, returning \code{NULL} invisibly.
#' 
#' \code{.loadList} will return a list containing the contents of \code{name}.
#' This is guaranteed to contain only vectors (or fail) if \code{vectors.only=TRUE}.
#'
#' \code{.pickArrayType} will return a string containing the \pkg{chihaya} type for an array-like \code{x}.
#'
#' @author Aaron Lun
#' @name chihaya-utils
NULL

#' @importFrom rhdf5 h5read h5readAttributes
.load_dataset_with_attributes <- function(file, name, drop=FALSE) {
    out <- h5read(file, name, drop=drop)
    if (is.raw(out)) {
        storage.mode(out) <- "integer"
    }

    attrs <- h5readAttributes(file, name)

    placeholder <- attrs[["missing_placeholder"]]
    if (!is.null(placeholder)) {
        placeholder <- as.vector(placeholder)
        if (!is.na(placeholder)) {
            out[out == placeholder] <- NA
        } else if (is.nan(placeholder)) {
            out[is.nan(out)] <- NA
        }
    } else if (is.character(out)) {
        # Back-compatibility for the old undocumented way of holding missing values.
        placeholder <- attrs[["missing-value-placeholder"]]
        if (!is.null(placeholder)) {
            placeholder <- as.vector(placeholder)
            out[out == placeholder] <- NA
        }
    }

    is_boolean <- attrs[["is_boolean"]]
    if (!is.null(is_boolean) && as.integer(is_boolean) == 1L) {
        storage.mode(out) <- "logical"
    }

    out
}

.load_vector_with_attributes <- function(file, name) {
    .load_dataset_with_attributes(file, name, drop=TRUE)
}

#' @export
#' @rdname chihaya-utils
.saveList <- function(file, name, x, parent=NULL, vectors.only=FALSE) { 
    if (!is.null(parent)) {
        name <- file.path(parent, name)
    }

    h5createGroup(file, name)
    .label_group(file, name, list(delayed_type = "list", delayed_length=length(x)))

    for (i in seq_along(x)) {
        if (!is.null(x[[i]])) {
            j <- i - 1L
            if (vectors.only) {
                h5write(x[[i]], file, file.path(name, j))
            } else {
                saveDelayedObject(x[[i]], file, file.path(name, j))
            }
        }
    }

    invisible(NULL)
}

#' @export
#' @rdname chihaya-utils
#' @importFrom rhdf5 h5read
.loadList <- function(file, name, parent=NULL, vectors.only=FALSE) {
    if (!is.null(parent)) {
        name <- file.path(parent, name)
    }

    attrs <- h5readAttributes(file, name)
    vals <- vector("list", attrs$delayed_length)

    for (i in seq_along(vals)) {
        j <- as.character(i - 1L)
        if (!h5exists(file, name, j)) {
            next
        }

        if (vectors.only) {
            vals[[i]] <- h5read(file, file.path(name, j), drop=TRUE)
        } else {
            vals[[i]] <- .dispatch_loader(file, file.path(name, j))
        }
    }
    names(vals) <- as.vector(attrs$delayed_names)
    vals
}

#' @export
#' @rdname chihaya-utils
.labelOperationGroup <- function(file, name, op) {
    .label_group(file, name, c(delayed_type = "operation", delayed_operation = op))
}

#' @export
#' @rdname chihaya-utils
.labelArrayGroup <- function(file, name, arr) {
    .label_group(file, name, c(delayed_type = "array", delayed_array = arr))
}

#' @importFrom rhdf5 h5writeAttribute H5Fopen H5Fclose H5Gopen H5Gclose
.label_group <- function(file, name, values) {
    fhandle <- H5Fopen(file)
    on.exit(H5Fclose(fhandle))
    ghandle <- H5Gopen(fhandle, name)
    on.exit(H5Gclose(ghandle), add=TRUE, after=FALSE)
    for (n in names(values)) {
        h5writeAttribute(values[[n]], ghandle, n, asScalar = TRUE, encoding = "UTF-8")
    }
    invisible(NULL)
}

#' @export
#' @rdname chihaya-utils
#' @importFrom rhdf5 H5Fopen H5Dopen h5writeAttribute H5Fclose H5Dclose h5write
#' @importFrom HDF5Array writeHDF5Array
.saveDataset <- function(file, name, x, parent=NULL, scalar=FALSE, optimize.type=FALSE, h5type=NULL, chunks=NULL) {
    if (is.null(parent)) {
        parent <- dirname(name)
        name <- basename(name)
    }

    if (scalar && length(x) != 1) {
        scalar <- FALSE
    }

    dname <- paste0(parent, "/", name)
    .non_scalar_saver <- function(x, final.h5type) {
        if (is.null(dim(x))) {
            if (is.null(chunks)) {
                chunks <- length(x)
            }
            h5createDataset(file, dname, dims=length(x), H5type=final.h5type, chunk=chunks)
            h5write(x, file, dname)
        } else {
            writeHDF5Array(x, file, dname, H5type=final.h5type, chunkdim=chunks)
        }
    }

    placeholder <- NULL
    if (is.character(x)) {
        if (anyNA(x)) {
            placeholder <- "NA"
            search <- unique(x)
            while (placeholder %in% search) {
                placeholder <- paste0("_", placeholder)
            }
            x[is.na(x)] <- placeholder
        }

        if (scalar) {
            write_string_scalar(file, parent, name, x)
        } else {
            .non_scalar_saver(x, NULL)
        } 

    } else if (is.integer(x)) {
        if (anyNA(x)) {
            placeholder <- NA_integer_
        }

        if (scalar) {
            write_integer_scalar(file, parent, name, x)
        } else {
            h5type <- NULL
            if (optimize.type) {
                h5type <- get_best_type_int(x)
            }
            .non_scalar_saver(x, h5type)
        } 

    } else if (is.logical(x)) {
        storage.mode(x) <- "integer"
        if (anyNA(x)) {
            placeholder <- -1L
            x[is.na(x)] <- placeholder
        }

        if (scalar) {
            write_integer_scalar(file, parent, name, x)
        } else {
            .non_scalar_saver(x, "H5T_NATIVE_INT8")
        }

        (function() {
            fhandle <- H5Fopen(file)
            on.exit(H5Fclose(fhandle))
            dhandle <- H5Dopen(fhandle, dname)
            on.exit(H5Dclose(dhandle), add=TRUE, after=FALSE)
            h5writeAttribute(1L, dhandle, "is_boolean", asScalar = TRUE, encoding = "UTF-8")
        })()

    } else if (is.double(x)) {
        if (anyNA(x) && sum(is.nan(x)) < sum(is.na(x))) {
            placeholder <- NA_real_
        }

        if (scalar) {
            write_double_scalar(file, parent, name, x)
        } else {
            h5type <- NULL
            if (optimize.type) {
                h5type <- get_best_type_double(x)
            }
            .non_scalar_saver(x, h5type)
        }

    } else {
        stop("unsupported scalar type '", typeof(x), "'")
    }

    if (!is.null(placeholder)) {
        (function() {
            fhandle <- H5Fopen(file)
            on.exit(H5Fclose(fhandle))
            dhandle <- H5Dopen(fhandle, dname)
            on.exit(H5Dclose(dhandle), add=TRUE, after=FALSE)
            h5writeAttribute(placeholder, dhandle, "missing_placeholder", asScalar = TRUE, encoding = "UTF-8")
        })()
    }

    invisible(NULL)
}

#' @export
#' @rdname chihaya-utils
#' @importFrom DelayedArray type
.pickArrayType <- function(x) {
    ty <- type(x)
    if (ty == "logical") {
        "BOOLEAN"
    } else if (ty == "integer") {
        "INTEGER"
    } else if (ty == "double") {
        "FLOAT"
    } else if (ty == "character") {
        "STRING"
    } else {
        stop("unrecognized array type '", ty, "'")
    }
}
