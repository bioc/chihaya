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
#' \item For \code{.saveScalar}, this should be a length-1 integer, logical, character or double vector.
#' }
#' @param parent String containing the name of the parent containing the child \code{name}.
#' @param vectors.only Logical scalar indicating whether elements of \code{x} should be saved and loaded as 1-d arrays rather than seeds.
#' @param op String containing the name of the delayed operation to use to label the group.
#' @param arr String containing the name of the delayed array to use to label the group.
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
#' @aliases
#' .saveList
#' .loadList
#' .labelArrayGroup
#' .labelOperationGroup
#' .writeScalar
#'
#' @name chihaya-utils
NULL

.load_simple_vector <- function(file, name) {
    as.vector(h5read(file, name))
}

#' @export
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
            vals[[i]] <- .load_simple_vector(file, file.path(name, j))
        } else {
            vals[[i]] <- .dispatch_loader(file, file.path(name, j))
        }
    }
    names(vals) <- as.vector(attrs$delayed_names)
    vals
}

#' @export
.labelOperationGroup <- function(file, name, op) {
    .label_group(file, name, c(delayed_type = "operation", delayed_operation = op))
}

#' @export
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
.saveScalar <- function(file, name, x, parent=NULL) {
    if (is.null(parent)) {
        parent <- dirname(name)
        name <- basename(name)
    }

    if (is.character(x)) {
        write_string_scalar(file, parent, name, x)
    } else if (is.integer(x)) {
        write_integer_scalar(file, parent, name, x)
    } else if (is.logical(x)) {
        write_integer_scalar(file, parent, name, as.integer(x))

        # Decorating it with the same stuff as rhdf5 uses.
        fhandle <- H5Fopen(file)
        on.exit(H5Fclose(fhandle))
        dhandle <- H5Dopen(fhandle, file.path(parent, name))
        on.exit(H5Dclose(dhandle), add=TRUE, after=FALSE)
        h5writeAttribute("logical", dhandle, "storage.mode", asScalar = TRUE, encoding = "UTF-8")
    } else if (is.double(x)) {
        write_double_scalar(file, parent, name, x)
    } else {
        stop("unsupported scalar type '", typeof(x), "'")
    }

    invisible(NULL)
}

#' @export
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
