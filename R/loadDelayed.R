#' Load a DelayedMatrix
#'
#' Load a \linkS4class{DelayedMatrix} object from a location within a HDF5 file.
#'
#' @param file String containing a path to a HDF5 file.
#' @param path String containing a path inside a HDF5 file containing the DelayedMatrix.
#' 
#' @return A \linkS4class{DelayedMatrix} containing the contents at \code{path}.
#'
#' @author Aaron Lun
#' @examples
#' library(HDF5Array)
#' X <- rsparsematrix(100, 20, 0.1)
#' Y <- DelayedArray(X)
#' Z <- log2(Y + 1)
#'
#' temp <- tempfile(fileext=".h5")
#' saveDelayed(Z, temp)
#' loadDelayed(temp)
#'
#' @export
loadDelayed <- function(file, path="delayed") {
    contents <- .list_contents(file)
    fragmented <- strsplit(path, "/")[[1]]
    if (fragmented[1] == "") fragmented <- fragmented[-1]
    .dispatch_loader(file, path, contents[[fragmented]])
}

#' @importFrom rhdf5 h5ls
.list_contents <- function(file) {
    manifest <- h5ls(file)

    set_myself <- function(x, series, value) {
        if (length(series)!=1) {
            value <- set_myself(x[[series[1]]], series[-1], value)
        }
        if (is.null(x)) {
            x <- list()
        }
        x[[series[1]]] <- value
        x
    }

    contents <- list()
    for (i in seq_len(nrow(manifest))) {
        components <- c(strsplit(manifest[i, "group"], "/")[[1]], manifest[i, "name"])
        if (components[1] == "") {
            components <- components[-1]
        }
        if (manifest[i, "otype"]=="H5I_GROUP") {
            info <- list()
        } else {
            info <- TRUE
        }
        contents <- set_myself(contents, components, info)
    }

    contents
}

#' @importFrom rhdf5 h5readAttributes h5read
.dispatch_loader <- function(file, path, contents) {
    attrs <- h5readAttributes(file, path)

    if (is.list(contents)) {
        if (is.null(attrs$delayed_type)) {
            vals <- .load_list(file, path, contents)

        } else if (identical(attrs$delayed_type[1], "operation")) {
            FUN <- switch(attrs$delayed_type[2],
                subset=.load_delayed_subset,
                transpose=.load_delayed_aperm,
                combine=.load_delayed_combine,
                `n-ary isometric`=.load_delayed_nary_iso,
                dimnames=.load_delayed_dimnames,
                subassign=.load_delayed_subassign,
                `unary isometric stack`=.load_delayed_unary_iso_stack,
                `unary isometric with arguments`=.load_delayed_unary_iso_with_args
            )
            vals <- FUN(file, path, contents)

        } else if (identical(attrs$delayed_type[1], "seed")) {
            FUN <- switch(attrs$delayed_type[2],
                `csparse matrix`=.load_csparse_matrix,
                `external hdf5 array`=.load_dense_hdf5_array,
                `external sparse hdf5 matrix`=.load_sparse_hdf5_matrix,
                `external h5ad matrix`=.load_h5ad_matrix,
                `external tenx matrix`=.load_tenx_matrix
            ) 
            vals <- FUN(file, path, contents)

        } else if (identical(attrs$delayed_type[1], "array")) {
            vals <- .load_array(file, path, contents)

        } else {
            stop("unsupported type '", attrs$delayed_type[1], "'")
        }

    } else {
        vals <- h5read(file, path)
    }

    vals
}
