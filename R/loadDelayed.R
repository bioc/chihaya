#' @export
loadDelayed <- function(file, path="delayed") {
    contents <- .list_contents(file)
    .dispatch_loader(file, path, contents[[path]])
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
        if (is.null(attrs$`_class`)) {
            vals <- vector("list", attrs$`_length`)
            for (i in names(contents)) {
                vals[[as.integer(i)]] <- .dispatch_loader(file, file.path(path, i), contents[[i]])
            }
            names(vals) <- as.vector(attrs$`_names`)

        } else if (identical(attrs$`_class`[1], "operation")) {
            FUN <- switch(attrs$`_class`[2],
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
        }
    } else {
        vals <- h5read(file, path)
        if (identical(attrs$`_class`[1], "vector")) {
            vals <- as.vector(vals)
        }
    }

    vals
}
