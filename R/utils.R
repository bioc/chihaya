.load_simple_vector <- function(file, name) {
    as.vector(h5read(file, name))
}

.save_list <- function(x, file, name, vectors.only=FALSE) { 
    h5createGroup(file, name)
    .label_group(file, name, "delayed_length", length(x))
    nms <- names(x)
    if (!is.null(nms)) {
        .label_group(file, name, "delayed_names", nms)
    }

    for (i in seq_along(x)) {
        if (!is.null(x[[i]])) {
            if (vectors.only) {
                h5write(x[[i]], file, file.path(name, i))
            } else {
                saveLayer(x[[i]], file, file.path(name, i))
            }
        }
    }
}

.load_list <- function(file, name, contents, vectors.only=FALSE) {
    attrs <- h5readAttributes(file, name)
    vals <- vector("list", attrs$delayed_length)
    for (i in names(contents)) {
        if (vectors.only) {
            vals[[as.integer(i)]] <- .load_simple_vector(file, file.path(name, i))
        } else {
            vals[[as.integer(i)]] <- .dispatch_loader(file, file.path(name, i), contents[[i]])
        }
    }
    names(vals) <- as.vector(attrs$delayed_names)
    vals
}

.label_group_class <- function(file, name, type) {
    .label_group(file, name, "delayed_type", type)
}

#' @importFrom rhdf5 h5writeAttribute H5Fopen H5Fclose H5Gopen H5Gclose
.label_group <- function(file, name, field, value) {
    fhandle <- H5Fopen(file)
    on.exit(H5Fclose(fhandle))
    ghandle <- H5Gopen(fhandle, name)
    on.exit(H5Gclose(ghandle), add=TRUE, after=FALSE)
    h5writeAttribute(value, ghandle, field)
}
