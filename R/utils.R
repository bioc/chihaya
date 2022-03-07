.load_simple_vector <- function(file, name) {
    as.vector(h5read(file, name))
}

.save_list <- function(x, file, name, vectors.only=FALSE) { 
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
}

.load_list <- function(file, name, vectors.only=FALSE) {
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

.label_group_operation <- function(file, name, op) {
    .label_group(file, name, c(delayed_type = "operation", delayed_operation = op))
}

.label_group_seed <- function(file, name, seed) {
    .label_group(file, name, c(delayed_type = "array", delayed_array = seed))
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
}

write_number_scalar <- function(file, host, name, val) {
    if (is.integer(val)) {
        write_integer_scalar(file, host, name, val)
    } else {
        write_double_scalar(file, host, name, val)
    }
}
