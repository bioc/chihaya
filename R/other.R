#' Saving other seed classes
#'
#' Optional methods to save other classes, depending on the availability of the packages in the current R installation.
#'
#' @param x An R object of a supported class, see Details.
#' @inheritParams saveDelayedObject
#'
#' @details
#' The ANY method will dispatch to classes that are implemented in other packages:
#' \itemize{
#' \item If \code{x} is a LowRankMatrixSeed from the \pkg{BiocSingular} package, it is handled as a delayed matrix product.
#' \item If \code{x} is a ResidualMatrixSeed from the \pkg{ResidualMatrix} package, it is converted into the corresponding series of delayed operations.
#' However, the top-level group will contain a \code{"r_type_hint"} dataset to indicate that it was originally a ResidualMatrix object.
#' This provides R clients with the opportunity to reload it as a ResidualMatrix, which may be more efficient than the naive DelayedArray representation.
#' \item Otherwise, if \code{x} comes from package \pkg{Y}, we will try to load \pkg{chihaya.Y}.
#' This is assumed to define an appropriate \code{saveDelayedObject} method for \code{x}.
#' }
#' 
#' @return A \code{NULL}, invisibly.
#' A group is created at \code{name} containing the contents of \code{x}.
#'
#' @author Aaron Lun
#'
#' @examples
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
#' @rdname other
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
    } 

    R <- .dispatch_loader(file, paste0(name, "/right_seed"))
    Rori <- h5read(file, paste0(name, "/right_orientation"))
    if (length(Rori) == 1 && as.character(Rori) == "N") {
        R <- t(R)
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
    Q <- .dispatch_loader(file, paste0(name, "/right/left_seed"))
    Qty <- .dispatch_loader(file, paste0(name, "/right/right_seed"))

    if (!isNamespaceLoaded("ResidualMatrix")) {
        loadNamespace("ResidualMatrix")
    }
    seed <- new("ResidualMatrixSeed", .matrix = .matrix, Q = Q, Qty = Qty, transposed = transposed)
    DelayedArray(seed)
}
