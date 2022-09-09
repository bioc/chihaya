external.env <- new.env()
external.env$allow <- TRUE

#' Allow saving of external seeds
#'
#' Should external array seeds be saved in \code{\link{saveDelayed}}?
#' If \code{FALSE}, an error is raised upon encountering external references such as \linkS4class{HDF5ArraySeed}s.
#' This prevents the creation of delayed objects that cannot be used on different filesystems.
#'
#' @param allow Logical scalar indicating whether to allow downloads of external seeds.
#' 
#' @return If \code{allow} is not supplied, the current value of this flag is returned.
#'
#' If \code{allow} is supplied, it is used to define the value of this flag, and the \emph{previous} value of the flag is returned.
#'
#' @author Aaron Lun
#'
#' @examples
#' allowExternalSeeds()
#'
#' a <- allowExternalSeeds(FALSE)
#' allowExternalSeeds()
#'
#' # Setting it back
#' allowExternalSeeds(a)
#' 
#' @export
allowExternalSeeds <- function(allow) {
    prev <- external.env$allow
    if (missing(allow)) {
        prev
    } else {
        external.env$allow <- allow
        invisible(prev)
    }
}
