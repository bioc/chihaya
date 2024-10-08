% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DelayedUnaryIsoOpWithArgs.R
\name{saveDelayedObject,DelayedUnaryIsoOpWithArgs-method}
\alias{saveDelayedObject,DelayedUnaryIsoOpWithArgs-method}
\title{Saving a DelayedUnaryIsoOpWithArgs}
\usage{
\S4method{saveDelayedObject}{DelayedUnaryIsoOpWithArgs}(x, file, name)
}
\arguments{
\item{x}{A \linkS4class{DelayedUnaryIsoOpWithArgs} object.}

\item{file}{String containing the path to a HDF5 file.}

\item{name}{String containing the name of the group to save into.}
}
\value{
A \code{NULL}, invisibly.
A group is created at \code{name} containing the contents of the DelayedUnaryIsoOpWithArgs.
}
\description{
Save a \linkS4class{DelayedUnaryIsoOpWithArgs} object into a HDF5 file.
See the \dQuote{Unary ...} operation at \url{https://artifactdb.github.io/chihaya/} for more details.
}
\examples{
X <- DelayedArray(matrix(runif(100), ncol=5))
Y <- (1:20 + X) / runif(5)
temp <- tempfile(fileext=".h5")
saveDelayed(Y, temp)
rhdf5::h5ls(temp)
loadDelayed(temp)

}
\author{
Aaron Lun
}
