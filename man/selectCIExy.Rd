\name{selectCIExy}
\alias{selectCIExy}
\title{
Select a region of the CIE chromaticity diagram.
}
\description{
Select a series of CIE xy values in the wedge defined by the complement of \code{L1} and \code{L2}.
}
\usage{
selectCIExy(L1, L2, colSpace = "sRGB", ff = 1.0, ...)
}
\arguments{
  \item{L1}{
Numeric, giving the lower wavelength at the edge of the region to be analyzed.
}
  \item{L2}{
Numeric, giving the upper wavelength at the edge of the region to be analyzed.
}
  \item{colSpace}{
Character string giving the color space to use for drawing the gradient.  One of \code{c("sRGB", "Apple RGB")}.  \code{Apple RGB} is mainly of historical interest; no physical devices use it at this time.
}
  \item{ff}{
Numeric.  The 'fudge factor' to use.  NEED TO EXPLAIN AND SHOW IN EXAMPLES.
}
  \item{\ldots}{
Arguments to be passed to \code{\link{plotCIEchrom}}, typically one of the \code{opts}.
}

}
\details{
MAY NEED DETAILS
}
\value{
An object of class \code{\link{CIEwedge}}.
}
\references{
NEED A REFERENCE
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\keyword{ plot }
