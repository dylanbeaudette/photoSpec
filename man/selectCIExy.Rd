\name{selectCIExy}
\alias{selectCIExy}
\title{
Select a region of the CIE chromaticity diagram.
}
\description{
NEED A DESCRIPTION
}
\usage{
selectCIExy(L1, L2, colSpace = "sRGB", ff = 1.0, ...)
}
\arguments{
  \item{L1}{
Numeric, giving the wavelength at one edge of the region to be analyzed.
}
  \item{L2}{
Numeric, giving the wavelength at one edge of the region to be analyzed.
}
  \item{colSpace}{
Character string giving the color space to use for drawing the gradient.  One of \code{c("sRGB", "Apple RGB")}.
}
  \item{ff}{
Numeric.  The 'fudge factor' to use.  NEED TO EXPLAIN AND SHOW IN EXAMPLES.
}
  \item{\ldots}{
Arguments to be passed to \code{plotCIEchrom}, typically one of the \code{opts}.
}

}
\details{
MAY NEED DETAILS
}
\value{
A vector of colors that fall in the polygon specified by the wavelength range.  Currently, this is sampled down to just 10 values.
}
\references{
NEED A REFERENCE
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\keyword{ plot }
