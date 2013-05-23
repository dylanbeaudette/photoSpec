\name{genCalCols}
\alias{genCalCols}
\title{
Generate a set of calibration colors
}
\description{
This function generates a set of calibration colors corresponding to the complement of the input wavelengths.  It is the starting point for the use of \code{photoSpec}.
}
\usage{
genCalCols(L1 = NULL, L2 = NULL, colSpace = "sRGB", ff = 1, nDiv = 10, pcpd = 1L, divMode = "linear", ...)
}
%- maybe also 'usage' for other objects documented here.
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
  \item{nDiv}{
Integer.  The number of bands to be created within the wedge.
}
  \item{pcpd}{
Integer.  "Paint chips per division".  The number of colors to be selected from each band.
}
  \item{divMode}{
Character.  The mode used to divide up the wedge into bands.  Currently only \code{"linear"} is available.
}
  \item{\dots}{
Other parameters to be passed to downstream functions.
}
}
\details{
This function uses \code{selectCIExy} and \code{selectCalCols} which would generally not be called by the user.
}
\value{
A list of calibration colors in hexadecimal mode.  The number of colors will be \code{nDiv} * \code{pcpd}.
}
\references{
NEED A REFERENCE
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\section{Warning }{
The appearance of the color gradient will vary with the device, surface and incident light used to view it and is not likely correct anywhere.
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
\keyword{ plot }
\keyword{ utilities }
