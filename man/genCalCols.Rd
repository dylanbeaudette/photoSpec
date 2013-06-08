\name{genCalCols}
\alias{genCalCols}
\title{
Generate a set of calibration colors
}
\description{
This function generates a set of calibration colors corresponding to the complement of the input wavelengths.  It is the starting point for the use of \code{photoSpec}.  A plot of the 1931 CIE chromaticity diagram is drawn highlighting the wedge of CIE xy values selected.
}
\usage{
genCalCols(L1 = NULL, L2 = NULL, colSpace = "sRGB", ff = 1,
nDiv = 10, pcpd = 1L, divMode = "linear", pMode = "snow", ...)
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
Numeric.  The 'fudge factor' to use.  The fudge factor must be used with \strong{extreme care}.  Larger values of \code{ff} make the white point whiter in the plot, and lightens colors near the spectral locus.  One purpose of \code{ff} is to alter the aesthetics of the plot - that is, to make the white "whiter" for the visual effect (perhaps in teaching CIE concepts).  It also has \strong{significant effects} on the sample card and calibration curve which must be used with care and competence!
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
  \item{pMode}{
Character.  The parallel processing mode to use.  Currently one of \code{c("none", "snow")}.  This is useful to speed up the processing of the bands.
}

  \item{\dots}{
Other parameters to be passed to downstream functions.
}
}
\details{
This function uses \code{\link{selectCIExy}} and \code{\link{selectCalCols}} which would generally not be called by the user.
}
\value{
An object of class \code{\link{CIEwedge}}.  The number of colors will be \code{nDiv} * \code{pcpd}.
}
\references{
NEED A REFERENCE
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\section{Warning }{
The appearance of the color gradient will vary with the device, surface and incident light used to view it and is not likely correct anywhere.  \strong{It also varies with ff}.
}
\examples{
# NEED AN EXAMPLE
}
\keyword{ plot }
