\name{calcSampleValue}
\alias{calcSampleValue}
\title{
Project sample colors onto the principal curve formed by the calibration colors
}
\description{
Using a set of calibration colors and a set of sample colors, this function maps the sample colors onto the principal curve created from the calibration colors plus pure white and pure black.  Experiments have shown that this value is approximately proportional to the Munsell value.  Called by \code{showRGBcalibration} and not generally by the user.
}
\usage{
calcSampleValue(calCols, sampCol = NULL)
}
\arguments{
  \item{calCols}{
An list with elements \code{rgb} and \code{hexcol}. See \code{\link{genCalCols}} for details.
}

  \item{sampCol}{
A data frame with columns \code{cols} which contains hexadecimal strings, and \code{id} which has the sample names.  If missing, the principal curve is computed.
}
}

\details{
The principal curve through the calibration colors in rgb space is computed after adding pure white (1, 1, 1) and pure black (0, 0, 0) (this is analogous to finding a principal component).  The sample color is then projected onto this curve.  Its position along the curve as a percentage of the total length is reported as a value approximately proportional to the Munsell value.
}
\value{
A list with two top level elements: \code{calCols} is returned with two additional columns: \code{value} and \code{residual} containing the interpolated sample values and the distance of the samples from the principal curve. The second element is \code{pcfit} which contains the results of fitting the principal curve.
}
\references{
NEED A REFERENCE
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\seealso{
\code{\link{showRGBcalibration}} for an example.
}
\keyword{ utilities }
