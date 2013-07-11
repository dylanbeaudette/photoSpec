\name{computeSampleConc}
\alias{computeSampleConc}
\title{
Fit sample colors to a calibration curve
}
\description{
Using a set of calibration colors and a set of sample colors, this function maps the sample colors onto a calibration curve created from the calibration colors (the principal curve is used).  Called by \code{showCalColSpace} and not generally by the user.
}
\usage{
computeSampleConc(calCols, sampCol = NULL)
}
\arguments{
  \item{calCols}{
An list with elements \code{rgb} and \code{hexcol}. See \code{\link{genCalCols}} for details.
}

  \item{sampCol}{
A data frame with columns \code{cols} which contains hexadecimal strings, and \code{id} which has the sample names.
}
}

\details{
The principal curve through the calibration colors in rgb space is computed after adding pure white (1, 1, 1) and pure black (0, 0, 0) (this is analogous to finding a principal component).  The sample color is then projected onto this curve.  It's position along the curve as a percentage of the total length is reported as a value analogous to concentration.
}
\value{
A list with two top level elements: \code{calCols} is returned with two additional columns: \code{conc} and \code{dist} containing the interpolated sample 'concentrations' and the distance of the samples from the calibration curves. The second element is \code{pcfit} which contains the results of fitting the principal curve.
}
\references{
NEED A REFERENCE
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\seealso{
\code{\link{showCalColSpace}} for an example.
}
\keyword{ utilities }
