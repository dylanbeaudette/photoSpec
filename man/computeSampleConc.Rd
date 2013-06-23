\name{computeSampleConc}
\alias{computeSampleConc}
\title{
Fit a sample color to a color calibration curve
}
\description{
Using a vector of calibration colors and a single sample color, this function maps the sample color onto a calibration curve created from the calibration colors (the principal curve is used).  Called by \code{showCalColSpace} and not generally by the user.
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
The principal curve through the calibration colors in rgb space is computed (this is analogous to finding a principal component).  The sample color is then projected onto this curve.  It's position along the curve as a percentage of the total length is reported as a value analogous to concentration.
}
\value{
\code{calCols} is returned with two additional columns: \code{conc} and \code{dist} containing the interpolated sample 'concentration' and the distance of the sample from the calibration curves.
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
