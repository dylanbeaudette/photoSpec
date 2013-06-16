\name{computeSampleAbs}
\alias{computeSampleAbs}
\title{
Fit a sample color to a color calibration curve
}
\description{
Using a vector of calibration colors and a single sample color, this function maps the sample color onto a calibration curve created from the calibration colors (the principal curve is used).  Called by \code{showCalColSpace} and not generally by the user.
}
\usage{
computeSampleAbs(calCols, sampCol = NULL)
}
\arguments{
  \item{calCols}{
An list with elements \code{rgb} and \code{hexcol}.
}

  \item{sampCol}{
A single color as a hexadecimal string to be interpolated by projection onto the calibration curve.
}
}

\details{
The principal curve through the calibration colors in rgb space is computed (this is analogous to finding a principal component).  The sample color is then projected onto this curve.  It's position along the curve as a percentage of the total length is reported as a value analogous to absorbance.
}
\value{
NEEDS TO BE UPDATED
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
