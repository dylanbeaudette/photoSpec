\name{computeSampleAbs}
\alias{computeSampleAbs}
\title{
Fit a sample color to a color calibration curve
}
\description{
Using a vector of calibration colors and a single sample color, this function maps the sample color onto a calibration curve created from the calibration colors.  Called by \code{showCalColSpace} and not generally by the user.
}
\usage{
computeSampleAbs(calCols, sampCol, PC1, calVals)
}
\arguments{
  \item{calCols}{
A vector of colors as hexadecimal strings to be used as the calibration points.  Derived from \code{wedge$calCols}.
}
  \item{sampCol}{
A single color as a hexadecimal string to be interpolated by projection onto the calibration curve.
}
  \item{PC1}{
A matrix giving two points on PC1 (the first eigenvector).
}
  \item{calVals}{
A length 2 numeric vector giving an upper and lower values for the calibration curve.
}

}
\details{
\code{calCols} are converted to 3D space as their rgb values.  Principal components analysis is then used to fit a line of best fit (the first principal component).  Finally, \code{sampCol} is projected onto the 1st PC and converted to an 'absorbance' value by interpolating along \code{calVals}.  NOTE: the current implementation only uses the most extreme calibration colors to compute the 'absorbance' CHECK THIS.
}
\value{
Diagnostic information and the 'answer' are printed at the console.
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
