\name{computeSampleAbs}
\alias{computeSampleAbs}
\title{
Fit a sample color to a color calibration curve
}
\description{
Using a vector of calibration colors and a single sample color, this function maps the sample color onto a calibration curve created from the calibration colors.  Called by \code{showCalColSpace} and not generally by the user.
}
\usage{
computeSampleAbs(calCols, sampCol, sampName)
}
\arguments{
  \item{calCols}{
A vector of colors as hexadecimal strings to be used as the calibration points.
}
  \item{sampCol}{
A single color as a hexadecimal string to be interpolated by projection onto the calibration curve.
}
  \item{sampName}{
A character vector giving the name of the sample.
}
}
\details{
A very simple fit of the sample to the two closest calibration colors is conducted, and the sample color is interpolated.  In future versions, more sophisticated fits will be implemented.
}
\value{
Diagnostic information and the answer are printed at the console.
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
