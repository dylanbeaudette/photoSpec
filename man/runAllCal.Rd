\name{runAllCal}
\alias{runAllCal}
\title{
Run all calibration options
}
\description{
This function runs both \code{\link{showCIEcalibration}} and \code{\link{showRGBcalibration}} and returns the results in a single data frame.
}
\usage{
runAllCal(calCols, sampCol = NULL, title = NULL, opts = "sRGB", ...)
}
\arguments{
  \item{calCols}{
An list with elements \code{rgb} and \code{hexcol}.  See \code{\link{genCalCols}} for details.
}
  \item{sampCol}{
A data frame with columns \code{hex} which contains hexadecimal strings, and \code{id} which has the sample names.
}
  \item{title}{
A character vector giving a title for the plot.
}
  \item{opts}{
Character vector passed to \code{plotCIEchrom}.
}
  \item{\dots}{
Other options to be passed to either \code{grid} plotting functions, for \code{\link{showCIEcalibration}}, or \code{rgl} plotting functions, for \code{\link{showRGBcalibration}}.  Arguments must be appropriate for the plotting system.  See those functions for the possible options, along with the examples below.
}
}
\value{
A plot is drawn using either \code{rgl} or \code{grid} graphics, and information about the calibration is returned in a data frame (DESCRIBE).
}
\references{
\url{https://github.com/bryanhanson/photoSpec}
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\examples{
##---- Should be DIRECTLY executable !! ----
}
\keyword{ plot }
