\name{showCalColSpace}
\alias{showCalColSpace}
\title{
Use rgl to draw a plot of calibration colors in the rgb space.  Optionally show a sample color.
}
\description{
This function takes a vector of colors used for calibration and plots them in the rgb color space using package \code{rgl}.  If a sample color is provided, the position of the sample relative to the calibration colors can be inspected to determine if the sample is sufficiently close to the calibration colors.  In addition, some measures of the fit of the sample color to the calibration colors are printed at the console. ELABORATE
}
\usage{
showCalColSpace(calCols, sampCol = NULL, sampName = "Demo")
}
\arguments{
  \item{calCols}{
An list with elements \code{rgb} and \code{hexcol}.  See \code{\link{genCalCols}} for details.
}
  \item{sampCol}{
A single color as a hexadecimal string to be plotted for evaluation.
}
  \item{sampName}{
A character vector giving the name of the sample.
}
}
\value{
A plot is drawn using \code{rgl} graphics, and information about the fit is printed at the console.
}
\references{
NEED A REFERENCE
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
require("rgl")
#
# Let's illustrate the general idea with some examples.
# First, generate some calibration colors:
#
blu <- genCalCols(minHue = "2.5B", maxHue = "10B",
	minVal = 1, maxVal = 10, minChroma = 2, maxChroma = 12,
	plotPC = FALSE, showCal = FALSE)
#
# And some sample colors for experimentation:
#
bad <- "#FF0000" # red, obviously not a blue!
better <- "#0000FF" # good old blue
best <- "#98F5FF" # cadetblue1, right in range
corn <- "#6495ED" # cornflower; has a little red mixed in
#
# Pass red as the sample, not a good fit:
#
showCalColSpace(calCols = blue1, sampCol = bad, sampName = "Bad Choice")
#
# A closer color.  Note that this good old blue is out of gamut for Munsell. CHECK!
#
showCalColSpace(calCols = blue1, sampCol = good, sampName = "Better Choice")
#
# Now a close color:
#
showCalColSpace(calCols = blue1, sampCol = best, sampName = "Best Choice")
#
# More typically, you have some samples and need to select calibration colors
# that match your expected sample colors.  Imagine your color sample was in the
# blue-red range, let's use cornflower:
#
purblu <- genCalCols(minHue = "2.5PB", maxHue = "10PB",
	minVal = 2, maxVal = 9, minChroma = 2, maxChroma = 12,
	plotPC = FALSE, showCal = FALSE)

showCalColSpace(calCols = purblu, sampCol = corn, sampName = "Best Choice")
#

}
\keyword{ multivariate }
\keyword{ plot }
