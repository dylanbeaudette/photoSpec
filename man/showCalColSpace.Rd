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
require("munsell")
#
# Let's illustrate the general idea with some examples.
# First, generate some calibration colors:
#
blue1 <- genCalCols(minHue = "2.5B", maxHue = "10B",
	minVal = 1, maxVal = 10, minChroma = 2, maxChroma = 22,
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
# A closer color, good old blue.  Note that this good old blue isn't 
# in the calibration space:
#
showCalColSpace(calCols = blue1, sampCol = better, sampName = "Better Choice")
#
# Why? Compare good old blue with the nearest Munsell color.  Notice that
# good old blue is a PB in the Munsell system (purple-blue):
#
x11()
plot_mnsl(rgb2mnsl(R = 0.0, G = 0.0, B = 1.0))
x11() # open new window
plot_hex("#0000FF")
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
# Let's go back to that good old blue.  Expand the chroma range, and the
# calibration colors are much closer, but not close enough.
#
purblu <- genCalCols(minHue = "2.5PB", maxHue = "10PB",
	minVal = 2, maxVal = 9, minChroma = 2, maxChroma = 24,
	plotPC = FALSE, showCal = FALSE)

showCalColSpace(calCols = purblu, sampCol = "#0000FF", sampName = "Blue isn't Blue")

}
\keyword{ multivariate }
\keyword{ plot }
