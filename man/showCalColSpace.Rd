\name{showCalColSpace}
\alias{showCalColSpace}
\title{
Use rgl to draw a plot of calibration colors in the rgb space.  Optionally show a set of sample colors.
}
\description{
This function takes a vector of colors used for calibration and plots them in the rgb color space using package \code{rgl}.  If sample colors are provided, the position of the samples relative to the calibration colors can be inspected to determine if the samples are sufficiently close to the calibration colors.  In addition, measures of the fit of the sample color to the calibration colors are returned in the assigned value.
}
\usage{
showCalColSpace(calCols, sampCol = NULL, title = NULL)
}
\arguments{
  \item{calCols}{
An list with elements \code{rgb} and \code{hexcol}.  See \code{\link{genCalCols}} for details.
}
  \item{sampCol}{
A data frame with columns \code{cols} which contains hexadecimal strings, and \code{id} which has the sample names.
}
  \item{title}{
A character vector giving a title for the plot.
}
}
\value{
A plot is drawn using \code{rgl} graphics, and information about the fit is returned as a data frame if \code{sampCols != NULL}.
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
bluehues <- genCalCols(minHue = "2.5B", maxHue = "10B",
	minVal = 1, maxVal = 10, minChroma = 2, maxChroma = 22,
	plotPC = FALSE, showCal = FALSE)
#
# And some sample colors for experimentation:
#
sc <- data.frame(cols = c("#FF0000", "#0000FF", "#98F5FF", "#6495ED"),
	id = c("red", "blue", "cadetblue1", "cornflower"))
#
# Obviously red isn't a shade of blue.  But good old blue isn't necessarily in the blue
# range either.  Along with the other two shades of blue, let's see what happens:
#
showCalColSpace(calCols = bluehues, sampCol = sc, title = "Good Bad & Ugly")
#
# What's going on with good old blue?
# Compare it with the nearest Munsell color.  Notice that
# good old blue is a PB in the Munsell system (purple-blue):
#
x11()
plot_mnsl(rgb2mnsl(R = 0.0, G = 0.0, B = 1.0))
x11()
plot_hex("#0000FF")
#
# More typically, you have some samples and need to select calibration colors
# that match your expected sample colors.  Imagine your color sample was in the
# blue-red range, let's use cornflower:
#
purblu <- genCalCols(minHue = "2.5PB", maxHue = "10PB",
	minVal = 2, maxVal = 9, minChroma = 2, maxChroma = 12,
	plotPC = FALSE, showCal = FALSE)
#
showCalColSpace(calCols = purblu, sampCol = sc[4,], title = "Good Fit")
#
# Not too bad of a fit! Even though the dist is about the same, the sample is
# in the thick of the calibration cloud.
}
\keyword{ multivariate }
\keyword{ plot }
