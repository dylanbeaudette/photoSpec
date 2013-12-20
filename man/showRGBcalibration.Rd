\name{showRGBcalibration}
\alias{showRGBcalibration}
\alias{showCIE}
\title{
Draw a plot of calibration colors in a color space.  Optionally show a set of sample colors.
}
\description{
These functions takes a vector of colors used for calibration and plots them in either the RGB or CIE color spaces.  \code{showRGBcalibration} uses package \code{rgl} to make a 3D interactive plot.  \code{\link{showCIE}} will display the CIE chromaticity diagram.  If sample colors are provided, the position of the samples relative to the calibration colors can be inspected to see if the sample colors are within the calibration region.  If sample colors are provided, the projection of the colors onto the principal curve is returned if using \code{showRGBcalibration} or the color purity is returned if using \code{\link{showCIE}}.
}
\usage{
showRGBcalibration(calCols, sampCols = NULL, title = NULL, ...)
showCIE(calCols, sampCols = NULL, title = NULL,
gradient = NULL, opts = "sRGB", ...)
}
\arguments{
  \item{calCols}{
An list with elements \code{rgb} and \code{hexcol}.  See \code{\link{genManyCols}} for details.
}
  \item{sampCols}{
A data frame with columns \code{hex} which contains hexadecimal strings, and \code{id} which has the sample names.
}
  \item{title}{
A character string giving a title for the plot.
}
  \item{gradient}{
Character string passed to \code{\link{plotCIEchrom}}.
}
  \item{opts}{
Character vector passed to \code{\link{plotCIEchrom}}.
}

  \item{...}{
Additional arguments to be passed to \code{\link{showCIE}}.  See the examples.
}

}
\value{
A plot is drawn using either \code{rgl} or \code{grid} graphics, and information about the calibration is returned in a data frame if \code{sampCols} is provided.  The data frame is \code{sampCols} to which has been appended the purity and apparent lambda max (appLmax)if \code{\link{showCIE}} was called.  If \code{lambdas} are provided, those results are appended to the data frame as well.  If \code{\link{showRGBcalibration}} was called it appends the value and residual.
}
\references{
\url{https://github.com/bryanhanson/photoSpec}
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
blues <- genManyCols(minHue = "2.5B", maxHue = "10B", plotPC = FALSE)
#
# And some sample colors for experimentation:
#
sc <- data.frame(hex = c("#FF0000", "#0000FF", "#98F5FF", "#6495ED"),
	id = c("red", "blue", "cadetblue1", "cornflower"))
#
# Obviously red isn't a shade of blue.  But good old blue isn't necessarily in the blue
# range either.  Along with the other two shades of blue, let's see what happens:
#
showRGBcalibration(calCols = blues, sampCols = sc, title = "Good Bad & Ugly")
showCIE(calCols = blues, sampCols = sc, title = "Good Bad & Ugly")
#
# What's going on with good old blue?
# Compare it with the nearest Munsell color.  Notice that
# good old blue is a PB (purple-blue) in the Munsell system.
# Compare these two color swatches:
#
plot_mnsl(rgb2mnsl(R = 0.0, G = 0.0, B = 1.0))
plot_hex("#0000FF")
#
# More typically, you have some samples and need to select calibration colors
# that are similar to your expected sample colors.  Imagine your color sample
# was cornflower, let's get new calibration colors that are more appropriate:
#
purblues <- genManyCols(minHue = "2.5PB", maxHue = "10PB",
	minChroma = 2, maxChroma = 12, plotPC = FALSE)
#
showRGBcalibration(calCols = purblues, sampCols = sc[4,], title = "Good Fit")
showCIE(calCols = purblues, sampCols = sc[4,], title = "Good Fit")
#
# Not too bad of a fit!
#
# The apparent lambda max is around 575 nm.  Let's pass that to the function
# along with a nearby value.
showCIE(calCols = purblues, sampCols = sc[4,], title = "Good Fit", lambdas = c(575, 550), plotPts = TRUE, plotLambdas = TRUE)
#
}
\keyword{ multivariate }
\keyword{ plot }
