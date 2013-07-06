\name{plotSampleCard}
\alias{plotSampleCard}
\title{
Plot a sample card containing calibration colors (paint chips)
}
\description{
This function plots a 'card' containing a series of calibration colors (paint chips or color swatches) and a simple grid calibrated in centimeters.  A sample is placed on the grid and photographed.  The photo is then analyzed.
}
\usage{
plotSampleCard(calCols, size = c(6, 4), ruler = c(3.5, 2.5),
chip.order = "pale2dark", chip.rep = 1, title = "no title", guide = FALSE)
}
\arguments{
  \item{calCols}{
An list with elements \code{rgb} and \code{hexcol}.  See \code{\link{genCalCols}} for details.
}
  \item{size}{
The \code{c(width, height)} of the printed card in inches.
}
  \item{ruler}{
The \code{c(width, height)} of the sample scale to be drawn, in cm.
}
  \item{chip.order}{
String.  Determines how the colors will be positioned on the card.  One of \code{c("random", "munsell", "pale2dark", "fill")}.  See the examples.
}
  \item{chip.rep}{
Numeric; the number of replicates of \code{calCols} to use.  If \code{chip.rep >= 1} it is taken as an integer and the calibration colors are replicated accordingly (this gives copies of the same color which can be useful in a possibly unevenly lit photograph). If \code{chip.rep < 1} \code{chip.rep} must be in \code{c(0.5, 0.33, 0.25, 0.2)} and 1/2, 1/3, 1/4 or 1/5 respectively of the calibration colors will be used.
}
  \item{title}{
String.  A title for the card.
}
  \item{guide}{
Logical.  If TRUE, outline the card colors and give the hexadecimal codes for each.
}
  \item{...}{
Arguments to be passed downstream, likely to \code{grid} drawing functions.
}
}
\details{
Pure black, 18 percent gray and pure white are added to \code{calCols} to serve as references when analyzing.
}
\section{Warning}{
Be sure when printing that scaling is set to 100% or the scale will not be accurate.  Low ink supplies could lead to poor color reproduction.
}

\value{
None.  A plot is drawn using \code{grid} graphics.  This should be saved as a pdf or jpeg and printed for use.
}
\references{
NEED A REFERENCE
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\examples{
require("grid")
# Generate some calibration colors:
#
green1 <- genCalCols(minHue = "5G", maxHue = "5G",
	plotPC = FALSE, showCal = FALSE)
#
# Plot the card:
#
card1 <- plotSampleCard(calCols = green1, size = c(6, 4), ruler = c(3.5, 2.5),
chip.order = "pale2dark", chip.rep = 3, guide = FALSE)
#
# A larger set of red calibration colors, so use every 2nd color.
# Also, show the guides for fun.
#
red1 <- genCalCols(minHue = "2.5R", maxHue = "10R",
	plotPC = FALSE, showCal = FALSE)
#
card2 <- plotSampleCard(calCols = red1, size = c(6, 4), ruler = c(3.5, 2.5),
chip.order = "munsell", chip.rep = 0.5, guide = TRUE)

}
\keyword{ plot }
