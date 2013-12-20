\name{plotLargeCard}
\alias{plotLargeCard}
\alias{plotMiniCard}
\title{
Plot a sample card containing calibration colors (paint chips)
}
\description{
These functions plot a 'card' containing a series of calibration colors (paint chips or color swatches) and a simple grid calibrated in centimeters.  A sample is placed on the card and photographed.  The photo is then analyzed.
}
\usage{
plotLargeCard(calCols = NULL, size = c(6, 4), ruler = c(3.5, 2.5),
chip.order = "pale2dark", chip.rep = 1, title = "no title", guide = "none")
plotMiniCard(calCols = NULL, title = "no title", guide = "none")
}
\arguments{
  \item{calCols}{
An list with elements \code{rgb} and \code{hexcol}.  See \code{\link{genManyCols}} (for \code{plotLargeCard} and \code{\link{gen5cols}} (when using \code{plotMiniCard}) for details.
}
  \item{size}{
(\code{plotLargeCard} only) The \code{c(width, height)} of the printed card in inches.
}
  \item{ruler}{
(\code{plotLargeCard} only) The \code{c(width, height)} of the sample scale to be drawn, in cm.
}
  \item{chip.order}{
(\code{plotLargeCard} only) String.  Determines how the colors will be positioned on the card.  One of \code{c("random", "munsell", "pale2dark", "fill")}.  See the examples.
}
  \item{chip.rep}{
(\code{plotLargeCard} only) Numeric; the number of replicates of \code{calCols} to use.  If \code{chip.rep >= 1} it is taken as an integer and the calibration colors are replicated accordingly (this gives copies of the same color which can be useful in a possibly unevenly lit photograph). If \code{chip.rep < 1} \code{chip.rep} must be in \code{c(0.5, 0.33, 0.25, 0.2)} in which case 1/2, 1/3, 1/4 or 1/5 respectively of the calibration colors will be used.
}
  \item{title}{
String.  A title for the card.
}
  \item{guide}{
Character.  If \code{hex}, outline the card colors and give the hexadecimal codes for each.  If \code{Munsell}, use the Munsell nomenclature.  Note that with the Munsell system some rounding occurs.
}
  \item{...}{
Arguments to be passed downstream, likely to \code{grid} drawing functions.
}
}
\details{
For \code{plotLargeCard}, pure black, 18 percent gray and pure white are added to \code{calCols} every 25 colors to serve as references when analyzing.  These are always outlined to identify them.  If there is a color cast or shadow on the photo, you can choose the reference swatches that are least affected. \code{plotMiniCard} plots 5 colors specified by the user along with a white to black gradient.
}
\section{Warning}{
Be sure when printing that scaling is set to 100 percent or the scale will not be accurate.  Low ink supplies could lead to poor color reproduction.
}

\value{
None.  A plot is drawn using \code{grid} graphics.  This should be saved as a pdf or jpeg and printed for use.  Note that a serial number is added to each plot so that the card can be uniquely identified.
}
\references{
\url{https://github.com/bryanhanson/photoSpec}
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\examples{
require("grid")
### plotLargeCard Examples
# Generate some colors:
#
green1 <- genManyCols(minHue = "5G", maxHue = "5G", plotPC = FALSE)
#
# Plot the card:
#
greenCard <- plotLargeCard(calCols = green1, size = c(6, 4), ruler = c(3.5, 2.5),
chip.order = "pale2dark", chip.rep = 3, guide = "hex",
title = "Middle Greens")
#
# A larger set of red colors, so use every 2nd color.
# Also, illustrate labeling with the Munsell notation.
#
red1 <- genManyCols(minHue = "2.5R", maxHue = "10R", plotPC = FALSE)
#
redCard <- plotLargeCard(calCols = red1, size = c(6, 4), ruler = c(3.5, 2.5),
chip.order = "munsell", chip.rep = 0.5, guide = "Munsell", title = "Half of All the Munsell Reds")

### plotMiniCard Examples
# Generate 5 colors:
yor <- gen5cols(plotPC = FALSE)
plotMiniCard(calCols = yor, guide = "Munsell")
}
\keyword{ plot }
