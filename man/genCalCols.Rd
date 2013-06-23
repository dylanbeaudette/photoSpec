\name{genCalCols}
\alias{genCalCols}
\title{
Generate a set of calibration colors
}
\description{
This function generates a set of calibration colors.  It is the starting point for the use of \code{photoSpec}.
}
\usage{
genCalCols(minHue = "2.5R", maxHue = "10R",
	minVal = 1, maxVal = 9, minChroma = 2, maxChroma = 12,
	plotPC = FALSE, showCal = TRUE, ...)
}
\arguments{
  \item{minHue}{
Character.  The minimum Munsell hue.
}
  \item{maxHue}{
Character.  The maximum Munsell hue.
}
  \item{minVal}{
Integer.  The minimum Munsell value.
}
  \item{maxVal}{
Integer.  The maximum Munsell value.
}
  \item{minChroma}{
Integer.  The minimum Munsell chroma.
}
  \item{maxChroma}{
Integer.  The minimum Munsell chroma.
}
  \item{plotPC}{
Logical. Shall a plot of the generated colors be drawn as paint chips?
}
  \item{showCal}{
Logical. Shall the calibration color space be shown in 3D?
}

  \item{\dots}{
Other parameters to be passed to downstream functions.
}
}
\value{
A list of 2 (rgb values are in [0...1]):

\tabular{llll}{
\emph{element} \tab \emph{type} \tab \emph{description} \tab \cr
$hexcol \tab char \tab A vector giving the chosen colors as hexadecimals. \tab \cr
$rgb \tab data frame \tab Data frame containing the rgb coordinates of the chosen colors. \tab \cr
\tab $r  \tab num \tab Values for red channel. \cr
\tab $g  \tab num \tab Values for green channel. \cr
\tab $b  \tab num \tab Values for blue channel. \cr
}
}
\references{
NEED A REFERENCE
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
require("munsell")
require("rgl")
# Let's play with greens; both 2D and 3D views open
#
# The center greens (note that 3D view is a surface):
#
green1 <- genCalCols(minHue = "5G", maxHue = "5G",
	minVal = 1, maxVal = 9, minChroma = 2, maxChroma = 12,
	plotPC = TRUE, showCal = TRUE)
#
# A wider range of greens; but note that by narrowing the
# value range, the calibration curve is not smooth/usable.
# In this case the 3D view is a solid:
#
green2 <- genCalCols(minHue = "2.5G", maxHue = "10G",
	minVal = 3, maxVal = 7, minChroma = 2, maxChroma = 12,
	plotPC = TRUE, showCal = TRUE)
#
# All possible greens; 2D paint chip view is too busy so
# turn that off:
#
green3 <- genCalCols(minHue = "2.5G", maxHue = "10G",
	minVal = 1, maxVal = 10, minChroma = 2, maxChroma = 12,
	plotPC = FALSE, showCal = TRUE)
#
# All possible reds.  Note how the shape of all reds is thicker
# than all possible greens (the Munsell solid is asymmetric).
#
red1 <- genCalCols(minHue = "2.5R", maxHue = "10R",
	minVal = 1, maxVal = 10, minChroma = 2, maxChroma = 12,
	plotPC = FALSE, showCal = TRUE)
#
# All possible blues. Calibration curve has a noticable hump, 
# again a consequence of the asymmetric nature of the Munsell solid.
#
blue1 <- genCalCols(minHue = "2.5B", maxHue = "10B",
	minVal = 1, maxVal = 10, minChroma = 2, maxChroma = 12,
	plotPC = FALSE, showCal = TRUE)
#
# Note on all of these how the orientation of the color wedges
# at each level of value varies.
}
\keyword{ plot }
