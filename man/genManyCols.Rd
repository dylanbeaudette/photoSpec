\name{genManyCols}
\alias{genManyCols}
\title{
Generate a set of calibration colors from a Munsell specification
}
\description{
This function generates a set of calibration colors from Munsell specifications.  It is the starting point for the use of \code{photoSpec}.
}
\usage{
genManyCols(minHue = "2.5R", maxHue = "10R",
minVal = 1, maxVal = 9, minChroma = 2, maxChroma = 26,
plotPC = TRUE, showRGB = FALSE, showCIE = FALSE, ...)
}
\arguments{
  \item{minHue}{
Character.  The minimum Munsell hue.
}
  \item{maxHue}{
Character.  The maximum Munsell hue.
}
  \item{minVal}{
Integer.  The minimum Munsell value, should be >= 1.
}
  \item{maxVal}{
Integer.  The maximum Munsell value, should be <= 9.
}
  \item{minChroma}{
Integer.  The minimum Munsell chroma.
}
  \item{maxChroma}{
Integer.  The maximum Munsell chroma.  The default, 26, is the highest chroma of any hue that is in gamut, that is, can be represented in the sRGB colorspace (chroma goes as high as 38, which people can distinguish from nearby colors, but computers cannot display these colors).
}
  \item{plotPC}{
Logical. Shall a 2D plot of the generated colors be drawn as paint chips?
}
  \item{showRGB}{
Logical. Shall the calibration color space be shown in 3D using \code{rgl}?
}
  \item{showCIE}{
Logical. Shall the calibration color space be shown in a CIE chromaticity diagram?
}

  \item{\dots}{
Other parameters to be passed to downstream functions.
}
}
\value{
A list of 3 (RGB values are in [0...1]):

\tabular{llll}{
\emph{element} \tab \emph{type} \tab \emph{description} \tab \cr
$hexcol \tab char \tab A vector giving the chosen colors as hexadecimals. \tab \cr
$rgb \tab data frame \tab Data frame containing the RGB coordinates of the chosen colors. \tab \cr
\tab $r  \tab num: Values for red channel. \cr
\tab $g  \tab num: Values for green channel. \cr
\tab $b  \tab num: Values for blue channel. \cr
$Munsell \tab char \tab A vector with the Munsell color designation.
}
}
\details{
If you don't use the maximum range for values ([1...9]) the calibration curve in RGB space (using \code{\link{calcSampleValue}} via \code{\link{showRGBcalibration}}) may have an exaggerated hump in it.  This is not recommended as the validity of the results is unknown.  The Munsell colors returned are in effect 'rounded' as the Munsell system is quantized rather than continuous (i.e. there is less resolution in the Munsell system than in the CIE or RGB color spaces, and the nearest Munsell color is returned).
}
\section{Warning}{The Munsell hues are stored internally in order 5R -> 10RP, but conceptually the Munsell system wraps around in a circle.  This affects how minHue and maxHue are processed, but we believe that any range of hues returns the correct value.  Check it anyway!
}

\references{
\url{https://github.com/bryanhanson/photoSpec}
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
require("munsell")
require("rgl")
# Let's play with greens
#
# The center greens (note that the 3D view is a surface):
#
green1 <- genManyCols(minHue = "5G", maxHue = "5G", showRGB = TRUE)
#
# All possible greens; 2D paint chip view is too busy so
# turn that off:
#
green2 <- genManyCols(minHue = "2.5G", maxHue = "10G", plotPC = FALSE, showRGB = TRUE)
#
# All possible reds.  Note how the shape of all reds is thicker
# than all possible greens (the Munsell solid is asymmetric).
# Also show the CIE chromaticity diagram.
#
red1 <- genManyCols(minHue = "2.5R", maxHue = "10R",
plotPC = FALSE, showRGB = TRUE, showCIE = TRUE,
title = "All Possible Munsell Reds")
}
\keyword{ utilities }
