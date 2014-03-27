\name{genColsUsingLambdas}
\alias{genColsUsingLambdas}
\title{
Generate calibration colors from a region of the CIE chromaticity diagram complementary to input wavelengths
}
\description{
Select a series of CIE xy values in the wedge defined by the complement of \code{L1} and \code{L2}.
}
\usage{
genColsUsingLambdas(L1 = NULL, L2 = NULL, colSpace = "sRGB", ex = 1.0, res = 0.02,
	plotPC = TRUE, showRGB = FALSE, showCIE = TRUE, ...)
}
\arguments{
  \item{L1}{
Numeric, giving the lower wavelength at the edge of the region of interest.
}
  \item{L2}{
Numeric, giving the upper wavelength at the edge of the region of interest.
}
  \item{colSpace}{
Character string giving the color space to use for drawing the gradient.  One of \code{c("sRGB", "Apple RGB")}.  \code{Apple RGB} is mainly of historical interest; no physical devices use it at this time.
}
  \item{ex}{
Numeric.  The 'exposure' to use.  The exposure must be used with \strong{extreme care}.  Larger values of \code{exposure} make the white point whiter in the plot, and lightens colors near the spectral locus (driving some off the plot!).  The purpose is to alter the aesthetics of the plot - that is, to make the white "whiter" so that it looks "right".  The effect of exposure will vary with the display device.
}
  \item{res}{
Numeric, giving the resolution of the grid to be laid over the wedge of color created by the selection.  Points on the grid become Munsell colors.
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

  \item{\ldots}{
Arguments to be passed to \code{\link{plotCIEchrom}}, typically one of the \code{opts}.
}

}
\value{
A list of 3 (RGB values are in [0...1]):

\tabular{llll}{
\emph{element} \tab \emph{type} \tab \emph{description} \tab \cr
$Munsell \tab char \tab A vector with the Munsell color designation. \tab \cr
$rgb \tab data frame \tab Data frame containing the RGB coordinates of the chosen colors. \tab \cr
\tab $r  \tab num: Values for red channel. \cr
\tab $g  \tab num: Values for green channel. \cr
\tab $b  \tab num: Values for blue channel. \cr
$hexcol \tab char \tab A vector giving the chosen colors as hexadecimals. \tab \cr

%A list as follows:
%
%\tabular{lll}{
%\emph{element} \tab \emph{type} \tab \emph{description}\cr
%$wavelength \tab num \tab A vector giving the two input wavelengths. \cr
%$case \tab chr \tab The manner in which the L1/L2 -> D65 vector hits the spectral locus. \cr
%$verts  \tab data.frame \tab x & y wedge vertices. \cr
%$raster \tab raster \tab A raster object giving the gradient \cr
%$colSpace \tab chr \tab Character string giving the color space in use. \cr
%$ex \tab num \tab The exposure value \cr
%$p4 \tab num \tab The point where L1 -> D65 intersects the spectral locus. \cr
%$p5 \tab num \tab The point where L2 -> D65 intersects the spectral locus. \cr
%$p6 \tab num \tab The point where (L2 - L1) -> D65 intersects the spectral locus. \cr
%$xPt \tab num \tab The point in the wedge farthest from D65. \cr
%}
%Some of these quantities date to early versions of the package and are not currently used, but are kept here as %they may be useful later.
}
}
\references{
\url{https://github.com/bryanhanson/photoSpec}
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\examples{
# Basic demo
demo <- genColsUsingLambdas(L1 = 575, L2 = 600, colSpace = "sRGB", ex = 1.0, res = 0.02,
	plotPC = TRUE, showRGB = FALSE, showCIE = TRUE)

}
\keyword{ plot }
