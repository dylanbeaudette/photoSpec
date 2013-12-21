\name{gen5cols}
\alias{gen5cols}
\title{
Generate 5 colors for use with plotMiniCard
}
\description{
This function generates a set of 5 colors for use with \code{plotMiniCard}.
}
\usage{
gen5cols(cols = c("yellow", "orange", "red"),
plotPC = TRUE, showRGB = FALSE, showCIE = FALSE, ...)
}
\arguments{
  \item{cols}{
A character vector of color names.  These will be passed to \code{\link{colorRampPalette}} where they will be interpolated.  See there for details.
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
\references{
\url{https://github.com/bryanhanson/photoSpec}
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\details{
The Munsell colors returned are in effect 'rounded' as the Munsell system is quantized rather than continuous (i.e. there is less resolution in the Munsell system than in the CIE or RGB color spaces, and the nearest Munsell color is returned).
}

\examples{
gen5cols() # defaults give yellow-orange-red swatches
}
\keyword{ utilities }
