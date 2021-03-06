\name{plotCIEchrom}
\alias{plotCIEchrom}
\title{
Draw the 1931 CIE chromaticity diagram
}
\description{
This function draws the 1931 CIE chromaticity diagram with various decorations and annotations.  
}
\usage{
plotCIEchrom(gradient = NULL, colSpace = "sRGB", ex = 1.0,
	opts = c("D65", "specLocus", "purples"), title = NULL,  ...)

}
\arguments{
  \item{gradient}{
Character: either \code{"sl"}, \code{NULL}, or a data frame with columns x and y.  If \code{NULL}, no gradient is drawn.  If \code{"sl"} a gradient filling the entire spectral locus is drawn.  If a data frame, the vertices should specify a polygon to be filled with the gradient (see the examples for convenient ways to specify the gradient).
}
  \item{colSpace}{
Character string giving the color space to use for drawing the gradient.  One of \code{c("sRGB", "Apple RGB")}.  \code{Apple RGB} is mainly of historical interest; no physical devices use it at this time.
}
  \item{ex}{
Numeric.  The 'exposure' to use.  The exposure must be used with \strong{extreme care}.  Larger values of \code{exposure} make the white point whiter in the plot, and lightens colors near the spectral locus (driving some off the plot!).  The purpose is to alter the aesthetics of the plot - that is, to make the white "whiter" so that it looks "right".  The effect of exposure will vary with the display device.
}
  \item{opts}{
A character vector of options to be employed.  One or more of c("D65", "D50", "C", "E", "specLocus", "purples", "Munsell", "sRGB", "SWOP", "Apple", "NTSC", "Adobe", "CIE").  The first few of these are reference white points.  \code{"specLocus"} and \code{"purples"} cause the spectral locus and line of purples to be labeled.  \code{"Munsell"} causes the approximate Munsell hues to be marked along the spectral locus at the appropriate wavelength.  The last few options cause the requested gamut to be outlined.
}
  \item{title}{
A character string to be plotted at the top of the diagram.  If NULL, the title defaults to "1931 CIE Chromaticity Diagram".  If no title is desired, set it to an empty string.
}
  \item{\dots}{
Additional arguments to be passed downstream, to \code{grid} functions.
}
}
\value{
A plot is drawn using \code{grid} graphics.
}
\references{
For \code{opts = "Munsell"} the Munsell designation by wavelength are taken from Romney & Indow \url{pnas.org/cgi/doi/10.1073/pnas.162368999}

\url{https://github.com/bryanhanson/photoSpec}
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\section{Warning }{
The appearance of the color gradient will vary with the device, surface and incident light used to view it and is not likely correct anywhere.  \strong{The appearance varies strongly with exposure}.
}

\examples{
plotCIEchrom() # no gradient
## These are a too slow for CRAN checks:
\dontrun{
plotCIEchrom(gradient = "sl") # basic plot
# Notice there is not much yellow in that plot.  Increase
# the exposure to bring in some yellow, at the expense of some blues:
plotCIEchrom(gradient = "sl", ex = 1.4)
# Next show a gradient for the CMYK printing process
# and outline the colors a typical monitor can display.
plotCIEchrom(gradient = getGamutValues("SWOP"), opts = c("D65", "SWOP", "sRGB"))
}
}
\keyword{ plot }
