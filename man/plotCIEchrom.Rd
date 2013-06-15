\name{plotCIEchrom}
\alias{plotCIEchrom}
\title{
Draw the 1931 CIE chromaticity diagram
}
\description{
This function draws the 1931 CIE chromaticity diagram with various decorations and annotations.  This is useful for understanding the relevant color concepts.  
}
\usage{
plotCIEchrom(gradient = NULL, colSpace = "sRGB", ff = 1.0,
	opts = c("D65", "specLocus", "purples"), title = NULL,  ...)

}
\arguments{
  \item{gradient}{
Character: either \code{"sl"}, \code{NULL}, or a data frame with columns x and y.  If \code{NULL}, no gradient is drawn.  If \code{"sl"} a gradient filling the entire spectral locus is drawn.  If a data frame, the vertices specify a polygon to be filled with the gradient.  The argument \code{"colSpace"} determines the color space of the gradient to be drawn.
}
  \item{colSpace}{
Character string giving the color space to use for drawing the gradient.  One of \code{c("sRGB", "Apple RGB")}.  \code{Apple RGB} is mainly of historical interest; no physical devices use it at this time.
}
  \item{ff}{
Numeric.  The 'fudge factor' to use.  The fudge factor must be used with \strong{extreme care}.  Larger values of \code{ff} make the white point whiter in the plot, and lightens colors near the spectral locus.  One purpose of \code{ff} is to alter the aesthetics of the plot - that is, to make the white "whiter" for the visual effect (perhaps in teaching CIE concepts).  It must be used with care and competence!
}
  \item{opts}{
A character vector of options to be employed.  One or more of c("D65", "D50", "C", "E", "specLocus", "purples", "sRGB", "SWOP", "Apple", "NTSC", "Adobe", "CIE").  The first few of these are reference white points.  \code{"specLocus"} and \code{"purples"} cause the spectral locus and line of purples to be labeled.  The last few options cause the requested gamut to be outlined AND FILLED?
}
  \item{title}{
A character string to be plotted at the top of the diagram.  If NULL, the title defaults to "1931 CIE Chromaticity Diagram".  If no title is desired, set it to an empty string.
}
  \item{\dots}{
Additional arguments to be passed downstream, to \code{grid} functions.
}
}
\value{
A plot is drawn (using \code{grid} graphics).
}
\references{
NEED A REFERENCE
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\section{Warning }{
The appearance of the color gradient will vary with the device, surface and incident light used to view it and is not likely correct anywhere.  \strong{It also varies with ff}.
}

\examples{
plotCIEchrom() # no gradient
# These are a little slow for CRAN checks:
\dontrun{
plotCIEchrom(gradient = "sl") # basic plot
# This next one shows a typical gamut for a CMYK printing process
plotCIEchrom(gradient = getGamutValues("SWOP"), opts = c("D65", "SWOP"))
}
}
\keyword{ plot }
