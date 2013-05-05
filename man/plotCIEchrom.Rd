\name{plotCIEchrom}
\alias{plotCIEchrom}
\title{
Draw the 1931 CIE chromaticity diagram
}
\description{
This function draws the 1931 CIE chromaticity diagram with various decorations and annotations.
}
\usage{
plotCIEchrom(gradient = NULL, colSpace = "sRGB", ff = 1.0,
	opts = c("D65", "specLocus", "purples"), ...)

}
\arguments{
  \item{gradient}{
Character: either \code{"sl"}, \code{NULL}, or a data frame with columns x and y.  If \code{NULL}, no gradient is drawn.  If \code{"sl"} a gradient filling the entire spectral locus is drawn.  If a data frame, the vertices specify a polygon to be filled with the gradient.  The argument \code{"colSpace"} determines the color space of the gradient to be drawn.
}
  \item{colSpace}{
Character string giving the color space to use for drawing the gradient.  One of \code{c("sRGB", "Apple RGB")}.
}
  \item{ff}{
Numeric.  The 'fudge factor' to use.  NEED TO EXPLAIN AND SHOW IN EXAMPLES.
}
  \item{opts}{
A character vector of options to be employed.  One or more of \code{c("D65", "D50", "C", "E", "specLocus", "purples", "sRGB", "SWOP", "Apple", "NTSC", "Adobe", "CIE")}.  The first few of these are reference white points.  \code{"specLocus"} and \code{"purples"} cause the spectral locus and line of purples to be labeled.  The last few options cause the requested gamut to be outlined AND FILLED?
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
The appearance of the color gradient will vary with the device, surface and incident light used to view it and is not likely correct anywhere.
}

\examples{
plotCIEchrom()
# These are a little slow for CRAN checks
\dontrun{
plotCIEchrom(gradient = "sl")
verts <- data.frame(x = c(0.2, 0.72, 0.33),
	y = c(0.04, 0.29, 0.33))
plotCIEselection(verts)
}
}
\keyword{ plot }
