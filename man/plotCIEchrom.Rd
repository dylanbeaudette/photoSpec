\name{plotCIEchrom}
\alias{plotCIEchrom}
\alias{plotCIEselection}
\title{
Draw the 1931 CIE chromaticity diagram
}
\description{
These functions draw the 1931 CIE chromaticity diagram with various decorations and annotations.
}
\usage{
plotCIEchrom(gradient = NULL, colSpace = "sRGB", ff = 1,
	opts = c("D65", "specLocus", "purples"), ...)
plotCIEselection(vertices, ff = 1, ...)

}
\arguments{
  \item{gradient}{
Logical: shall a gradient be drawn?
}
  \item{colSpace}{
Character string giving the color space to use for ????
}
  \item{ff}{
Numeric.  The 'fudge factor' to use.  NEED TO EXPLAIN AND SHOW IN EXAMPLES.
}
  \item{opts}{
A character vector of options to be employed.  One or more of \code{c()}.
}
  \item{vertices}{
A data frame containing the vertices of the region to be outlined and filled in with a gradient.
}
  \item{\dots}{
Additional arguments to be passed downstream, to grid functions.
}
}
\value{
A plot is drawn (using grid graphics).
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
# NEED AN EXAMPLE
}
\keyword{ plot }
