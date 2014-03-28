\name{useArbCols}
\alias{useArbCols}
\title{
Convert user supplied colors into a calCols object
}
\description{
This function allows the user to supply an arbitrary selection of colors in several formats, and converts them into a calCols object.
}
\usage{
useArbCols(munCols = NULL, rgbCols = NULL, hexCols = NULL, namedCols = NULL, plotPC = TRUE, showRGB = FALSE, showCIE = FALSE, ...)
}
\arguments{
  \item{munCols}{
Character. A vector of Munsell colors.
}
  \item{rgbCols}{
Matrix?  Need to fix this up.
}
  \item{hexCols}{
Character. A vector of hexadecimal colors.
}
  \item{namedCols}{
Character. A vector of named R colors.
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
\details{
If more than one of the first four arguments is supplied, the last will generate the return value.
}
\value{
A \code{\link{calCols}} object is returned.
}
\references{
\url{https://github.com/bryanhanson/photoSpec}
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\keyword{ utilities }
