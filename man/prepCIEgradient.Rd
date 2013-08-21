\name{prepCIEgradient}
\alias{prepCIEgradient}
\title{
Compute a gradient for display in a CIE chromaticity diagram
}
\description{
This function computes a gradient for display in a CIE chromaticity diagram. Not generally called by the user.
}
\usage{
prepCIEgradient(vertices, colSpace, ex, ...)
}
\arguments{
  \item{vertices}{
A data frame containing the x and y coordinates of the vertices.
}
  \item{colSpace}{
Character string giving the color space to use for drawing the gradient.  One of \code{c("sRGB", "Apple RGB")}.  \code{Apple RGB} is mainly of historical interest; no physical devices use it at this time.
}
  \item{ex}{
Numeric.  The 'exposure' to use.  The exposure must be used with \strong{extreme care}.  Larger values of \code{ex} make the white point whiter in the plot, and lightens colors near the spectral locus.  The purpose is to alter the aesthetics of the plot - that is, to make the white "whiter" for the visual effect.  It must be used with care!
}
  \item{...}{
Arguments to be passed downstream.
}
}
\value{
A raster object composed of 3 planes, one each for the red, green and blue channels.  The dimensions are those of \code{vertices}.
}
\references{
\url{https://github.com/bryanhanson/photoSpec}
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\keyword{ utilities }
