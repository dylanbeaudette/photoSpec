\name{prepCIEgradient}
\alias{prepCIEgradient}
\title{
Compute a gradient for display in a CIE chromaticity diagram
}
\description{
This function computes a gradient for display in a CIE chromaticity diagram.
}
\usage{
prepCIEgradient(vertices, colSpace, ff)
}
\arguments{
  \item{vertices}{
A data frame containing the x & y coordinates of the vertices.
}
  \item{colSpace}{
Character string giving the color space to use for drawing the gradient.  One of \code{c("sRGB", "Apple RGB")}.  \code{Apple RGB} is mainly of historical interest; no physical devices use it at this time.
}
  \item{ff}{
The fudge factor to use
}
}
\value{
A raster object as an x,y matrix of colors given in hexadecimal.  NEED TO CHECK
}
\references{
NEED A REFERENCE
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\keyword{ utilities }
