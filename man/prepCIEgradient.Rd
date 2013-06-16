\name{prepCIEgradient}
\alias{prepCIEgradient}
\title{
Compute a gradient for display in a CIE chromaticity diagram
}
\description{
This function computes a gradient for display in a CIE chromaticity diagram.
}
\usage{
prepCIEgradient(vertices, colSpace, ex, ...)
}
\arguments{
  \item{vertices}{
A data frame containing the x & y coordinates of the vertices.
}
  \item{colSpace}{
Character string giving the color space to use for drawing the gradient.  One of \code{c("sRGB", "Apple RGB")}.  \code{Apple RGB} is mainly of historical interest; no physical devices use it at this time.
}
  \item{ex}{
Numeric.  The 'exposure' to use.  The exposure must be used with \strong{extreme care}.  Larger values of \code{exposure} make the white point whiter in the plot, and lightens colors near the spectral locus.  The purpose is to alter the aesthetics of the plot - that is, to make the white "whiter" for the visual effect (perhaps in teaching CIE concepts).  It must be used with care and competence!
}
  \item{...}{
Arguments to be passed to ???
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
