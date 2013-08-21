\name{getGamutValues}
\alias{getGamutValues}
\alias{getWhiteValues}
\title{
Look up gamut and white point values in the 1931 CIE system
}
\description{
These functions provide a simple way of storing white point and gamut data for use in drawing CIE chromaticity diagrams.  Not intended for users; called by other user-facing functions.
}
\usage{
getGamutValues(gamut)
getWhiteValues(white)
}
\arguments{
  \item{gamut}{
The desired gamut values.  One of \code{c("Apple", "CIE", "Adobe", "sRGB", "NTSC", "SWOP")}.
}
  \item{white}{
The desired white point values.  One of \code{c("D65", "E", "C", "D50")}.
}

}

\value{
A data frame with columns x, y containing the vertices of the requested gamut in CIE chromaticity coordinates, or, for a white point, a data frame containing the coordinates of the requested white point.
}
\references{
\url{https://github.com/bryanhanson/photoSpec}
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\keyword{ utilities }
