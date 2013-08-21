\name{selectCIExy}
\alias{selectCIExy}
\title{
Select a region of the CIE chromaticity diagram complementary to input wavelengths.
}
\description{
Select a series of CIE xy values in the wedge defined by the complement of \code{L1} and \code{L2}.
}
\usage{
selectCIExy(L1, L2, colSpace = "sRGB", ex = 1.0, ...)
}
\arguments{
  \item{L1}{
Numeric, giving the lower wavelength at the edge of the region to be analyzed.
}
  \item{L2}{
Numeric, giving the upper wavelength at the edge of the region to be analyzed.
}
  \item{colSpace}{
Character string giving the color space to use for drawing the gradient.  One of \code{c("sRGB", "Apple RGB")}.  \code{Apple RGB} is mainly of historical interest; no physical devices use it at this time.
}
  \item{ex}{
Numeric.  The 'exposure' to use.  The exposure must be used with \strong{extreme care}.  Larger values of \code{exposure} make the white point whiter in the plot, and lightens colors near the spectral locus.  The purpose is to alter the aesthetics of the plot - that is, to make the white "whiter" for the visual effect (perhaps in teaching CIE concepts).  It must be used with care and competence!
}
  \item{\ldots}{
Arguments to be passed to \code{\link{plotCIEchrom}}, typically one of the \code{opts}.
}

}
\value{
A list as follows:

\tabular{lll}{
\emph{element} \tab \emph{type} \tab \emph{description}\cr
$wavelength \tab num \tab A vector giving the two input wavelengths. \cr
$case \tab chr \tab The manner in which the L1/L2 -> D65 vector hits the spectral locus. \cr
$verts  \tab data.frame \tab x & y wedge vertices. \cr
$raster \tab raster \tab A raster object giving the gradient \cr
$colSpace \tab chr \tab Character string giving the color space in use. \cr
$ex \tab num \tab The exposure value \cr
$p4 \tab num \tab The point where L1 -> D65 intersects the spectral locus. \cr
$p5 \tab num \tab The point where L2 -> D65 intersects the spectral locus. \cr
$p6 \tab num \tab The point where (L2 - L1) -> D65 intersects the spectral locus. \cr
$xPt \tab num \tab The point in the wedge farthest from D65. \cr
}
}
\references{
\url{https://github.com/bryanhanson/photoSpec}
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\keyword{ plot }
