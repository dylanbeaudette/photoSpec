\name{CIEwedge}
\alias{CIEwedge}
\title{
The CIEwedge class/object
}
\description{
This class is used to share information among \code{photoSpec} functions.
}
\section{Structure}{
A \code{CIEwedge} object is a list of 12 as follows:

\tabular{lll}{
\emph{element} \tab \emph{type} \tab \emph{description}\cr
$wavelength \tab num \tab A vector giving the two input wavelengths. \cr
$Case \tab chr \tab The manner in which the L1/L2 -> D65 vector hits the spectral locus. \cr
$verts  \tab data.frame \tab x & y wedge vertices. \cr
$raster \tab raster \tab A raster object giving the gradient \cr
$colSpace \tab chr \tab Character string giving the color space in use. \cr
$ff \tab num \tab The fudge factor \cr
$p4 \tab num \tab The point where L1 -> D65 intersects the spectral locus. \cr
$p5 \tab num \tab The point where L2 -> D65 intersects the spectral locus. \cr
$p6 \tab num \tab The point where (L2 - L1) -> D65 intersects the spectral locus. \cr
$xPt \tab num \tab The point in the wedge farthest from D65. \cr
$bands \tab list of lists \tab The x & y vertices of each band. \cr
$calCols \tab chr \tab A vector of calibration colors as hexadecimals. \cr
}
}
\details{
No checking of class integrity is done at this time.  This class is currently very informal, there is no attribute slot naming this class at this time.  For details on the definitions of the various slots, see the relevant functions.  This object grows as it is passed through the various functions - not all slots are present at a particular step.
}
\references{
NEED REFERENCE
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\keyword{ classes }
