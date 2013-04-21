\name{CIExyz}
\alias{CIExyz}
\docType{data}
\title{
Spectral Locus for the 1931 CIE chromaticity diagram
}
\description{
This data set gives wavelengths every 0.1 nm, along with the associated CIE xyz values for the spectral locus of the 1931 CIE chromaticity diagram.
}
\usage{data(CIExyz)}
\format{
  A data frame with 4400 observations on the following 4 variables.
  \describe{
    \item{\code{wavelength}}{a numeric vector; units are nanometers (nm)}
    \item{\code{x}}{a numeric vector}
    \item{\code{y}}{a numeric vector}
    \item{\code{z}}{a numeric vector}
  }
}
\source{Color Vision Research Lab.  http://cvrl.ioo.ucl.ac.uk/index.htm  Go to this URL, then choose 'NEW CIE XYZ...' In the new page that opens, go to 'Chromaticity coordinates' and get the 2-deg coordinates at 0.1 nm resolution
}
\references{
NEED REFERENCE
}
\examples{
data(CIExyz)
str(CIExyz)
}
\keyword{ datasets }
