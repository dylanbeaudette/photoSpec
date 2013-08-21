\name{CIExyz}
\alias{CIExyz}
\docType{data}
\title{
Spectral Locus for the 1931 CIE chromaticity diagram
}
\description{
This data set gives wavelengths every 0.1 nm, along with the associated CIE xyz values for the spectral locus of the 1931 CIE chromaticity diagram.  They are called xyz values here as they are called that in the original source, but they are also known as xyY or XYZ values (need to double check).
}
\usage{data(CIExyz)}
\format{
A data frame with 4400 observations each with the following 4 variables.\cr
$ wavelength: num  390 390 390 390 390 ...\cr
$ x         : num  0.166 0.166 0.166 0.166 0.166 ...\cr
$ y         : num  0.0183 0.0183 0.0184 0.0184 0.0184 ...\cr
$ z         : num  0.815 0.815 0.815 0.815 0.815 ...\cr
}
\source{Color Vision Research Lab.  \url{cvrl.ioo.ucl.ac.uk/index.htm}  Go to this URL, then choose 'NEW CIE XYZ...' In the new page that opens, go to 'New physiologically-relevant CIE x,y chromaticity coordinates (proposed)' and get the 2-deg coordinates at 0.1 nm resolution
}
\references{
\url{https://github.com/bryanhanson/photoSpec}
}
\examples{
data(CIExyz)
str(CIExyz)
}
\keyword{ datasets }
