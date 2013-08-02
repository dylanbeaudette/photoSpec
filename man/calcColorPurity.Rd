\name{calcColorPurity}
\alias{calcColorPurity}
\title{
Calculate the purity of a color
}
\description{
This function takes a data frame of sample colors in hexadecimal code, converts them into the CIE color space, and uses the CIE xy values to compute the color purity.  Color purity is the distance from the white point D65 to the color, divided by the distance from D65 to either sRGB gamut or the spectral locus along the line extended from D65 through the color (expressed as a percentage).
}
\usage{
calcColorPurity(sampCol = NULL, gamut = "sRGB", plotPts = TRUE, ...)
}
\arguments{
  \item{sampCol}{
A data frame with columns \code{hex} which contains hexadecimal strings, and \code{id} which has the sample names.
}
  \item{gamut}{
Character.  One of \code{c("sRGB", "sl")}.  \code{"sl"} refers to the spectral locus.
}
  \item{plotPts}{
Logical.  Shall the colors be plotted on the CIE diagram?
}
  \item{\dots}{
Arguments to be passed downstream.  For instance, \code{gradient = getGamutValues("sRGB")} will draw a gradient inside the sRGB gamut.
}
}
\details{
The brightness component of the CIE system is discarded.  Color purity using sRGB is larger than when using the spectral locus due to their relative sizes.  However, if you are working with colors that are intrinsically limited to sRGB, calculating the purity based upon the sRGB gamut likely provides a more reasonable answer.
}
\value{
A plot is made, and the results returned as a modifed \code{sampCols}: a new column called \code{purity} is added.
}
\references{
NEED A REFERENCE
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
# standard colors on the sRGB gamut including corners
hex <- c("#FF0000", "#FF00FF", "#0000FF", "#00FFFF", "#00FF00", "#FFFF00")
id <- c("red", "magenta", "blue", "cyan", "green", "yellow")
sc1 <- data.frame(cols = hex, id = id)

# colors in the interior
hex <- c("#7FFFD4", "#CD4F39", "#6495ED", "#E9967A", "#FF69B4", "#ADFF2F", "#BF3EFF")
id <- c("aquamarine", "tomato3", "cornflowerblue", "darksalmon", "hotpink", "greenyellow", "darkorchid1")
sc2 <- data.frame(cols = hex, id = id)

tst1 <- calcColorPurity(sampCol = sc1)
tst2 <- calcColorPurity(sampCol = sc2)
# Drawing the gradient takes a bit of time
\dontrun{
tst3 <- calcColorPurity(sampCol = sc2, gradient = getGamutValues("sRGB"))
}
}

\keyword{ utilities }
