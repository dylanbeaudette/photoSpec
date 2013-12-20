\name{calcSampleValue}
\alias{calcSampleValue}
\title{
Project sample colors onto the principal curve formed by the calibration colors in RGB colorspace
}
\description{
Using a set of calibration colors and a set of sample colors, this function maps the sample colors onto the principal curve created from the calibration colors plus pure white and pure black in the RGB colorspace.  This number approximates the Munsell value, and is somewhat analogous to a grayscale value.  Called by \code{showRGBcalibration} and not generally by the user.
}
\usage{
calcSampleValue(calCols, sampCols = NULL)
}
\arguments{
  \item{calCols}{
An list with elements \code{rgb} and \code{hexcol}. See \code{\link{genManyCols}} for details.
}

  \item{sampCols}{
A data frame with columns \code{hex} which contains hexadecimal strings, and \code{id} which has the sample names.  May be missing, in which case only the principal curve is returned.
}
}

\details{
The principal curve through the calibration colors in RGB space is computed after adding pure white (1, 1, 1) and pure black (0, 0, 0) to the data supplied (the process is analogous to finding a principal component).  The sample color is then projected onto this curve.  Its position along the curve as a percentage of the total length is reported, along with the residual distance (the distance from the principal curve).  This position approximates the Munsell value and is analogous to a grayscale value.
}
\value{
A list with two top level elements: \code{calCols} is returned with two additional columns: \code{value} and \code{residual}, containing the interpolated sample values and the distance of the samples from the principal curve. The second element is \code{pcfit} which contains the results of fitting the principal curve.
}
\section{Warning}{
If \code{calCols} was not created using the full range of Munsell values (i.e., 1:9), then the principal curve may have a distinct hump in it (the size of the hump varies with the hues in use).  This should be avoided, as the validity of the results in these cases has not been explored.

Results should always be visually inspected, as reasonable values may be returned even for colors which are completely outside the wedge of calibration colors.  Do not call this function directly, but rather \code{\link{showRGBcalibration}}, the user facing function which also shows you the results visually.
}
\references{
\url{https://github.com/bryanhanson/photoSpec}
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\seealso{
See \code{\link{showRGBcalibration}}, the user facing function, for examples.
}
\examples{
# These examples serve mainly to verify that the code works.
# See ?showRGBcalibration for examples that create plots.
blues <- genManyCols(minHue = "2.5B", maxHue = "10B", plotPC = FALSE)
sc <- data.frame(hex = c("#FF0000", "#0000FF", "#98F5FF", "#6495ED"),
	id = c("red", "blue", "cadetblue1", "cornflower"))
tst <- calcSampleValue(calCols = blues, sampCols = sc)
tst$sampCols
}
\keyword{ utilities }
