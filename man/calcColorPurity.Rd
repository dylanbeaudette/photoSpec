\name{calcColorPurity}
\alias{calcColorPurity}
\title{
Calculate the purity of a color
}
\description{
This function takes a data frame of sample colors in hexadecimal code, converts them into the CIE color space, and uses the CIE xy values to compute their color purity.  Color purity is the distance from the white point D65 to the color, divided by the distance from D65 to either the sRGB gamut or the spectral locus, along the line extended from D65 through the color (expressed as a percentage).  Called by \code{\link{showCIE}} and generally not the user.  Optionally, a vector of wavelengths can be supplied, and the projection (color purity) of each color onto that wavelength is returned. NEEDS A PICTURE OF THE COLOR PURITY CONCEPT.
}
\usage{
calcColorPurity(sampCols = NULL, gamut = "sRGB", lambdas = NULL,
	plotPts = TRUE, plotLambdas = FALSE, ...)
}
\arguments{
  \item{sampCols}{
A data frame with columns \code{hex} which contains hexadecimal strings, and \code{id} which has the sample names.
}
  \item{gamut}{
Character.  One of \code{c("sRGB", "sl")}.  \code{"sl"} refers to the spectral locus.
}
  \item{lambdas}{
Numeric.  A vector of wavelengths.  Each sample color will be projected onto each wavelength, and the color purity at that wavelength computed. 
}

  \item{plotPts}{
Logical.  Shall the sample colors be plotted on the CIE diagram?
}
  \item{plotLambdas}{
Logical.  If \code{TRUE}, details of the projection of the colors onto the lambdas are drawn with a red X along with the a reference line passing through each lambda and D65.
}
  \item{\dots}{
Arguments to be passed downstream.  For instance, \code{gradient = getGamutValues("sRGB")} will draw a gradient inside the sRGB gamut.
}
}
\details{
The brightness component of the CIE system is discarded.  Color purity using sRGB is larger than that for the spectral locus due to their relative sizes.  However, if you are working with colors that are intrinsically limited to sRGB, calculating the purity based upon the sRGB gamut provides a more reasonable answer.
}
\section{Warning}{
The accuracy of the values returned by this function depends entirely on the accuracy of the supplied sample colors.  There is no calibration performed in this function, it merely takes the supplied sample colors and expresses them in different terms.
}

\section{Another Warning}{
The color purity calculated depends upon the gamut supplied, see 'Details'.
}

\value{
The supplied data frame \code{sampColss} is returned with two new columns: \code{purity} and \code{appLmax}.  The latter gives the apparent lambda max (dominant wavelength) of the color, obtained by extrapolating from the color back to D65 and then to the spectral locus, where the wavelength is obtained.  If \code{!is.null(lambdas)} additional columns are provided giving the color purity (projection) of each sample at each wavelength.  If the projection onto the reference line is outside the specified gamut, \code{NA} is returned.

If a CIE diagram has already been drawn (as it would if this function were called by \code{\link{showCIE}}) AND  \code{plotLambdas = TRUE}, a reference line is drawn from each lambda through D65, and an red X indicates where each sample projects onto the reference line.
}
\seealso{
See the examples in \code{\link{showCIE}} which is the user-facing function.
}

\references{
\url{https://github.com/bryanhanson/photoSpec}
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
# These examples serve mainly to verify that the code works.
# See ?showCIE for examples that create plots.
#
# standard colors on the sRGB gamut including corners
hex <- c("#FF0000", "#FF00FF", "#0000FF", "#00FFFF", "#00FF00", "#FFFF00")
id <- c("red", "magenta", "blue", "cyan", "green", "yellow")
sc1 <- data.frame(hex = hex, id = id)

# colors in the interior
hex <- c("#7FFFD4", "#CD4F39", "#6495ED", "#E9967A", "#FF69B4", "#ADFF2F", "#BF3EFF")
id <- c("aquamarine", "tomato3", "cornflowerblue", "darksalmon", "hotpink", "greenyellow", "darkorchid1")
sc2 <- data.frame(hex = hex, id = id)

tst1 <- calcColorPurity(sampCols = sc1, plotPts = FALSE, lambdas = c(500, 610))
# NOTE SINGULARITY IF LAMBDA = 550 IS USE WITH sc1.  549 OR 551 WORK FINE.
tst2 <- calcColorPurity(sampCols = sc2, plotPts = FALSE)
}

\keyword{ utilities }
