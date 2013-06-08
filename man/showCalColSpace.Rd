\name{showCalColSpace}
\alias{showCalColSpace}
\title{
Use rgl to draw a plot of calibration colors in the rgb space.
}
\description{
This function takes a vector of colors used for calibration and plots them in the rgb color space using package \code{rgl}.  A sample color is also plotted, along with an optional ellipsoid showing the 95 percent confidence region around the calibration colors.  The position of the sample relative to the calibration colors and the ellipsoid can be inspected to determine if the sample is sufficiently close to the calibration colors.  In addition, some measures of the fit of the sample color to the calibration colors are printed at the console.
}
\usage{
showCalColSpace(wedge, sampCol = NULL, sampName = "Demo", calVals = c(1,10), ellipsoid = TRUE, space = "rgb")
}
\arguments{
  \item{wedge}{
An object of class \code{CIEwedge}.
}
  \item{sampCol}{
A single color as a hexadecimal string to be plotted for comparison.
}
  \item{sampName}{
A character vector giving the name of the sample.
}
  \item{ellipsoid}{
Logical: shall a 95 percent confidence ellipsoid be drawn around the calibration points?
}
  \item{calVals}{
A length 2 numeric vector giving an upper and lower value for the calibration curve.  POSSIBLY NOT USED CORRECTLY.  CHECK!
}
  \item{space}{
A character vector giving the space in which to display the calibration.  One of \code{c("rgb", "pca")}.  The \code{"rgb"} option shows the results in the rgb space in which it is conceptually easy to evaluate the goodness of the fit visually.
}

}
\details{
The confidence ellipsoid is drawn using \code{makeEllipsoid} from package \code{ChemoSpec}.
}
\value{
A plot is drawn using \code{rgl} graphics, and information is printed at the console.
}
\references{
NEED A REFERENCE
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
# some data
good.cal <- c("#FF00FF", "#FF48FF", "#FF69FF", "#FF83FF", "#FF9AFF",
	"#FFB0FF", "#FFC4FF", "#FFD8FF", "#FFECFF", "#FEFFFF")
bad.cal <- c("#a52a2a", "#ff4040", "#8b0000", "#ff1493", "#cd1076",
	"#B22222", "#ff69b4", "#8b3a62", "#ff00ff", "#ff34b3")
tp1 <- "#E17DFF" # close to the good.cal calibration line
tp2 <- "#FF34B3" # maroon1; well off the calibration line
#
# create a minimal wedge structure
demo <- vector("list", 1L)
demo$calCols <- bad.cal
#
# A bad calibration set, where the sample is doesn't much matter:
showCalColSpace(wedge = demo, sampCol = tp2)
# Now try the others yourself!
}
\keyword{ multivariate }
\keyword{ plot }
