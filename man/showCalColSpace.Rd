\name{showCalColSpace}
\alias{showCalColSpace}
\title{
Use rgl to draw a plot of calibration colors in the rgb space.  Optionally show a sample color.
}
\description{
This function takes a vector of colors used for calibration and plots them in the rgb color space using package \code{rgl}.  If a sample color is provided, the position of the sample relative to the calibration colors can be inspected to determine if the sample is sufficiently close to the calibration colors.  In addition, some measures of the fit of the sample color to the calibration colors are printed at the console.
}
\usage{
showCalColSpace(calCols, sampCol = NULL, sampName = "Demo")
}
\arguments{
  \item{calCols}{
An list with elements \code{rgb} and \code{hexcol}.
}
  \item{sampCol}{
A single color as a hexadecimal string to be plotted for comparison.
}
  \item{sampName}{
A character vector giving the name of the sample.
}
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
require("rgl")
# some data
good.cal <- c("#FF00FF", "#FF48FF", "#FF69FF", "#FF83FF", "#FF9AFF",
	"#FFB0FF", "#FFC4FF", "#FFD8FF", "#FFECFF", "#FEFFFF")
bad.cal <- c("#a52a2a", "#ff4040", "#8b0000", "#ff1493", "#cd1076",
	"#B22222", "#ff69b4", "#8b3a62", "#ff00ff", "#ff34b3")
tp1 <- "#E17DFF" # close to the good.cal calibration line
tp2 <- "#FF34B3" # maroon1; well off the calibration line
}
\keyword{ multivariate }
\keyword{ plot }
