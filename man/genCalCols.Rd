\name{genCalCols}
\alias{genCalCols}
\title{
Generate a set of calibration colors
}
\description{
This function generates a set of calibration colors.  It is the starting point for the use of \code{photoSpec}.
}
\usage{
genCalCols(minHue = "2.5R", maxHue = "10R",
	minVal = 1, maxVal = 9, minChroma = 2, maxChroma = 12,
	plotPC = FALSE, showCal = TRUE, ...)
}
\arguments{
  \item{minHue}{
Character.
}
  \item{maxHue}{
Character.
}
  \item{minVal}{
Integer.
}
  \item{maxVal}{
Integer.
}
  \item{minChroma}{
Integer
}
  \item{maxChroma}{
Integer.
}
  \item{plotPC}{
Logical. Shall a plot of the generated colors be drawn as paint chips?
}
  \item{showCal}{
Logical. Shall the calibration color space be shown in 3D?
}

  \item{\dots}{
Other parameters to be passed to downstream functions.
}
}
\value{
UPDATE
}
\references{
NEED A REFERENCE
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
# NEED AN EXAMPLE
}
\keyword{ plot }
