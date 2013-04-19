\name{plotSampleCard}
\alias{plotSampleCard}
\title{
Plot a sample card containing calibration colors (paint chips)
}
\description{
NEED DESCRIPTION
}
\usage{
plotSampleCard(calCols = NULL, size = c(6, 4), wavelength = NULL,
	ruler = c(3.5, 2.5), chips = "random", chip.rep = 3, ff = 1, guide = FALSE)
}
\arguments{
  \item{calCols}{
A vector of strings describing colors in hexidecimal format.
}
  \item{size}{
The width, height of the printed card in inches.
}
  \item{wavelength}{
A length two numeric vector giving the first and last wavelengths to be analyzed (units are nm).
}
  \item{ruler}{
The width, height of the sample scale to be drawn, in cm.
}
  \item{chips}{
String, currently only \code{"random"}.  Determines how the colors will be positioned on the card.
}
  \item{chip.rep}{
Integer; the number of replicates of \code{"calCols"} to use.
}
  \item{ff}{
Numeric.  The 'fudge factor' to use.  NEED TO EXPLAIN AND SHOW IN EXAMPLES.
}
  \item{guide}{
Logical.  If TRUE, annotate the card colors to show which ones are unique, and which are duplicates.  This is useful to determine if the colors you have selected are in fact suitable for use in a calibration curve.
}
}
\value{
None.  A plot is drawn using \code{grid} graphics.
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
