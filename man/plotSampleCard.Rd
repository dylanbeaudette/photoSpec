\name{plotSampleCard}
\alias{plotSampleCard}
\title{
Plot a sample card containing calibration colors (paint chips)
}
\description{
This function plots a 'card' containing a series of calibration colors (paint chips) and a simple grid calibrated in cm.  A sample is placed on the grid and photographed.  The photo is saved as a jpeg file, which is then analyzed.
}
\usage{
plotSampleCard(wedge, size = c(6, 4), ruler = c(3.5, 2.5),
chips = "random", chip.rep = 3, guide = FALSE)
}
\arguments{
  \item{wedge}{
An object of class \code{CIEwedge}
}
  \item{size}{
The \code{c(width, height)} of the printed card in inches.
}
  \item{ruler}{
The \code{c(width, height)} of the sample scale to be drawn, in cm.
}
  \item{chips}{
String, currently only \code{"random"}.  Determines how the colors will be positioned on the card.
}
  \item{chip.rep}{
Integer; the number of replicates of \code{"wedge$calCols"} to use.
}
  \item{guide}{
Logical.  If TRUE, annotate the card colors to show which ones are unique, and which are duplicates.  This is useful to determine if the colors you have selected are in fact suitable for use in a calibration curve.
}
}
\value{
None.  A plot is drawn using \code{grid} graphics.  This should be saved as a pdf or jpeg and printed for use.
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
