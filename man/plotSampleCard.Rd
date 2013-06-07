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
String.  Determines how the colors will be positioned on the card.  One of \code{c("random", "as.given", "pale2dark", "fill")}.  See the examples.
}
  \item{chip.rep}{
Integer; the number of replicates of \code{"wedge$calCols"} to use.
}
  \item{guide}{
Logical.  If TRUE, annotate the card colors to show which ones are unique, and which are duplicates.  This is useful to determine if the colors you have selected are in fact suitable for use in a calibration curve.
}
}
\details{
Pure black and pure white are appended at the ends of \code{calCols} to serve as references when analyzing.
}
\warning{
Be sure when printing that scaling is set to 100% or the grid will not be accurate.  Low ink supplies could lead to poor reproduction.
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
# create a minimal wedge structure, then try it out
demo <- vector("list", 2L)
demo$wavelengths <- c("low", "high")
demo$calCols <- cm.colors(10)
#
plotSampleCard(wedge = demo, chips = "random", chip.rep = 1)
plotSampleCard(wedge = demo, chips = "as.given", chip.rep = 1)
plotSampleCard(wedge = demo, chips = "pale2dark", chip.rep = 1)
plotSampleCard(wedge = demo, chips = "fill", chip.rep = 1)}
\keyword{ plot }
