\name{plotSampleCard}
\alias{plotSampleCard}
\title{
Plot a sample card containing calibration colors (paint chips)
}
\description{
This function plots a 'card' containing a series of calibration colors (paint chips) and a simple grid calibrated in cm.  A sample is placed on the grid and photographed.  The photo is saved as a jpeg file, which is then analyzed.
}
\usage{
plotSampleCard(calCols, size = c(6, 4), ruler = c(3.5, 2.5),
chips = "pale2dark", chip.rep = 1, guide = FALSE, ...)
}
\arguments{
  \item{calCols}{
An list with elements \code{rgb} and \code{hexcol}.
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
Numeric; the number of replicates of \code{calCols} to use.  If \code{chip.rep >= 1} it is taken as an integer and the calibration colors are replicated accordingly (this gives copies of the same color which can be checked for consistency). If \code{chip.rep < 1} \code{chip.rep} must be in \code{c(0.5, 0.33, 0.25, 0.2)} and 1/2, 1/3, 1/4 or 1/5 of the calibration colors will be used.
}
  \item{guide}{
Logical.  If TRUE, annotate the card colors to show which ones are unique, and which are duplicates.  This is useful to determine if the colors you have selected are in fact suitable for use in a calibration curve.
}
  \item{...}{
Arguments to be passed downstream, likely to \code{grid} drawing functions.
}
}
\details{
Pure black and pure white are appended at the ends of \code{calCols} to serve as references when analyzing.
}
\section{Warning}{
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
# NEED EXAMPLE
}
\keyword{ plot }
