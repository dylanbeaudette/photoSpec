\name{selectCalCols}
\alias{selectCalCols}
\title{
Select colors for calibration
}
\description{
Selects some colors FINISH
}
\usage{
selectCalCols(wedge, nDiv = 10, pcpd = 1, divMode = "linear", ...)
}
\arguments{
  \item{wedge}{
List.  Passed from \code{selectCIExy}.
}
  \item{nDiv}{
Integer.  The number of bands to be created within the wedge.
}
  \item{pcpd}{
Integer.  "Paint chips per division".  The number of colors to be selected from each band.
}
  \item{divMode}{
Character.  The mode used to divide up the wedge into bands.  Currently only \code{"linear"} is available.
}
  \item{\dots}{
Other parameters to be passed to downstream functions.
}
}
\value{
A list of calibration colors in hexadecimal mode.  The number of colors will be \code{nDiv} * \code{pcpd}.
}
\references{
NEED A REFERENCE
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

}
\keyword{ utilities? }
