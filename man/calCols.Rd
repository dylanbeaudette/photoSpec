\name{calCols}
\alias{calCols}
\title{
Structure of a calCols object
}
\description{
This page describes the structure of a \code{calCols} object.  This is not a formal class.  The description here is for convenience only.
}
\section{Structure}{
A list of 3 (RGB values are in [0...1]):

\tabular{llll}{
\emph{element} \tab \emph{type} \tab \emph{description} \tab \cr
$hexcol \tab char \tab A vector giving the chosen colors as hexadecimals. \tab \cr
$rgb \tab data frame \tab Data frame containing the RGB coordinates of the chosen colors. \tab \cr
\tab $r  \tab num: Values for red channel. \cr
\tab $g  \tab num: Values for green channel. \cr
\tab $b  \tab num: Values for blue channel. \cr
$Munsell \tab char \tab A vector with the Munsell color designation.
}
}
\references{
\url{https://github.com/bryanhanson/photoSpec}
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
