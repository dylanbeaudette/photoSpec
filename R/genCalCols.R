

genCalCols <- function(L1 = NULL, L2 = NULL, colSpace = "sRGB", ff = 1.0,
	nDiv = 10, pcpd = 1L, divMode = "linear", pMode = "snow", ...) {

	# Bryan Hanson, DePauw University, May 2013 hanson@depauw.edu
	# Part of the photoSpec package
	
	# A wrapper to get it all done.
	
	w1 <- selectCIExy(L1, L2, colSpace, ff, ...)
	w2 <- selectCalCols(wedge = w1, nDiv, pcpd, divMode, ff, pMode, ...)
	return(w2)	
	}
