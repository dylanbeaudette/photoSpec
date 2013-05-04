

getWhiteValues <- function(white) {
	
	# Returns values for reference white points
	
	# Bryan Hanson, DePauw University, March 2013 hanson@depauw.edu
	# Part of the photoSpec package

	if (white == "D65") ans <- data.frame(x = 0.3127, y = 0.3290)
	if (white == "E") ans <- data.frame(x = 0.333, y = 0.333)
	if (white == "C") ans <- data.frame(x = 0.3101, y = 0.3161)
	if (white == "D50") ans <- data.frame(x = 0.3457, y = 0.3585)

	ans
	}