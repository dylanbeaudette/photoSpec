

getWhiteValues <- function(white) {
	
	# Returns values for reference white points
	
	if (gamut == "D65") ans <- data.frame(x = 0.3127, y = 0.3290)
	if (gamut == "E") ans <- data.frame(x = 0.333, y = 0.333)
	if (gamut == "C") ans <- data.frame(x = 0.3101, y = 0.3161)
	if (gamut == "D50") ans <- data.frame(x = 0.3457, y = 0.3585)

	ans
	}