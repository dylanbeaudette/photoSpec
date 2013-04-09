

getGamutValues <- function(gamut) {
	
	# gamut data verified with multiple sources
	# CIE is the theoretical primaries from CIE
	# sRGB is the standard for computer monitors and HDTV
	# Apple is the standard for Apple monitors based upon Trinitron phosphors
	#       which is no longer in use. Same as SGI
	# NTSC is the 1953 CRT TV standard
	
	if (gamut == "Apple") { # relative to D65
		x <- c(0.640, 0.300, 0.150)
		y <- c(0.330, 0.600, 0.060)
		ans <- data.frame(x, y)
		}
		
	if (gamut == "CIE") { # relative to E
		x <- c(0.735, 0.274, 0.167)
		y <- c(0.265, 0.717, 0.009)
		ans <- data.frame(x, y)
		}

	if (gamut == "Adobe") { # relative to D65
		x <- c(0.640, 0.210, 0.150)
		y <- c(0.330, 0.600, 0.060)
		ans <- data.frame(x, y)
		}

	if (gamut == "sRGB") { # relative to D65
		x <- c(0.640, 0.300, 0.150)
		y <- c(0.330, 0.600, 0.060)
		ans <- data.frame(x, y)
		}

	if (gamut == "NTSC") { # relative to C
		x <- c(0.670, 0.210, 0.140)
		y <- c(0.330, 0.710, 0.080)
		ans <- data.frame(x, y)
		}

	if (gamut == "SWOP") { # A type of CMYK (approx values)
		x <- c(0.205, 0.172, 0.225, 0.430, 0.610, 0.470)
		y <- c(0.125, 0.226, 0.540, 0.500, 0.320, 0.235)
		ans <- data.frame(x, y)
		}

	ans
	}