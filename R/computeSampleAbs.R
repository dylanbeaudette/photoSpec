

computeSampleAbs <- function(calCols, sampCol) {
	
	# Function to interpolate a sample color on the paint chip scale

	# Bryan Hanson, DePauw University, March 2013 hanson@depauw.edu
	# Part of the photoSpec package

	# Carry out a principal.curve fit
	# Add pure black and pure white to the points to be fit
	calCols$rgb <- rbind(calCols$rgb, c(0.0, 0.0, 0.0), c(1, 1, 1))	
	fit <- principal.curve(as.matrix(calCols$rgb))
	invisible(fit)
	}
