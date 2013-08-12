


runAllCal <- function(calCols, sampCol = NULL, title = NULL, opts = "sRGB", ...) {

	# Wrapper to run all calibration options
		
	# Bryan Hanson, DePauw University, August 2013 hanson@depauw.edu
	# Part of the photoSpec package
	
	val <- showRGBcalibration(calCols = calCols, sampCol = sampCol, title = title, ...)
	cie <- showCIEcalibration(calCols = calCols, sampCol = sampCol, title = title, ...)
	
	if (!is.null(sampCol)) {
		res <- cie
		res$value <- val$value
		res$residual <- val$residual
		}

	if (is.null(sampCol)) return(invisible)
	
	res
	
	}