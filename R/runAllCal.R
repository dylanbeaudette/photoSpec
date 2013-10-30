


runAllCal <- function(calCols, sampCols = NULL, title = NULL, opts = "sRGB", ...) {

	# Wrapper to run all calibration options
		
	# Bryan Hanson, DePauw University, August 2013 hanson@depauw.edu
	# Part of the photoSpec package
	
	val <- showRGBcalibration(calCols = calCols, sampCols = sampCols, title = title, ...)
	cie <- showCIEcalibration(calCols = calCols, sampCols = sampCols, title = title, ...)
	
	if (!is.null(sampCols)) {
		res <- cie
		res$value <- val$value
		res$residual <- val$residual

		# Extract just the R, G or B channel for each hex code passed
	
		res$R <- getRGorB(hexvector = as.character(sampCols$hex), channel = "R")
		res$G <- getRGorB(hexvector = as.character(sampCols$hex), channel = "G")
		res$B <- getRGorB(hexvector = as.character(sampCols$hex), channel = "B")
		}

	if (is.null(sampCols)) return(invisible)
	
	res
	
	}