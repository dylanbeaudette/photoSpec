
useArbCols <- function(munCols = NULL, rgbCols = NULL,
	hexCols = NULL, namedCols = NULL,
	plotPC = TRUE, showRGB = FALSE, showCIE = FALSE, ...) {

	# Bryan Hanson, DePauw University, February 2014 hanson@depauw.edu
	# Part of the photoSpec package
	
	# Use user supplied colors in any format
	# to create a calCols structure
	
	if ( (is.null(munCols)) & (is.null(rgbCols)) &
		(is.null(hexCols)) & (is.null(namedCols)) ) {
		stop("No colors provided. Pls give munCols, rgbCols, hexCols or namedCols")
		}

	# Currently NO check to see if more than one color specification is given
	# In this case the last processed one will be the return value
	
	if (!is.null(munCols)) {
		calCols <- makecC(munCols = munCols)
		}

	if (!is.null(rgbCols)) {
		calCols <- makecC(rgbCols = rgbCols)
		}

	if (!is.null(hexCols)) {
		calCols <- makecC(hexCols = hexCols)
		}

	if (!is.null(namedCols)) {
		calCols <- makecC(namedCols = namedCols)
		}
	
	# Send out for visualization if requested
	
	if (plotPC) print(plot_hex(calCols$hexcol))
	if (showRGB) showRGBcalibration(calCols, ...)
	if (showCIE) showCIE(calCols, ...)
	
	invisible(calCols)
	}