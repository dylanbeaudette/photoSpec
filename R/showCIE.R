
showCIE <- function(calCols = NULL, sampCols = NULL, title = NULL, gradient = NULL, opts = "sRGB", ...) {
		
	# Function to show any combination of calibration colors and sample colors
	# on the CIE chromaticity diagram
	
	# Bryan Hanson, DePauw University, August 2013 hanson@depauw.edu
	# Part of the photoSpec package
	
	if ((is.null(calCols)) & (is.null(sampCols))) stop("Nothing to draw (no calCols, no sampCols)")

	plotCIEchrom(gradient = gradient, opts = opts, title = title, ...)
	
	if (!is.null(calCols)) {
		# Convert to CIE xy  * this approach ignores brightness*
		cie <- hex2CIExy(calCols)
		if (is.null(gradient)) mycols <- calCols$hexcol
		if (!is.null(gradient)) mycols <- "black"
		grid.points(cie[,1], cie[,2], size = unit(0.25, "char"), gp = gpar(col = mycols))
		}
		
	if (!is.null(sampCols)) {
		res <- calcColorPurity(sampCols, ...) # plotting of samples and projections handled here
		return(res)
		}
	invisible()
	}