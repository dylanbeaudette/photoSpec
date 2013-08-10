
showCIEcalibration <- function(calCols, sampCol = NULL, title = NULL, gradient = NULL, opts = "sRGB", ...) {
		
	# Function to show calibration colors, and optionally sample colors
	# on the CIE chromaticity diagram
	
	# Bryan Hanson, DePauw University, August 2013 hanson@depauw.edu
	# Part of the photoSpec package
	
	# Convert to CIE xy  * this approach ignores brightness*
	rgb <- t(col2rgb(calCols$hexcol)/255)
	XYZ <- convertColor(rgb, from = "sRGB", to = "XYZ")
	x <- XYZ[,1]/rowSums(XYZ)
	y <- XYZ[,2]/rowSums(XYZ)
	cie <- cbind(x, y)

	plotCIEchrom(gradient = gradient, opts = opts, title = title, ...)
	if (is.null(gradient)) mycols <- calCols$hexcol
	if (!is.null(gradient)) mycols <- "black"
	grid.points(cie[,1], cie[,2], size = unit(0.25, "char"), gp = gpar(col = mycols))

	if (!is.null(sampCol)) {
		if ("cols" %in% names(sampCol)) sampCol$hex <- sampCol$cols # backward compatability
		res <- calcColorPurity(sampCol, ...)
		return(res)
		}
	invisible()
	}