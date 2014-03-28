
gen5cols <- function(cols = c("yellow", "orange", "red"),
	plotPC = TRUE, showRGB = FALSE, showCIE = FALSE, ...) {

	# Bryan Hanson, DePauw University, December 2013 hanson@depauw.edu
	# Part of the photoSpec package
	
	# Generate 5 colors for use with plotMiniCard
	# Code generally follows logic of genManyCols
	
	hc <- colorRampPalette(cols, space = "Lab")(5)
	calCols <- makeCalCols(hexCols = hc)
		
	# Send out for visualization if requested
	if (plotPC) print(plot_hex(calCols$hexcol))
	if (showRGB) showRGBcalibration(calCols, ...)
	if (showCIE) showCIE(calCols, ...)
	
	invisible(calCols)
	}