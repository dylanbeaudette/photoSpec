
gen5cols <- function(cols = c("yellow", "orange", "red"),
	plotPC = TRUE, showRGB = FALSE, showCIE = FALSE, ...) {

	# Bryan Hanson, DePauw University, December 2013 hanson@depauw.edu
	# Part of the photoSpec package
	
	# Generate 5 colors for use with plotMiniCard
	# Code generally follows logic of genManyCols
	
	hc <- colorRampPalette(cols, space = "Lab")(5)
	
	# Convert to rgb for plotting in rgl
	hrgb <- hex2RGB(hc)@coords

	# Convert  to Munsell for labeling
	mm <- rgb2mnsl(hrgb)
	
	# Assemble list for return
	
	calCols <- vector("list")
	calCols$hexcol <- hc
	calCols$rgb <- hrgb
	calCols$Munsell <- mm
	
	# Send out for visualization if requested
	if (plotPC) print(plot_hex(hc)) # draws paint chips & labels them
	if (showRGB) showRGBcalibration(calCols, ...)
	if (showCIE) showCIEcalibration(calCols, ...)
	
	invisible(calCols)
	}