
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
	
	if (!is.null(munCols)) {
		
		}

	if (!is.null(rgbCols)) {
		
		}

	if (!is.null(hexCols)) {
		
		}

	if (!is.null(namedCols)) {
		# Convert to hexadecimal
		hc <- ColToHex(namedCols)
		# Convert to rgb
		hrgb <- col2rgb(namedCols)
		# Convert  to Munsell
		mm <- rgb2mnsl(hrgb)	
		}

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
	if (showCIE) showCIE(calCols, ...)
	
	invisible(calCols)
	}