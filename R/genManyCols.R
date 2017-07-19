

genManyCols <- function(minHue = "2.5R", maxHue = "10R",
	minVal = 1, maxVal = 9, minChroma = 2, maxChroma = 26,
	plotPC = TRUE, showRGB = FALSE, showCIE = FALSE, ...) {

	# Bryan Hanson, DePauw University, June 2013 hanson@depauw.edu
	# Part of the photoSpec package

	# Check input data
	
	if (minVal == 0) { # munsell can't handle these
		minVal <- 1
		message("Minimum value set to 1")
		}

	if (maxVal == 10) {
		maxVal <- 9
		message("Maximum value set to 9")
		}

	if (maxVal < minVal) stop("minVal should be less than maxVal")
	if (maxChroma < minChroma) stop("minChroma should be less than maxChroma")

	# Process arguments and get all combos
	
	hueMin <- which(mnsl_hues() == minHue)
	hueMax <- which(mnsl_hues() == maxHue)
	
	# May need to wrap colors relative to how they are stored in mnsl_hues

	if (hueMax >= hueMin) hues <- mnsl_hues()[hueMin:hueMax]
	if (hueMax < hueMin) {
		hues <- c(mnsl_hues()[hueMin:40], mnsl_hues()[1:hueMax])
		}
	
	hvc <- expand.grid(hue = hues, value = minVal:maxVal,
		chroma = seq(minChroma, maxChroma, by = 2)) # defaults give 216 colors (? no longer true)
	m <- paste(hvc$hue, " ", hvc$value, "/", hvc$chroma, sep = "")
	
	# Convert colors to hexadecimals for plotting
	msg <- paste("Total colors attempted:", length(m), "of which",
		length(mh), "were in gamut", sep = " ")
	message(msg)

	if (plotPC) print(plot_hex(mh)) # draws paint chips & labels them
	
	# Convert colors to rgb for plotting in rgl
	mrgb <- hex2RGB(mh)@coords

	# Convert back to Munsell for labeling
	mm <- rgb2mnsl(mrgb)
	
	# Assemble list for return
	
	calCols <- vector("list")
	calCols$hexcol <- mh
	calCols$rgb <- mrgb
	calCols$Munsell <- mm
	
	# Send out for 3D view if requested
	
	if (showRGB) showRGBcalibration(calCols, ...)
	if (showCIE) showCIE(calCols, ...)
	
	invisible(calCols)
	}
