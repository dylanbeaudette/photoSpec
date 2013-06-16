

genCalCols <- function(minHue = "2.5R", maxHue = "10R",
	minVal = 1, maxVal = 9, minChroma = 2, maxChroma = 12,
	plotPC = FALSE, showCal = TRUE, ...) {

	# Bryan Hanson, DePauw University, June 2013 hanson@depauw.edu
	# Part of the photoSpec package

	# Process arguments to formated colors
	hueMin <- which(mnsl_hues() == minHue)
	hueMax <- which(mnsl_hues() == maxHue)
	hues <- mnsl_hues()[hueMin:hueMax]
	hvc <- expand.grid(hue = hues, value = minVal:maxVal,
		chroma = seq(minChroma, maxChroma, by = 2)) # defaults give 216 colors
	m <- paste(hvc$hue, " ", hvc$value, "/", hvc$chroma, sep = "")

	# Convert colors to hexadecimals for plotting
	mh <- mnsl(m) # out of gamut will be NA
	bad <- which(is.na(mh))
	msg <- paste("Total colors requested:", length(m), "of which",
		length(m) - length(bad), " were usable", sep = " ")
	message(msg)
	mh <- mh[-bad]

	if (plotPC) print(plot_hex(mh)) # draws paint chips & labels them
	
	# Convert colors to rgb for plotting in rgl
	hvC <- strsplit(m[-bad], " ")
	h <- c()
	vC <- c()
	for (n in 1:length(hvC)) {
		h <- c(h, hvC[[n]][1])
		vC <- c(vC, hvC[[n]][2])	
		}
	vC <- strsplit(vC, "/")
	v <- c()
	C <- c()
	for (n in 1:length(vC)) {
		v <- c(v, vC[[n]][1])
		C <- c(C, vC[[n]][2])	
		}

	mrgb <- munsell2rgb(h, v, C, alpha = 1, 
		maxColorValue = 1, return_triplets = TRUE)

	# Assemble list for return
	
	calCols <- vector("list")
	calCols$hexcol <- mh
	calCols$rgb <- mrgb
	
	# Send out for 3D view if requested
	
	if (showCal) showCalColSpace(calCols)
	
	invisible(calCols)
	}
