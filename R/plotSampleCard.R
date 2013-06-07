

plotSampleCard <- function(wedge, size = c(6, 4), ruler = c(3.5, 2.5),
	chips = "random", chip.rep = 3L, guide = FALSE, ...) {

	# Bryan Hanson, DePauw University, March 2013 hanson@depauw.edu
	# Part of the photoSpec package

### Helper Function

	sortFromWhite <- function(Cols) {
		# Cols should be a vector of hexadecimal colors
		# Cols will be compared to pure white
		oCols <- Cols # Save the original
		Cols <- col2rgb(Cols) # now in rgb
	    whdist <- apply(Cols, 2, function(z) {sqrt(sum((z - 255)^2))})
		whdist <- 100*whdist/(1.73*255) # gives answer a percentage of max dist
		# max distance is from pure white to complete black
		# return Cols sorted by distance from white (pale to dark)
		# Needs package plyr
		df <- data.frame(cols = oCols, dist = whdist)
		df <- arrange(df, dist)
		return(as.character(df$cols))
		}

### End of Helper Function

	if (ruler[1] > size[1]*2.54) stop("Grid width is larger than the width of the card")
	if (ruler[2] > size[2]*2.54) stop("Grid height is larger than the height of the card")
	calCols <- wedge$calCols
	wavelength <- wedge$wavelengths

	# Main viewport
	
	grid.newpage()
	pushViewport(viewport(width = size[1], height = size[2], default.units = "in"))
#	grid.rect() # just for reference while troubleshooting
	msg1 <- bquote("photoSpec sample card for " ~lambda[max] ~.(wedge$wavelength[1]) - .(wedge$wavelength[2]) ~ "nm")
	grid.text(msg1, x = 0.02, y = 0.97, just = "left", gp = gpar(cex = 0.75))
	desc <- packageDescription("photoSpec")
	msg2 <- paste("photoSpec", desc$Version, "processed", Sys.Date(), sep = " ")
	grid.text(msg2, x = 0.98, y = 0.97, just = "right", gp = gpar(cex = 0.75))
	
	# Draw the sample region with calibration grill/grid
	
	pushViewport(viewport(width = ruler[1], height = ruler[2], default.units = "cm",
		xscale = c(0, ruler[1]), yscale = c(0, ruler[2])))
	tickposX <- seq(0.0, ruler[1], by = 0.5)
	tickposY <- seq(0.0, ruler[2], by = 0.5)
	grid.grill(v = unit(tickposX, "cm"), h = unit(tickposY, "cm"),
		gp = gpar(col = "grey70"))

	grid.xaxis(at = tickposX, gp = gpar(cex = 0.5))
	grid.yaxis(at = tickposY, gp = gpar(cex = 0.5))
	grid.xaxis(at = tickposX, gp = gpar(cex = 0.5), main = FALSE)
	grid.yaxis(at = tickposY, gp = gpar(cex = 0.5), main = FALSE)

	popViewport()
	
	# Now draw paint chips in the main vp
	# First, a grid of equally spaced paint chip locations

	cx <- size[1]*2.54
	cy <- size[2]*2.54
	pcx <- seq(1L, round(cx -1))
	pcy <- seq(1L, round(cy -1))
	xy <- expand.grid(x = pcx, y = pcy) # grid over whole card on 1 cm centers
	
	# Now figure out which of xy would overlap the grid
	# and drop them
	minx <- floor(0.5*cx - 0.5*ruler[1])-1
	maxx <- ceiling(0.5*cx + 0.5*ruler[1])+1
	miny <- floor(0.5*cy - 0.5*ruler[2])-1
	maxy <- ceiling(0.5*cy + 0.5*ruler[2])+1
	nox <- (xy$x > minx) & (xy$x < maxx)
	noy <- (xy$y > miny) & (xy$y < maxy)
	xy <- subset(xy, !(nox & noy))
	# plot(xy, asp = cx/cy) # proof of concept
	# rect(minx, miny, maxx, maxy, col = "red")
	
	# Get the paint chips ready
	# Add pure black and pure white as references
	# White will always have a guide square
	
	if (chips == "pale2dark") calCols <- sortFromWhite(calCols)
	calCols <- c("#FFFFFF", calCols, "#000000")
	rpc <- as.integer(chip.rep) # repeat the color chips
	calCols <- rep(calCols, rpc) # this is now the proper length for use
	ncc <- length(calCols)
	if (ncc > nrow(xy)) {
		stop("Too many calibration colors to fit on the card\n  Increase the card size, decrease the grid size,\n  reduce chip.reps or the no. of calibration colors")
		}
	wht <- which(calCols == "#FFFFFF")

	# Each of the following choices needs to create xy and labs properly
	# for guide = TRUE which is the next step
	
	if (chips == "random") { # not sure this is the most sensible option
		wh <- sample(1:nrow(xy), ncc)
		xy <- xy[wh,] # these will be the positions to be used
		grid.rect(x = xy$x, y = xy$y, width = 0.5, height = 0.5, default.units = "cm",
		gp = gpar(fill = calCols, col = "transparent"))
		labs <- calCols
		}

	if ((chips == "as.given") | (chips == "pale2dark")) {
		xy <- xy[1:ncc,]
		grid.rect(x = xy$x, y = xy$y, width = 0.5, height = 0.5, default.units = "cm",
		gp = gpar(fill = calCols, col = "transparent"))
		labs <- calCols
		}

	if (chips == "fill") { # fills the page, in order given
		grid.rect(x = xy$x, y = xy$y, width = 0.5, height = 0.5, default.units = "cm",
		gp = gpar(fill = calCols, col = "transparent"))
		labs <- calCols
		}

	# Now mark the whites
	grid.rect(x = xy$x[wht], y = xy$y[wht], width = 0.6, height = 0.6, default.units = "cm")

	if (guide) {
		grid.rect(x = xy$x, y = xy$y, width = 0.6, height = 0.6, default.units = "cm")
		grid.text(label = labs, x = xy$x, y = xy$y - 0.5,
			default.units = "cm", just = "center", gp = gpar(cex = 0.5))
		}
	
	invisible() # nothing
	
	} # end of function

