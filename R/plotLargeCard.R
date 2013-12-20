

plotLargeCard <- function(calCols = NULL, size = c(6, 4), ruler = c(3.5, 2.5),
	chip.order = "pale2dark", chip.rep = 1, title = "no title",
	guide = "none") {

	# Bryan Hanson, DePauw University, March 2013 hanson@depauw.edu
	# Part of the photoSpec package

	if (is.null(calCols)) stop("calCols must be provided")

	if (ruler[1] > size[1]*2.54) stop("Grid width is larger than the width of the card")
	if (ruler[2] > size[2]*2.54) stop("Grid height is larger than the height of the card")
	hexcol <- calCols$hexcol
	
	# Main viewport
	
	grid.newpage()
	pushViewport(viewport(width = size[1], height = size[2], default.units = "in"))
	
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
	
	# Now figure out which of xy would overlap the grid and drop them
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
	# Add pure black, 18% gray and pure white as references
	# These always have a guide square
	
	if (chip.order == "pale2dark") hexcol <- sortFromWhite(hexcol)
	
	if (chip.rep >= 1) { # simply repeat the chips
		rpc <- as.integer(chip.rep)
		# insert references every 25 chips then replicate the whole
		where <- seq(25, length(hexcol), by = 25)
		refs <- c("#FFFFFF", "#C7C7C7", "#000000")
		for (i in 0:(length(where)-1)) hexcol <- append(hexcol, refs, after = (where[i+1]+i))
		hexcol <- rep(hexcol, rpc) # this is now the proper length for use
		}

	if (chip.rep < 1) { # reduce the number of chips by choosing every nth one
		if (!chip.rep %in% c(0.5, 0.33, 0.25, 0.2)) stop("chip.rep must be > 1 or one of c(0.5, 0.33, 0.25, 0.2)")
		if (chip.rep == 0.5) keep <- rep(c(TRUE, FALSE), length.out = length(hexcol))
		if (chip.rep == 0.33) keep <- rep(c(TRUE, FALSE, FALSE), length.out = length(hexcol))
		if (chip.rep == 0.25) keep <- rep(c(TRUE, FALSE, FALSE, FALSE), length.out = length(hexcol))
		if (chip.rep == 0.2) keep <- rep(c(TRUE, FALSE, FALSE, FALSE, FALSE), length.out = length(hexcol))
		hexcol <- hexcol[keep] # this is now the proper length for use
		where <- seq(25, length(hexcol), by = 25)
		refs <- c("#FFFFFF", "#C7C7C7", "#000000")
		for (i in 0:(length(where)-1)) hexcol <- append(hexcol, refs, after = (where[i+1]+i))
		}

	ncc <- length(hexcol)
	if (ncc > nrow(xy)) {
		stop("Too many calibration colors to fit on the card\n  Increase the card size, decrease the grid size,\n  reduce chip.reps or the no. of calibration colors")
		}

	# Each of the following choices needs to create xy and labs properly
	# for guide which is the next step
	
	if (chip.order == "random") { # not sure this is the most sensible option
		wh <- sample(1:nrow(xy), ncc)
		xy <- xy[wh,] # these will be the positions to be used
		grid.rect(x = xy$x, y = xy$y, width = 0.5, height = 0.5, default.units = "cm",
		gp = gpar(fill = hexcol, col = "transparent"))
		if (guide == "hex") labs <- hexcol
		if (guide == "Munsell") {
			mrgb <- hex2RGB(hexcol)@coords
			labs <- rgb2mnsl(mrgb + 0.0001) # fix for buglet in munsell
			}
		}

	if ((chip.order == "munsell") | (chip.order == "pale2dark")) {
		xy <- xy[1:ncc,]
		grid.rect(x = xy$x, y = xy$y, width = 0.5, height = 0.5, default.units = "cm",
		gp = gpar(fill = hexcol, col = "transparent"))
		if (guide == "hex") labs <- hexcol
		if (guide == "Munsell") {
			mrgb <- hex2RGB(hexcol)@coords
			labs <- rgb2mnsl(mrgb + 0.0001) # fix for buglet in munsell
			}
		}

	if (chip.order == "fill") { # fills the page, in order provided
		nr <- floor(nrow(xy)/ncc)
		rem <- nrow(xy) %% ncc
		hexcol <- rep(hexcol, nr)
		hexcol <- c(hexcol, hexcol[1:rem])
		grid.rect(x = xy$x, y = xy$y, width = 0.5, height = 0.5, default.units = "cm",
		gp = gpar(fill = hexcol, col = "transparent"))
		if (guide == "hex") labs <- hexcol
		if (guide == "Munsell") {
			mrgb <- hex2RGB(hexcol)@coords
			labs <- rgb2mnsl(mrgb + 0.0001) # fix for buglet in munsell
			}
		}

	# Now mark the white, 18% gray and black
	wht <- which(hexcol == "#FFFFFF")
	blk <- which(hexcol == "#000000")
	gry <- which(hexcol == "#C7C7C7")
	grid.rect(x = xy$x[wht], y = xy$y[wht], width = 0.6, height = 0.6, default.units = "cm")
	grid.rect(x = xy$x[gry], y = xy$y[gry], width = 0.6, height = 0.6, default.units = "cm")
	grid.rect(x = xy$x[blk], y = xy$y[blk], width = 0.6, height = 0.6, default.units = "cm")

	if (guide == "hex") {
		grid.rect(x = xy$x, y = xy$y, width = 0.6, height = 0.6, default.units = "cm")
		grid.text(label = labs, x = xy$x, y = xy$y - 0.5,
			default.units = "cm", just = "center", gp = gpar(cex = 0.5))
		}

	if (guide == "Munsell") {
		grid.rect(x = xy$x, y = xy$y, width = 0.6, height = 0.6, default.units = "cm")
		grid.text(label = labs, x = xy$x, y = xy$y - 0.5,
			default.units = "cm", just = "center", gp = gpar(cex = 0.5))
		}
	
	# Add a serial number for tracking/reproduction
	sn <- digest(hexcol)
	desc <- packageDescription("photoSpec")
	msg <- paste("photoSpec", desc$Version, Sys.Date(), title, sep = "   ")
	grid.text(msg, x = 0.02, y = 0.97, just = "left", gp = gpar(cex = 0.75))
	grid.text(paste("csn:", sn, sep = " "), x = 0.02, y = 0.01, just = "left", gp = gpar(cex = 0.75))

	invisible() # nothing
	
	} # end of function

