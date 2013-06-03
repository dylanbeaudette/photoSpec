

plotSampleCard <- function(wedge, size = c(6, 4), ruler = c(3.5, 2.5),
	chips = "random", chip.rep = 3, guide = FALSE) {

	# Bryan Hanson, DePauw University, March 2013 hanson@depauw.edu
	# Part of the photoSpec package

	# Main viewport
	
	calCols <- wedge$calCols
	wavelength <- wedge$wavelengths
	ff <- wedge$ff # not used anywhere?
	
	grid.newpage()
	pushViewport(viewport(width = size[1], height = size[2], default.units = "in"))
#	grid.rect() # just for reference while troubleshooting
	msg <- bquote("photoSpec sample card for " ~lambda[max] ~.(wedge$wavelength[1]) - .(wedge$wavelength[2]) ~ "nm")
	grid.text(msg, x = 0.02, y = 0.97, just = "left",
		gp = gpar(cex = 0.75))
	
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

	# if (is.null(calCols)) {
		
		# # Compute paint chip colors
		# message("No calCols provided, drawing sample paint chips")
		
		# # The following draws some purples as the complement of 550 nm
		# # aka the dominant wavelength
		# # pc = no. paint chips to draw
		# pc <- 10
		
		# # Original approach: equally spaced in CIE space, but the
		# # perceptual distance is not equal
		# # myc <- data.frame(x = 0.312, y = seq(0.09, 0.329, length.out = pc))
		# # myc$z <- 1 - myc$x - myc$y
		# # myc2 <- convertColor(myc, from = "XYZ", to = "sRGB")
		# # myc2 <- myc2*ff # may wish to hardwire to 1.65 for the example
		# # myc2[myc2 > 1] <- 1.0
		# # myc3 <- rgb(myc2)
		# # myc4 <- cbind(myc, myc2, myc3)
		# # names(myc4) <- c("CIE_x", "CIE_y", "CIE_z", "r", "g", "b", "hex")
		
		# # 2nd approach: Use Lab space to get consistent perceptual distance
		# C0 <- c(0.312, 0.09, 0.598) # one end of the gradient in XYZ
		# C1 <- c(0.312, 0.329, 0.359) # the other end
		# myc <- matrix(c(C0, C1), byrow = TRUE, nrow = 2)
		# myc2 <- convertColor(myc, from = "XYZ", to = "sRGB")
		# myc2 <- myc2*1.65
		# myc2[myc2 > 1] <- 1.0 # only needed due to ff
		# myc3 <- getInterRGB(seq(0, 1, length.out = pc),
			# rgb(myc2[1,1], myc2[1,2], myc2[1,3]),
			# rgb(myc2[2,1], myc2[2,2], myc2[2,3]))

		# # Set up locations of paint chips (in cm)
		# # x values spread across width, starting 1 cm in from edges 
		# x <- seq(1, size[1]*2.54 -1, length.out = pc)
		# x <- c(x, rev(x))
		# # y values centered in space above and below the grid
		# # note the grid is centered on the page
		# ay <- (size[2]*2.54 - ruler[2])*0.25
		# y <- rep(c(ay, (size[2]*2.54 - ay)), each = pc)
		# df <- data.frame(x, y)

		# if (guide) {
			# grid.rect(df$x[c(pc, pc*2)], df$y[c(pc, pc*2)],
				# width = 1.0, height = 1.0, default.units = "cm")
			# dups <- duplicated(myc3)
			# labsL <- rep("unique", pc)
			# labsL <- ifelse(dups == TRUE, "dup", labsL)
			# labsL[pc] <- "white?"
			# grid.text(label = 1:pc, x = df$x, y = df$y - rep(c(0.75, -0.75), each = pc),
				# default.units = "cm", just = "center", gp = gpar(cex = 0.75))
			# grid.text(label = labsL, x = df$x, y = df$y - rep(c(-0.75, 0.75), each = pc),
				# default.units = "cm", just = "center", gp = gpar(cex = 0.75))
			# # add hex code labels
			# }
			
		# grid.rect(x = df$x, y = df$y, width = 0.5, height = 0.5, default.units = "cm",
			# gp = gpar(fill = myc3, col = "transparent"))
				
		# }

	if (!is.null(calCols)) {
		
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
		
		rpc <- chip.rep # repeat the color chips; could maximize this w/i nrow(xy)
		if (length(calCols)*rpc > nrow(xy)) {
			stop("Too many calibration colors to fit on the card\n  Increase the card size, decrease the grid size\n  or reduce chip.reps")
			}

		if (chips == "random") {
			wh <- sample(1:nrow(xy), length(calCols)*rpc)
			xy <- xy[wh,] # these will be the pc's to be used
			grid.rect(x = xy$x, y = xy$y, width = 0.5, height = 0.5, default.units = "cm",
			gp = gpar(fill = calCols, col = "transparent"))

			}
		if (guide) {
			grid.rect(x = xy$x, y = xy$y, width = 0.6, height = 0.6, default.units = "cm")
			labs <- rep(calCols, rpc)
			grid.text(label = labs, x = xy$x, y = xy$y - 0.5,
				default.units = "cm", just = "center", gp = gpar(cex = 0.5))
			}
		}
	
	# Return the collection of colors to be drawn for troubleshooting
	
	return(TRUE)
	
	} # end of function

