

plotSampleCard <- function(colors = NULL, size = c(6, 4), wavelength = NULL,
	scale = c(3.5, 2.5), ff = 1.65, guide = FALSE) {

	# Main viewport
	
	pushViewport(viewport(width = size[1], height = size[2], default.units = "in"))
#	grid.rect() # just for reference while troubleshooting
	if (is.null(colors)) wavelength <- c(550, 550)
	msg <- bquote("photoSpec sample card for " ~lambda[max] ~.(wavelength[1]) - .(wavelength[2]) ~ "nm")
#	grid.text(msg, x = 0.02, y = 0.97, just = "left",
#		gp = gpar(cex = 0.75))
	
	# Draw the sample region with calibration grill/grid
	
	pushViewport(viewport(width = scale[1], height = scale[2], default.units = "cm",
		xscale = c(0, scale[1]), yscale = c(0, scale[2])))
	# tickposX <- seq(0.0, scale[1], by = 0.5)
	# tickposY <- seq(0.0, scale[2], by = 0.5)
	# grid.grill(v = unit(tickposX, "cm"), h = unit(tickposY, "cm"),
		# gp = gpar(col = "grey70"))

	# grid.xaxis(at = tickposX, gp = gpar(cex = 0.5))
	# grid.yaxis(at = tickposY, gp = gpar(cex = 0.5))
	# grid.xaxis(at = tickposX, gp = gpar(cex = 0.5), main = FALSE)
	# grid.yaxis(at = tickposY, gp = gpar(cex = 0.5), main = FALSE)

	popViewport()
	
	# Now draw paint chips in the main vp

	if (is.null(colors)) {
		
		# Compute paint chip colors
		message("No colors provided, drawing sample paint chips")
		
		# The following draws some purples as the complement of 550 nm
		# aka the dominant wavelength
		# pc = no. paint chips to draw
		pc <- 6
		myc <- data.frame(x = 0.312, y = seq(0.09, 0.329, length.out = pc))
		myc$z <- 1 - myc$x - myc$y
		myc2 <- convertColor(myc, from = "XYZ", to = "sRGB")
		myc2 <- myc2*ff
		myc2[myc2 > 1] <- 1.0
		myc3 <- rgb(myc2)
		myc4 <- cbind(myc, myc2, myc3)
		names(myc4) <- c("CIE_x", "CIE_y", "CIE_z", "r", "g", "b", "hex")

		# Set up locations of paint chips
		x <- seq(1, size[1]*2.54 -1, length.out = pc)
		x <- c(x, rev(x))
		y <- rep(c(2, 8), each = pc)
		df <- data.frame(x, y)

		if (guide) {
			grid.rect(df$x[c(pc, pc*2)], df$y[c(pc, pc*2)], width = 1.0, height = 1.0, default.units = "cm")
			dups <- duplicated(myc2)
			labs <- rep("unique", pc)
			labs <- ifelse(dups == TRUE, "dup", labs)
			labs[pc] <- "white?"
			grid.text(label = 1:pc, x = df$x, y = df$y - rep(c(0.75, -0.75), each = pc),
				default.units = "cm", just = "center", gp = gpar(cex = 0.75))
			grid.text(label = labs, x = df$x, y = df$y - rep(c(-0.75, 0.75), each = pc),
				default.units = "cm", just = "center", gp = gpar(cex = 0.75))
			}
			
		grid.rect(x = df$x, y = df$y, width = 0.5, height = 0.5, default.units = "cm",
			gp = gpar(fill = myc3, col = "transparent"))
				
		}

	if (!is.null(colors)) {
		
		# Need to make a general layout depending upon number of colors passed
		
		}
	
	# Return the collection of colors to be drawn for troubleshooting
	myc4
	
	} # end of function

