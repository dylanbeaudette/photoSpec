

plotCIEselection <- function(vertices, ff = 1.0, ...) {

	# NEEDS A COLSPACE ARGUMENT
	
	# Function to draw the CIE chromaticity diagram
	# showing the region of interest to the user
	
	# Bryan Hanson, DePauw University, April 2013 hanson@depauw.edu
	# Part of the photoSpec package
	# Derived from plotCIEchrom() - more comments there
	
	data("CIExyz", envir = environment())
	Lxyz <- subset(CIExyz, CIExyz$wavelength <= 650)
	bgr <- prepCIEgradient(vertices, "sRGB", ff)
#	print(head(bgr))
	finras <- as.raster(bgr)
#	print(head(finras))
	
##### Create the plot using grid graphics

	off <- 0.1 # needed due to vp origin at -0.1
	Lxyz$x <- Lxyz$x + off
	Lxyz$y <- Lxyz$y + off

	grid.newpage()
	grid.text("Colors Selected", x = 0.5, y = 0.9,
		gp = gpar(fontface = "bold", cex = 1.2))
	grid.text(expression(italic(x)), x = 0.5, y = 0.05)
	grid.text(expression(italic(y)), x = 0.05, y = 0.5, rot = 90)

	# Now the data in it's own viewport; raster goes underneath

	pushViewport(viewport(width = 0.7, height = 0.7,
		xscale = c(-0.1, 0.9), yscale = c(-0.1, 0.9)))
	grid.raster(finras, x = 0.5, y = 0.5, interpolate = FALSE, default.units = "npc")
	grid.polygon(Lxyz$x, Lxyz$y)
	grid.rect()
	tickpos <- seq(0.0, 0.8, by = 0.1)
	grid.xaxis(at = tickpos)
	grid.yaxis(at = tickpos)
	msg <- "Warning: the color gradient appearance\nwill vary with the device, surface\n& incident light used to view it\nand is not likely correct anywhere"
	grid.text(msg, x = 0.98, y = 0.9, gp = gpar(fontface = "italic", cex = 0.9), just = "right")

	# Labels for the spectral locus
	
	sl <- c(100, 850, 1100, 1350, 1600, 1850, 2100, 2350, 2600)
	labs <- c("400 nm  ", "475 nm  ", "500 nm  ", "  525 nm", "  550 nm",
		"  575 nm", "  600 nm", "  625 nm", "  650 nm")
	
	grid.points(x = Lxyz$x[sl], y = Lxyz$y[sl], gp = gpar(col = "black"),
		size = unit(0.5, "char"), default.units = "npc")
	grid.text(label = labs, Lxyz$x[sl], Lxyz$y[sl],
		hjust = c(1, 1, 1, 0, 0, 0, 0, 0, 0),
		vjust = c(1, 0, 0, 0, 0, 0, 0, 0, 1),
		gp = gpar(cex = 0.75))

	grid.polygon(vertices$x, vertices$y, default.units = "native")

	invisible(finras)

	}