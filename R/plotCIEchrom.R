
plotCIEchrom <- function(gradient = NULL, colSpace = "sRGB", ff = 1.0,
	opts = c("D65", "specLocus", "purples"), ...) {

	# Function to draw the CIE chromaticity diagram
	# with various decorations
	
	# Bryan Hanson, DePauw University, March 2013 hanson@depauw.edu
	# Part of the photoSpec package
	
##### Get and prepare the data
	# These are the coordinates of the spectral locus, which is a curve
	# describing the pure colors of the spectrum/rainbow

	data("CIExyz", envir = environment())
	# Note z = 1- x - y
	# Cutoff the data at 650 nm; beyond that the curve strangely
	# turns back on itself
	Lxyz <- subset(CIExyz, CIExyz$wavelength <= 650)
		
##### Prepare the raster with the color gradient
	
	if (!is.null(gradient)) {
		if (!(colSpace == "sRGB") || (colSpace == "Apple RGB")) stop("colSpace must be sRGB or Apple RGB")
		if (is.character(gradient)) {
			finras <- as.raster(prepCIEgradient(vertices = Lxyz,
				colSpace = colSpace, ff = ff, ...))
			}
			
		if (is.data.frame(gradient)) {
			finras <- as.raster(prepCIEgradient(vertices = gradient,
				colSpace = colSpace, ff = ff, ...))
			}
		}
	
##### Create the plot using grid graphics

	off <- 0.1 # needed due to vp origin at -0.1
	Lxyz$x <- Lxyz$x + off
	Lxyz$y <- Lxyz$y + off

	# First plot titles & labels in the vp of the entire device

	grid.newpage()
	grid.text("1931 CIE Chromaticity Diagram", x = 0.5, y = 0.9,
		gp = gpar(fontface = "bold", cex = 1.2))
	grid.text(expression(italic(x)), x = 0.5, y = 0.05)
	grid.text(expression(italic(y)), x = 0.05, y = 0.5, rot = 90)

	# Now the data in it's own viewport; raster goes underneath

	pushViewport(viewport(width = 0.7, height = 0.7,
		xscale = c(-0.1, 0.9), yscale = c(-0.1, 0.9)))

	if (!is.null(gradient)) {
		grid.raster(finras, x = 0.5, y = 0.5, interpolate = FALSE, default.units = "npc")
		msg <- "Warning: the color gradient appearance\nwill vary with the device, surface\n& incident light used to view it\nand is not likely correct anywhere"
		grid.text(msg, x = 0.98, y = 0.9, gp = gpar(fontface = "italic", cex = 0.9),
			just = "right")
		}

	grid.polygon(Lxyz$x, Lxyz$y)
	grid.rect()
	tickpos <- seq(0.0, 0.8, by = 0.1)
	grid.xaxis(at = tickpos)
	grid.yaxis(at = tickpos)

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
	
	# Optional labeling
	
	# Need to add lty and a legend to the various gamut options
	
	# White point labels
	
	if ("D65" %in% opts) {
		wh <- getWhiteValues("D65")
		grid.points(wh$x, wh$y, gp = gpar(col = "black"),
			size = unit(0.5, "char"), default.units = "native")
		grid.text(wh$x, wh$y, label = "  D65", just = "left",
			gp = gpar(cex = 0.75), default.units = "native")
		}

	if ("D50" %in% opts) {
		wh <- getWhiteValues("D50")
		grid.points(wh$x, wh$y, gp = gpar(col = "black"),
			size = unit(0.5, "char"), default.units = "native")
		grid.text(wh$x, wh$y, label = "  D50", just = "left",
			gp = gpar(cex = 0.75), default.units = "native")
		}

	if ("C" %in% opts) {
		wh <- getWhiteValues("C")
		grid.points(wh$x, wh$y, gp = gpar(col = "black"),
			size = unit(0.5, "char"), default.units = "native")
		grid.text(wh$x, wh$y, label = "  C", just = "left",
			gp = gpar(cex = 0.75), default.units = "native")
		}

	if ("E" %in% opts) {
		wh <- getWhiteValues("E")
		grid.points(wh$x, wh$y, gp = gpar(col = "black"),
			size = unit(0.5, "char"), default.units = "native")
		grid.text(wh$x, wh$y, label = "  E", just = "left",
			gp = gpar(cex = 0.75), default.units = "native")
		}

	# gamut outlining
	
	if ("sRGB" %in% opts) {
		g <- getGamutValues("sRGB")
		grid.polygon(g$x, g$y, default.units = "native")
		}

	if ("SWOP" %in% opts) {
		g <- getGamutValues("SWOP")
		grid.polygon(g$x, g$y, default.units = "native")
		}

	if ("Apple" %in% opts) {
		g <- getGamutValues("Apple")
		grid.polygon(g$x, g$y, default.units = "native")
		}

	if ("NTSC" %in% opts) {
		g <- getGamutValues("NTSC")
		grid.polygon(g$x, g$y, default.units = "native")
		}

	if ("Adobe" %in% opts) {
		g <- getGamutValues("Adobe")
		grid.polygon(g$x, g$y, default.units = "native")
		}

	if ("CIE" %in% opts) {
		g <- getGamutValues("CIE")
		grid.polygon(g$x, g$y, default.units = "native")
		}

	# Misc decorations
	
	if ("specLocus" %in% opts) {
		grid.text(0.75, 0.55, label = "spectral\nlocus", just = "left",
			gp = gpar(cex = 0.75), default.units = "native")
		grid.segments(0.73, 0.53, 0.61, 0.41, default.units = "native",
			arrow = arrow(ends = "last", length = unit(0.025, "npc"),
			angle = 15, type = "closed"))
		}


	if ("purples" %in% opts) {
		grid.text(0.65, 0.05, label = "line of\npurples", just = "left",
			gp = gpar(cex = 0.75), default.units = "native")
		grid.segments(0.63, 0.07, 0.5, 0.17, default.units = "native",
			arrow = arrow(ends = "last", length = unit(0.025, "npc"),
			angle = 15, type = "closed"))

		}

	if (is.null(gradient)) ans <- NULL
	if (!is.null(gradient)) ans <- finras
	invisible(ans)
	}