
verts <- data.frame(x = c(0.2, 0.72, 0.33),
	y = c(0.04, 0.29, 0.33))

plotCIEselection(verts)

plotCIEselection <- function(vertices, ff = 1.0, ...) {

	# Function to draw the CIE chromaticity diagram
	# showing the region of interest to the user
	
	# Bryan Hanson, DePauw University, April 2013 hanson@depauw.edu
	# Part of the photoSpec package
	# Derived from plotCIEchrom() - more comments there
	
	Lxyz <- loadObject("CVRLxyz.RData")
	Lxyz <- subset(Lxyz, wavelength <= 650)
	message("I'm painting a beautiful gradient, please give me a moment...")
		
	xx <- seq(-0.1, 0.9, 0.002) 
	yy <- seq(0.9, -0.1, -0.002)
	xyz <- expand.grid(xx,yy)
	names(xyz) <- c("x", "y")

	grad <- vertices
	insideL <- inout(xyz, grad, bound = TRUE) # TRUE = inside
	outsideL <-!insideL # TRUE = outside now
	xyz$z <- 1 - xyz$x - xyz$y
	xyzrgb <- convertColor(xyz, from = "XYZ", to = "sRGB")
	xyzrgb <- xyzrgb*ff
	xyzrgb[xyzrgb > 1] <- 1.0
	xyzrgb[outsideL,] <- 1.0 # Set the color outside to white

	fin <- array(dim = c(length(xx), length(yy), 3))
	names(fin) <- c("x", "y", "rgb")
	
	mr <- matrix(data = xyzrgb[,1], ncol = length(xx), byrow = FALSE)
	mg <- matrix(data = xyzrgb[,2], ncol = length(xx), byrow = FALSE)
	mb <- matrix(data = xyzrgb[,3], ncol = length(xx), byrow = FALSE)
	fin[,,1] <- mr
	fin[,,2] <- mg
	fin[,,3] <- mb
	fin <- aperm(fin, c(2,1,3)) # This is needed to position the fin correctly
	finras <- as.raster(fin)
	
##### Create the plot using grid graphics

	off <- 0.1 # needed due to vp origin at -0.1
	Lxyz$x <- Lxyz$x + off
	Lxyz$y <- Lxyz$y + off

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


	}