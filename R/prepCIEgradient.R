
prepCIEgradient <- function(vertices = NULL, colSpace = "sRGB", ex = 1.0, ...) {

	# Bryan Hanson, DePauw University, March 2013 hanson@depauw.edu
	# Part of the photoSpec package
	
	xx <- seq(-0.1, 0.9, 0.002) # The raster that will be created must cover the entire plotting region
	yy <- seq(0.9, -0.1, -0.002) # The descending order here is important, but not intuitive
	xyz <- expand.grid(xx,yy)
	names(xyz) <- c("x", "y")
	xyz$z <- (1 - xyz$x - xyz$y)

	# Find the points inside & outside the requested polygon
		
	insideL <- inout(xyz, vertices, bound = FALSE) # TRUE = inside
	outsideL <-!insideL # TRUE = outside now
	
	# Get the colors and size ready
	
	xyzrgb <- convertColor(xyz, from = "XYZ", to = colSpace)
	xyzrgb <- xyzrgb*ex # push the whole color space
    xyzrgb[xyzrgb > 1] <- 1 # This is critical for ex > 1 and size of tongue
	xyzrgb[outsideL,] <- 1.0 # Set the color outside the spectral locus to white
	
	# The actual drawing of the gradient will be done with a rasterGrob
	# We need an array with separate planes for r, g, b
	# It needs to be the size of xy (raster objects are rectangular)
	
	fin <- array(dim = c(length(xx), length(yy), 3))
	names(fin) <- c("x", "y", "rgb")
	
	mr <- matrix(data = xyzrgb[,1], ncol = length(xx), byrow = FALSE)
	mg <- matrix(data = xyzrgb[,2], ncol = length(xx), byrow = FALSE)
	mb <- matrix(data = xyzrgb[,3], ncol = length(xx), byrow = FALSE)
	fin[,,1] <- mr
	fin[,,2] <- mg
	fin[,,3] <- mb
	fin <- aperm(fin, c(2,1,3)) # This is needed to position the fin correctly
	}