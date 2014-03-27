

##### Internal Helper functions

	# Bryan Hanson, DePauw University, March 2013 hanson@depauw.edu
	# Part of the photoSpec package

##### Misc Functions
##### Other groups of functions below
##### These are not exported

	prepCIEgradient <- function(vertices = NULL, colSpace = "sRGB", ex = 1.0, ...) {
	
		# Bryan Hanson, DePauw University, March 2013 hanson@depauw.edu
		# Part of the photoSpec package
		
		if (is.null(vertices)) stop("prepCIEgradient needs vertices")
		
		# Send out for a high res grid and associated colors
		
		xyzrgb <- grid2rgb(vertices = vertices, colSpace = colSpace, ex = ex, res = 0.002)
		
		# The actual drawing of the gradient will be done with a rasterGrob
		# We need an array with separate planes for r, g, b
		# It needs to be the size of np*np (raster objects are rectangular)
		
		np <- length(seq(-0.1, 0.9, 0.002)) # no. of points used to create grid
		fin <- array(dim = c(np, np, 3))
		names(fin) <- c("x", "y", "rgb")
		
		mr <- matrix(data = xyzrgb[,1], ncol = np, byrow = FALSE)
		mg <- matrix(data = xyzrgb[,2], ncol = np, byrow = FALSE)
		mb <- matrix(data = xyzrgb[,3], ncol = np, byrow = FALSE)
		fin[,,1] <- mr
		fin[,,2] <- mg
		fin[,,3] <- mb
		fin <- aperm(fin, c(2,1,3)) # This is needed to position the fin correctly
		}
	
#####

	makecC <- function(munCols = NULL, rgbCols = NULL,
		hexCols = NULL, namedCols = NULL,
		tidy = FALSE, sort = TRUE)  {
	
		# Bryan Hanson, DePauw University, March 2014 hanson@depauw.edu
		# Part of the photoSpec package
		
		# Use user supplied colors in any format
		# to create a calCols structure
		
		if ( (is.null(munCols)) & (is.null(rgbCols)) &
			(is.null(hexCols)) & (is.null(namedCols)) ) {
			stop("No colors provided. Please give munCols, rgbCols, hexCols or namedCols")
			}
	
		# Currently NO check to see if more than one color specification is given
		# In this case the last processed one will be the return value
			
		if (!is.null(munCols)) { # Munsell colors are potentially out of gamut
			cm <- munCols # unique is used in case duplicates are created (see fix_mnsl)
			ch <- unique(mnsl2hex(munCols, fix = TRUE)) # out of gamut will be NA & warning is issued w/o fix = T
			crgb <- hex2RGB(ch)@coords
			
			msg <- paste("Total colors attempted:", length(munCols), "of which", length(ch), "were in gamut", sep = " ")
			message(msg)
			}
	
		if (!is.null(rgbCols)) {
			crgb <- rgbCols
			cm <- rgb2mnsl(crgb)
			ch <- mnsl2hex(cm, fix = FALSE) # FALSE is default but should not occur
			}
	
		if (!is.null(hexCols)) {
			ch <- hexCols
			crgb <- hex2RGB(ch)@coords
			cm <- rgb2mnsl(crgb)	
			}
	
		if (!is.null(namedCols)) {
			ch <- ColToHex(namedCols)
			crgb <- col2rgb(namedCols)
			cm <- rgb2mnsl(crgb)
			}
	
	# Assemble list for return
	
	calCols <- vector("list")
	calCols$Munsell <- cm
	calCols$rgb <- crgb
	calCols$hexcol <- ch

	if (tidy) {
		
		}

	if (sort) {
		
		}
	
	return(calCols)
	}
	
#####

	xy2cC <- function(wedge, res = 0.02, colSpace = "sRGB", ex = 1.0, ...) {
		
		# Bryan Hanson, DePauw University, May 2013 hanson@depauw.edu
		# Part of the photoSpec package
	
		# This function originated in v 3.0 as selectCalCols.  Brought back with modifications
		# for v 5.0-3 in March 2014.
		
		# Function to process CIE xy colors selected by the user and
		# reduce them in a sensible way to a limited no. of values for
		# use as calibration colors
				
		colSpace <- wedge$colSpace
		ex <- wedge$ex
		verts <- wedge$verts
		
		xyz <- prepGrid(vertices = verts, res = res)
		xyz <- xyz[xyz$inside,]
		
		# Add to diagram (is this the best place?)
		grid.points(xyz$x, xyz$y, default.units = "native", size = unit(0.25, "char"))
	
		# Convert to other color spaces
		xyz <- xyz[,-4]
		rgbCols <- convertColor(xyz, from = "XYZ", to = colSpace)
		calCols <- makecC(rgbCols = rgbCols)
		return(calCols)

		} # end of function

#####

	hex2CIExy <- function(somecols) {
		# Convert to CIE xy  * this approach ignores brightness*
		rgb <- t(col2rgb(somecols$hex)/255)
		XYZ <- convertColor(rgb, from = "sRGB", to = "XYZ")
		x <- XYZ[,1]/rowSums(XYZ)
		y <- XYZ[,2]/rowSums(XYZ)
		cie <- cbind(x, y)
		cie
		}

#####

	grid2rgb <- function(vertices = NULL, colSpace = "sRGB", ex = 1.0, res = 0.002) {
	
		# Bryan Hanson, DePauw University, March 2014 hanson@depauw.edu
		# Part of the photoSpec package
		# Helper function to lay a grid over the CIE space and return
		# rgb colors w/i a specified set of vertices
				
		# Go get the grid
		
		xyz <- prepGrid(vertices = vertices, res = res)
		outsideL <- !xyz$inside
		xyz <- xyz[,-4]
		
		# Get the colors ready and postion correct
		
		xyzrgb <- convertColor(xyz, from = "XYZ", to = colSpace)
		xyzrgb <- xyzrgb*ex # push the whole color space
	    xyzrgb[xyzrgb > 1] <- 1 # This is critical for ex > 1 and size of tongue
		xyzrgb[outsideL,] <- 1.0 # Set the color outside the spectral locus to white
		return(xyzrgb)
		}

#####

	prepGrid <- function(vertices = NULL, res = 0.02) {
	
		# Bryan Hanson, DePauw University, March 2014 hanson@depauw.edu
		# Part of the photoSpec package
		
		# Helper function to lay a grid over the CIE space
		# at a given resolution, then note the area w/i the vertices w/a logical vector
		# The z coordinate must be kept for certain color conversions in other functions
		
		x <- seq(-0.1, 0.9, res) # The raster that will be created must cover the entire plotting region
		y <- seq(0.9, -0.1, -res) # The descending order here is important, but not intuitive
		xyz <- expand.grid(x,y)
		names(xyz) <- c("x", "y")
		xyz$z <- (1 - xyz$x - xyz$y)
	
		# Find the points inside the requested polygon
			
		insideL <- inout(xyz, vertices, bound = FALSE) # TRUE = inside
		xyz$inside <- insideL
		return(xyz)
		}

#####
		
	getRGorB <- function(hexvector = NULL, channel = "R") {
	
		# Function to take a vector of hex values and return just the
		# selected channel on a 0...1 scale
		
		# Check some basic things
		if (is.null(hexvector)) stop("Must provide a hexadecimal value")
		if (!identical(nchar(hexvector), rep(7L, length(hexvector)))) stop("hexvector should be of form #abcdef")
	
		# Get the selected channel and convert to integer, then to 0...1 scale
		
		if (channel == "R") {
			ans <- substr(hexvector, 2, 3)
			ans <- strtoi(ans, 16L)/255
			}
	
		if (channel == "G") {
			ans <- substr(hexvector, 4, 5)
			ans <- strtoi(ans, 16L)/255
			}
	
		if (channel == "B") {
			ans <- substr(hexvector, 6, 7)
			ans <- strtoi(ans, 16L)/255
			}
	
		ans
		}

#####

	ccp <- function(cie, gamut) { # Calc color purity
		# cie is matrix of colors giving cie xy

		# Get needed data
		ns <- nrow(cie)
		
		# cat("Hello from ccp\n")
		# cat("ns = ", ns, "\n")
		# cat("cie =")
		# print(cie)

		D65 <- getWhiteValues("D65")
		if (gamut == "sl") { # spectral locus data (shark fin)
			pg <- CIExyz[,c(2,3)] # 441 rows
			pg <- rbind(pg, pg[1,]) # repeat row so that polygon can close
			}
	
		if (gamut == "sRGB") { # device color space
			pg <- getGamutValues("sRGB")
			pg <- rbind(pg, pg[1,]) # repeat row so that polygon can close
			}

		cie2 <-  extendAndRotateAroundD65(cie) # defaults: simply extends
		hits <- findPolygonIntersection(XY = cie2, xy = pg) # indices of intersections
		if (length(hits) != ns) {
			message("Wrong number of gamut intersections.")
			message("\tIf using lambdas, move the problem lambda by 1 nm either way.")
			stop("May have encountered a zero length line segment.")
			}

		dc <- dAB(D65, cie) # distance from D65 to color
		where <- lineIntersection(D65[1], D65[2], cie2[,1], cie2[,2],
			pg[hits, 1], pg[hits, 2], pg[hits + 1, 1], pg[hits + 1, 2])
		dg <- dAB(D65, where) # distance from D65 to gamut
		# Final calc
		cp <- dc*100/dg # these are by definition always (+)
		for (n in 1:length(cp)) {
			if (cp[n] > 100) cp[n] <- NA # these are out of gamut
			}
		cp
		}

#####

	sortFromWhite <- function(Cols) { # sort colors based upon distance from white
		# Cols should be a vector of hexadecimal colors
		oCols <- Cols # save the original
		Cols <- col2rgb(Cols) # now in rgb
	    whdist <- apply(Cols, 2, function(z) {sqrt(sum((z - 255)^2))})
		whdist <- 100*whdist/(1.73*255) # gives answer a percentage of max dist
		# max distance is from pure white to complete black
		# namely the major diagonal of a cube, which is 1.73
		# return Cols as hex sorted by distance from white (pale to dark)
		# needs package plyr
		df <- data.frame(cols = oCols, dist = whdist)
		df <- arrange(df, dist)
		return(as.character(df$cols))
		}

#####

	dAB <- function(A, B) { # Euclidian distance between pts A & B
		# Vectorized
		# A, B each as data frames with columns x, y (not necessarily named)
		# return value is a vector of distances
		dAB <- sqrt((B[,2]-A[,2])^2 + (B[,1]-A[,1])^2)
		}

#####

	findCIEindex <- function(x) { # find the index of a wavelength in CIExyz
		# This version is vectorized. It
		# returns the indicies of the first occurance
		# of each x entry in CIExyz$wavelength
		data(CIExyz)
		i <- match(x, CIExyz$wavelength, nomatch = NA)

		if (any(is.na(i))) { # give the user some troubleshooting info
			bad <- which(is.na(i))
			if (length(bad) == 1) {
				msg <- paste("Wavelength", x[bad], "is out of range on the CIE diagram")
				stop(msg)
				}
			
			if (length(bad) > 1) {
				allbad <- paste(x[bad], collapse = ",")
				msg <- paste("Wavelengths", allbad, "are out of range on the CIE diagram")
				stop(msg)
				}
			}
		i
		}

##### Functions dealing with vectors, polygons and intersections (used in CIE functions)

	extendAndRotateAroundD65 <- function(pts, ang = 0.0, fac = 200) {
		# Vectorized (unit test available)
		# pts is a data frame or matrix of x, y coords in columns
		# This function takes a segment from each pt to D65
		# and lengthens it by fac so that it extends past either
		# the spectral locus or line of purples.
		# Can also rotate around D65 by ang (in radians)
		D65 <- as.numeric(getWhiteValues("D65"))
		x = D65[1] + ((cos(ang) * (pts[,1] - D65[1])) - (sin(ang) * (pts[,2] - D65[2])))*fac
		y = D65[2] + ((sin(ang) * (pts[,1] - D65[1])) + (cos(ang) * (pts[,2] - D65[2])))*fac
		return(data.frame(x = x, y = y))
		}

#####

	findPolygonIntersection <- function(XY, xy) {

		# XY & xy input as 2 column matrices
		# XY are one end of a segment starting at D65
		# xy are the vertices of the polygon
		# treated as true line segments which are not extended
		# returns indices of intersection for xy
				
		D65 <- as.numeric(getWhiteValues("D65"))
		keep <- c()

		TS <- FALSE # troubleshooting flag
		
	 	if (TS) cat("\nHello from findPolygonIntersection\n")
#	 	if (TS) {cat("\nXY = \n"); print(XY)}
	
		for (i in 1:nrow(XY)) { # Loop over the line segments in XY
			its <- nrow(xy)-1 # xy already has an extra row to close the polygon
			
			# Check to see if the line segments intersect
			
			# If a point is in the corner of the polygon
			# and there is an intersection, it will be counted
			# twice.  Once an intersection is found, check
			# for this condition and adjust the indices
			
			inter <- FALSE # intersection found
			corner <- FALSE # intersection at polygon vertex/corner

		 	if (TS) cat("Checking sample point ", i, "\n")
			
			for (n in 1:its) { # loop over xy (polygon)

				inter <- doSegmentsIntersect(
			        segment1 = c(XY[i,1], XY[i,2], D65[1], D65[2]),
			        segment2 = c(xy[n,1], xy[n,2], xy[n+1,1], xy[n+1,2]))		
			    
			    #if (TS) cat("\tChecking polygon segment ", n, "\n")

				# if ((!inter) & (TS)) {
					# cat("Segments:\n")
					# print(c(XY[i,1], XY[i,2], D65[1], D65[2]))
					# print(c(xy[n,1], xy[n,2], xy[n+1,1], xy[n+1,2]))	
				# }
									
			    if (inter) {
			    	# Check for intersection at a polygon vertex (pure red in sRBG for instance)
			    	# In this case the the point of intersection is at the end
			    	# of one polygon segment and at the beginning of the next
			    	# It is sufficient to check that the point is on the end of one
			    	# segment; by definition it will also be at the beginning of the next segment
			    	
			    	where <- lineIntersection(XY[i,1], XY[i,2], D65[1], D65[2],
			    		xy[n,1], xy[n,2], xy[n+1,1], xy[n+1,2])
			    	#print(str(where))
			    	#
			    	# if (TS) {
			    		# plotCIEchrom()
			    		# grid.points(x = where$x, y = where$y, gp = gpar(col = "red"))
			    		# }
			    	pd <- as.numeric(where) - c(xy[n+1,1], xy[n+1,2])
			    	msg <- paste("\tThere was an intersection at XY sample no.", i, "and xy polygon segment no.", n)
			    	if (TS) message(msg)
			    	keep <- c(keep, n)
			    	corner <- isTRUE(all.equal(pd, c(0.0, 0.0)))
			    	if ((corner) & (TS)) message("\t\t & it was at a vertex")
			    	if (corner) keep <- keep[-length(keep)] # remove latest entry (double-counted)
			    	}
				}
	 		} # end of loop that checks each XY segment

	 	if (TS) cat("keep = ", keep, "\n")
		return(keep)
		}

#####

	lineIntersection <- function(x1, y1, x2, y2, x3, y3, x4, y4) {
		
		# Finds the intersection of two lines
		# specified as two line segments (so they are 'extended')
		# Based on code initially written by M. Kukurugya
		# Vectorized
		x1 <- as.numeric(x1) # some inputs are data frames
		x2 <- as.numeric(x2)
		x3 <- as.numeric(x3)
		x4 <- as.numeric(x4)
		y1 <- as.numeric(y1)
		y2 <- as.numeric(y2)
		y3 <- as.numeric(y3)
		y4 <- as.numeric(y4)
		
		den <- (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
		for (n in 1:length(den)) {
			if (den[n] == 0) warning("lineIntersection failed to find an intersection")
			}
		numA <- ((x1 * y2) - (y1 * x2))
		numB <- ((x3 * y4) - (y3 * x4))
		num1 <-  numA * (x3 - x4)
		num2 <- (x1 - x2) * numB
		num3 <- numA * (y3 -y4)
		num4 <- (y1 - y2) * numB 
		Px = (num1 - num2)/den
		Py = (num3 - num4)/den
		return(data.frame(x = Px, y = Py))
		}

#####

	pointOnLineNearPoint <- function(Px, Py, slope, intercept) {
		# Vectorized
		Ax <- Px*10 # push the point out
	 	Bx <- Px*-10
	 	Ay <- Ax * slope + intercept
	 	By <- Bx * slope + intercept
	 	ans <- pointOnLine(Px, Py, Ax, Ay, Bx, By)
	 	ans
		}

#####
	
	pointOnLine <- function(Px, Py, Ax, Ay, Bx, By) {
		# Vectorized
		PB <- data.frame(x = Px - Bx, y = Py - By)
		AB <- data.frame(x = Ax - Bx, y = Ay - By)
		PB <- as.matrix(PB)
		AB <- as.matrix(AB)
		k_raw <- k <- c()
		for (n in 1:nrow(PB)) {
			k_raw[n] <- (PB[n,] %*% AB[n,])/(AB[n,] %*% AB[n,])
			if (k_raw[n] < 0)  { k[n] <- 0
				} else { if (k_raw[n] > 1) k[n] <- 1
					else k[n] <- k_raw[n] }
			}
		x = (k * Ax + (1 - k)* Bx)
		y = (k * Ay + (1 - k)* By)
		ans <- data.frame(x, y)
		ans
		}

##### These functions support the geometry-oriented functions above
##### gist.github.com/bryanhanson/5471173 has more info on these next ones

	getBoundingBox <- function(P0, P1) {
	
	    # P0, P1 each have c(x,y)
	    # ll = lower left
	    # ur = upper right
	
	    llx <- min(P0[1], P1[1])
	    lly <- min(P0[2], P1[2])
	    urx <- max(P0[1], P1[1])
	    ury <- max(P0[2], P1[2])
	
	    bb <- c(llx, lly, urx, ury)
	    }
	
	doBoxesIntersect <- function(box1, box2) {
	    
	    ans <- FALSE
	    chk1 <- box1[1] <= box2[3]
	    chk2 <- box1[3] >= box2[1]
	    chk3 <- box1[2] <= box2[4]
	    chk4 <- box1[4] >= box2[2]
	    if (chk1 && chk2 && chk3 && chk4) ans <- TRUE
	    ans
	    }
	
	isPointOnLine <- function(segment, point) {
		# segment is c(x1, y1, x2, y2)
		# translate segment to origin
		newseg <- c(0.0, 0.0, segment[3] - segment[1], segment[4] - segment[2])
		newpt <- c(point[1] - segment[1], point[2] - segment[2])
		# calc a modified cross product:
		# a.x * b.y - b.x * a.y
		# if zero, point is on segment
		# basically, you have two vectors sharing 0,0 as one end
		ans <- newseg[3]*newpt[2] - newpt[1]*newseg[4]
		return(isTRUE(all.equal(abs(ans), 0)))
		}
	
	isPointRightOfLine <- function(segment, point) {
		# see notes in isPointOnLine
		newseg <- c(0.0, 0.0, segment[3] - segment[1], segment[4] - segment[2])
		newpt <- c(point[1] - segment[1], point[2] - segment[2])
		ans <- newseg[3]*newpt[2] - newpt[1]*newseg[4]
		return(ans < 0)
		}
	
	lineSegmentTouchesOrCrossesLine <- function(segment1, segment2) {
		# segments given as c(x1, y1, x2, y2)
		ans <- 	(isPointOnLine(segment1, segment2[1:2]) ||
				isPointOnLine(segment1, segment2[3:4]) ||
				xor(isPointRightOfLine(segment1, segment2[1:2]),
				isPointRightOfLine(segment1, segment2[3:4])))
		return(ans)
		}
	
	doSegmentsIntersect <- function(segment1, segment2) {
		# segments given as c(x1, y1, x2, y2)
		box1 <- getBoundingBox(segment1[1:2], segment1[3:4])
		box2 <- getBoundingBox(segment2[1:2], segment2[3:4])
		return(doBoxesIntersect(box1, box2) &&
			lineSegmentTouchesOrCrossesLine(segment1, segment2) &&
			lineSegmentTouchesOrCrossesLine(segment2, segment1))
		}
	
##### End of helper functions