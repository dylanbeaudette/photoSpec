

##### Helper functions

	# Bryan Hanson, DePauw University, March 2013 hanson@depauw.edu
	# Part of the photoSpec package

	hex2CIExy <- function(somecols) {
		# Convert to CIE xy  * this approach ignores brightness*
		rgb <- t(col2rgb(somecols$hex)/255)
		XYZ <- convertColor(rgb, from = "sRGB", to = "XYZ")
		x <- XYZ[,1]/rowSums(XYZ)
		y <- XYZ[,2]/rowSums(XYZ)
		cie <- cbind(x, y)
		cie
		}
		
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

	# End of helper function
			

	pointOnLineNearPoint <- function(Px, Py, slope, intercept) {
		# Vectorized
		Ax <- Px*10 # push the point out
	 	Bx <- Px*-10
	 	Ay <- Ax * slope + intercept
	 	By <- Bx * slope + intercept
	 	ans <- pointOnLine(Px, Py, Ax, Ay, Bx, By)
	 	ans
		}
	
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

	# ccp <- function(cie, gamut) { # Calc color purity
		# # cie is matrix of colors giving cie xy

		# # Get needed data
		# ns <- nrow(cie)
		# D65 <- getWhiteValues("D65")	
		# if (gamut == "sl") { # spectral locus data (shark fin)
			# pg <- CIExyz[,c(2,3)] # 4400 rows
			# pg <- rbind(pg, pg[1,]) # repeat row so that polygon can close
			# }
	
		# if (gamut == "sRGB") { # device color space
			# pg <- getGamutValues("sRGB")
			# pg <- rbind(pg, pg[1,]) # repeat row so that polygon can close
			# }

		# cie2 <-  extendAndRotateAroundD65(cie) # defaults: simply extends
		# hits <- findPolygonIntersection(XY = cie2, xy = pg) # indices of intersections
		# if (length(hits) != ns) stop("Wrong number of gamut intersections")

		# cp <- rep(NA, ns)
		# for (i in 1:ns) {
			# dc <- dAB(D65, cie[i,]) # distance from D65 to color
			# ndx <- hits[i]
			# where <- lineIntersection(D65[1], D65[2], cie2[i,1], cie2[i,2],
				# pg[ndx, 1], pg[ndx, 2], pg[ndx + 1, 1], pg[ndx + 1, 2])
			# dg <- dAB(D65, where) # distance from D65 to gamut
			# # Final calc
			# cp[i] <- dc*100/dg # these are by definition always (+)
			# if (cp[i] > 100) cp[i] <- NA # these are out of gamut
			# }
		# cp
		# }

	ccp <- function(cie, gamut) { # Calc color purity
		# cie is matrix of colors giving cie xy

		# Get needed data
		ns <- nrow(cie)
		D65 <- getWhiteValues("D65")	
		if (gamut == "sl") { # spectral locus data (shark fin)
			pg <- CIExyz[,c(2,3)] # 4400 rows
			pg <- rbind(pg, pg[1,]) # repeat row so that polygon can close
			}
	
		if (gamut == "sRGB") { # device color space
			pg <- getGamutValues("sRGB")
			pg <- rbind(pg, pg[1,]) # repeat row so that polygon can close
			}

		cie2 <-  extendAndRotateAroundD65(cie) # defaults: simply extends
		hits <- findPolygonIntersection(XY = cie2, xy = pg) # indices of intersections
		if (length(hits) != ns) stop("Wrong number of gamut intersections")

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

	# dAB <- function(A, B) { # Euclidian distance between pts A & B
		# # NOT vectorized
		# # A, B each as c(x, y)
		# dAB <- sqrt((B[2]-A[2])^2 + (B[1]-A[1])^2)
		# }

	dAB <- function(A, B) { # Euclidian distance between pts A & B
		# Vectorized
		# A, B each as data frames with columns x, y (not necessarily named)
		# return value is a vector of distances
		dAB <- sqrt((B[,2]-A[,2])^2 + (B[,1]-A[,1])^2)
		}

	findCIEindex <- function(x) { # find the index of a wavelength in CIExyz
		# NOT vectorized
		data(CIExyz)
		i <- grep(x, CIExyz$wavelength)
		ans <- i[1] # index/row number of the 1st occurance
		}

	findCIEindex2 <- function(x) { # find the index of a wavelength in CIExyz
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

	findPolygonIntersection <- function(XY, xy) {

		# XY & xy input as 2 column matrices
		# XY are one end of a segment starting at D65
		# xy are the vertices of the polygon
		# treated as true line segments which are not extended
		# returns indices of intersection for xy
				
		D65 <- as.numeric(getWhiteValues("D65"))
		keep <- c()
	
		for (i in 1:nrow(XY)) { # Loop over the line segments in XY
			its <- nrow(xy)-1
		
			# Check to see if the line segments intersect
			
			# If a point is in the corner of the polygon
			# and there is an intersection, it will be counted
			# twice.  Once an intersection is found, check
			# for this condition and adjust the indices
			
			for (n in 1:its) { # loop over xy (polygon)
				inter <- doSegmentsIntersect(
			        segment1 = c(XY[i,1], XY[i,2], D65[1], D65[2]),
			        segment2 = c(xy[n,1], xy[n,2], xy[n+1,1], xy[n+1,2]))
			    if (inter) { # check for intersection at a corner
			    	keep <- c(keep, n)
			    	# cat("inter is TRUE\n")
			    	# cat("its = ", its, "\n")
			    	# cat("i = ", i, "\n\n")
			    	twoInt <- isPointOnLine(c(XY[i,1], XY[i,2], D65[1], D65[2]), c(xy[n,1], xy[n,2]))
			    	if (twoInt) keep <- {
			    		cat("twoInt is TRUE\n")
			    		keep[-length(keep)]
			    		}
			    	}
				}
	 		} # end of loop that checks each XY segment
	
		return(keep)
		}

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

	# lineIntersection <- function(x1, y1, x2, y2, x3, y3, x4, y4) {
		
		# # Finds the intersection of two lines
		# # specified as two line segments (so they are 'extended')
		# # Based on code initially written by M. Kukurugya
		
		# den <- (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
		# if (den == 0) {
			# warning("No intersection found")
			# return(c(NA, NA))
			# }
		# numA <- ((x1 * y2) - (y1 * x2))
		# numB <- ((x3 * y4) - (y3 * x4))
		# num1 <-  numA * (x3 - x4)
		# num2 <- (x1 - x2) * numB
		# num3 <- numA * (y3 -y4)
		# num4 <- (y1 - y2) * numB 
		# Px = (num1 - num2)/den
		# Py = (num3 - num4)/den
		# # cat("Px is", Px, "\n")
		# # cat("Py is", Py, "\n")
		# return(c(Px, Py))
		# }

# gist.github.com/bryanhanson/5471173 has more info on these next ones

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