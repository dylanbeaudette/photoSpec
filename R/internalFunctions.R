

##### Helper functions

	# Bryan Hanson, DePauw University, March 2013 hanson@depauw.edu
	# Part of the photoSpec package

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

	ang0to2pi <- function(segment) {
		# segment given as c(x1, y1, x2, y2)
		# compute angle relative to horiz axis
		# using the usual unit circle conventions
		# the segments need not intersect using this method
		seg1 <- c(segment[3]-segment[1], segment[4]-segment[2])
		theta <- atan2(seg1[2], seg1[1])
		if (theta < 0) theta <- theta + 2*pi
		return(theta)
		}

	dAB <- function(A, B) { # distance between pts A & B
		# A, B each as c(x, y)
		dAB <- sqrt((B[2]-A[2])^2 + (B[1]-A[1])^2)
		}

	findCIEindex <- function(x) { # find the index of a wavelength in CIExyz
		data(CIExyz)
		i <- grep(x, CIExyz$wavelength)
		ans <- i[1] # this is an index/row number
		}

	rot180aroundD65 <- function(pt, ...) {

		# pt input as c(x,y)
		# This function creates a segment from 'pt' on the spectral locus
		# through D65 and rotates it 180, and lengthens it so that it
		# extends past either the spectral locus or line of purples
		# on the far side
		D65 <- as.numeric(getWhiteValues("D65"))	
		ang <- pi
		fac <- 2.0 # Ensures that the line segment is long enough to reach
		# the far side of the shark fin or line of purples
		x = D65[1] + (cos(ang) * (pt[1] - D65[1]) + sin(ang) * (pt[2] - D65[2]))*fac
		y = D65[2] + (-sin(ang) * (pt[2] - D65[2]) + cos(ang) * (pt[2] - D65[2]))*fac
		return(c(x,y))
		}

	findPolygonIntersection <- function(XY, xy) {

		# XY & xy input as 2 column matrices
		# XY is the points to be tested
		# xy is the vertices of the polygon
		# returns indices of intersection
		D65 <- as.numeric(getWhiteValues("D65"))
		keep <- c()
	
		for (i in 1:nrow(XY)) { # Loop over the line segments in XY
			its <- nrow(xy)-1
		
			# Check to see if the line segments intersect
				
			for (n in 1:its) { # loop over shark fin	
				inter <- doSegmentsIntersect(
			        segment1 = c(XY[i,1], XY[i,2], D65[1], D65[2]),
			        segment2 = c(xy[n,1], xy[n,2], xy[n+1,1], xy[n+1,2]))
			    if (inter) keep <- c(keep, n)
				}
	 		} # end of loop that checks each line segment
	
		return(keep)
		}
	
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

	lineIntersection <- function(x1, y1, x2, y2, x3, y3, x4, y4) {
		
		# Finds the intersection of two lines
		# specified as two line segments
		# Based on code initially written by M. Kukurugya
		
		den <- (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
		if (den == 0) stop("No intersection found")
		numA <- ((x1 * y2) - (y1 * x2))
		numB <- ((x3 * y4) - (y3 * x4))
		num1 <-  numA * (x3 - x4)
		num2 <- (x1 - x2) * numB
		num3 <- numA * (y3 -y4)
		num4 <- (y1 - y2) * numB 
		Px = (num1 - num2)/den
		Py = (num3 - num4)/den
		# cat("Px is", Px, "\n")
		# cat("Py is", Py, "\n")
		return(c(Px, Py))
		}

	
##### End of helper functions