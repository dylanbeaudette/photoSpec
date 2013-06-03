

selectCIExy <- function(L1 = NULL, L2 = NULL, colSpace = "sRGB", ff = 1.0, ...) {

	# Bryan Hanson, DePauw University, March 2013 hanson@depauw.edu
	# Part of the photoSpec package
	# Major contributions from Matthew Kukurugya

	diagnostics <- TRUE # maybe make this an argument
	
##### Helper Functions #####

	dAB <- function(A, B) { # distance between pts A & B
		# A, B each as c(x, y)
		dAB <- sqrt((B[2]-A[2])^2 + (B[1]-A[1])^2)
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

##### End of Helper Functions #####

	# Check input data
	if (is.null(L1)) stop("You must give a wavelength for L1")
	if (is.null(L2)) stop("You must give a wavelength for L2")
	if (L1 > L2) stop("L1 must be less than L2")

	# Re-order things so that L1 < L2 < L3 in what comes later
	L3 <- L2	
	L2 <- L1 + 0.5*(L2-L1) # This will be the mid point
	
	# Load spectral locus data (shark fin)
	data(CIExyz)
	xy <- CIExyz[,c(2,3)] # 4400 rows
	xy <- rbind(xy, xy[1,]) # repeat row so that polygon can close

	# Find the coordinates of the input wavelengths
	ans1 <- grep(L1, CIExyz$wavelength)
	ans1 <-ans1[1] # this is an index/row number
	ans2 <- grep(L2, CIExyz$wavelength)
	ans2 <-ans2[1]
	ans3 <- grep(L3, CIExyz$wavelength)
	ans3 <-ans3[1]
	D65 <- getWhiteValues("D65")

	# Put all the needed data in one place: p1 from L1, p2 from L2, p3 from L3
	segs <- data.frame(
		x = c(xy[ans1,1], xy[ans2,1], xy[ans3,1], D65[1,1]),
		y = c(xy[ans1,2], xy[ans2,2], xy[ans3,2], D65[1,2]))
	row.names(segs) <- c("p1", "p2", "p3", "D65")
	in.segs <- segs # save a copy for later
	
	# Flip the line segments 180 degrees and extend to ensure intersection
	# FIX: some of these terms are zero and can be dropped
	
	ang <- pi
	fac <- 2.0 # Ensures that the line segment is long enough to reach
	# the far side of the shark fin or line of purples
	segs[1,1] = segs[4,1] + (cos(ang) * (segs[1,1] - segs[4,1]) + sin(ang) * (segs[1,2] - segs[4,2]))*fac
	segs[1,2] = segs[4,2] + (-sin(ang) * (segs[1,2] - segs[4,2]) + cos(ang) * (segs[1,2] - segs[4,2]))*fac
	
	segs[2,1] = segs[4,1] + (cos(ang) * (segs[2,1] - segs[4,1]) + sin(ang) * (segs[2,2] - segs[4,2]))*fac
	segs[2,2] = segs[4,2] + (-sin(ang) * (segs[2,2] - segs[4,2]) + cos(ang) * (segs[2,2] - segs[4,2]))*fac

	segs[3,1] = segs[4,1] + (cos(ang) * (segs[3,1] - segs[4,1]) + sin(ang) * (segs[3,2] - segs[4,2]))*fac
	segs[3,2] = segs[4,2] + (-sin(ang) * (segs[3,2] - segs[4,2]) + cos(ang) * (segs[3,2] - segs[4,2]))*fac
	row.names(segs) <- c("p4", "p5", "p6", "D65")
	
	# Now loop over shark fin and check for intersections	

	keep <- c() # keep will always be length 3

	for (i in 1:3) { # Loop over the 3 line segments
		its <- nrow(xy)-1
	
		# Check to see if the line segments intersect
			
		for (n in 1:its) { # loop over shark fin	
			inter <- doSegmentsIntersect(
		        segment1 = c(segs[i,1], segs[i,2], segs[4,1], segs[4,2]),
		        segment2 = c(xy[n,1], xy[n,2], xy[n+1,1], xy[n+1,2]))
		    if (inter) keep <- c(keep, n)
			}
 		} # end of loop that checks each line segment

	# Here are the cases to be considered next.
	# Segments are in wavelength order.
	# 1.  no line segment intersects the line of purples.
	# 2.  segment 1 hits the l of p, segments 2 & 3 the shark fin
	# 3.  segments 1 & 2 hit the l of p, segment 3 the shark fin
	# 4.  all segments intersect the l of p
	# 5.  only the middle segments hits the l of p
	# 6.  segment 1 hits the shark fin, the other two l of p
	# 7.  segments 1 & 2 hit shark fin, segment 3 the l of p
	# The line of purples is segment xy[c(4400,4401),]

	# Store the intersection points p4, p5 & p6:
	# L1/p1 -> D65 -> p4; L2/p2 -> D65 -> p5; L3/p3 -> D65 -> p6
	# Record the 'case' and the point in the wedge farthest
	# from D65 (xPt)
	
	Case1 <- Case2 <- Case3 <- Case4 <- Case5 <- Case6 <- Case7 <- FALSE
	Case <- NA
	xPt <- vector("numeric", 2)
	
	if (!4400 %in% keep) Case1 <- TRUE
	if ((keep[1] == 4400) && (!keep[2] == 4400) && (!keep[3] == 4400)) Case2 <- TRUE 	
	if ((keep[1] == 4400) && (keep[2] == 4400) && (!keep[3] == 4400)) Case3 <- TRUE 	
	if ((keep[1] == 4400) && (keep[2] == 4400) && (keep[3] == 4400)) Case4 <- TRUE
	if ((!keep[1] == 4400) && (keep[2] == 4400) && (!keep[3] == 4400)) Case5 <- TRUE 	
	if ((!keep[1] == 4400) && (keep[2] == 4400) && (keep[3] == 4400)) Case6 <- TRUE 	
	if ((!keep[1] == 4400) && (!keep[2] == 4400) && (keep[3] == 4400)) Case7 <- TRUE 	
	
	if (Case1) { # Case 1 (OK)
		Case <- "Case1"
	 	verts <- rbind(CIExyz[keep[1]:keep[3],], CIExyz[keep[3]+1,], c(NA, segs[4,1], segs[4,2], NA))
	 	verts <- verts[,2:3]
	 	p4 <- CIExyz[keep[1],2:3]
	 	p5 <- CIExyz[keep[2],2:3]
	 	p6 <- CIExyz[keep[3],2:3]
	 	# Figure xPt
	 	d4 <- dAB(D65, p4)
	 	d5 <- dAB(D65, p5)
	 	d6 <- dAB(D65, p6)
	 	l <- c(d4, d5, d6)
	 	dm <- which.max(l)
	 	if (dm == 1) xPt <- p4
	 	if (dm == 2) xPt <- p5
	 	if (dm == 3) xPt <- p6
 	 	}
		
	if (Case2) { # Case 2 (OK)
		Case <- "Case2"
		p4 <- lineIntersection(in.segs[1,1], in.segs[1,2], in.segs[4,1],
			in.segs[4,2], xy[4400,1], xy[4400,2], xy[4401,1], xy[4401,2])
	 	p5 <- CIExyz[keep[2],2:3]
	 	p6 <- CIExyz[keep[3],2:3]	 	
	 	verts <- rbind(c(NA, segs[4,1], segs[4,2], NA),
	 		c(NA, p4[1], p4[2], NA), CIExyz[1:keep[3],])
	 	verts <- verts[,2:3]
	 	# Figure xPt
	 	d4 <- dAB(D65, p4)
	 	d5 <- dAB(D65, p5)
	 	d6 <- dAB(D65, p6)
	 	xy1 <- dAB(D65, CIExyz[1,2:3])
	 	l <- c(d4, d5, d6, xy1)
	 	dm <- which.max(l)
	 	if (dm == 1) xPt <- p4
	 	if (dm == 2) xPt <- p5
	 	if (dm == 3) xPt <- p6
	 	if (dm == 4) xPt <- CIExyz[1,2:3]
		}
		
	if (Case3) { # Case 3 (OK)
		Case <- "Case3"
		p4 <- lineIntersection(in.segs[1,1], in.segs[1,2], in.segs[4,1],
			in.segs[4,2], xy[4400,1], xy[4400,2], xy[4401,1], xy[4401,2])
		p5 <- lineIntersection(in.segs[2,1], in.segs[2,2], in.segs[4,1],
			in.segs[4,2], xy[4400,1], xy[4400,2], xy[4401,1], xy[4401,2])
	 	p6 <- CIExyz[keep[3],2:3]	 	
	 	verts <- rbind(c(NA, segs[4,1], segs[4,2], NA),
	 		c(NA, p4[1], p4[2], NA), CIExyz[1:keep[3],])
	 	verts <- verts[,2:3]
	 	# Figure xPt
	 	d4 <- dAB(D65, p4)
	 	d5 <- dAB(D65, p5)
	 	d6 <- dAB(D65, p6)
	 	xy1 <- dAB(D65, CIExyz[1,2:3])
	 	l <- c(d4, d5, d6, xy1)
	 	dm <- which.max(l)
	 	if (dm == 2) xPt <- p5
	 	if (dm == 3) xPt <- p6
	 	if (dm == 4) xPt <- CIExyz[1,2:3]
		}

	if (Case4) { # Case 4 (OK)
		Case <- "Case4"
		p4 <- lineIntersection(in.segs[1,1], in.segs[1,2], in.segs[4,1],
			in.segs[4,2], xy[4400,1], xy[4400,2], xy[4401,1], xy[4401,2])
		p5 <- lineIntersection(in.segs[2,1], in.segs[2,2], in.segs[4,1],
			in.segs[4,2], xy[4400,1], xy[4400,2], xy[4401,1], xy[4401,2])
		p6 <- lineIntersection(in.segs[3,1], in.segs[3,2], in.segs[4,1],
			in.segs[4,2], xy[4400,1], xy[4400,2], xy[4401,1], xy[4401,2])
		verts <- rbind(segs[4,], p4, p6)
		colnames(verts) <- c("x", "y")
	 	# Figure xPt
	 	d4 <- dAB(D65, p4)
	 	d5 <- dAB(D65, p5)
	 	d6 <- dAB(D65, p6)
	 	l <- c(d4, d5, d6)
	 	dm <- which.max(l)
	 	if (dm == 1) xPt <- p4
	 	if (dm == 2) xPt <- p5
	 	if (dm == 3) xPt <- p6
		}

	if (Case5) { # Case 5 (OK)
		Case <- "Case5"
	 	p4 <- CIExyz[keep[1],2:3]
		p5 <- lineIntersection(in.segs[2,1], in.segs[2,2], in.segs[4,1],
			in.segs[4,2], xy[4400,1], xy[4400,2], xy[4401,1], xy[4401,2])
	 	p6 <- CIExyz[keep[3],2:3]
	 	verts <- rbind(CIExyz[keep[3]:4400,], CIExyz[1:keep[1],], c(NA, segs[4,1], segs[4,2], NA))
	 	verts <- verts[,2:3]
	 	# Figure xPt
	 	d4 <- dAB(D65, p4)
	 	d5 <- dAB(D65, p5)
	 	d6 <- dAB(D65, p6)
	 	xy1 <- as.numeric(CIExyz[1,2:3])
	 	xy4400 <- as.numeric(CIExyz[4400,2:3])
	 	dxy1 <- dAB(D65, xy1)
	 	dxy4400 <- dAB(D65, xy4400)
	 	l <- c(d4, d5, d6, dxy1, dxy4400)
	 	dm <- which.max(l)
	 	if (dm == 1) xPt <- p4
	 	if (dm == 2) xPt <- p5
	 	if (dm == 3) xPt <- p6
	 	if (dm == 4) xPt <- xy1
	 	if (dm == 5) xPt <- xy4400
	 	}

	if (Case6) { # Case 6 (OK)
		Case <- "Case6"
	 	p4 <- CIExyz[keep[1],2:3]
		p5 <- lineIntersection(in.segs[2,1], in.segs[2,2], in.segs[4,1],
			in.segs[4,2], xy[4400,1], xy[4400,2], xy[4401,1], xy[4401,2])
		p6 <- lineIntersection(in.segs[3,1], in.segs[3,2], in.segs[4,1],
			in.segs[4,2], xy[4400,1], xy[4400,2], xy[4401,1], xy[4401,2])
	 	verts <- rbind(CIExyz[keep[1]:4400,], c(NA, p5[1], p5[2], NA),
	 		c(NA, p6[1], p6[2], NA), c(NA, segs[4,1], segs[4,2], NA))
	 	verts <- verts[,2:3]
	 	# Figure xPt
	 	d4 <- dAB(D65, p4)
	 	d5 <- dAB(D65, p5)
	 	d6 <- dAB(D65, p6)
	 	xy1 <- as.numeric(CIExyz[1,2:3])
	 	xy4400 <- as.numeric(CIExyz[4400,2:3])
	 	dxy1 <- dAB(D65, xy1)
	 	dxy4400 <- dAB(D65, xy4400)
	 	l <- c(d4, d5, d6, dxy1, dxy4400)
	 	dm <- which.max(l)
	 	if (length(xPt) > 1) xPt <- xPt[1] # In the event of a tie
	 	if (dm == 1) xPt <- p4
	 	if (dm == 2) xPt <- p5
	 	if (dm == 3) xPt <- p6
	 	if (dm == 4) xPt <- xy1
	 	if (dm == 5) xPt <- xy4400
 	 	}

	if (Case7) { # Case 7 (OK)
		Case <- "Case7"
	 	p4 <- CIExyz[keep[1],2:3]
	 	p5 <- CIExyz[keep[2],2:3]
		p6 <- lineIntersection(in.segs[3,1], in.segs[3,2], in.segs[4,1],
			in.segs[4,2], xy[4400,1], xy[4400,2], xy[4401,1], xy[4401,2])
	 	verts <- rbind(CIExyz[keep[1]:4400,],
	 		c(NA, p6[1], p6[2], NA), c(NA, segs[4,1], segs[4,2], NA))
	 	verts <- verts[,2:3]
	 	# Figure xPt
	 	d4 <- dAB(D65, p4)
	 	d5 <- dAB(D65, p5)
	 	d6 <- dAB(D65, p6)
	 	xy4400 <- as.numeric(CIExyz[4400,2:3])
	 	dxy4400 <- dAB(D65, xy4400)
	 	l <- c(d4, d5, d6, dxy4400)
	 	dm <- which.max(l)
	 	if (length(xPt) > 1) xPt <- xPt[1] # In the event of a tie
	 	if (dm == 1) xPt <- p4
	 	if (dm == 2) xPt <- p5
	 	if (dm == 3) xPt <- p6
	 	if (dm == 4) xPt <- xy4400
	 	}

 	# Now that the proper vertices have been selected, do the plot
	message("I'm painting a beautiful gradient, please give me a moment...")
	bgr <- plotCIEchrom(gradient = verts, colSpace, ff, opts = c())
	grid.polygon(verts$x, verts$y, default.units = "native")
	grid.segments(x0 = in.segs[c(1,3),1], y0 = in.segs[c(1,3),2], # these are the dotted lines
		x1 = in.segs[c(4,4),1], y1 = in.segs[c(4,4),2], # from L1 and L2 to D65
		default.units = "native", gp = gpar(lty = 2))

	# Prepare the enclosed colors to serve as the calibration colors
	# Convert the raster into a list of calibration colors;
	# Eliminate the pure whites which are outside the vertices

	bgr2 <- as.raster(bgr)
	bgr2 <- as.vector(bgr2)
	bgr2 <- bgr2[bgr2 != "#FFFFFF"]

	message("Total colors to choose from:", length(bgr2))
	
	# Assemble a list for return
	wedge <- vector("list")
	wedge$wavelength <- c(L1, L3) # input wavelengths
	wedge$Case <- Case
	wedge$verts <- verts # vertices of wedge
	wedge$raster <- bgr # colors in wedge
	wedge$colSpace <- colSpace
	wedge$ff <- ff
	wedge$p4 <- as.numeric(p4) # see above for defs of these pts
	wedge$p5 <- as.numeric(p5)
	wedge$p6 <- as.numeric(p6)
	wedge$xPt <- as.numeric(xPt)
	
	if (diagnostics) {
		grid.points(x = p4[1], y = p4[2], default.units = "native",
			gp = gpar(col = "red"), size = unit(0.5, "char"))
		grid.points(x = p5[1], y = p5[2], default.units = "native",
			gp = gpar(col = "red"), size = unit(0.5, "char"))
		grid.points(x = p6[1], y = p6[2], default.units = "native",
			gp = gpar(col = "red"), size = unit(0.5, "char"))
		grid.points(x = segs[4,1], y =segs[4,2], default.units = "native",
			gp = gpar(col = "red"), size = unit(0.5, "char"))
		}

	invisible(wedge)

	} # end of function