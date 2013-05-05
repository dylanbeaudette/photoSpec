

selectCIExy <- function(L1 = NULL, L2 = NULL, colSpace = "sRGB", ff = 1.0, ...) {

	# Bryan Hanson, DePauw University, March 2013 hanson@depauw.edu
	# Part of the photoSpec package
	# Major contributions from Matthew Kukurugya

##### Helper Functions #####
# See gist.github.com/bryanhanson/5471173
# These are placed here so they are available to this function only

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

	if (is.null(L1)) stop("You must give a wavelength for L1")
	if (is.null(L2)) stop("You must give a wavelength for L2")
	if (L1 > L2) stop("L1 must be less than L2")
	
	# load data (shark fin)
	data(CIExyz)
	xy <- CIExyz[,c(2,3)] # 4400 rows
	xy <- rbind(xy, xy[1,]) # repeat row so that polygon can close

	# Find the coordinates of the given wavelengths
	ans1 <- grep(L1, CIExyz$wavelength)
	ans1 <-ans1[1] # this is an index/row number
	ans2 <- grep(L2, CIExyz$wavelength)
	ans2 <-ans2[1]

	D65 <- getWhiteValues("D65")
	# Put all the needed data in one place: p0 from L1, p1 from L2
	segs <- data.frame(
		x = c(xy[ans1,1], xy[ans2,1], D65[1,1]),
		y = c(xy[ans1,2], xy[ans2,2], D65[1,2]))
	row.names(segs) <- c("p0", "p1", "D65")
	in.segs <- segs # save a copy for later
	
	###### Flip the line segments 180 degrees and extend to ensure intersection
		
	# segs[1,1] = segs[3,1] - (segs[1,1] - segs[3,1]) 
	# segs[1,2] = segs[3,2] + (segs[1,2] - segs[3,2])
	# segs[2,1] = segs[3,1] - (segs[2,1] - segs[3,1]) 
	# segs[2,2] = segs[3,2] + (segs[2,2] - segs[3,2])
	# print(segs)
	ang <- pi
	fac <- 2.0 # this ensures that the line segment is long enough to reach
	# the far side of the shark fin or line of purples
	segs[1,1] = segs[3,1] + (cos(ang) * (segs[1,1] - segs[3,1]) + sin(ang) * (segs[1,2] - segs[3,2]))*fac
	segs[1,2] = segs[3,2] + (-sin(ang) * (segs[1,2] - segs[3,2]) + cos(ang) * (segs[1,2] - segs[3,2]))*fac
	
	segs[2,1] = segs[3,1] + (cos(ang) * (segs[2,1] - segs[3,1]) + sin(ang) * (segs[2,2] - segs[3,2]))*fac
	segs[2,2] = segs[3,2] + (-sin(ang) * (segs[2,2] - segs[3,2]) + cos(ang) * (segs[2,2] - segs[3,2]))*fac
	# print(segs)
	
	# Now loop over shark fin and check for intersections	

	keep <- c() # keep will always be length 2

	for (i in 1:2) { # loop over both line segments
		its <- nrow(xy)-1
	
		# Check to see if the line segments intersect
			
		for (n in 1:its) { # loop over shark fin	
			inter <- doSegmentsIntersect(
		        segment1 = c(segs[i,1], segs[i,2], segs[3,1], segs[3,2]),
		        segment2 = c(xy[n,1], xy[n,2], xy[n+1,1], xy[n+1,2]))
		    if (inter) keep <- c(keep, n)
			}
 		} # end of loop that checks each line segment

	# There are 4 cases to be considered next:
	# 1.  neither line segment intersects the line of purples.
	# 2.  the 1st line segments hits the l of p, the 2nd the shark fin
	# 3.  the 1st line segment hits the shark fin, the 2nd the l of p
	# 4.  both segments intersect the l of p
	# 5.  neither line segment intersects l of p, but one originates
	#     on the right side, the other on the left side
	# The line of purples is segment xy[c(4400,4401),]
	
#	print(keep) #
 	 	
	if (!4400 %in% keep) { # Case 1
	 	verts <- rbind(CIExyz[min(keep):max(keep),], CIExyz[max(keep)+1,], c(NA, segs[3,1], segs[3,2], NA))
	 	verts <- verts[,2:3]
 	 	}
		
	if ((keep[1] == 4400) && !(length(unique(keep)) == 1L)) { # Case 2
		keep <- keep[-1]
		P3 <- lineIntersection(in.segs[1,1], in.segs[1,2], in.segs[3,1],
			in.segs[3,2], xy[4400,1], xy[4400,2], xy[4401,1], xy[4401,2])
	 	verts <- rbind(c(NA, segs[3,1], segs[3,2], NA), c(NA, P3[1], P3[2], NA), CIExyz[1:max(keep),])
	 	verts <- verts[,2:3]
		}
		
	if (keep[length(keep)] == 4400 && !(length(unique(keep)) == 1L)) { # Case 3
		P4 <- lineIntersection(in.segs[2,1], in.segs[2,2], in.segs[3,1],
			in.segs[3,2], xy[4400,1], xy[4400,2], xy[4401,1], xy[4401,2])
	 	verts <- rbind(CIExyz[min(keep):max(keep),], c(NA, P4[1], P4[2], NA), c(NA, segs[3,1], segs[3,2], NA))
	 	verts <- verts[,2:3]
		}

	if (unique(keep) == 4400 && (length(unique(keep)) == 1L)) { # Case 4, both segments hit the l of p
		# Go back to in.segs and see where they hit the l of p
		P3 <- lineIntersection(in.segs[1,1], in.segs[1,2], in.segs[3,1],
			in.segs[3,2], xy[4400,1], xy[4400,2], xy[4401,1], xy[4401,2])
		P4 <- lineIntersection(in.segs[2,1], in.segs[2,2], in.segs[3,1],
			in.segs[3,2], xy[4400,1], xy[4400,2], xy[4401,1], xy[4401,2])
		verts <- rbind(segs[3,], P3, P4)
		colnames(verts) <- c("x", "y")
		}

	if ((!4400 %in% keep) && (keep[1] > keep[2])) { # Case 5
	 	verts <- rbind(CIExyz[max(keep):4400,], CIExyz[1:min(keep),], c(NA, segs[3,1], segs[3,2], NA))
	 	verts <- verts[,2:3]
 	 	}

 	# Now that the proper vertices have been selected, do the plot
 	
	bgr <- plotCIEchrom(gradient = verts, colSpace, ff, opts = c())
	grid.polygon(verts$x, verts$y, default.units = "native")
	grid.segments(x0 = in.segs[1:2,1], y0 = in.segs[1:2,2], # these are the dotted lines
		x1 = in.segs[c(3,3),1], y1 = in.segs[c(3,3),2], # from L1 and L2
		default.units = "native", gp = gpar(lty = 2))

	# Prepare the enclosed colors to serve as the calibration colors
	# Convert the raster into a list of calibration colors;
	# Eliminate the pure whites which are outside the vertices

	bgr <- as.raster(bgr)
	bgr <- as.vector(bgr)
	bgr <- bgr[bgr != "#FFFFFF"]

	cat("Total colors selected:", length(bgr), "\n")
	
	invisible(bgr)

	} # end of function