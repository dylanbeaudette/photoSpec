

selectCIExy <- function(L1 = NULL, L2 = NULL) {

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
	in.segs <- segs # save a copy for drawing dotted lines later
#	print(segs)

	# Solve y = mx + b (keep for now, we'll probably use)
	# m0 <- (seg[1,2] - seg[3,2]) / (seg[1,1] - seg[3,1]) # m0 slope of line seg from p0 -> D65
	# b0 <- (-m0 * seg[1,1]) + seg[1,2] # b0 is y-intercept of line seg from p0 -> D65
	
	###### Flip the line segments 180 degrees and extend to ensure intersection
		
	# segs[1,1] = segs[3,1] - (segs[1,1] - segs[3,1]) 
	# segs[1,2] = segs[3,2] + (segs[1,2] - segs[3,2])
	# segs[2,1] = segs[3,1] - (segs[2,1] - segs[3,1]) 
	# segs[2,2] = segs[3,2] + (segs[2,2] - segs[3,2])
	# print(segs)
	ang = pi
	fac <- 2.0 # this ensures that the line segment is long enough to reach
	# the far side of the shark fin
	segs[1,1] = segs[3,1] + (cos(ang) * (segs[1,1] - segs[3,1]) + sin(ang) * (segs[1,2] - segs[3,2]))*fac
	segs[1,2] = segs[3,2] + (-sin(ang) * (segs[1,2] - segs[3,2]) + cos(ang) * (segs[1,2] - segs[3,2]))*fac
	
	segs[2,1] = segs[3,1] + (cos(ang) * (segs[2,1] - segs[3,1]) + sin(ang) * (segs[2,2] - segs[3,2]))*fac
	segs[2,2] = segs[3,2] + (-sin(ang) * (segs[2,2] - segs[3,2]) + cos(ang) * (segs[2,2] - segs[3,2]))*fac
	# print(segs)
	
	# Now loop over shark fin and check for intersections	

	keep <- c()

	for (i in 1:2) { # loop over both line segments
		its <- nrow(xy)-1
		# for (n in 1:its) { # loop over shark fin to find intersection (obsolete?)
			# # Formula in digestable pieces using indices/also makes checking easier
			# den <- (x1 - x2) * (xy[n,2] - xy[n+1,2]) - (y1 - y2) * (xy[n,1] - xy[n+1,1])
			# if (den == 0) {
				# #cat("denominator was zero for n = ", n, "\n")
				# next
				# }
			# num1 <- (x1 * y2 - y1 * x2) * (xy[n,1] - xy[n+1,1])
			# num2 <- (x1 - x2) * (xy[n,1] * xy[n+1,2] - xy[n,2] * xy[n+1,1])
			# num3 <- (x1 * y2 - y1 * x2) * (xy[n,2] - xy[n+1,2])
			# num4 <- (y1 - y2) * (xy[n,1] * xy[n+1,2] - xy[n,2] * xy[n+1,1]) 
			# Px = (num1 - num2)/den
			# Py = (num3 - num4)/den
			# # cat("Px for n =", n, "is", Px, "\n")
			# # cat("Py for n =", n, "is", Py, "\n")
			# }
	
		# Check to see if the line segments intersect
			
		for (n in 1:its) { # loop over shark fin	
			inter <- doSegmentsIntersect(
		        segment1 = c(segs[i,1], segs[i,2], segs[3,1], segs[3,2]),
		        segment2 = c(xy[n,1], xy[n,2], xy[n+1,1], xy[n+1,2]))
		    if (inter) keep <- c(keep, n)
			}
 		} # end of loop that checks each line segment

	# There are 4 cases to be considered here:
	# 1.  neither line segment intersects the line of purples.
	# 2.  the 1st line segment hits the shark fin, the 2nd the l of p
	# 3.  the 1st line segments hits the l of p, the 2nd the shark fin
	# 4.  both segments intersect the l of p
	
	# Only case #1 is handled correctly right now.
	
#	print(keep)
 	mink <- min(keep)
 	maxk <- max(keep)
 	verts <- rbind(CIExyz[mink:maxk,], CIExyz[maxk+1,], c(NA, segs[3,1], segs[3,2], NA))
 	
 	# Do the plot
	rgb <- plotCIEselection(verts[,2:3])
	grid.segments(x0 = in.segs[1:2,1], y0 = in.segs[1:2,2],
		x1 = in.segs[c(3,3),1], y1 = in.segs[c(3,3),2],
		default.units = "native", gp = gpar(lty = 2))
	
	# NEED colSpace and ff ARGUMENTS

	# convert the raster into a list of calibration colors
	# eliminate the pure whites
	rgb <- as.vector(rgb)
	rgb <- rgb[rgb != "#FFFFFF"]

	# This next part is to get it running, we can do better
	
	rgb <- sample(unique(rgb), 10)
	
	invisible(rgb)

	} # end of function