 

genColsUsingLambdas <- function(L1 = NULL, L2 = NULL, colSpace = "sRGB", ex = 1.0, res = 0.02,
	plotPC = TRUE, showRGB = FALSE, showCIE = TRUE, ...) {

	# Bryan Hanson, DePauw University, March 2013 hanson@depauw.edu
	# Part of the photoSpec package
	# Major contributions from Matthew Kukurugya

	diagnostics <- FALSE # maybe make this an argument
	
	# Check input data
	if (is.null(L1)) stop("You must give a wavelength for L1")
	if (is.null(L2)) stop("You must give a wavelength for L2")
	if (L1 > L2) stop("L1 must be less than L2")

	# Re-order things so that L1 < L2 < L3 in what comes later
	L3 <- L2	
	L2 <- L1 + 0.5*(L2-L1) # This will be the mid point
	L2 <- floor(L2) # round down in the low res version
	
	# Load spectral locus data (shark fin)
	data(CIExyz)
	xy <- CIExyz[,c(2,3)] # 441 rows
	xy <- rbind(xy, xy[1,]) # repeat row so that polygon can close
	D65 <- getWhiteValues("D65")
	
	# Find the coordinates of the input wavelengths
	ans1 <- findCIEindex(L1)
	ans2 <- findCIEindex(L2)
	ans3 <- findCIEindex(L3)

	# Put all the needed data in one place: p1 from L1, p2 from L2, p3 from L3
	segs <- data.frame(
		x = c(xy[ans1,1], xy[ans2,1], xy[ans3,1], D65[1,1]),
		y = c(xy[ans1,2], xy[ans2,2], xy[ans3,2], D65[1,2]))
	row.names(segs) <- c("p1", "p2", "p3", "D65")
	in.segs <- segs # save a copy for later
	
	segs[1,] <- extendAndRotateAroundD65(segs[1,], ang = pi) # flip the segments
	segs[2,] <- extendAndRotateAroundD65(segs[2,], ang = pi)
	segs[3,] <- extendAndRotateAroundD65(segs[3,], ang = pi)
	
	# Now loop over shark fin and check for intersections	

	keep <- findPolygonIntersection(XY = segs, xy = xy)

	# Here are the cases to be considered next.
	# Segments are in wavelength order.
	# 1.  no line segment intersects the line of purples.
	# 2.  segment 1 hits the l of p, segments 2 & 3 the shark fin
	# 3.  segments 1 & 2 hit the l of p, segment 3 the shark fin
	# 4.  all segments intersect the l of p
	# 5.  only the middle segments hits the l of p
	# 6.  segment 1 hits the shark fin, the other two l of p
	# 7.  segments 1 & 2 hit shark fin, segment 3 the l of p
	# The line of purples is segment xy[c(441,442),]

	# Store the intersection points p4, p5 & p6:
	# L1/p1 -> D65 -> p4; L2/p2 -> D65 -> p5; L3/p3 -> D65 -> p6
	# Record the 'case' and the point in the wedge farthest
	# from D65 (xPt)
	
	Case1 <- Case2 <- Case3 <- Case4 <- Case5 <- Case6 <- Case7 <- FALSE
	Case <- NA
	xPt <- vector("numeric", 2)
	
	if (!441 %in% keep) Case1 <- TRUE
	if ((keep[1] == 441) && (!keep[2] == 441) && (!keep[3] == 441)) Case2 <- TRUE 	
	if ((keep[1] == 441) && (keep[2] == 441) && (!keep[3] == 441)) Case3 <- TRUE 	
	if ((keep[1] == 441) && (keep[2] == 441) && (keep[3] == 441)) Case4 <- TRUE
	if ((!keep[1] == 441) && (keep[2] == 441) && (!keep[3] == 441)) Case5 <- TRUE 	
	if ((!keep[1] == 441) && (keep[2] == 441) && (keep[3] == 441)) Case6 <- TRUE 	
	if ((!keep[1] == 441) && (!keep[2] == 441) && (keep[3] == 441)) Case7 <- TRUE 	
	
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
			in.segs[4,2], xy[441,1], xy[441,2], xy[442,1], xy[442,2])
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
			in.segs[4,2], xy[441,1], xy[441,2], xy[442,1], xy[442,2])
		p5 <- lineIntersection(in.segs[2,1], in.segs[2,2], in.segs[4,1],
			in.segs[4,2], xy[441,1], xy[441,2], xy[442,1], xy[442,2])
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
			in.segs[4,2], xy[441,1], xy[441,2], xy[442,1], xy[442,2])
		p5 <- lineIntersection(in.segs[2,1], in.segs[2,2], in.segs[4,1],
			in.segs[4,2], xy[441,1], xy[441,2], xy[442,1], xy[442,2])
		p6 <- lineIntersection(in.segs[3,1], in.segs[3,2], in.segs[4,1],
			in.segs[4,2], xy[441,1], xy[441,2], xy[442,1], xy[442,2])
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
			in.segs[4,2], xy[441,1], xy[441,2], xy[442,1], xy[442,2])
	 	p6 <- CIExyz[keep[3],2:3]
	 	verts <- rbind(CIExyz[keep[3]:441,], CIExyz[1:keep[1],], c(NA, segs[4,1], segs[4,2], NA))
	 	verts <- verts[,2:3]
	 	# Figure xPt
	 	d4 <- dAB(D65, p4)
	 	d5 <- dAB(D65, p5)
	 	d6 <- dAB(D65, p6)
	 	xy1 <- as.numeric(CIExyz[1,2:3])
	 	xy441 <- as.numeric(CIExyz[441,2:3])
	 	dxy1 <- dAB(D65, xy1)
	 	dxy441 <- dAB(D65, xy441)
	 	l <- c(d4, d5, d6, dxy1, dxy441)
	 	dm <- which.max(l)
	 	if (dm == 1) xPt <- p4
	 	if (dm == 2) xPt <- p5
	 	if (dm == 3) xPt <- p6
	 	if (dm == 4) xPt <- xy1
	 	if (dm == 5) xPt <- xy441
	 	}

	if (Case6) { # Case 6 (OK)
		Case <- "Case6"
	 	p4 <- CIExyz[keep[1],2:3]
		p5 <- lineIntersection(in.segs[2,1], in.segs[2,2], in.segs[4,1],
			in.segs[4,2], xy[441,1], xy[441,2], xy[442,1], xy[442,2])
		p6 <- lineIntersection(in.segs[3,1], in.segs[3,2], in.segs[4,1],
			in.segs[4,2], xy[441,1], xy[441,2], xy[442,1], xy[442,2])
	 	verts <- rbind(CIExyz[keep[1]:441,], c(NA, p5[1], p5[2], NA),
	 		c(NA, p6[1], p6[2], NA), c(NA, segs[4,1], segs[4,2], NA))
	 	verts <- verts[,2:3]
	 	# Figure xPt
	 	d4 <- dAB(D65, p4)
	 	d5 <- dAB(D65, p5)
	 	d6 <- dAB(D65, p6)
	 	xy1 <- as.numeric(CIExyz[1,2:3])
	 	xy441 <- as.numeric(CIExyz[441,2:3])
	 	dxy1 <- dAB(D65, xy1)
	 	dxy441 <- dAB(D65, xy441)
	 	l <- c(d4, d5, d6, dxy1, dxy441)
	 	dm <- which.max(l)
	 	if (length(xPt) > 1) xPt <- xPt[1] # In the event of a tie
	 	if (dm == 1) xPt <- p4
	 	if (dm == 2) xPt <- p5
	 	if (dm == 3) xPt <- p6
	 	if (dm == 4) xPt <- xy1
	 	if (dm == 5) xPt <- xy441
 	 	}

	if (Case7) { # Case 7 (OK)
		Case <- "Case7"
	 	p4 <- CIExyz[keep[1],2:3]
	 	p5 <- CIExyz[keep[2],2:3]
		p6 <- lineIntersection(in.segs[3,1], in.segs[3,2], in.segs[4,1],
			in.segs[4,2], xy[441,1], xy[441,2], xy[442,1], xy[442,2])
	 	verts <- rbind(CIExyz[keep[1]:441,],
	 		c(NA, p6[1], p6[2], NA), c(NA, segs[4,1], segs[4,2], NA))
	 	verts <- verts[,2:3]
	 	# Figure xPt
	 	d4 <- dAB(D65, p4)
	 	d5 <- dAB(D65, p5)
	 	d6 <- dAB(D65, p6)
	 	xy441 <- as.numeric(CIExyz[441,2:3])
	 	dxy441 <- dAB(D65, xy441)
	 	l <- c(d4, d5, d6, dxy441)
	 	dm <- which.max(l)
	 	if (length(xPt) > 1) xPt <- xPt[1] # In the event of a tie
	 	if (dm == 1) xPt <- p4
	 	if (dm == 2) xPt <- p5
	 	if (dm == 3) xPt <- p6
	 	if (dm == 4) xPt <- xy441
	 	}

 	# Now that the proper vertices have been selected, do the plot
	message("I'm painting a beautiful gradient, please give me a moment...")
	bgr <- plotCIEchrom(gradient = verts, colSpace = colSpace, ex = ex, opts = c())
	grid.polygon(verts$x, verts$y, default.units = "native")
	grid.segments(x0 = in.segs[c(1,3),1], y0 = in.segs[c(1,3),2], # these are the dotted lines
		x1 = in.segs[c(4,4),1], y1 = in.segs[c(4,4),2], # from L1 and L2 to D65
		default.units = "native", gp = gpar(lty = 2))

	# Label L1 and L2
	lab.pos <- in.segs[c(1,3), ]
	for (i in 1:2) { 		# this is now vectorized so the loop can be eliminated
		lab.pos[i,] <- extendAndRotateAroundD65(pts = lab.pos[i,], ang = 0, fac = 1.1)
		}
	grid.text(label = expression(lambda[1]), lab.pos[1,1], lab.pos[1,2], default.units = "native")
	grid.text(label = expression(lambda[2]), lab.pos[2,1], lab.pos[2,2], default.units = "native")
		
	# Assemble a list (in early versions, this was the return value)
	wedge <- vector("list")
	wedge$verts <- verts # vertices of wedge
	wedge$colSpace <- colSpace
	wedge$ex <- ex
	wedge$wavelength <- c(L1, L3) # input wavelengths
	wedge$case <- Case
	wedge$raster <- bgr # colors in wedge
	wedge$p4 <- as.numeric(p4) # see above for defs of these pts
	wedge$p5 <- as.numeric(p5)
	wedge$p6 <- as.numeric(p6)
	wedge$xPt <- as.numeric(xPt)
		
	# Go and select colors from the wedge as calibration colors, and show them on CIE diagram

	calCols <- xy2cC(wedge, res, colSpace, ex, ...)
		
	if (diagnostics) { # mark some of the relevent points
		grid.points(x = p4[1], y = p4[2], default.units = "native",
			gp = gpar(col = "red"), size = unit(0.5, "char"))
		grid.points(x = p5[1], y = p5[2], default.units = "native",
			gp = gpar(col = "red"), size = unit(0.5, "char"))
		grid.points(x = p6[1], y = p6[2], default.units = "native",
			gp = gpar(col = "red"), size = unit(0.5, "char"))
		grid.points(x = segs[4,1], y =segs[4,2], default.units = "native",
			gp = gpar(col = "red"), size = unit(0.5, "char"))
		}

	# Send out for visualization if requested
	if (plotPC) print(plot_hex(calCols$hexcol)) # draws paint chips & labels them
	if (showRGB) showRGBcalibration(calCols, ...)
	if (showCIE) showCIE(calCols, ...)

	invisible(calCols)

	} # end of function