

computeSampleAbs <- function(calCols, sampCol, sampName) {
	
	# Function to interpolate a sample color on the paint chip scale
	
	# This next part is based upon color.id {plotrix}
    sc <- col2rgb(sampCol)
    coltab <- col2rgb(calCols)
    # distances between each paint chip and the sample:
    cdist <- apply(coltab, 2, function(z) {sqrt(sum((z - sc)^2))})
	cdist2 <- data.frame(entry = 1:ncol(coltab), dist = cdist,
		red = coltab[1,], green = coltab[2,], blue = coltab[3,])
	# sorted from closest to furthest:
	cdist2 <- arrange(cdist2, dist) # 
	
	# Now some trigonometry
	
	# P1, P2, the two closest paint chips; Ps the sample point
	# A is the distance between P1 & Ps
	# B is the distance between P2 & Ps
	# C is the distance between P1 & P2
	A <- cdist2[1,2]
	B <- cdist2[2,2]
	cdist3 <- cdist2[c(1,2),c(3,4,5)] # just the two closest points
	C <- sqrt((cdist3[1,1] - cdist3[2,1])^2 + (cdist3[1,2] - cdist3[2,2])^2
		+ (cdist3[1,3] - cdist3[2,3])^2)
		
	# a will be cos of the angle Ps P2 P1
	# b will be cos of the angle Ps P1 P2
	a <- (A^2 - B^2 - C^2)/(-2*B*C)
	b <- (B^2 - A^2 - C^2)/(-2*A*C)

	# C1 & C2 will be the two pieces of C where the normal dropped from Ps hits
	C1 <- A*b
	C2 <- B*a
#	print(C1+C2-C) # looks correct!
	res <- getInterRGB(C1/C, calCols[cdist2[1,1]], calCols[cdist2[1,2]])
	res # returns hex code, need to put on some kind of quantitative 
}

getInterRGB
function (vals, zeroColor, oneColor) 
{
    if (any(vals < 0 || vals > 1)) {
        stop("vals values should range in [0,1]")
    }
    labzero <- RGB2Lab(col2rgb(zeroColor)/255)
    labone <- RGB2Lab(col2rgb(oneColor)/255)
    interL <- vals * labone[1] + (1 - vals) * labzero[1]
    intera <- vals * labone[2] + (1 - vals) * labzero[2]
    interb <- vals * labone[3] + (1 - vals) * labzero[3]
    rgbmat <- Lab2RGB(cbind(interL, intera, interb))
    rgbres <- rgb(rgbmat[, 1], rgbmat[, 2], rgbmat[, 3])
    return(rgbres)
}
<environment: namespace:patchPlot>
