

plotMiniCard <- function(calCols = c("red", "green", "yellow", "blue", "orange"),
	title = "no title", guide = "none") {

	# Bryan Hanson, DePauw University, November 2013 hanson@depauw.edu
	# Part of the photoSpec package
	# Create a simple card: 5 color swatches, 5 gray scale swatches & identifying info
	
	#hexcol <- calCols$hexcol
	hexcol <- calCols
	
	# Main viewport
	
	grid.newpage()
	pushViewport(viewport(width = 6, height = 4, default.units = "cm"))
	
	# Draw gray scale paint chips
	
	gx <- as.numeric(1:5)
	gy <- 1.0
	grays <- c("white", "gray75", "gray50", "gray25", "black")
	grid.rect(x = gx, y = gy, width = 0.5, height = 0.5, default.units = "cm",
		gp = gpar(fill = grays, col = "transparent"))
	grid.rect(x = 1.0, y = 1.0, width = 0.6, height = 0.6, default.units = "cm")

	# Draw calibration colors

	cx <- as.numeric(1:5)
	cy <- 2.0
	grid.rect(x = cx, y = cy, width = 0.5, height = 0.5, default.units = "cm",
		gp = gpar(fill = hexcol, col = "transparent"))

	# Add a serial number for tracking/reproduction + title
	
	sn <- digest(hexcol)
	desc <- packageDescription("photoSpec")
	msg <- paste("photoSpec", desc$Version, Sys.Date(), sep = "   ")
	grid.text(title, x = 0.5, y = 3.6, just = "left", gp = gpar(cex = 0.75), default.units = "cm")
	grid.text(msg, x = 0.5, y = 3.1, just = "left", gp = gpar(cex = 0.75), default.units = "cm")
	grid.text(paste("csn:", sn, sep = " "), x = 0.5, y = 2.6, just = "left",
		gp = gpar(cex = 0.75), default.units = "cm")

	# Label as requested
	
	if (guide == "hex") labs <- hexcol
	if (guide == "Munsell") { # may already be stored in a proper gCC object
		mrgb <- hex2RGB(hexcol)@coords
		labs <- rgb2mnsl(mrgb + 0.0001) # fix for buglet in munsell
		}
	
	if ((guide == "hex") | (guide == "Munsell")) {
		#grid.rect(x = c(cx, gx), y = c(cy, gy), width = 0.6, height = 0.6, default.units = "cm")
		grid.text(label = labs, x = c(cx, gx), y = c(cy, gy) - 0.5,
			default.units = "cm", just = "center", gp = gpar(cex = 0.5))
		}

	invisible() # nothing
	
	} # end of function

