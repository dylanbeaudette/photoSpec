
plotMiniCard <- function(calCols = NULL, title = "no title", guide = "none") {

	# Bryan Hanson, DePauw University, November 2013 hanson@depauw.edu
	# Part of the photoSpec package
	# Create a simple card: 5 color swatches, 5 gray scale swatches & identifying info
	
	# need a mode where user gives just color names of their choice
	
	if (is.null(calCols)) stop("calCols must be provided")
	
	hexcol <- calCols$hexcol
	if (!length(hexcol) == 5) stop("For the miniCard, calCols must have exactly 5 colors")
	Mun <- calCols$Munsell

	# Main viewport
	
	grid.newpage()
	pushViewport(viewport(width = 6, height = 4, default.units = "cm"))
	
	# Draw gray scale paint chips
	
	gx <- as.numeric(1:5)
	gy <- rep(1.0, 5)
	grays <- c("white", "gray75", "gray50", "gray25", "black")
	grid.rect(x = gx, y = gy, width = 0.5, height = 0.5, default.units = "cm",
		gp = gpar(fill = grays, col = "transparent"))
	# Box around white:
	grid.rect(x = 1.0, y = 1.0, width = 0.6, height = 0.6, default.units = "cm")

	# Draw calibration colors

	cx <- as.numeric(1:5)
	cy <- rep(2.0, 5)
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

	# Add scale for reference
	
	grid.segments(x0 = 0.5, y0 = 0.0, x1 = 5.5, y1 = 0.0,
		gp = gpar(lwd = 1), default.units = "cm")
	tickPos <- seq(0.5, 5.5, by = 0.5)
	grid.segments(x0 = tickPos, y0 = 0.0, x1 = tickPos, y1 = -0.2,
		gp = gpar(lwd = 1), default.units = "cm")
	grid.text(x = seq(0.5, 5.5, by = 1.0), y = -0.5,
		label = c("0.0", "1.0", "2.0", "3.0", "4.0", "5.0"),
		default.units = "cm", gp = gpar(cex = 0.5))
	
	# Label as requested
	
	if ((guide == "hex") | (guide == "Munsell")) {
		if (guide == "hex") labs <- as.character(hexcol)
		if (guide == "Munsell") labs <- as.character(Mun)
		labs <- c(labs, grays)
		#grid.rect(x = c(cx, gx), y = c(cy, gy), width = 0.6, height = 0.6, default.units = "cm")
		grid.text(label = labs, x = c(cx, gx), y = c(cy, gy) - 0.5,
			default.units = "cm", just = "center", gp = gpar(cex = 0.5))
		}

	invisible() # nothing
	
	} # end of function

