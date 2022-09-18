pkgDNLs_worldmapcolor <- function (pkg_name, dataset, remove_dups = TRUE, ...){
	require2("ggplot2")
	require2("data.table")
	require2("sp")
	data <- dataset[which(dataset$package == pkg_name), ]
	if (remove_dups) {
		data <- data[!duplicated(data$ip_id), ]
	}
	counts <- cbind.data.frame(table(data$country))
	names(counts) <- c("country", "count")
	data("WorldBordersData", envir = environment(), package = "installr")
	ISO_full <- get("ISO_full")
	colcode <- numeric(length(ISO_full))
	names(colcode) <- ISO_full
	dnl_places <- names(colcode[which(names(colcode) %in% as.character(counts$country))])
	rownames(counts) <- counts$country
	colcode[dnl_places] <- counts[dnl_places, "count"]
	world@data$id <- rownames(world@data)
	world.points <- fortify(world, by = "id")
	names(colcode) <- rownames(world@data)
	world.points$dnls <- colcode[world.points$id]
	world.map <- ggplot(data = world.points) + geom_polygon(aes_string(x = "long", 
																																		 y = "lat", group = "group", fill = "dnls"), color = "black") + 
		coord_equal() + scale_fill_gradientn(colours = c("white", 
																										 "yellow", "red"), name = "Downloads", values = c(0, 0.25, 
																										 																								 1)) + labs(title = paste(pkg_name, " downloads from the Rstudio '0-Cloud' CRAN mirror by country\nfrom ", 
																										 																								 												 min(dataset$date), " to ", max(dataset$date), "\n(Total downloads: ", 
																										 																								 												 sum(counts$count), ")", sep = ""))
	return(world.map)
}

if(FALSE){
	library(devtools)
	library(installr)
	RStudio_CRAN_data_folder <- download_RStudio_CRAN_data(START = '2015-03-29', END = '2015-03-30')
	my_RStudio_CRAN_data <- read_RStudio_CRAN_data(RStudio_CRAN_data_folder)
	#debug(pkgDNLs_worldmapcolor)
	wm <- pkgDNLs_worldmapcolor(pkg_name="plyr", dataset = my_RStudio_CRAN_data)
	wm
}
