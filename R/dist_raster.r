### Function to turn categorical raster to raster of the nearest distance to each categorical feature
# field = define field of categorical raster
# if field is missing -> distance to all features
dist_raster <- function(poly, r, field = NULL){
	# polygon = categorical polygon that you want to transform it to raster
	# r = template raster for rasterizing polygon
	# field = field of polygon you want as raster
	if(is.null(field)) {
	cat('Missing field\n')
	stop()
	}
	else{
	cat(paste0('Have field: ', field))
	poly <- poly[as.character(field)]
	nam <- as.data.frame(poly)[,1]
	nam.un <- unique(nam)
	r.stack <- stack()
	for(i in nam.un){
		poly.i <- poly[nam == i,]
		r.poly <- rasterize(poly.i, r) %>% distance()
		names(r.poly) <- paste0(i,'_dist')
		r.stack <- stack(r.stack, r.poly)
	}
	}
	return(r.stack)
}