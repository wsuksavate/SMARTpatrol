#' Dummy raster function
#'
#' This function allows you to extract the las element of a vector.
#' @param v A vector.
#' @keywords last
#' @export
#' @examples
#' y<-rnorm(10,0,1)
#' dist_raster(poly, r, field = NULL)

### Function to turn categorical spatialdataframe to dummy raster
# spatialdataframe input (poly) must have only 1 field
dummy_raster <- function(poly, r, field){
	# polygon = categorical polygon that you want to transform it to raster
	# r = template raster for rasterizing polygon
	# field = field of polygon you want as raster
	poly <- poly[as.character(field)]
	nam <- as.data.frame(poly)[,1]
	nam.un <- unique(nam)
	r.stack <- stack()
	for(i in nam.un){
		poly.i <- poly[nam == i,]
		### r.poly <- rasterize(poly.i, r)  ### not used (use next line instead)
		r.poly <- raster_line_coverage(poly.i, r, prec=1e5, res.fact=1, is_line=F)
		r.poly[is.na(r.poly)] <- 0
		names(r.poly) <- paste0(i,'_bin')
		r.stack <- stack(r.stack, r.poly)
	}
	r.stack <- r.stack > 0 # set value more than 0 to 1
	return(r.stack)
}