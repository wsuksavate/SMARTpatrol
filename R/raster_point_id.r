#' Number of point within raster function
#'
#' This function allows you to extract the las element of a vector.
#' @param v A vector.
#' @keywords last
#' @export
#' @examples
#' y<-rnorm(10,0,1)
#' dist_raster(poly, r, field = NULL)

### number of point in each raster filtered out by duplicated ID ###

raster_point_id <- function(point, rst, id.col=NULL, year.col=NULL, prefix=NULL){
	# point = point data frame
	# rst = raster
	# id.col = column number of duplicated id
	rs <- rst %>% setValues(0)
	dat <- st_drop_geometry(point)
	
	id <- unique(dat[[id.col]]) # array of unique id
	year.arr <- sort(unique(point[[year.col]]))
	
	r.year <- rst
	r.list <- list()
	# Loop though year
	for(i in year.arr){
	  r.year[] <- 0
	  point.year <- point[point[[year.col]]==i,]
	  id.year <- point.year[[id.col]]
	  # Loop through id within year
	  for(j in id.year){
	    point.id <- point.year[point.year[[id.col]]==j,] # filter point with id
	    xy.id <- st_coordinates(point.id) # extract coordinate
	    counts.id <- table(cellFromXY(rst,xy.id))
	    r.year[as.numeric(names(counts.id))] <- r.year[as.numeric(names(counts.id))] + 1 # add one to each cell within id
	  }
	  r.year[is.na(rst)] <- NA
	  names(r.year) <- paste0(prefix,'_',i)
	  r.list <- c(r.list, r.year)
	}
	# Output
	rs <- stack(r.list)
	return(rs)
}	
