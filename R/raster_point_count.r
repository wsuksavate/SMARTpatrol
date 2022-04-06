### Number of point on raster grid ###

raster_point_count <- function(point, rst, year.col=NULL, prefix=NULL){
  # point = sp or sf point or xy dataframe
  # r = background raster
  # year.col = column number of year
  # prefic = initial of raster name
  
  # make a raster of zeroes like the input
  r.sum <- rst
  r.sum[] <- 0
  if(class(point)[1] == 'sf'){
    xy <- st_coordinates(point)
  }
  if(is.null(year.col)){ # Do not separate by year
  # get the cell index for each point and make a table:
  counts <- table(cellFromXY(rst,xy))
  # fill in the raster with the counts from the cell index:
  r.sum[as.numeric(names(counts))] <- counts
  r.sum[is.na(rst)] <- NA
  return(r.sum)
  }
  else{ # Separately count by year
  year.arr <- sort(unique(point[[year.col]]))
  r.list.count <- list()
	for(i in year.arr){
	r.sum[] <- 0
	point.sub <- point[point[[year.col]]==i,]
	if(class(point)[1] == 'sf'){
		xy.sub <- st_coordinates(point.sub)
		}
	counts.sub <- table(cellFromXY(rst,xy.sub))
	r.sum[as.numeric(names(counts.sub))] <- counts.sub
	r.sum[is.na(rst)] <- NA
	names(r.sum) <- paste0(prefix,'_',i)
	r.list.count <- c(r.list.count, r.sum)
	}
	r.stack.count <- stack(r.list.count)
	return(r.stack.count)
  }
}

