### Map line (shp file) to pta raster as coverage by factor of resolution ###

raster_time_coverage <- function(line, type=c('month', 'week', 'yday'), date.col, rst, mask=T, prec=1e5){
  # Line = SpatialLinesDataFrame data
  # rst = background raster data-grid (must have the same CRS)
  # date.col = column number of date data
  
  rs <- rst
  rs[] <- 1:ncell(rs) # insert layer id as raster value
  rsp <- rasterToPolygons(rs) # raster to polygon grid
  names(rsp) <- 'layer'
  
  # Fix non-node intersection problems by removing non-valid
  if(class(line)[1]!='sf') {line <- st_as_sf(line)}
  line_valid <- line %>% st_make_valid() %>% st_set_precision(prec)
  # select only line features
  types <- vapply(sf::st_geometry(line_valid), function(x) {class(x)[2]}, "")
  line_valid <- line_valid[ grepl("*LINE", types), ]  
  
  # intersect polygon grid with line data (get new line data), then convert back to sp
  rp <- st_intersection(line_valid, st_as_sf(rsp))

  # calculate date-time for each line and count number of date-time in each cell
  rp$time <- do.call(type[1], list(as.Date(rp[[6]])))
  y <- tapply(rp$time, rp$layer, function(x) length(unique(x))) # sum all unique time in the same layer (grid) using layer id as index
  r <- raster(rs) %>% setValues(0)
  r[as.integer(names(y))] <- y
  
  # Then aggregate by summing the coverage to original grid size
  
  # masking if true
  if(mask == T){r.out <- mask(r,rst)}
  return(r.out)
}
