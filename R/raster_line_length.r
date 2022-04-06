### Map line (shp file) to pta raster as length (meter) ###

raster_line_length <- function(line, rst, mask=T, prec=1e5){
  # Line = SpatialLinesDataFrame data
  # rst = background raster data-grid (must have the same CRS)
  rs <- rst # create temp raster for layer id 
  rs[] <- 1:ncell(rst) # insert layer id as raster value
  rsp <- rasterToPolygons(rs) # raster to polygon grid
  names(rsp) <- 'layer'
  
  # Fix non-node intersection problems by removing non-valid
  if(class(line)[1]!='sf') {line <- st_as_sf(line)}
  line_valid <- line %>% st_make_valid() %>% st_set_precision(prec)
  
  # intersect polygon grid with line data (get new line data), then convert back to sp
  rp <- st_intersection(line_valid, st_as_sf(rsp)) 
  
  rp$length <- st_length(rp, by_element=TRUE) # calculate length for each newly intersected line
  x <- tapply(rp$length, rp$layer, sum) # sum all length in the same layer (grid) using layer id as index
  
  # then create output raster
  r <- raster(rs) %>% setValues(0)
  r[as.integer(names(x))] <- x # insert length to raster by id "names(x)"
  
  # masking if true
  if(mask == T){r <- mask(r,rst)}
  return(r)
}
