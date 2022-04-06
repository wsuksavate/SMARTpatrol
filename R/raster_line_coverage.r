### Map line (shp file) to pta raster as coverage by factor of resolution ###

raster_line_coverage <- function(line, rst, res.fact=5, mask=T, prec=1e5, binary = TRUE, is_line=TRUE){
  # Line = SpatialLinesDataFrame data
  # rst = background raster data-grid (must have the same CRS)
  rs <- disaggregate(rst, fact = res.fact) # create temp raster for layer id 
  rs[] <- 1:ncell(rs) # insert layer id as raster value
  rsp <- rasterToPolygons(rs) # raster to polygon grid
  names(rsp) <- 'layer'
  
  # Fix non-node intersection problems by removing non-valid
  if(class(line)[1]!='sf') {line <- st_as_sf(line)}
  line_valid <- line %>% st_make_valid() %>% st_set_precision(prec)
  # select only line features
  if(is_line == TRUE){
  types <- vapply(sf::st_geometry(line_valid), function(x) {class(x)[2]}, "")
  line_valid <- line_valid[ grepl("*LINE", types), ]  
  }
  
  # intersect polygon grid with line data (get new line data), then convert back to sp
  rp <- st_intersection(line_valid, st_as_sf(rsp))
  
  # check presence of line inside grid
  x <- tapply(st_drop_geometry(rp)[,1], rp$layer, length) > 0 # sum all length in the same layer (grid) using layer id as index
  r <- raster(rs) %>% setValues(0)
  r[as.integer(names(x))] <- x
  
  # Then aggregate by summing the coverage to original grid size
  r.agg <- aggregate(r, fact = res.fact, fun=sum)/(res.fact^2)
  
  # masking if true
  if(mask == T){r.agg <- mask(r.agg,rst)}
  return(r.agg)
}
