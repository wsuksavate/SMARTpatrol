stack_rst <- function(r.list, rst){
	r.pool <- lapply(r.list, FUN=function(x) raster::raster(x) %>% raster::reclassify(cbind(-Inf, NA, 0)) %>% raster::resample(pta))
	r.stack <- raster::stack(r.pool)
	return(r.stack)
}