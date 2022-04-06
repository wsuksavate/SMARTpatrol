#' stack raster function
#'
#' This function allows you to extract the las element of a vector.
#' @param v A vector.
#' @keywords last
#' @export
#' @examples
#' y<-rnorm(10,0,1)
#' stack(r.list, rst)

stack_rst <- function(r.list, rst){
	r.pool <- lapply(r.list, FUN=function(x) raster::raster(x) %>% raster::reclassify(cbind(-Inf, NA, 0)) %>% raster::resample(pta))
	r.stack <- raster::stack(r.pool)
	return(r.stack)
}