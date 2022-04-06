#' PTA to raster function
#'
#' This function allows you to extract the las element of a vector.
#' @param v A vector.
#' @keywords last
#' @export
#' @examples
#' y<-rnorm(10,0,1)
#' dist_raster(poly, r, field = NULL)

### Function to transform PTA shp file to 1x1 km raster ###

pta_to_raster <- function(pta, crs=NULL, res=c(1000,1000)){
  # need 'sp' and 'raster' package
  # pta = sp object (polygon) of protected area (must be UTM CRS)
  # crs = output CRS as number (for transgorm non-UTM to UTM)
  # res = resolution
  if(class(pta)[1]=='sf') {pta <- as_Spatial(pta)}
  if(!is.null(crs)) {pta <- spTransform(pta, CRS(paste0("+init=epsg:",crs)))}
  ext <- extent(pta)
  rst <- raster(ext, res=res)
  pta_r <- rasterize(pta, rst)
  return(pta_r)
}
