#' Recale raster by pooling the stack function
#'
#' This function allows you to extract the las element of a vector.
#' @param v A vector.
#' @keywords last
#' @export
#' @examples
#' y<-rnorm(10,0,1)
#' dist_raster(poly, r, field = NULL)

### Function to recale raster by pooling

rpool_scale <- function(rst){
  dat <- unlist(as.data.frame(rst), use.names = F)
  rmu <- mean(dat, na.rm=T)
  rsd <- sd(dat, na.rm=T)
  rst <- (rst - rmu) / rsd
  return(rst)
}
