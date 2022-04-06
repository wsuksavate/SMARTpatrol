#' Linearly stretch function
#'
#' This function allows you to extract the las element of a vector.
#' @param v A vector.
#' @keywords last
#' @export
#' @examples
#' y<-rnorm(10,0,1)
#' dist_raster(poly, r, field = NULL)

# function to linearly stretch value from 0 to 1
rng_stretch <- function(x, lb=0, ub=1){
  if(class(x)[1] == 'RasterStack'){
    max.x <- max(as.vector(values(x)), na.rm=T)
    min.x <- min(as.vector(values(x)), na.rm=T)
  }
  else{
    max.x <- max(as.vector(x), na.rm=T)
    min.x <- min(as.vector(x), na.rm=T)
  }
  y <- (((x-min.x)*(ub-lb))/(max.x-min.x))+lb
  return(y)
  }
