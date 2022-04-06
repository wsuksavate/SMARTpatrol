#' Raster cross-correlation function
#'
#' This function allows you to extract the las element of a vector.
#' @param v A vector.
#' @keywords last
#' @export
#' @examples
#' y<-rnorm(10,0,1)
#' dist_raster(poly, r, field = NULL)

# Cross-Correlation between 2 rasterstack
# Require the stack to be sorted as time series

raster_cross_cor <- function(x, y, lag=0, invar=NA){
  # x, y = raster but x should be exploratory variable
  # lag = time lag
  # initiate output raster
  r <- x[[1]]
  names(r) <- 'x_cor'
  if(ncell(x) == ncell(y)){n = ncell(x)}
  for(i in 1:n){
    if(is.na(r[i])){r[i]<-NA;next;} # next if NA
    xn <- as.vector(x[i])
    yn <- as.vector(y[i])
    if(var(xn)==0 | var(yn)==0){r[i]<-invar;next;} # next if no variation
    xcor <- ccf(xn, yn, plot = FALSE)
    r[i] <- xcor[as.character(lag)][[1]] # populate correlation value to raster
  }
  return(r)
}