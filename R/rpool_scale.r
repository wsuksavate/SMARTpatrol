### Function to recale raster by pooling

rpool_scale <- function(rst){
  dat <- unlist(as.data.frame(rst), use.names = F)
  rmu <- mean(dat, na.rm=T)
  rsd <- sd(dat, na.rm=T)
  rst <- (rst - rmu) / rsd
  return(rst)
}
