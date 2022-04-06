quantile_raster <- function(rst, sep.cdf=FALSE){
	# rst = raster
  # sep.cdf = FALSE -> calculate pooled cdf for all layers
  # sep.cdf = TRUE -> calculate cdf for each layer separately
	if(sep.cdf==FALSE){
	cdf <- ecdf(values(rst))
	quant_rst <- rst %>% setValues(cdf(values(rst)))
	return(quant_rst)
	}
	else{
		rst.out <- list()
		for(i in 1:nlayers(rst)){
			cdf <- ecdf(values(rst[[i]]))
			quant_rst <- rst[[i]] %>% setValues(cdf(values(rst[[i]])))
			rst.out <- c(rst.out, quant_rst)
		}
		return(stack(rst.out))
	}
}