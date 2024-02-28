# Functions for Spatial RF

percentage.na <- function(rst){
  na.pix <- sum(is.na(values(rst[[1]])))
  p <- (na.pix / ncell(rst[[1]])) * 100
  return(p)
}
percentage.na(rst)
