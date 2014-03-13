#' Scale point sizes using circle area, but specifying the radius in
#' pixels.
scale_size_animint <- function(pixel.range=c(2,20), breaks=NULL){
  continuous_scale("size","area",palette=function(x){
    scales:::rescale(sqrt(abs(x)), pixel.range, c(0,1))
  },breaks=breaks)
}
