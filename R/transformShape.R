#' Function to transform R shapes into d3 shapes...
#' @param dframe Data frame with columns shape, fill, colour.
#' @return Data frame transformed so that shape corresponds to d3 shape. Also includes Rshape column for debugging.
transform_shape <- function(dframe){
  dframe[,2:3] <- apply(dframe[,2:3], 2, as.character)
  unfilled <- which(dframe$shape<=14)
  solid <- which(dframe$shape>14 & dframe$shape<=20)
  outlined <- which(dframe$shape>20)
  xold <- dframe$shape
  shapeidx <- data.frame(x = 0:25, shape="", stringsAsFactors=FALSE)
  shapeidx[c(0, 7, 12, 13, 14, 15, 22)+1,2] <- "square"
  shapeidx[c(3, 4, 8)+1, 2] <- "cross"
  shapeidx[c(5, 9, 18, 23)+1, 2] <- "diamond"
  shapeidx[c(1, 10, 16, 19, 20, 21)+1, 2] <- "circle"
  shapeidx[c(6, 11, 25)+1, 2] <- "triangle-down"
  shapeidx[c(2, 17, 24)+1, 2] <- "triangle-up"
  shapeidx$fill <- c(rep(FALSE, 15), rep(TRUE, 6), rep(TRUE, 5))
  shapeidx$line <- c(rep(TRUE, 15), rep(FALSE, 6), rep(TRUE, 5))
  vals <- unique(xold)
  
  dframe$fill[unfilled] <- "null"
  dframe$colour[solid] <- "null"
  dframe$Rshape <- dframe$shape
  dframe$shape <- shapeidx$shape[xold+1]
  
  
  # 16, 19, 20 are duplicates
  # 0, 7, 12, 13, 14 are duplicates
  # 3, 4, 8 are duplicates
  # 5, 9 are duplicates
  # 1, 10 are duplicates
  # 6, 11 are duplicates
  # 4, 7, 8, 9, 10, 11, 12, 13, 14, 19, 20 are not equivalent to their R shapes
  return(dframe)
}