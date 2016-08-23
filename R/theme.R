##' theme for passing animint specific params
##' 
##' Theme without checks. This allows us to write
##' \code{theme_animint(width=500)}, instead of \code{theme(animint.width=500)}
##' which gives an error in ggplot2 because users should be informed
##' if they mis-type standard theme element
##' names. https://github.com/hadley/ggplot2/issues/938
##' @param ... theme options such as \code{width}. Use \code{update_axes=c("x", "y")} to update the axes of plots. Works for single selection variables.
##' @return ggplot theme list with names such as \code{animint.width}.
##' @examples 
##' mtcars$cyl <- as.factor(mtcars$cyl)
##' p <- ggplot() +
##'   geom_point(aes(x=wt, y=mpg, colour=cyl),
##'              data=mtcars) +
##'   ## set width and height values and update both axes
##'   theme_animint(width=600, height=600, update_axes=c("x", "y"))
##' viz <- list(plot=p, selector.types=list(cyl="single"))
##' animint2dir(viz)
##' @export
##' @author Toby Dylan Hocking
theme_animint <- function(...){
  elements <- list(...)
  names(elements) <- paste0("animint.", names(elements))
  elements$validate <- FALSE
  do.call(theme, elements)
}
