##' Theme without checks. This would allow us to write
##' theme_animint(width=500), which would give an error under standard
##' ggplot2.
##' @param ... theme options.
##' @param complete TRUE or FALSE.
##' @return ggplot theme list.
##' @export
##' @author Toby Dylan Hocking
theme_animint <- function(..., complete=FALSE){
  elements <- list(...)
  names(elements) <- paste0("animint.", names(elements))
  structure(elements, class=c("theme", "gg"), complete=complete)
}
