##' Theme without checks. This allows us to write
##' theme_animint(width=500), instead of theme(animint.width=500)
##' which gives an error in ggplot2 because users should be informed
##' if they mis-type standard theme element
##' names. https://github.com/hadley/ggplot2/issues/938
##' @param ... theme options such as width.
##' @return ggplot theme list with names such as animint.width.
##' @export
##' @author Toby Dylan Hocking
theme_animint <- function(...){
  elements <- list(...)
  names(elements) <- paste0("animint.", names(elements))
  elements$validate <- FALSE
  do.call(theme, elements)
}
