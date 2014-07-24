# Filter ranges to keep only axis information to be rendered
#
# Each facet *could* have it's own x/y axis. 'Built ranges' contain
# info on each axis that *could* be drawn. It turns out that
# facet_wrap and facet_grid have different rules for drawing axes,
# so this function will determine which to keep where to NULL out
# the axis info.

filter_range <- function(facet, range, layout) 
  UseMethod("filter_range")

#' @export
filter_range.wrap <- function(facet, range, layout) {
  for (xy in c("x", "y")) {
    cap.xy <- toupper(xy)
    idx <- lapply(range, seq_along)
    candidate <- which(!layout[[paste0("AXIS_", cap.xy)]])
    if (length(candidate)) {
      expr <- paste0("^", xy, "\\.")
      range[candidate] <- lapply(range[candidate], 
                                 function(x) x[!grepl(expr, names(x))])
    }
  }
  range
}

#' @export
filter_range.grid <- function(facet, range, layout) {
  # 'grid rules' are to draw y-axis on panels with COL == 1
  # and ROW == max(ROW). This function assumes the order of range
  # matches the panel (is this a safe assumption?)
  draw.y <- layout$COL == 1
  draw.x <- layout$ROW == max(layout$ROW)
  candidate <- which(!draw.y)
  if (length(candidate)) {
    range[candidate] <- lapply(range[candidate], 
                               function(x) x[!grepl("^y\\.", names(x))])
  }
  candidate <- which(!draw.x)
  if (length(candidate)) {
    range[candidate] <- lapply(range[candidate], 
                               function(x) x[!grepl("^x\\.", names(x))])
  }
  range
}
