
# Determine titles to put on facet panels.
# The implementation here is a modified version of ggplot2:::facet_strips.
getStrips <- function(facet, panel, ...)
  # ... is a placeholder at the moment in case we want to implement 
  # themes or special options later
  UseMethod("getStrips")

getStrips.grid <- function(facet, panel, ...) {
  col_vars <- unique(panel$layout[names(facet$cols)])
  row_vars <- unique(panel$layout[names(facet$rows)])
  list(
    right = build_strip(panel, row_vars, facet$labeller, side = "right", ...),
    top = build_strip(panel, col_vars, facet$labeller, side = "top", ...)
  )
}

build_strip <- function(panel, label_df, labeller, side = "right", ...) {
  side <- match.arg(side, c("top", "left", "bottom", "right"))
  horizontal <- side %in% c("top", "bottom")
  labeller <- match.fun(labeller)
  # No labelling data, so return empty string?
  if (plyr::empty(label_df)) {
    return("")
  }
  # Create matrix of labels
  labels <- matrix(list(), nrow = nrow(label_df), ncol = ncol(label_df))
  for (i in seq_len(ncol(label_df))) {
    labels[, i] <- labeller(names(label_df)[i], label_df[, i])
  }
  labels
}

getStrips.wrap <- function(facet, panel, ...) {
  labels_df <- panel$layout[names(facet$facets)]
  labels_df[] <- plyr::llply(labels_df, format, justify = "none")
  apply(labels_df, 1, paste, collapse = ", ")
}

getStrips.null <- function(facet, panel, ...) {
  return("")
}


# Attach AXIS_X/AXIS_Y columns to the panel layout if 
# facet_grids is used.

flag_axis <- function(facet, layout) 
 UseMethod("flag_axis")

flag_axis.grid <- function(facet, layout) {
  # 'grid rules' are to draw y-axis on panels with COL == 1
  # and ROW == max(ROW).
  layout$AXIS_Y <- layout$COL == 1
  layout$AXIS_X <- layout$ROW == max(layout$ROW)
  layout
}

flag_axis.wrap <- function(facet, layout) {
  if (sum(grepl("^AXIS_[X-Y]$", names(layout))) != 2)
    stop("Expected 'AXIS_X' and 'AXIS_Y' to be in panel layout")
  layout
}

flag_axis.null <- function(facet, layout) {
  cbind(layout, AXIS_X = TRUE, AXIS_Y = TRUE)
}



#####
# OLD CODE
#####

# Filter ranges to keep only axis information to be rendered
#
# Each facet *could* have it's own x/y axis. 'Built ranges' contain
# info on each axis that *could* be drawn. It turns out that
# facet_wrap and facet_grid have different rules for drawing axes,
# so this function will determine which to keep where to NULL out
# the axis info.

# filter_range <- function(facet, range, layout) 
#   UseMethod("filter_range")
# 
# #' @export
# filter_range.wrap <- function(facet, range, layout) {
#   for (xy in c("x", "y")) {
#     cap.xy <- toupper(xy)
#     idx <- lapply(range, seq_along)
#     candidate <- which(!layout[[paste0("AXIS_", cap.xy)]])
#     if (length(candidate)) {
#       expr <- paste0("^", xy, "\\.")
#       range[candidate] <- lapply(range[candidate], 
#                                  function(x) x[!grepl(expr, names(x))])
#     }
#   }
#   range
# }
# 
# #' @export
# filter_range.grid <- function(facet, range, layout) {
#   # 'grid rules' are to draw y-axis on panels with COL == 1
#   # and ROW == max(ROW). This function assumes the order of range
#   # matches the panel (is this a safe assumption?)
#   draw.y <- layout$COL == 1
#   draw.x <- layout$ROW == max(layout$ROW)
#   candidate <- which(!draw.y)
#   if (length(candidate)) {
#     range[candidate] <- lapply(range[candidate], 
#                                function(x) x[!grepl("^y\\.", names(x))])
#   }
#   candidate <- which(!draw.x)
#   if (length(candidate)) {
#     range[candidate] <- lapply(range[candidate], 
#                                function(x) x[!grepl("^x\\.", names(x))])
#   }
#   range
# }
