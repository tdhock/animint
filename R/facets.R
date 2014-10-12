
# Determine titles to put on facet panels.
# The implementation here is a modified version of ggplot2:::facet_strips.
getStrips <- function(facet, panel, ...)
  # ... is a placeholder at the moment in case we want to implement 
  # themes or special options later
  UseMethod("getStrips")

getStrips.grid <- function(facet, panel, ...) {
  npanels <- nrow(panel$layout)
  # preallocate strip labels to a default of ""
  strips.empty <- strips.right <- strips.top <- rep("", npanels)
  # right strips in a grid are only drawn on the last column
  row_vars <- unique(panel$layout[names(facet$rows)])
  strips.right[with(panel$layout, COL == max(COL))] <- 
    build_strip(panel, row_vars, facet$labeller, side = "right", ...)
  # top strips in a grid layout are only drawn on the first row
  col_vars <- unique(panel$layout[names(facet$cols)])
  strips.top[panel$layout$ROW == 1] <-
    build_strip(panel, col_vars, facet$labeller, side = "top", ...)
  strips <- list(right = strips.right, top = strips.top)
  # the right/top element should exist if there are non-trivial labels
  strips[sapply(strips, function(x) !identical(x, strips.empty))]
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
  # unlike ggplot2, we collapse "layers" of strips into 1 layer
  apply(labels, 1, paste, collapse = "; ")
}

getStrips.wrap <- function(facet, panel, ...) {
  labels_df <- panel$layout[names(facet$facets)]
  labels_df[] <- plyr::llply(labels_df, format, justify = "none")
  # facet_wrap labels always go on top
  # we return a list so p_info.strips is always an object (on the JS side)
  l <- list(top = apply(labels_df, 1, paste, collapse = ", "))
  l[!identical(l$top, rep("", nrow(panel$layout)))]
}

getStrips.null <- function(facet, panel, ...) {
  return(list())
}

# TODO: how to 'train_layout' for non-cartesian coordinates?
# https://github.com/hadley/ggplot2/blob/dfcb56ec067910e1a3a04693d8f1e146cc7fb796/R/coord-.r

train_layout <- function(facet, coord, layout, ranges) {
  npanels <- dim(layout)[1]
  nrows <- max(layout$ROW)
  ncols <- max(layout$COL)
  ydiffs <- sapply(ranges, function(z) diff(z$y.range))
  xdiffs <- sapply(ranges, function(z) diff(z$x.range))
  # if x or y scale is 'free', then ignore the ratio
  if (length(unique(xdiffs)) > 1 || length(unique(ydiffs)) > 1) 
    coord$ratio <- NULL
  has.ratio <- !is.null(coord$ratio)
  layout$coord_fixed <- has.ratio
  if (has.ratio) { 
    spaces <- fixed_spaces(ranges, coord$ratio)
    layout <- cbind(layout, width_proportion = spaces$x, height_proportion = spaces$y)
    layout$width_proportion <- layout$width_proportion/ncols
    layout$height_proportion <- layout$height_proportion/nrows
  } else {
    vars <- NULL
    if (isTRUE(facet$space_free$x)) vars <- c(vars, "x")
    if (isTRUE(facet$space_free$y)) vars <- c(vars, "y")
    if (is.null(vars)) { # fill the entire space and give panels equal area
      layout <- cbind(layout, width_proportion = rep(1/ncols, npanels),
                      height_proportion = rep(1/nrows, npanels))
    } else { #special handling for 'free' space
      for (xy in vars) {
        u.type <- toupper(xy)
        l.type <- tolower(xy)
        scale.type <- paste0("SCALE_", u.type)
        range.type <- paste0(l.type, ".range")
        space.type <- paste0("SPACE_", u.type)
        vals <- layout[[scale.type]]
        uv <- unique(vals)
        diffs <- sapply(ranges[which(vals %in% uv)],
                        function(x) { diff(x[[range.type]]) })
        udiffs <- unique(diffs)
        # decide the proportion of the height/width each scale deserves based on the range
        props <- data.frame(tmp1 = uv, tmp2 = udiffs / sum(udiffs))
        names(props) <- c(scale.type, space.type)
        layout <- plyr::join(layout, props, by = scale.type)
      }
      names(layout) <- gsub("SPACE_X", "width_proportion", names(layout), fixed = TRUE)
      names(layout) <- gsub("SPACE_Y", "height_proportion", names(layout), fixed = TRUE)
    }
  }
  getDisplacement(layout)
}

# fixed cartesian coordinates (on a 0-1 scale)
# inspired from https://github.com/hadley/ggplot2/blob/dfcb56ec067910e1a3a04693d8f1e146cc7fb796/R/coord-fixed.r#L34-36
fixed_spaces <- function(ranges, ratio = 1) {
  aspect <- sapply(ranges, 
                   function(z) diff(z$y.range) / diff(z$x.range) * ratio)
  spaces <- list(y = aspect)
  spaces$x <- 1/spaces$y
  lapply(spaces, function(z) min(z, 1))
}

# compute x/y coordinates of where to start drawing each panel
getDisplacement <- function(layout) {
  npanels <- dim(layout)[1]
  if (npanels == 1) {
    xdisplace <- (1 - layout$width_proportion)/2
    ydisplace <- (1 - layout$height_proportion)/2
    layout <- cbind(layout, xdisplace, ydisplace)
  } else {
    xvars <- c("COL", "width_proportion")
    x <- unique(layout[xvars])
    xinit <- (1 - sum(x$width_proportion))/2
    xprop <- x$width_proportion
    x$xdisplace <- cumsum(c(xinit, xprop[-length(xprop)]))
    yvars <- c("ROW", "height_proportion")
    y <- unique(layout[yvars])
    yinit <- (1 - sum(y$height_proportion))/2
    yprop <- y$height_proportion
    y$ydisplace <- cumsum(c(yinit, yprop[-length(yprop)]))
    layout <- plyr::join(layout, x, by = xvars)
    layout <- plyr::join(layout, y, by = yvars)
  }
  layout
}


# Attach AXIS_X/AXIS_Y columns to the panel layout if 
# facet_grids is used.
# Currently every axis is rendered,
# but this could be helpful if we decide not to that.
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
