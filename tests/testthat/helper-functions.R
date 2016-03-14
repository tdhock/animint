#' Apply `animint2dir` to a list ggplots and extract the (rendered) page source via RSelenium
#'
#' @param plotList A named list of ggplot2 objects
animint2HTML <- function(plotList) {
  res <- animint2dir(plotList, out.dir = "animint-htmltest", 
                     open.browser = FALSE)
  remDr$refresh()
  Sys.sleep(1)
  res$html <- getHTML()
  res
}

translatePattern <-
  paste0("translate[(]",
         "(?<x>.*?)",
         ",",
         "(?<y>.*?)",
         "[)]")

acontext <- function(...){
  print(...)
  context(...)
}

## Parse the first occurance of pattern from each of several strings
## using (named) capturing regular expressions, returning a matrix
## (with column names).
str_match_perl <- function(string,pattern){
  stopifnot(is.character(string))
  stopifnot(is.character(pattern))
  stopifnot(length(pattern)==1)
  parsed <- regexpr(pattern,string,perl=TRUE)
  captured.text <- substr(string,parsed,parsed+attr(parsed,"match.length")-1)
  captured.text[captured.text==""] <- NA
  captured.groups <- do.call(rbind,lapply(seq_along(string),function(i){
    st <- attr(parsed,"capture.start")[i,]
    if(is.na(parsed[i]) || parsed[i]==-1)return(rep(NA,length(st)))
    substring(string[i],st,st+attr(parsed,"capture.length")[i,]-1)
  }))
  result <- cbind(captured.text,captured.groups)
  colnames(result) <- c("",attr(parsed,"capture.names"))
  result
}

## Parse several occurances of pattern from each of several strings
## using (named) capturing regular expressions, returning a list of
## matrices (with column names).
str_match_all_perl <- function(string,pattern){
  stopifnot(is.character(string))
  stopifnot(is.character(pattern))
  stopifnot(length(pattern)==1)
  parsed <- gregexpr(pattern,string,perl=TRUE)
  lapply(seq_along(parsed),function(i){
    r <- parsed[[i]]
    starts <- attr(r,"capture.start")
    if(r[1]==-1)return(matrix(nrow=0,ncol=1+ncol(starts)))
    names <- attr(r,"capture.names")
    lengths <- attr(r,"capture.length")
    full <- substring(string[i],r,r+attr(r,"match.length")-1)
    subs <- substring(string[i],starts,starts+lengths-1)
    m <- matrix(c(full,subs),ncol=length(names)+1)
    colnames(m) <- c("",names)
    if("name" %in% names){
      rownames(m) <- m[, "name"]
    }
    m
  })
}

getSelectorWidgets <- function(html=getHTML()){
  tr.list <- getNodeSet(html, 
                        '//table[@class="table_selector_widgets"]//tr')
  td.list <- sapply(tr.list[-1], function(tr)xmlChildren(tr)[[1]])
  sapply(td.list, xmlValue)
}

clickHTML <- function(...){
  v <- c(...)
  stopifnot(length(v) == 1)
  e <- remDr$findElement(names(v), as.character(v))
  e$clickElement()
  Sys.sleep(1)
  getHTML()
}

clickID <- function(...){
  v <- c(...)
  stopifnot(length(v) == 1)
  e <- remDr$findElement("id", as.character(v))
  e$clickElement()
}

getHTML <- function(){
  XML::htmlParse(remDr$getPageSource(), asText = TRUE)
}

rgba.pattern <- paste0(
  "(?<before>rgba?)",
  " *[(] *",
  "(?<red>[0-9]+)",
  " *, *",
  "(?<green>[0-9]+)",
  " *, *",
  "(?<blue>[0-9]+)",
  "(?:",
  " *, *",
  "(?<alpha>[^)]+)",
  ")?",
  " *[)]")
ensure_rgba <- function(color.vec){
  match.mat <- str_match_perl(color.vec, rgba.pattern)
  is.not.rgb <- is.na(match.mat[,1])
  hex.vec <- toRGB(color.vec[is.not.rgb])
  not.rgb.mat <- col2rgb(hex.vec, alpha=TRUE)
  rgb.cols <- c("red", "green", "blue")
  match.mat[is.not.rgb, rgb.cols] <- t(not.rgb.mat[rgb.cols,])
  match.mat[is.not.rgb, "alpha"] <- not.rgb.mat["alpha",]/255
  is.rgb <- match.mat[, "before"] == "rgb"
  match.mat[is.rgb, "alpha"] <- 1
  is.transparent <- match.mat[, "alpha"] == 0
  match.mat[, rgb.cols] <- 0
  opacity <- as.numeric(match.mat[, "alpha"])
  if(any(is.na(opacity))){
    print(match.mat)
    stop("missing alpha opacity value")
  }
  match.mat[, "alpha"] <- paste(opacity)
  rgba.cols <- c(rgb.cols, "alpha")
  rgba.mat <- matrix(match.mat[, rgba.cols], nrow(match.mat), length(rgba.cols))
  no.paren <- apply(rgba.mat, 1, function(x)paste(x, collapse=", "))
  paste0("rgba(", no.paren, ")")
}
stopifnot(ensure_rgba("transparent") == ensure_rgba("rgba(0, 0, 0, 0.0)"))
stopifnot(ensure_rgba("rgba(0, 0, 0, 0.0)") == ensure_rgba("rgba(0, 0, 0, 0)"))
stopifnot(ensure_rgba("rgba(0, 0, 0, 0.1)") != ensure_rgba("rgba(0, 0, 0, 0)"))
stopifnot(ensure_rgba("rgb(0, 0, 0)") == ensure_rgba("rgba(0, 0, 0, 1)"))

expect_color <- function(computed.vec, expected.vec) {
  computed.rgb <- ensure_rgba(computed.vec)
  expected.rgb <- ensure_rgba(expected.vec)
  expect_identical(computed.rgb, expected.rgb)
}

expect_transform <- function(actual, expected, context = "translate", tolerance = 5) {
  # supports multiple contexts
  nocontext <- gsub(paste(context, collapse = "||"), "", actual)
  # reduce to some 'vector' of numbers: (a, b, c, ...)
  vec <- gsub("\\)\\(", ",", nocontext)
  clean <- gsub("\\)", "", gsub("\\(", "", vec))
  nums <- as.numeric(strsplit(clean, split = "\\,")[[1]])
  expect_equal(nums, expected, tolerance, scale = 1)
}

expect_links <- function(html, urls){
  expect_attrs(html, "a", "href", urls)
}

expect_attrs <- function(html, element.name, attr.name, urls){
  stopifnot(is.character(urls))
  xpath <- paste0("//", element.name)
  pattern <- paste0(attr.name, "$")
  node.set <- getNodeSet(html, xpath)
  rendered.urls <- rep(NA, length(node.set))
  for(node.i in seq_along(node.set)){
    node <- node.set[[node.i]]
    node.attrs <- xmlAttrs(node)
    href.i <- grep(pattern, names(node.attrs))
    if(length(href.i)==1){
      rendered.urls[[node.i]] <- node.attrs[[href.i]]
    }
  }
  for(u in urls){
    expect_true(u %in% rendered.urls)
  }
}

expect_styles <- function(html, styles.expected){
  stopifnot(is.list(styles.expected))
  stopifnot(!is.null(names(styles.expected)))
  geom <- getNodeSet(html, '//*[@class="geom"]')
  style.strs <- as.character(sapply(geom, function(x) xmlAttrs(x)["style"]))
  pattern <-
    paste0("(?<name>\\S+?)",
           ": *",
           "(?<value>.+?)",
           ";")
  style.matrices <- str_match_all_perl(style.strs, pattern)
  for(style.name in names(styles.expected)){
    style.values <- sapply(style.matrices, function(m)m[style.name, "value"])
    for(expected.regexp in styles.expected[[style.name]]){
      ## Test that one of the observed styles matches the expected
      ## regexp.
      expect_match(style.values, expected.regexp, all=FALSE)
    }
  }
}

getTextValue <- function(tick)xmlValue(getNodeSet(tick, "text")[[1]])

getStyleValue <- function(html, xpath, style.name) {
  node.list <- getNodeSet(html, xpath)
  attr.mat <- sapply(node.list, xmlAttrs)
  style.vec <- if("style" %in% rownames(attr.mat)){
    attr.mat["style", ]
  }else{
    rep("", length(node.list))
  }
  pattern <-paste0(
    "(?<name>\\S+?)",
    ": *",
    "(?<value>.+?)",
    ";")
  style.matrices <- str_match_all_perl(style.vec, pattern)
  sapply(style.matrices, function(m){
    ## style.name can be a vector of style names to extract!
    val.vec <- rep(NA, length(style.name))
    if(1 < length(style.name))names(val.vec) <- style.name
    found <- style.name %in% rownames(m)
    if(any(found)){
      style.found <- style.name[found]
      val.vec[found] <- m[style.found, "value"]
    }
    val.vec
  })
}

## testthat there is no warning generated by a piece of code.
gives_no_warning <- function(){
  function(expr) {
    warnings <- evaluate_promise(expr)$warnings
    s <- ifelse(length(warnings)==1, "", "s")
    expectation(
      length(warnings) == 0,
      paste0("created ", length(warnings),
             " warning", s, ": ",
             paste(warnings, collapse=", ")),
      "no warnings given"
    )
  }
}
expect_no_warning <- function(object, ..., info=NULL, label=NULL){
  if (is.null(label)) {
    label <- testthat:::find_expr("object")
  }
  expect_that(object, gives_no_warning(), info=info, label=label)
}

getTransform <- function(tick)xmlAttrs(tick)[["transform"]]

## get difference between axis ticks in both pixels and on original data scale
## @param doc rendered HTML document
## @param ticks which ticks? (can use text label of the tick)
## @param axis which axis?
getTickDiff <- function(doc, ticks = 1:2, axis="x"){
  g.ticks <- getNodeSet(doc, "g[@class='tick major']")
  tick.labs <- sapply(g.ticks, getTextValue)
  names(g.ticks) <- tick.labs
  tick.transform <- sapply(g.ticks[ticks], getTransform)
  trans.mat <- str_match_perl(tick.transform, translatePattern)
  num <- as.numeric(trans.mat[, axis])
  val <- abs(diff(num))
  attr(val, "label-diff") <- diff(as.numeric(names(tick.transform)))
  val
}
both.equal <- function(x, tolerance = 0.1){
  if(is.null(x) || !is.vector(x) || length(x) != 2){
    return(FALSE)
  }
  isTRUE(all.equal(x[[1]], x[[2]], tolerance))
}

# normalizes tick differences obtained by getTickDiff
normDiffs <- function(xdiff, ydiff, ratio = 1) {
  xlab <- attr(xdiff, "label-diff")
  ylab <- attr(ydiff, "label-diff")
  if (is.null(xlab) || is.null(ylab)) warning("label-diff attribute is missing")
  c(ratio * xdiff / xlab, ydiff / ylab)
}

getTicks <- function(html, p.name){
  xp <- sprintf('//svg[@id="%s"]//g[contains(@class, "xaxis")]//text', p.name)
  nodes <- getNodeSet(html, xp)
  stopifnot(length(nodes) > 0)
  sapply(nodes, xmlAttrs)
}

expect_rotate_anchor <- function(info, rotate, anchor){
  not <- getTicks(info$html, 'not')
  expect_match(not["style", ], "text-anchor: middle", fixed=TRUE)
  expect_match(not["transform", ], "rotate(0", fixed=TRUE)
  rotated <- getTicks(info$html, 'rotated')
  expect_match(rotated["style", ], paste("text-anchor:", anchor), fixed=TRUE)
  expect_match(rotated["transform", ], paste0("rotate(", rotate), fixed=TRUE)
  # http://stackoverflow.com/questions/442404/retrieve-the-position-x-y-of-an-html-element
  tick_box <- remDr$executeScript('return document.getElementsByClassName("xaxis")[0].firstChild.getBoundingClientRect()')
  title_box <- remDr$executeScript('return document.getElementsByClassName("xtitle")[0].getBoundingClientRect()')
  expect_true(title_box$top >= tick_box$bottom)
}
