#### helper functions to be reused in testing #####

#' Apply `animint2dir` to a list ggplots and extract the (rendered) page source via RSelenium
#' 
#' @details This function assumes an object named 'context' exists -- this should be a 
#' character string as it will be used specify a directory which is bound to a testing context.
#' @author Carson Sievert
#' @param plotList A named list of ggplot2 objects
#' @param dir a name for a directory (this should be specific to a testing context)
#' @param subdir a name for a subdirectory (under dir) to place files
animint2HTML <- function(plotList, out.dir = "htmltest") {
  res <- animint2dir(plotList, out.dir, open.browser = FALSE)
  address <- sprintf("http://localhost:4848/testthat/%s/", out.dir)
  remDr$navigate(address)
  ## find/get methods are kinda slow in RSelenium (here is an example)
  ## remDr$navigate(attr(info, "address"))
  ## t <- remDr$findElement(using = 'class name', value = 'title')
  ## t$getElementText()[[1]]

  ## I think parsing using XML::htmlParse() and XML::getNodeSet() is faster/easier
  res$html <- XML::htmlParse(remDr$getPageSource(), asText = TRUE)

  res
}

expect_links <- function(info, urls){
  expect_attrs(info, "a", "href", urls)
}

expect_attrs <- function(info, element.name, attr.name, urls){
  stopifnot(is.character(urls))
  xpath <- paste0("//", element.name)
  pattern <- paste0(attr.name, "$")
  node.set <- getNodeSet(info$html, xpath)
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

expect_styles <- function(info, styles.expected){
  stopifnot(is.list(styles.expected))
  stopifnot(!is.null(names(styles.expected)))
  geom <- getNodeSet(info$html, '//*[@class="geom"]')
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
