library(testthat)
context("malaria data viz")

library(animint)
data(malaria)

fp.fn.colors <- c(FP="skyblue",
                  fp="skyblue",
                  fn="#E41A1C",
                  FN="#E41A1C",
                  tn="white",
                  tp="grey",
                  errors="black")

chrom2int <- function(chrom){
  only.num <- sub("PyYM_([0-9]{2})_v1", "\\1", chrom)
  factor(as.integer(only.num), 1:14)
}

for(df.name in names(malaria)){
  df <- malaria[[df.name]]
  if("chrom" %in% names(df)){
    malaria[[df.name]]$chrom.fac <- chrom2int(df$chrom)
  }
}

rownames(malaria$amplicons) <- malaria$amplicons$LOCUS_ID
normalize <- function(LOCUS_ID, position){
  LID <- paste(LOCUS_ID)
  firstVariant <- malaria$amplicons[LID, ]$firstVariant
  lastVariant <- malaria$amplicons[LID, ]$lastVariant
  mid <- (firstVariant+lastVariant)/2
  left <- firstVariant
  left <- mid-200
  right <- mid+200
  bases <- right - left
  (position-left)/bases
}

malaria$error.variants$POS.norm <- with(malaria$error.variants, {
  normalize(LOCUS_ID, POS)
})
malaria$regions$regionStart.norm <- with(malaria$regions, {
  normalize(LOCUS_ID, regionStart)
})
malaria$regions$regionEnd.norm <- with(malaria$regions, {
  normalize(LOCUS_ID, regionEnd)
})
malaria$amplicons$firstVariant.norm <- with(malaria$amplicons, {
  normalize(LOCUS_ID, firstVariant)
})
malaria$amplicons$lastVariant.norm <- with(malaria$amplicons, {
  normalize(LOCUS_ID, lastVariant)
})

viz <-
  list(errorCurves=ggplot()+
         theme_bw()+
         ggtitle(paste("error curves, select MQ threshold"))+
         xlab("MQ threshold")+
         ylab("incorrectly called variants")+
         make_tallrect(malaria$error.curves, "filterVar.thresh")+
         geom_line(aes(filterVar.thresh, metric.value,
                       group=metric.name,
                       color=metric.name),
                   data=malaria$error.curves)+
         scale_color_manual(values=fp.fn.colors)+
         geom_text(aes(filterVar.thresh, metric.value+offset,
                       color=metric.name,
                       label=paste(metric.value, metric.name, " "),
                       showSelected=filterVar.thresh),
                   hjust=1,
                   data=malaria$error.curves),

       chroms=ggplot()+
         theme_bw()+
         ggtitle("Sanger sequenced amplicons")+
         theme_animint(width=600)+
         geom_text(aes(chrom.fac, position/1e3,
                       label=sprintf("MQ threshold = %.1f",
                         filterVar.thresh),
                       showSelected=filterVar.thresh),
                   data=malaria$filterVar.labels)+
         geom_text(aes(chrom.fac, position/1e3,
                       label=paste(fp, "fp_"),
                       clickSelects=LOCUS_ID,
                       showSelected3=annotation,
                       showSelected2=highly.divergent.regions,
                       showSelected=filterVar.thresh),
                   hjust=1,
                   color=fp.fn.colors[["fp"]],
                   data=subset(malaria$error.amplicons, fp != 0))+
         geom_text(aes(chrom.fac, position/1e3,
                       label=paste0("_" , fn, " fn"),
                       clickSelects=LOCUS_ID,
                       showSelected3=annotation,
                       showSelected2=highly.divergent.regions,
                       showSelected=filterVar.thresh),
                   color=fp.fn.colors[["fn"]],
                   hjust=0,
                   data=subset(malaria$error.amplicons, fn != 0))+
         geom_segment(aes(chrom.fac, 0, 
                          yend=bases/1e3, xend=chrom.fac),
                      data=malaria$chroms)+
         geom_point(aes(chrom.fac, position/1e3,
                        color=highly.divergent.regions,
                        fill=annotation,
                        clickSelects=LOCUS_ID),
                    size=5,
                    pch=21,
                    data=malaria$amplicons)+
         scale_color_manual(values=c(none="white", some="black"))+
         scale_x_discrete("Malaria parasite yoelii yoelii chromosome",
                          drop=FALSE)+
         ylab("position on chromosome (kilo bases = kb)"),

       variants=ggplot()+
         theme_bw()+
         ggtitle("Variants in each sanger sequenced amplicon")+
         theme_animint(width=1000, height=600)+
         scale_fill_manual(values=fp.fn.colors)+
         scale_y_discrete("amplicon LOCUS_ID", drop=FALSE)+
         scale_x_continuous("relative position on amplicon",
                            limits=c(-0.05, 1.05),
                            breaks=c())+
         geom_text(aes(firstVariant.norm, LOCUS_ID,
                       showSelected=highly.divergent.regions,
                       showSelected2=annotation,
                       label=paste0(firstVariant, "_")),
                   hjust=1,
                   data=malaria$amplicons)+
         geom_text(aes(lastVariant.norm, LOCUS_ID,
                       showSelected=highly.divergent.regions,
                       showSelected2=annotation,
                       label=paste0("_", lastVariant, " --- ",
                                    lastVariant-firstVariant, " bases")),
                   hjust=0,
                   data=malaria$amplicons)+
         geom_segment(aes(firstVariant.norm, LOCUS_ID,
                          xend=lastVariant.norm, yend=LOCUS_ID,
                          showSelected=highly.divergent.regions,
                          showSelected2=annotation,
                          clickSelects=LOCUS_ID),
                      size=12,
                      alpha=0.6,
                      data=malaria$amplicons)+
         geom_segment(aes(regionStart.norm, LOCUS_ID,
                          xend=regionEnd.norm, yend=LOCUS_ID,
                          showSelected=highly.divergent.regions,
                          showSelected2=annotation,
                          color=region.type),
                      size=8,
                      data=malaria$regions)+
         scale_color_manual(values=c("#E41A1C", #red
                              "#377EB8", #blue
                              "#4DAF4A", #green
                              "#984EA3", #purple
                              "#FF7F00", #orange
                              LCR="#FFFF33", #yellow
                              "#A65628",
                              "#F781BF",
                                     HDR="black"))+
         geom_point(aes(POS.norm, LOCUS_ID,
                        tooltip=paste(Coding, Variant_type),
                        showSelected=highly.divergent.regions,
                        showSelected2=annotation,
                        showSelected3=filterVar.thresh,
                        fill=error.type),
                    color="black",
                    pch=21,
                    size=4,
                    data=malaria$error.variants),

       first=list(filterVar.thresh=malaria$filterVar$best.thresh),

       title="Malaria parasite NextGenSeq variant calling errors")

info <- animint2HTML(viz)

style.pattern <-
  paste0("(?<name>\\S+?)",
         ": *",
         "(?<value>.+?)",
         ";")

get.style.vec <- function(xpath){
  node.set <- getNodeSet(getHTML(), xpath)
  expect_equal(length(node.set), 1)
  node <- node.set[[1]]
  attr.vec <- xmlAttrs(node)
  if("style" %in% names(attr.vec)){
    style <- attr.vec[["style"]]
    style.mat <- str_match_all_perl(style, style.pattern)[[1]]
    style.vec <- style.mat[, "value"]
    names(style.vec) <- rownames(style.mat)
    style.vec
  }else{
    character()
  }
}

region.lines <-
  getNodeSet(info$html, '//g[@class="geom12_segment_variants"]//line')

test_that("one line is rendered for each region", {
  expect_equal(length(region.lines), nrow(malaria$regions))
})

some <- get.style.vec('//tr[@id="some"]//circle')
none <- get.style.vec('//tr[@id="none"]//circle')

test_that("geom_point(aes(color)) legend shows as circle stroke", {
  expect_true(some[["stroke"]] != none[["stroke"]])
  expect_true(some[["fill"]] == none[["fill"]])
})

get.opacity <- function(xpath){
  style.vec <- get.style.vec(xpath)
  if("opacity" %in% names(style.vec)){
    as.numeric(style.vec[["opacity"]])
  }else{
    1
  }
}

intergenic.before <- get.opacity('//td[@id="INTERGENIC"]')

test_that("INTERGENIC legend entry opacity 1 before clicking", {
  expect_equal(intergenic.before, 1)
})

clickID("INTERGENIC")

intergenic.after <- get.opacity('//td[@id="INTERGENIC"]')

test_that("INTERGENIC legend entry opacity 0.5 after clicking", {
  expect_equal(intergenic.after, 0.5)
})

none.before <- get.opacity('//td[@id="none"]')

test_that("none legend entry opacity 1 before clicking", {
  expect_equal(none.before, 1)
})

clickID("none")

none.after <- get.opacity('//td[@id="none"]')

test_that("none legend entry opacity 0.5 after clicking", {
  expect_equal(none.after, 0.5)
})

## TODO: test number of geoms rendered in chroms and variants plots,
## before and after clicking.



