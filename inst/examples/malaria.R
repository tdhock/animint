library(animint)
data(malaria)

filterVar <- malaria$filterVar$filterVar

fp.fn.colors <- c(FP="skyblue",
                  fp="skyblue",
                  fn="#E41A1C",
                  FN="#E41A1C",
                  tn="white",
                  tp="grey",
                  errors="black")
fp.fn.linetypes <- c(errors="solid",
                     false.positive="solid",
                     false.negative="solid",
                     imprecision="dashed")
fp.fn.sizes <- c(errors=1,
                 false.positive=3,
                 false.negative=3,
                 imprecision=1)/1.2

chrom2int <- function(chrom){
  only.num <- sub("PyYM_([0-9]{2})_v1", "\\1", chrom)
  factor(as.integer(only.num), 1:14)
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

viz <-
  list(errorCurves=ggplot()+
         theme_bw()+
         ggtitle(paste("error curves, select",
                       filterVar, "threshold"))+
         xlab(paste(filterVar, "threshold"))+
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
         geom_text(aes(chrom2int(chrom), position/1e3,
                       label=sprintf("%s threshold = %.1f",
                         filterVar, filterVar.thresh),
                       showSelected=filterVar.thresh),
                   data=malaria$filterVar.labels)+
         geom_text(aes(chrom2int(chrom), position/1e3,
                       label=paste(fp, "fp_"),
                       clickSelects=LOCUS_ID,
                       showSelected3=annotation,
                       showSelected2=highly.divergent.regions,
                       showSelected=filterVar.thresh),
                   hjust=1,
                   color=fp.fn.colors[["fp"]],
                   data=subset(malaria$error.amplicons, fp != 0))+
         geom_text(aes(chrom2int(chrom), position/1e3,
                       label=paste0("_" , fn, " fn"),
                       clickSelects=LOCUS_ID,
                       showSelected3=annotation,
                       showSelected2=highly.divergent.regions,
                       showSelected=filterVar.thresh),
                   color=fp.fn.colors[["fn"]],
                   hjust=0,
                   data=subset(malaria$error.amplicons, fn != 0))+
         geom_segment(aes(chrom2int(chrom), 0, 
                          yend=bases/1e3, xend=chrom2int(chrom)),
                      data=malaria$chroms)+
         geom_point(aes(chrom2int(chrom), position/1e3,
                        color=highly.divergent.regions,
                        fill=annotation,
                        clickSelects=LOCUS_ID),
                    size=5,
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
         geom_text(aes(normalize(LOCUS_ID, firstVariant), LOCUS_ID,
                       showSelected=highly.divergent.regions,
                       showSelected2=annotation,
                       label=paste0(firstVariant, "_")),
                   hjust=1,
                   data=malaria$amplicons)+
         geom_text(aes(normalize(LOCUS_ID, lastVariant), LOCUS_ID,
                       showSelected=highly.divergent.regions,
                       showSelected2=annotation,
                       label=paste0("_", lastVariant, " --- ",
                                    lastVariant-firstVariant, " bases")),
                   hjust=0,
                   data=malaria$amplicons)+
         geom_segment(aes(normalize(LOCUS_ID, firstVariant), LOCUS_ID,
                          xend=normalize(LOCUS_ID, lastVariant), yend=LOCUS_ID,
                          showSelected=highly.divergent.regions,
                          showSelected2=annotation,
                          clickSelects=LOCUS_ID),
                      size=12,
                      alpha=0.6,
                      data=malaria$amplicons)+
         geom_segment(aes(normalize(LOCUS_ID, regionStart), LOCUS_ID,
                          xend=normalize(LOCUS_ID, regionEnd), yend=LOCUS_ID,
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
         geom_point(aes(normalize(LOCUS_ID, POS), LOCUS_ID,
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

animint2dir(viz, "malaria")

## BUG: metric.name and highly.divergent.regions legend entries do not
## fade to opacity: 0.5 after clicking.

## BUG: HDR color legend shows fill not stroke!

## BUG: LCR region.type does not show up at first (but it does after
## clicking the region.type legend).

##animint2gist(viz)

