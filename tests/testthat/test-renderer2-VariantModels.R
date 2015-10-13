acontext("VariantModels data viz")

data(VariantModels)

auc.min.error <- subset(VariantModels$auc, metric.name=="min.error")

add.filterVar <- function(df, levs){
  df$filterVar.fac <- factor(df$filterVar, levs)
  df
}
add.filterVar.fac <- function(df){
  add.filterVar(df, rev(paste(VariantModels$ranks$filterVar)))
}
add.filterVar.rev <- function(df){
  add.filterVar(df, paste(VariantModels$ranks$filterVar))
}

thresh.colors <- c("min error"="black", selected="white")
method.colors <- 
  c(knn="#8DD3C7", #green
    "#FFFFB3", #yellow
    svmRadial="#BEBADA", #pale violet
    ada="#FB8072", #pink-orange
    gbm="#FB8072", #pink-orange
    glmnet="#80B1D3", #blue
    glmnetBinDev="#80B1D3", #blue
    glmnetAcc="#80B1D3", #blue
    MQ="#FDB462", #orange
    QUAL="#B3DE69", #green
    NegFQ="#FCCDE5", #pink-violet
    DP="#D9D9D9", #grey
    rf="#BC80BD", #purple
    "#CCEBC5", #greenish yellow
    "#FFED6F") #gold
fp.fn.colors <- c(FP="skyblue",
                  fp="skyblue",
                  fn="#E41A1C",
                  FN="#E41A1C",
                  tn="white",
                  tp="grey",
                  errors="black")

first.list <- with(auc.min.error, {
  structure(as.list(threshold), names=paste0(filterVar, "_fold", test.fold))
})
first.list$test.fold <- 2

viz <- list(
  auc=ggplot()+
    ggtitle("Performance on 3 test folds")+
    theme_bw()+
    theme_animint(height=500)+
    theme(panel.margin=grid::unit(0, "cm"))+
    facet_grid(.~metric.name, scales="free", space="fixed")+
    scale_y_discrete("method . weights")+
    scale_x_continuous("")+
    scale_color_manual(values=method.colors, guide="none")+
    scale_fill_manual("threshold", values=thresh.colors, guide="none")+
    geom_point(aes(metric.value, filterVar.fac, color=method,
                   fill=thresh.type,
                   showSelected=method,
                   showSelected2=thresh.type,
                   clickSelects=test.fold),
               size=5,
               pch=21,
               data=add.filterVar.rev(VariantModels$auc))+
    geom_point(aes(
      error.or.Inf,
      filterVar.fac, 
      showSelected=test.fold,
      key=filterVar,
      showSelected2=thresh.type,
      showSelected3=method,
      showSelected.variable=paste0(filterVar, "_fold", test.fold),
      showSelected.value=threshold,
      fill=thresh.type, color=method),
               size=4,
               pch=21,
               data=add.filterVar.rev(VariantModels$roc)),
  roc=ggplot()+
    ggtitle("ROC curves by weights and test fold")+
    scale_y_continuous("True positive rate")+
    scale_x_continuous("False positive rate",
                       breaks=c(0, 0.25, 0.5, 0.75, 1),
                       labels=c("0", "0.25", "0.5", "0.75", "1"))+
    scale_color_manual(values=method.colors)+
    coord_equal()+
    theme_bw()+
    theme_animint(width=500, height=500)+
    theme(panel.margin=grid::unit(0, "cm"))+
    facet_grid(test.fold ~ type, labeller=function(var, val){
      if(var=="test.fold"){
        paste("test fold", val)
      }else{
        paste(val)
      }
    })+
    geom_path(aes(FPR, TPR, clickSelects=test.fold,
                  showSelected=method,
                  group=method, tooltip=method, color=method),
              size=5,
              data=VariantModels$roc)+
    scale_fill_manual("threshold", values=thresh.colors)+
    geom_point(aes(FPR, TPR, color=method,
                   showSelected=method,
                   clickSelects=test.fold,
                   fill=thresh.type),
               pch=21,
               size=4,
               data=VariantModels$auc)+
    geom_point(aes(
      FPR, TPR, clickSelects=test.fold,
      key=method,
      showSelected.variable=paste0(filterVar, "_fold", test.fold),
      showSelected.value=threshold,
      showSelected=method,
      fill=thresh.type, color=method),
               size=3,
               pch=21,
               data=VariantModels$roc),
  error=ggplot()+
    geom_hline(aes(yintercept=min.errors,
                   showSelected=test.fold),
               data=VariantModels$minima,
               color="grey50")+
    geom_vline(aes(xintercept=threshold,
                   showSelected2=method,
                   showSelected3=thresh.type,
                   showSelected=test.fold),
               data=add.filterVar.fac(auc.min.error),
               color="grey50")+
    theme_bw()+
    theme_animint(width=1800, height=500)+
    theme(panel.margin=grid::unit(0, "cm"))+
    theme(axis.text.x=element_text(angle=90))+
    facet_grid(. ~ filterVar.fac, labeller=function(var, val){
      sub("balanced", "b", sub("one", "1", val))
    }, scales="free", space="fixed")+
    scale_color_manual(values=fp.fn.colors)+
    geom_line(aes(threshold, error.value,
                  showSelected=test.fold,
                  showSelected2=method,
                  showSelected3=thresh.type,
                  group=error.type, color=error.type),
              data=add.filterVar.fac(VariantModels$error))+
    scale_fill_manual(values=method.colors, guide="none")+
    geom_tallrect(aes(
      xmin=xmin, xmax=xmax,
      showSelected=test.fold,
      showSelected2=method,
      showSelected3=thresh.type,
      clickSelects.variable=paste0(filterVar.fac, "_fold", test.fold),
      clickSelects.value=threshold,
      fill=method),
                  alpha=0.5,
                  color=NA,
                  data=add.filterVar.fac(VariantModels$thresholds)),
  selector.types=list(method="multiple", thresh.type="multiple"),
  title="3-fold CV estimates variant calling test error",
  first=first.list,
  duration=with(auc.min.error, {
    structure(as.list(rep(2000, length(threshold))),
              names=paste0(filterVar, "_fold", test.fold))
  })
)

viz$error+
  facet_grid(test.fold ~ filterVar.fac, labeller=function(var, val){
    if(var=="test.fold"){
      paste("test fold", val)
    }else{
      paste(val)
    }
  }, scales="free", space="fixed")

info <- animint2HTML(viz)

test_that("no duplicated rows in common data", {
  common.tsv <- file.path("animint-htmltest", "geom8_line_error_chunk_common.tsv")
  common.df <- read.table(common.tsv, comment.char="", header=TRUE)
  common.unique <- unique(common.df)
  expect_identical(common.unique, common.df)
})

test_that("error lines rendered in all panels", {
  panel.list <- getNodeSet(info$html, '//g[@class="geom8_line_error"]//g')
  computed.counts <- sapply(panel.list, function(x)length(xmlChildren(x)))
  expected.counts <- rep(3, 20)
  expect_equal(computed.counts, expected.counts)
})
