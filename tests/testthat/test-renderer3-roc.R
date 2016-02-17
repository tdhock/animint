acontext("TestROC")

data(TestROC, package="animint")

path.before.params <- list(
  title="Test ROC curves, predicted peaks and errors",
  roc=ggplot()+
  geom_path(aes(FPR, TPR, group=Model, key=Model, color=Model),
            data=TestROC$roc)+
  geom_point(aes(FPR, TPR, color=Model, key=paste(model, parameter),
                 size=parameter,
                 fill=parameter),
             shape=21,
             data=subset(TestROC$parameters, parameter=="learned"))+
  geom_point(aes(FPR, TPR, color=Model, key=paste(model, parameter),
                 size=parameter,
                 fill=parameter),
             shape=21,
             data=subset(TestROC$parameters, parameter=="default"))+
  geom_point(aes(FPR, TPR, color=Model,
                 key=ModelParam,
                 clickSelects=ModelParam),
             size=4,
             alpha=0.9,
             data=subset(TestROC$roc, same.as.prev==FALSE))+
  scale_fill_manual(values=c(default="black", learned="white"))+
  scale_size_manual(values=c(default=7, learned=10))+
  scale_shape_manual(values=c(default=20, learned=1))+
  coord_equal()+
  theme_grey()+
  scale_x_continuous(paste(
    "False positive rate in test labels",
    "= Probability(peak | no peak)"),
                     breaks=seq(0, 1, by=0.2))+
  scale_y_continuous(paste(
    "True positive rate in test labels",
    "= Probability(peak | peak)"),
                     breaks=seq(0, 1, by=0.2))+
  theme(panel.margin=grid::unit(0, "lines")),
  first=list(ModelParam="PeakSegJoint -0.8"))

test_that("path before params, 5 paths rendered", {
  info <- animint2HTML(path.before.params)
  path.list <- getNodeSet(info$html, '//g[@class="geom1_path_roc"]//path')
  expect_equal(length(path.list), 5)
})

path.after.params <- list(
  title="Test ROC curves, predicted peaks and errors",
  roc=ggplot()+
  geom_point(aes(FPR, TPR, color=Model, key=paste(model, parameter),
                 size=parameter,
                 fill=parameter),
             shape=21,
             data=subset(TestROC$parameters, parameter=="learned"))+
  geom_point(aes(FPR, TPR, color=Model, key=paste(model, parameter),
                 size=parameter,
                 fill=parameter),
             shape=21,
             data=subset(TestROC$parameters, parameter=="default"))+
  geom_path(aes(FPR, TPR, group=Model, key=Model, color=Model),
            data=TestROC$roc)+
  geom_point(aes(FPR, TPR, color=Model,
                 key=ModelParam,
                 clickSelects=ModelParam),
             size=4,
             alpha=0.9,
             data=subset(TestROC$roc, same.as.prev==FALSE))+
  scale_fill_manual(values=c(default="black", learned="white"))+
  scale_size_manual(values=c(default=7, learned=10))+
  scale_shape_manual(values=c(default=20, learned=1))+
  coord_equal()+
  theme_grey()+
  scale_x_continuous(paste(
    "False positive rate in test labels",
    "= Probability(peak | no peak)"),
                     breaks=seq(0, 1, by=0.2))+
  scale_y_continuous(paste(
    "True positive rate in test labels",
    "= Probability(peak | peak)"),
                     breaks=seq(0, 1, by=0.2))+
  theme(panel.margin=grid::unit(0, "lines")),
  first=list(ModelParam="PeakSegJoint -0.8"))

test_that("path after params, 5 paths rendered", {
  info <- animint2HTML(path.after.params)
  path.list <- getNodeSet(info$html, '//g[@class="geom3_path_roc"]//path')
  expect_equal(length(path.list), 5)
})


