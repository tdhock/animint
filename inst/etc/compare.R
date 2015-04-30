load("~/compare-nips/simulation.samples.RData")

library(reshape2)
library(plyr)

## matrix versions of the norm.
funs <- list(l2=function(x)rowSums(x*x),
             l1=function(x)rowSums(abs(x))^2,
             linf=function(x)apply(abs(x), 1, max)^2)

size.list <- simulation.samples$data
err <- simulation.samples$err
rank.df <- simulation.samples$rank
keep <- seq(-2, 2, by=0.2)
is.ok <- with(rank.df, x1 %in% keep & x2 %in% keep)
rank.df <- rank.df[is.ok,]
err$percent <- err$error / err$count * 100
err$set.id <- NA
## sets of training data and bayes error on test data.
sets <- dcast(err, N + seed + norm ~ fit.name, value.var="percent")
sets$diff <- sets$compare-sets$rank
sets$set.id <- 1:nrow(sets)
diff.df <- ddply(sets, .(N, norm), summarize,
                   N=N[1], norm=norm[1],
                   mean=mean(diff), sd=sd(diff))
rank.df$set.id <- NA
train.df <- data.frame()
bayes.df <- data.frame()
for(set.id in sets$set.id){
  e <- sets[set.id,]
  N <- as.character(e$N)
  norm <- as.character(e$norm)
  seed <- as.character(e$seed)
  err$set.id[err$norm == norm & err$N == N & err$seed == seed] <- set.id
  rank.in.set <- rank.df$norm == norm & rank.df$N == N & rank.df$seed == seed
  rank.df$set.id[rank.in.set] <- set.id
  set.list <- size.list[[N]][[seed]][[norm]]
  info <- data.frame(N, norm, seed, set.id)
  ## The Bayes error on the test data set.
  test <- set.list$test
  fun <- funs[[norm]]
  fxdiff <- with(test, fun(Xip)-fun(Xi))
  yhat <- ifelse(fxdiff > 1, 1L,
                 ifelse(fxdiff < -1, -1L, 0))
  table(yhat, test$yi)
  percent <- mean(yhat != test$yi) * 100
  bayes.df <- rbind(bayes.df, data.frame(info, percent))
  ## Train pairs, oriented in the same way:
  pair.df <- with(set.list$train,{
    rbind(data.frame(Xt=Xi[yi==1,],Xtp=Xip[yi==1,],yt=1),
          data.frame(Xt=Xip[yi==-1,],Xtp=Xi[yi==-1,],yt=1),
          data.frame(Xt=Xi[yi==0,],Xtp=Xip[yi==0,],yt=-1))
  })
  train.df <- rbind(train.df, data.frame(pair.df, info))
}
## Show rank or compare model on ground truth level curves.
ord <- c("latent","compare","rank")
bayes.df$model <- factor("latent",ord)
err$fit.name <- factor(err$fit.name, ord)

## need error, bayes, train, rank.
compare <- list(error=err, bayes=bayes.df, rank=rank.df, train.pairs=train.df)

save(compare, file="../data/compare.RData")
