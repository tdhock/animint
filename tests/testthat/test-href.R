context("aes(href)")

color.df <-
  data.frame(x=c(1, 1, 2, 1, 2),
             university=c("Stanford",
               rep("UC Berkeley", 2),
               rep("Oregon State", 2)),
             color=c("red", "blue", "gold", "orange", "black"))
university.df <- as.data.frame(table(color.df$university))
names(university.df) <- c("university", "colors")

test_that("clickSelects and href is an error", {
  viz <-
    list(colors=ggplot()+
         geom_point(aes(x, university, color=color,
                        clickSelects=university, href=color),
                    data=color.df)+
         scale_color_identity())
  expect_error({
    animint2dir(viz, open.browser=FALSE)
  }, "aes(clickSelects) can not be used with aes(href)", fixed=TRUE)
})
