context("rotate")

ss <- data.frame(State=c("CA", "NY"),
                 Prop.Inv=c(0, 1),
                 Year=c(1984, 2015))

fg <- ggplot() +
  geom_point(aes(x=State, y=Prop.Inv, showSelected=Year), data=ss) +
  theme_animint(width=600, height=400) 
sg <- ggplot() +
  stat_summary(data=ss, aes(Year, Year, clickSelects=Year),
               fun.y=length, geom="bar")

test_that('axis.text.x=element_text(angle=90) means transform="rotate(-90)"', {
  map <-
    list(fg=fg+theme(axis.text.x=element_text(angle=90)),
         sg=sg)
  info <- animint2HTML(map)
  expect_rotate(info, "-90")
  expect_anchor(info, "middle")
})

test_that('axis.text.x=element_text(angle=70) means transform="rotate(-70)"', {
  map <-
    list(fg=fg+theme(axis.text.x=element_text(angle=70)),
         sg=sg)
  info <- animint2HTML(map)
  expect_rotate(info, "-70")
  expect_anchor(info, "middle")
})

test_that('and hjust=1 means style="text-anchor: end;"', {
  map <-
    list(fg=fg+theme(axis.text.x=element_text(angle=70, hjust=1)),
         sg=sg)
  info <- animint2HTML(map)
  expect_rotate(info, "-70")
  expect_anchor(info, "end")
})

test_that('and hjust=0 means style="text-anchor: start;"', {
  map <-
    list(fg=fg+theme(axis.text.x=element_text(angle=70, hjust=0)),
         sg=sg)
  info <- animint2HTML(map)
  expect_rotate(info, "-70")
  expect_anchor(info, "start")
})

test_that('and hjust=0.5 means style="text-anchor: middle;"', {
  map <-
    list(fg=fg+theme(axis.text.x=element_text(angle=70, hjust=0.5)),
         sg=sg)
  info <- animint2HTML(map)
  expect_rotate(info, "-70")
  expect_anchor(info, "middle")
})

## TODO: also test for y axis rotation.

