animint
=======

An interactive animation can be defined using a list of ggplots with
clickSelects and showSelected aesthetics.

```s
install.packages(c("devtools","ggplot2","RJSONIO"))
library(devtools)
install_github("animint","tdhock")
library(animint)
example(gg2animint)
```

For more examples, see [the animint tutorial](http://tdhock.github.io/animint/) or look at breakpoints.R evolution.R intreg.R and
Tornadoes.R in the examples folder.

A list of implemented features and TODOs can be found on the
gg2animint man page.
