animint
=======

An interactive animation can be defined using a list of ggplots with
clickSelects and showSelected aesthetics.

```s
install.packages("devtools")
devtools::install_github("tdhock/ggplot2", "tdhock/animint", build_vignettes=FALSE)
library(animint)
example(gg2animint)
```

For more examples, see [the animint
tutorial](http://tdhock.github.io/animint/) the [examples web
site](http://sugiyama-www.cs.titech.ac.jp/~toby/animint/index.html),
the [animint/examples/*.R
files](https://github.com/tdhock/animint/tree/master/examples), and
the [bigger examples in
animint-examples/examples/*.R](https://github.com/tdhock/animint-examples/tree/master/examples).

A list of implemented features can be found on the gg2animint man
page, and [TODOs are listed in the NEWS
file](https://github.com/tdhock/animint/blob/master/NEWS).

We have compiled
[short](https://github.com/tdhock/interactive-tutorial/tree/master/animation)
and
[long](https://github.com/tdhock/animint/blob/master/etc/references.org)
tables of related work explains the differences between animint and
several similar R interactive graphics packages.

We created [some documentation for un-documented ggplot2 functions
that we use in
animint](https://github.com/tdhock/animint/blob/master/etc/ggplot2.org).
