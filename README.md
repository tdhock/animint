animint
=======

An interactive animation can be defined using a list of ggplots with
clickSelects and showSelected aesthetics.

```s
install.packages("devtools")
library(devtools)
install_github("animint","tdhock")
library(animint)
example(gg2animint)
```

For more examples, see [the animint
tutorial](http://tdhock.github.io/animint/) the [examples web
site](http://sugiyama-www.cs.titech.ac.jp/~toby/animint/index.html)
and look at the
[examples/*.R](https://github.com/tdhock/animint/tree/master/examples)
files.

A list of implemented features can be found on the gg2animint man
page, and [TODOs are listed in the NEWS
file](https://github.com/tdhock/animint/blob/master/NEWS).

A [table of related
work](https://github.com/tdhock/animint/blob/master/etc/references.org)
explains the differences between animint and several similar R
interactive graphics packages.

We created [some documentation for un-documented ggplot2 functions
that we use in
animint](https://github.com/tdhock/animint/blob/master/etc/ggplot2.org).
