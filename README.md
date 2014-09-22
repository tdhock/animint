animint
=======

[![Build Status](https://travis-ci.org/tdhock/animint.png?branch=master)](https://travis-ci.org/tdhock/animint)

Interactive animations using [ggplot2](https://github.com/hadley/ggplot2)'s grammar of graphics implementation combined with clickSelects and showSelected aesthetics.

## Installation

```s
if (!require(devtools)) install.packages("devtools")
devtools::install_github(c("tdhock/ggplot2", "tdhock/animint"), build_vignettes=FALSE)
library(animint)
example(animint)
```

NOTE: tdhock/ggplot2 is required in order to use `theme_animint` --
there is a [pull request](https://github.com/hadley/ggplot2/pull/953)
to merge [tdhock/ggplot2](https://github.com/tdhock/ggplot2) with the
main ggplot2 repo.

## Examples

The best way to learn animint is through examples. A couple of good
introductions are Carson Sievert's [Interactive animations of
PITCHf/x](http://cpsievert.github.io/baseballR/20140818/) and Susan
VanderPlas' [animint tutorial](http://tdhock.github.io/animint/). In
addition, there is an [examples web
site](http://sugiyama-www.cs.titech.ac.jp/~toby/animint/index.html),
[examples distributed with
animint](https://github.com/tdhock/animint/tree/master/inst/examples), as
well as more complex 'big data' examples in the
[tdhock/animint-examples
repo](https://github.com/tdhock/animint-examples/tree/master/examples).

## Animint paper

Susan VanderPlas and Toby Dylan Hocking wrote an [8-page conference paper](https://github.com/tdhock/animint-paper/blob/master/HOCKING-animint.pdf?raw=true) in March 2014. If you would like to contribute code to animint, please read the paper first to get an overview of the package. It explains:
- the purpose of animint: make it easy to design interactive animations.
- the clickSelects and showSelected keywords which permit interactivity.
- the design of the animint package.
- the advantages and disadvantages of animint compared to other interactive data viz libraries.

## Related work

We have compiled [short](https://github.com/tdhock/interactive-tutorial/tree/master/animation) and [long](https://github.com/tdhock/animint/blob/master/etc/references.org) tables of related work.

## Contribute to animint!

#### Get acquainted

Before contributing to animint, you should first read about its design. There are two main components, which have separate wiki pages that explain their details:
- The [compiler](https://github.com/tdhock/animint/wiki/Compiler%20details) is written in R code.
- The [renderer](https://github.com/tdhock/animint/wiki/Renderer-details) is written in JavaScript code.

#### TODO items

We keep a TODO list at top of the [NEWS](https://github.com/tdhock/animint/blob/master/NEWS) file. They are categorized as follows:

- BUG: things which used to work but have stopped working. Tests should be added to prevent these. For example at one point in 2013, animint rendered the correct number of legend entries for the WorldBank viz, but in the beginning of 2014 animint rendered too many legend entries. 
- GGPLOT: things which ggplot2 supports but animint does not yet support. For example facets, coord_equal.
- DSL: changes to the animint domain-specific language (DSL) which would allow interactive/animated features. These involve changes to how we define the ggplots, and how the compiler works. For example custom alpha/color/etc for selected geoms, using selected.alpha/selected.color/etc aesthetics.
- EXAMPLE: examples to show off animint features, which should affect neither the compiler nor renderer. For example the pirates data set.
- RENDER: changes to the JavaScript rendering code which would result in better interactive plots, without having to change the definition of the ggplots. For example rendering a selection widget for every selection variable.
- OUTPUT: different output formats for viewing/sharing an interactive animation.
- OPTIMIZATION: things which are currently supported, but with an implementation that could be improved in terms of render/compile speed, disk usage, memory, etc. Typically these optimizations are not really noticed for small data sets, but make it easier to visualize large data sets. For example, can we gzip the TSV plot data files to reduce disk space and download times?

#### Submitting contributions

We welcome pull requests, but please `R CMD check` to run the package
[tests](https://github.com/tdhock/animint/wiki/Testing). You may
ignore WARNINGS and NOTES, but make sure there are no errors.

