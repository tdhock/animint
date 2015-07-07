#!/bin/bash

# only build tutorial when simulating merge with master -> http://docs.travis-ci.com/user/environment-variables/#Default-Environment-Variables
[ "${TRAVIS_PULL_REQUEST}" != "false" ] && exit 0

echo "Building tutorial"

git config user.name "cpsievert"
git config user.email "cpsievert1@gmail.com"

FULL_REPO="https://$GH_TOKEN@github.com/tdhock/animint.git"

git fetch origin gh-pages
git checkout gh-pages

# compile tutorial and push to gh-pages
R -e "knitr::knit2html('index.Rmd')"
git add --all
git commit -m "Pushed from -> https://travis-ci.org/ropensci/plotly/builds/$TRAVIS_BUILD_ID"
git push --quiet $FULL_REPO master:gh-pages

