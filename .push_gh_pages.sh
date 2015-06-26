#!/bin/bash

# no idea really
[ "${TRAVIS_PULL_REQUEST}" != "false" ] && exit 0

rm -rf tutorial || exit 0;
mkdir tutorial;

GH_REPO="@github.com/tdhock/animint.git"

FULL_REPO="https://$GH_TOKEN$GH_REPO"

for files in '*.tar.gz'; do
        tar xfz $files
done

# install package
R -e "devtools::install('animint')"

# initialize git repo
cd tutorial
git init
# why do I need to do this git config stuff?
git config user.name "kferris10"
git config user.email "kferris10@gmail.com"

# pull tutorial from GitHub
### this is overkill - I only need to pull the index.Rmd file
git pull $FULL_REPO gh-pages

# compile tutorial and push to gh-pages
R -e "knitr::knit2html('index.Rmd')"
git add --all
git commit -m "Re-build tutorial"
git push --force --quiet $FULL_REPO master:gh-pages

