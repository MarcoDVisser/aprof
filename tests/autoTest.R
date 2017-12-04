## devtools::install_github("goldingn/auto")
## install.packages("hunspell")
## This run checks on the current aprof version
## on the githib cran repro

setwd("~/symlinks/git/")
auto::check("cran/aprof")
devtools::spell_check("aprof")
