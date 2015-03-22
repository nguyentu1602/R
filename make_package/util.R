# this script prepares the documentation for making pakages

require(ggplot2)
setwd('/Users/cuongnguyen/program/R/make_package/')

install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
require(devtools)
require(roxygen2)
require(testthat)
require(knitr)
devtools::install_github("hadley/devtools")

library(devtools)
has_devel()
library(roxygen2)
library(testthat)
devtools::session_info()

# misc.

ls()
rm