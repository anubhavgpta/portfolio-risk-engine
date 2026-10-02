for (f in c("app.R", list.files("R", full.names = TRUE))) parse(f)
source("tests/testthat.R")

