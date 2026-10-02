packages <- c("shiny", "bslib", "ggplot2", "dplyr", "tidyr", "lubridate",
  "quantmod", "xts", "zoo", "DT", "plotly", "scales", "withr", "rmarkdown", "knitr", "testthat")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
cat("Dependencies ready. Run shiny::runApp() from the project root.\n")

