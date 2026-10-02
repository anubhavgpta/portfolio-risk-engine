check_returns <- function(r) {
  if (!is.numeric(r) || length(r) < 2 || any(!is.finite(r)))
    stop("At least two finite return observations are required.")
}
check_confidence <- function(c) stopifnot(length(c) == 1, is.finite(c), c > 0, c < 1)
money <- function(x) paste0("$", formatC(x, format = "f", digits = 2, big.mark = ","))
pct <- function(x) paste0(formatC(100 * x, format = "f", digits = 2), "%")
xts_table <- function(x) data.frame(Date = as.Date(zoo::index(x)), zoo::coredata(x), check.names = FALSE)
risk_theme <- function() ggplot2::theme_grey()

