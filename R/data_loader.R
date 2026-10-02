load_market_data <- function(tickers, start_date, end_date, fetch = quantmod::getSymbols) {
  start_date <- as.Date(start_date); end_date <- as.Date(end_date)
  if (is.na(start_date) || is.na(end_date) || start_date >= end_date)
    stop("Choose a start date before the end date.")
  result <- setNames(vector("list", length(tickers)), tickers)
  errors <- character()
  for (ticker in tickers) {
    result[ticker] <- list(tryCatch({
      x <- fetch(ticker, src = "yahoo", from = start_date,
                 to = end_date + 1, auto.assign = FALSE)
      if (!xts::is.xts(x) || NROW(x) < 2) stop("Fewer than two price observations.")
      x <- x[paste0(start_date, "/", end_date)]
      if (NROW(x) < 2) stop("No usable history in the selected range.")
      if (NCOL(quantmod::Ad(x)) != 1) stop("Adjusted price is unavailable.")
      x
    }, error = function(e) {
      errors[ticker] <<- paste(ticker, ":", conditionMessage(e))
      NULL
    }))
  }
  attr(result, "errors") <- errors
  result
}
# Synthetic data is an explicit mode, never a substitute for a failed download.
demo_market_data <- function(portfolio, start_date, end_date) {
  dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")
  dates <- dates[lubridate::wday(dates) %in% 2:6]
  if (length(dates) < 3) stop("Select at least three weekdays for the demo.")
  withr::with_seed(2401, {
    common <- rnorm(length(dates), 0.00025, 0.011)
    setNames(lapply(seq_len(nrow(portfolio)), function(i) {
      close <- portfolio$PurchasePrice[i] * cumprod(1 + common + rnorm(length(dates), 0, .009))
      x <- xts::xts(cbind(close, close * 1.005, close * .995, close,
                          rep(1000000, length(dates)), close), order.by = dates)
      colnames(x) <- paste(portfolio$Ticker[i], c("Open", "High", "Low", "Close", "Volume", "Adjusted"), sep = ".")
      x
    }), portfolio$Ticker)
  })
}

