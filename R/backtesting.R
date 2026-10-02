backtest_var <- function(r, dates, window = 250L, confidence = .99) {
  check_returns(r); check_confidence(confidence)
  if (window < 20 || window != as.integer(window) || length(r) <= window)
    stop("Backtesting needs more return observations than the rolling window (minimum 20).")
  stopifnot(length(dates) == length(r))
  indices <- seq.int(window + 1L, length(r))
  forecast <- vapply(indices, function(i) historical_var(r[(i - window):(i - 1)], confidence), numeric(1))
  detail <- data.frame(Date = dates[indices], VaR = forecast, Loss = -r[indices])
  detail$Exception <- detail$Loss > detail$VaR
  n <- nrow(detail); exceptions <- sum(detail$Exception); p <- 1 - confidence
  lr <- max(0, 2 * (dbinom(exceptions, n, exceptions / n, log = TRUE) - dbinom(exceptions, n, p, log = TRUE)))
  list(detail = detail, summary = data.frame(Observations = n, Exceptions = exceptions,
    ExpectedExceptions = n * p, ExceptionRate = exceptions / n, KupiecLR = lr,
    KupiecPValue = pchisq(lr, df = 1, lower.tail = FALSE)))
}

