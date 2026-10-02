calculate_asset_returns <- function(market_data) {
  simple <- logs <- list()
  for (ticker in names(market_data)) {
    x <- market_data[[ticker]]
    if (is.null(x)) next
    p <- quantmod::Ad(x)
    p[!is.finite(p) | p <= 0] <- NA_real_
    previous <- stats::lag(p, k = 1)
    simple[[ticker]] <- p / previous - 1
    logs[[ticker]] <- log(p / previous)
    colnames(simple[[ticker]]) <- colnames(logs[[ticker]]) <- ticker
  }
  list(simple = simple, log = logs)
}
combine_returns <- function(return_list) {
  if (!length(return_list) || any(vapply(return_list, is.null, logical(1)))) return(NULL)
  # Align both interval endpoints so a two-session return cannot be paired
  # with another asset's one-session return after a missing price observation.
  starts <- lapply(return_list, function(x) {
    v <- c(NA_real_, head(as.numeric(zoo::index(x)), -1))
    xts::xts(v, order.by = zoo::index(x))
  })
  x <- Reduce(function(a, b) merge(a, b, join = "inner"), return_list)
  s <- Reduce(function(a, b) merge(a, b, join = "inner"), starts)
  good <- apply(as.matrix(x), 1, function(v) all(is.finite(v))) &
    apply(as.matrix(s), 1, function(v) all(is.finite(v)) && length(unique(v)) == 1)
  x[good, ]
}
portfolio_returns <- function(returns, weights) {
  x <- as.matrix(returns)
  if (!is.null(names(weights))) weights <- weights[colnames(x)]
  stopifnot(ncol(x) == length(weights), all(is.finite(weights)),
            abs(sum(weights) - 1) < 1e-8, all(weights >= 0), all(is.finite(x)))
  drop(x %*% weights)
}
portfolio_metrics <- function(r, periods = 252) {
  check_returns(r)
  c(MeanDaily = mean(r), DailyVolatility = sd(r),
    AnnualizedReturn = exp(mean(log1p(r)) * periods) - 1,
    AnnualizedVolatility = sd(r) * sqrt(periods), MinimumDaily = min(r), MaximumDaily = max(r))
}
correlation_analytics <- function(returns, weights) {
  x <- as.matrix(returns)
  if (!is.null(names(weights))) weights <- weights[colnames(x)]
  covariance <- stats::cov(x)
  volatility <- sqrt(drop(t(weights) %*% covariance %*% weights))
  contribution <- if (volatility > 0) weights * drop(covariance %*% weights) / volatility else weights * 0
  list(covariance = covariance, correlation = suppressWarnings(stats::cor(x)),
       matrix_volatility = volatility, direct_volatility = sd(portfolio_returns(x, weights)),
       contributions = data.frame(Ticker = colnames(x), DailyVolContribution = contribution))
}

