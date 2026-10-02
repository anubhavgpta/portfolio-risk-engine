fixture <- function() data.frame(Ticker = c("A", "B"), Quantity = c(2, 3),
  PurchasePrice = c(100, 50), AssetClass = c("Equity", "ETF"))
test_that("CSV validation normalizes symbols and rejects invalid holdings", {
  p <- fixture(); p$Ticker[1] <- " a "; p$AssetClass[1] <- " Equity "
  expect_true(validate_portfolio(p)$valid)
  expect_equal(validate_portfolio(p)$data$Ticker[1], "A")
  p$Ticker <- c("a", " A "); expect_false(validate_portfolio(p)$valid)
  expect_false(validate_portfolio(fixture()[, -1])$valid)
  expect_false(validate_portfolio(fixture()[FALSE, ])$valid)
  for (column in c("Quantity", "PurchasePrice")) {
    for (bad in list(-1, 0, Inf, NA_real_, "oops")) {
      p <- fixture(); p[[column]][1] <- bad
      expect_false(validate_portfolio(p)$valid)
    }
  }
  p <- fixture(); p$AssetClass[1] <- "Bond"; expect_false(validate_portfolio(p)$valid)
  p <- fixture(); p$Ticker[1] <- ""; expect_false(validate_portfolio(p)$valid)
})
test_that("valuation preserves cost basis and uses latest valid close", {
  p <- fixture(); m <- demo_market_data(p, "2024-01-01", "2024-02-01")
  v <- calculate_portfolio_values(p, m)
  expect_equal(sum(v$data$Weight), 1)
  expect_equal(v$data$PurchasePrice, p$PurchasePrice)
  expect_equal(v$data$ValuationPrice[1], as.numeric(tail(quantmod::Cl(m$A), 1)))
  expect_equal(calculate_portfolio_values(p)$portfolio_value, 350)
})
test_that("failed downloads are recorded and do not drop tickers", {
  m <- load_market_data(c("BAD", "ALSO"), "2024-01-01", "2024-02-01", fetch = function(...) stop("network unavailable"))
  expect_named(m, c("BAD", "ALSO")); expect_length(attr(m, "errors"), 2)
  expect_null(m$BAD)
})
test_that("returns use adjusted price and align both endpoints", {
  dates <- as.Date("2024-01-01") + 0:4
  make <- function(p, dates, ticker) {
    x <- xts::xts(cbind(p, p), order.by = dates)
    colnames(x) <- paste0(ticker, c(".Close", ".Adjusted")); x
  }
  m <- list(A = make(c(100,110,99,108,112), dates, "A"),
    B = make(c(50,55,60,65), dates[-2], "B"))
  r <- calculate_asset_returns(m)
  expect_equal(as.numeric(r$simple$A)[2:3], c(.1,-.1))
  expect_equal(as.numeric(r$log$A)[2], log(1.1))
  x <- combine_returns(r$simple)
  expect_equal(as.character(zoo::index(x)), as.character(dates[4:5]))
  expect_true(all(is.finite(x)))
})
test_that("weighted returns and covariance volatility agree", {
  x <- cbind(A = c(-.02,.01,.03,-.01), B = c(.01,-.01,.02,0))
  w <- c(B = .25, A = .75)
  r <- portfolio_returns(x, w)
  expect_equal(r, .75*x[,1] + .25*x[,2], ignore_attr = TRUE)
  c <- correlation_analytics(x,w)
  expect_equal(c$matrix_volatility, sd(r))
  expect_equal(sum(c$contributions$DailyVolContribution), sd(r))
  expect_equal(portfolio_metrics(r)["AnnualizedReturn"], setNames(prod(1+r)^(252/length(r))-1, "AnnualizedReturn"))
})
test_that("VaR and ES follow stated conventions", {
  r <- c(-.1,-.05,0,.02,.03)
  expect_equal(historical_var(r,.95), .09)
  expect_equal(expected_shortfall(r,.95), .1)
  expect_equal(parametric_var(r,.99), -(mean(r)+qnorm(.01)*sd(r)))
  expect_gte(expected_shortfall(r,.95), historical_var(r,.95))
  expect_lt(historical_var(c(.01,.02,.03),.95), 0)
  expect_equal(expected_shortfall(rep(-.02,20)), .02)
})
test_that("Monte Carlo is reproducible and supports singular covariance", {
  x <- cbind(A = seq(-.02,.02,length.out=100), B = seq(-.02,.02,length.out=100))
  set.seed(1); original <- .Random.seed
  a <- simulate_portfolio(x,c(.5,.5),10000,42)
  expect_identical(.Random.seed, original)
  expect_identical(a, simulate_portfolio(x,c(.5,.5),10000,42))
  expect_equal(sd(a), sd(x[,1]), tolerance = .03, scale = sd(x[,1]))
  expect_error(simulate_portfolio(x,c(.5,.5),9999))
  expect_length(simulate_portfolio(x[,1,drop=FALSE],1),10000)
})
test_that("stress and equity sensitivity conserve value", {
  p <- calculate_portfolio_values(fixture())$data
  s <- apply_stress(p,c(Equity=-.2, ETF=-.1))
  expect_equal(s$pnl,-55); expect_equal(s$after,295)
  expect_equal(sum(s$by_class$StressPnL), s$pnl)
  x <- sensitivity_analysis(p,.05)
  expect_equal(sum(x$PnL[x$Shock > 0]),10)
})
test_that("backtesting excludes realized observations and handles boundaries", {
  r <- c(rep(0,20),-.1,rep(0,20))
  b <- backtest_var(r, as.Date("2024-01-01")+seq_along(r),20,.95)
  expect_equal(b$detail$VaR[1],0)
  expect_true(b$detail$Exception[1])
  expect_equal(b$summary$ExpectedExceptions,21*.05)
  r[21] <- -.9
  expect_equal(backtest_var(r,seq_along(r),20,.95)$detail$VaR[1],0)
  expect_true(is.finite(backtest_var(rep(0,50),1:50,20)$summary$KupiecPValue))
  expect_error(backtest_var(rep(0,20),1:20,20))
})

