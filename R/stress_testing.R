stress_scenarios <- function() {
  data.frame(Scenario = c("Equity Market Crash", "Interest Rate Shock", "FX Shock",
    "High Volatility Scenario", "GFC-style", "COVID-style", "EM FX-style"),
    Equity = c(-.20, -.08, -.06, -.12, -.40, -.30, -.12),
    ETF = c(-.15, -.06, -.04, -.10, -.35, -.25, -.08))
}
apply_stress <- function(portfolio, shocks) {
  shock <- unname(shocks[portfolio$AssetClass])
  if (any(!is.finite(shock)) || any(shock < -1)) stop("Supply shocks of at least -100% for each supported asset class.")
  holdings <- dplyr::mutate(portfolio, Shock = shock, StressPnL = PositionValue * Shock)
  by_class <- holdings |> dplyr::group_by(AssetClass) |>
    dplyr::summarise(StressPnL = sum(StressPnL), .groups = "drop")
  before <- sum(holdings$PositionValue); pnl <- sum(holdings$StressPnL)
  list(holdings = holdings, by_class = by_class, before = before,
       after = before + pnl, pnl = pnl, loss = -pnl, loss_pct = -pnl / before)
}
compare_scenarios <- function(portfolio) {
  scenarios <- stress_scenarios()
  scenarios$Loss <- vapply(seq_len(nrow(scenarios)), function(i)
    apply_stress(portfolio, c(Equity = scenarios$Equity[i], ETF = scenarios$ETF[i]))$loss, numeric(1))
  scenarios
}
sensitivity_analysis <- function(portfolio, magnitude = .05) {
  tidyr::expand_grid(Ticker = portfolio$Ticker, Shock = c(-magnitude, magnitude)) |>
    dplyr::left_join(portfolio[, c("Ticker", "AssetClass", "PositionValue")], by = "Ticker") |>
    dplyr::mutate(PnL = ifelse(AssetClass == "Equity", PositionValue * Shock, 0))
}

