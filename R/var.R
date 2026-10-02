historical_var <- function(r, confidence = .99) {
  check_returns(r); check_confidence(confidence)
  -unname(stats::quantile(r, 1 - confidence, type = 7))
}
parametric_var <- function(r, confidence = .99) {
  check_returns(r); check_confidence(confidence)
  -(mean(r) + stats::qnorm(1 - confidence) * stats::sd(r))
}
risk_comparison <- function(r, simulations, confidence, value) {
  data.frame(Model = c("Historical", "Normal parametric", "Correlated normal Monte Carlo"),
    VaR = c(historical_var(r, confidence), parametric_var(r, confidence), historical_var(simulations, confidence)),
    ES = c(expected_shortfall(r, confidence), normal_es(r, confidence), expected_shortfall(simulations, confidence))) |>
    dplyr::mutate(VaR_USD = VaR * value, ES_USD = ES * value)
}

