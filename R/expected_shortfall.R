expected_shortfall <- function(r, confidence = .99) {
  threshold <- -historical_var(r, confidence)
  # Empirical conditional tail mean includes ties at the quantile.
  -mean(r[r <= threshold])
}
normal_es <- function(r, confidence = .99) {
  check_returns(r); check_confidence(confidence)
  -mean(r) + sd(r) * dnorm(qnorm(1 - confidence)) / (1 - confidence)
}

