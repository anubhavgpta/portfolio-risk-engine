library(shiny)
for (f in list.files('R', full.names = TRUE)) source(f)
configure_pandoc()
cat('Pandoc:', rmarkdown::pandoc_available(), 'PDF:', pdf_available(), '\n')
p <- read.csv('data/sample_portfolio.csv')
m <- demo_market_data(p, '2023-01-01', '2025-12-31')
v <- calculate_portfolio_values(p,m)
x <- combine_returns(calculate_asset_returns(m)$simple)
w <- setNames(v$data$Weight, v$data$Ticker)
r <- portfolio_returns(x,w)
mc <- simulate_portfolio(x,w)
s <- list(holdings=v$data, value=v$portfolio_value, metrics=portfolio_metrics(r),
  risk=risk_comparison(r,mc,.99,v$portfolio_value), scenarios=compare_scenarios(v$data),
  stress=apply_stress(v$data,c(Equity=-.2,ETF=-.15)), selected_scenario='Equity Market Crash',
  correlation=correlation_analytics(x,w), backtest=backtest_var(r,zoo::index(x)),
  returns=data.frame(Date=as.Date(zoo::index(x)),Return=r), confidence=.99, draws=10000,seed=42,
  source='demo', requested_dates=as.Date(c('2023-01-01','2025-12-31')), fetched=Sys.time(),window=250,generated=Sys.time())
report_env <- new.env(parent = as.environment('package:stats'))
for (f in list.files('R', full.names = TRUE)) sys.source(f, envir = report_env)
# Isolate helpers from globalenv to reproduce Shiny's local app scope.
report_env$render_risk_report(s, 'reports/sample-risk-report.html')
cat('Report rendered.\n')
shiny::testServer(function(input,output,session) {
  state <- dashboard_state(input,output,session)
  dashboard_outputs(input,output,session,state)
}, {
  session$setInputs(portfolio_source='Sample Portfolio', data_mode='demo',
    market_dates=as.Date(c('2023-01-01','2025-12-31')), load=0, confidence='.99',
    simulations=10000, seed=42, window=250, scenario='Equity Market Crash',
    sensitivity=5, selected_ticker='AAPL', return_type='simple')
  session$setInputs(load=1)
  stopifnot(length(state$r()) > 500)
  ids <- c('data_status','portfolio_summary','overview_cards','portfolio_table','weights_plot',
    'portfolio_plot','market_table','returns_table','asset_plot','metrics_table','contributions',
    'risk_table','var_interpretation','var_plot','simulation_note','mc_table','mc_plot',
    'stress_cards','stress_plot','stress_holdings','stress_classes','scenarios_table',
    'sensitivity_table','sensitivity_total','heatmap','correlation_table','covariance_table',
    'volatility_check','backtest_summary','backtest_detail','backtest_plot')
  for (id in ids) { z <- output[[id]]; cat('Rendered:',id,'\n') }
  session$setInputs(data_mode='yahoo')
  stopifnot(inherits(try(state$market(),silent=TRUE),'try-error'))
})
cat('Server smoke checks passed.\n')
