dashboard_state <- function(input, output, session) {
  portfolio <- reactive({
    raw <- if (input$portfolio_source == "Sample Portfolio") {
      read.csv("data/sample_portfolio.csv", stringsAsFactors = FALSE)
    } else {
      req(input$portfolio_file)
      tryCatch(read.csv(input$portfolio_file$datapath, stringsAsFactors = FALSE),
        error = function(e) { validate(need(FALSE, paste("Cannot read CSV:", conditionMessage(e)))) })
    }
    checked <- validate_portfolio(raw)
    validate(need(checked$valid, paste(checked$errors, collapse = " ")))
    checked$data
  })
  observeEvent(portfolio(), updateSelectInput(session, "selected_ticker", choices = portfolio()$Ticker))
  loaded <- eventReactive(input$load, {
    p <- portfolio()
    dates <- as.Date(input$market_dates)
    validate(need(length(dates) == 2 && all(!is.na(dates)) && dates[1] < dates[2], "Choose a valid date range."))
    data <- withProgress(message = "Loading price history", value = .3, {
      tryCatch(if (input$data_mode == "demo") demo_market_data(p, dates[1], dates[2])
        else load_market_data(p$Ticker, dates[1], dates[2]),
        error = function(e) { validate(need(FALSE, conditionMessage(e))) })
    })
    list(data = data, portfolio = p, dates = dates, mode = input$data_mode, fetched = Sys.time())
  })
  market <- reactive({
    if (input$load == 0) return(list())
    x <- loaded()
    validate(need(identical(x$portfolio, portfolio()) &&
      identical(x$dates, as.Date(input$market_dates)) && identical(x$mode, input$data_mode),
      "Settings changed. Click Load / refresh prices to apply them."))
    x$data
  })
  valuation <- reactive(calculate_portfolio_values(portfolio(), market()))
  returns <- reactive(calculate_asset_returns(market()))
  aligned <- reactive({
    p <- portfolio(); m <- market()
    unavailable <- p$Ticker[vapply(p$Ticker, function(t) is.null(m[[t]]), logical(1))]
    validate(need(!length(unavailable), paste("Load usable prices for every holding. Missing:", paste(unavailable, collapse = ", "))))
    x <- combine_returns(returns()$simple[p$Ticker])
    validate(need(!is.null(x) && NROW(x) >= 30, "At least 30 common, finite return intervals are required. Expand the date range."))
    x
  })
  weights <- reactive(setNames(valuation()$data$Weight, valuation()$data$Ticker))
  r <- reactive(portfolio_returns(aligned(), weights()))
  confidence <- reactive(as.numeric(input$confidence))
  mc <- reactive({
    validate(need(is.finite(input$simulations) && input$simulations >= 10000 &&
      input$simulations <= 200000 && input$simulations %% 1 == 0, "Choose 10,000-200,000 integer simulations."),
      need(is.finite(input$seed) && input$seed >= 0 && input$seed <= 1000000, "Choose a seed from 0 to 1,000,000."))
    simulate_portfolio(aligned(), weights(), input$simulations, as.integer(input$seed))
  })
  risk <- reactive(risk_comparison(r(), mc(), confidence(), valuation()$portfolio_value))
  corr <- reactive(correlation_analytics(aligned(), weights()))
  scenarios <- reactive(compare_scenarios(valuation()$data))
  stress <- reactive({
    req(input$scenario)
    if (input$scenario == "Custom Scenario") shocks <- c(Equity = input$equity_shock / 100, ETF = input$etf_shock / 100)
    else {
      s <- stress_scenarios(); i <- match(input$scenario, s$Scenario)
      shocks <- c(Equity = s$Equity[i], ETF = s$ETF[i])
    }
    apply_stress(valuation()$data, shocks)
  })
  backtest <- reactive({
    tryCatch(backtest_var(r(), as.Date(zoo::index(aligned())), input$window, confidence()),
      error = function(e) list(error = conditionMessage(e)))
  })
  list(portfolio = portfolio, market = market, loaded = loaded, valuation = valuation,
    returns = returns, aligned = aligned, r = r, confidence = confidence, mc = mc,
    risk = risk, corr = corr, scenarios = scenarios, stress = stress, backtest = backtest)
}


