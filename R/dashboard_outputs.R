metric_rows <- function(...) {
  items <- list(...)
  groups <- split(seq_along(items), ceiling(seq_along(items) / 4))
  tagList(lapply(groups, function(i) fluidRow(items[i])))
}
risk_card <- function(label, value, note = NULL) {
  column(3, h4(label), p(strong(value)), if (!is.null(note)) p(note))
}
table_view <- function(x, percent = character(), currency = character(), digits = 5) {
  dt <- DT::datatable(x, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE, dom = "tip"))
  nums <- names(x)[vapply(x, is.numeric, logical(1))]
  if (length(nums)) dt <- DT::formatRound(dt, nums, digits)
  if (length(percent)) dt <- DT::formatPercentage(dt, percent, 2)
  if (length(currency)) dt <- DT::formatCurrency(dt, currency, currency = "$", digits = 2)
  dt
}
line_chart <- function(dates, values, title, y = "Daily return") {
  ggplot2::ggplot(data.frame(Date = dates, Value = values), ggplot2::aes(Date, Value)) +
    ggplot2::geom_line(colour = "black", linewidth = .45) +
    ggplot2::labs(title = title, x = NULL, y = y) + risk_theme()
}
loss_chart <- function(r, confidence, value = 1, title = "Daily return distribution") {
  ggplot2::ggplot(data.frame(PnL = r * value), ggplot2::aes(PnL)) +
    ggplot2::geom_histogram(bins = 55, fill = "black", colour = "white") +
    ggplot2::geom_vline(xintercept = -historical_var(r, confidence) * value, colour = "red", linewidth = 1) +
    ggplot2::labs(title = title, subtitle = "Red line: lower-tail VaR threshold", x = if (value == 1) "Return" else "P&L (USD)", y = "Observations") +
    risk_theme()
}
dashboard_outputs <- function(input, output, session, s) {
  output$data_status <- renderUI({
    m <- s$market(); p <- s$valuation()$data
    errors <- attr(m, "errors")
    label <- if (input$load == 0) "PRICES NOT LOADED  -  valuation uses cost basis."
      else if (s$loaded()$mode == "demo") "SYNTHETIC DEMONSTRATION  -  generated prices, not market history."
      else "YAHOO FINANCE  -  latest available close within the selected range; not live quotes."
    wellPanel( strong(label),
      if (input$load > 0) p(paste("Requested:", paste(s$loaded()$dates, collapse = " to "),
        "| Loaded:", format(s$loaded()$fetched, tz = "UTC", usetz = TRUE))),
      if (length(errors)) tags$ul(lapply(errors, tags$li)),
      if (any(p$PriceSource == "Cost basis fallback")) p("Fallback values are marked in the holdings table."),
      if (any(!is.na(p$PriceDate))) p(paste("Valuation price dates:", paste(unique(na.omit(p$PriceDate)), collapse = ", "))))
  })
  output$portfolio_summary <- renderUI({
    z <- calculate_portfolio_summary(s$valuation()$data); w <- z$largest_position_weight
    tagList(metric_rows(
      risk_card("Portfolio value", money(s$valuation()$portfolio_value)),
      risk_card("Holdings", z$number_of_holdings),
      risk_card("Largest position", z$largest_position_ticker, money(z$largest_position_value)),
      risk_card("Largest weight", pct(w))),
      p(if (w >= .5) "High concentration (>=50%)." else if (w >= .3) "Moderate concentration (>=30%)." else "No position reaches 30%.",
        " These thresholds are application-defined, not regulatory standards."))
  })
  output$overview_cards <- renderUI({
    r <- s$r(); v <- s$valuation()$portfolio_value; sc <- s$scenarios(); i <- which.max(sc$Loss)
    metric_rows(
      risk_card("Portfolio value", money(v)),
      risk_card("Modeled daily P&L", money(tail(r, 1) * v), as.character(tail(zoo::index(s$aligned()), 1))),
      risk_card("Annualized volatility", pct(sd(r) * sqrt(252))),
      risk_card("95% historical VaR", money(historical_var(r, .95) * v)),
      risk_card("99% historical VaR", money(historical_var(r, .99) * v)),
      risk_card("99% historical ES", money(expected_shortfall(r, .99) * v)),
      risk_card("Worst preset stress loss", money(sc$Loss[i]), sc$Scenario[i]),
      risk_card("Aligned observations", length(r), paste(range(zoo::index(s$aligned())), collapse = " to ")))
  })
  output$portfolio_table <- DT::renderDT(table_view(s$valuation()$data, "Weight", c("PurchasePrice", "ValuationPrice", "PositionValue")))
  output$weights_plot <- plotly::renderPlotly({
    p <- s$valuation()$data
    plotly::ggplotly(ggplot2::ggplot(p, ggplot2::aes(reorder(Ticker, Weight), Weight)) +
      ggplot2::geom_col(fill = "black") + ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(labels = scales::percent) + ggplot2::labs(x = NULL, y = "Portfolio weight") + risk_theme())
  })
  output$portfolio_plot <- plotly::renderPlotly(plotly::ggplotly(line_chart(zoo::index(s$aligned()), s$r(), "Fixed-weight daily returns")))
  selected_returns <- reactive({
    req(input$selected_ticker, input$return_type)
    x <- s$returns()[[input$return_type]][[input$selected_ticker]]
    validate(need(!is.null(x), "Load prices for this asset."))
    x
  })
  output$market_table <- DT::renderDT({
    req(input$selected_ticker)
    x <- s$market()[[input$selected_ticker]]
    validate(need(!is.null(x), "Market data is unavailable for this ticker."))
    table_view(xts_table(x))
  })
  output$returns_table <- DT::renderDT(table_view(xts_table(selected_returns()), input$selected_ticker))
  output$asset_plot <- plotly::renderPlotly(plotly::ggplotly(line_chart(zoo::index(selected_returns()), as.numeric(selected_returns()), paste(input$selected_ticker, input$return_type, "returns"))))
  output$metrics_table <- DT::renderDT({
    x <- portfolio_metrics(s$r())
    table_view(data.frame(Metric = names(x), Value = unname(x)), "Value")
  })
  output$contributions <- DT::renderDT(table_view(s$corr()$contributions, "DailyVolContribution"))
  output$risk_table <- DT::renderDT(table_view(s$risk(), c("VaR", "ES"), c("VaR_USD", "ES_USD")))
  output$var_interpretation <- renderText(paste0("A 1-day ", pct(s$confidence()), " historical VaR of ",
    money(historical_var(s$r(), s$confidence()) * s$valuation()$portfolio_value),
    " means losses are expected to exceed this threshold on roughly ", pct(1 - s$confidence()),
    " of trading days under this model. VaR is not a maximum possible loss."))
  output$var_plot <- plotly::renderPlotly(plotly::ggplotly(loss_chart(s$r(), s$confidence())))
  output$simulation_note <- renderText(paste(format(length(s$mc()), big.mark = ","), "correlated normal draws; seed", input$seed,
    "| Simulated returns below -100%:", sum(s$mc() < -1)))
  output$mc_table <- DT::renderDT(table_view(s$risk()[3, ], c("VaR", "ES"), c("VaR_USD", "ES_USD")))
  output$mc_plot <- plotly::renderPlotly(plotly::ggplotly(loss_chart(s$mc(), s$confidence(), s$valuation()$portfolio_value, "Simulated daily P&L")))
  output$stress_cards <- renderUI({
    x <- s$stress()
    metric_rows( risk_card("Before stress", money(x$before)),
      risk_card("After stress", money(x$after)), risk_card("Stress loss / (gain)", money(x$loss)),
      risk_card("Stress loss (%)", pct(x$loss_pct)))
  })
  output$stress_plot <- plotly::renderPlotly(plotly::ggplotly(
    ggplot2::ggplot(s$stress()$holdings, ggplot2::aes(reorder(Ticker, StressPnL), StressPnL)) +
      ggplot2::geom_col(fill = "red") + ggplot2::coord_flip() +
      ggplot2::labs(x = NULL, y = "Stress P&L (USD)") + risk_theme()))
  output$stress_holdings <- DT::renderDT(table_view(s$stress()$holdings[, c("Ticker", "AssetClass", "Shock", "StressPnL")], "Shock", "StressPnL"))
  output$stress_classes <- DT::renderDT(table_view(s$stress()$by_class, currency = "StressPnL"))
  output$scenarios_table <- DT::renderDT(table_view(s$scenarios(), c("Equity", "ETF"), "Loss"))
  sens <- reactive(sensitivity_analysis(s$valuation()$data, input$sensitivity / 100))
  output$sensitivity_table <- DT::renderDT(table_view(sens(), "Shock", c("PositionValue", "PnL")))
  output$sensitivity_total <- DT::renderDT(table_view(sens() |> dplyr::group_by(Shock) |> dplyr::summarise(PortfolioPnL = sum(PnL)), "Shock", "PortfolioPnL"))
  output$heatmap <- plotly::renderPlotly({
    m <- s$corr()$correlation
    plotly::plot_ly(x = colnames(m), y = rownames(m), z = m, type = "heatmap", zmin = -1, zmax = 1,
      colors = c("red", "#ffffff", "black")) |> plotly::layout(xaxis = list(title = ""), yaxis = list(title = ""))
  })
  matrix_frame <- function(x) data.frame(Ticker = rownames(x), x, check.names = FALSE)
  output$correlation_table <- DT::renderDT(table_view(matrix_frame(s$corr()$correlation)))
  output$covariance_table <- DT::renderDT(table_view(matrix_frame(s$corr()$covariance), digits = 8))
  output$volatility_check <- renderText({
    x <- s$corr()
    paste("Daily volatility  -  covariance matrix:", pct(x$matrix_volatility),
      "| Direct returns:", pct(x$direct_volatility),
      "| Consistent:", isTRUE(all.equal(x$matrix_volatility, x$direct_volatility, tolerance = 1e-10)))
  })
  bt <- reactive({
    x <- s$backtest(); validate(need(is.null(x$error), x$error)); x
  })
  output$backtest_summary <- DT::renderDT(table_view(bt()$summary, "ExceptionRate"))
  output$backtest_detail <- DT::renderDT(table_view(bt()$detail, c("VaR", "Loss")))
  output$backtest_plot <- plotly::renderPlotly({
    d <- bt()$detail
    plotly::ggplotly(ggplot2::ggplot(d, ggplot2::aes(Date)) +
      ggplot2::geom_line(ggplot2::aes(y = Loss), colour = "grey60") +
      ggplot2::geom_line(ggplot2::aes(y = VaR), colour = "black") +
      ggplot2::geom_point(data = d[d$Exception, ], ggplot2::aes(y = Loss), colour = "red") +
      ggplot2::labs(y = "Loss / forecast VaR", title = "Rolling out-of-window coverage", subtitle = "Red markers indicate exceptions") + risk_theme())
  })
  report_outputs(input, output, session, s)
}




