configure_pandoc <- function() {
  if (rmarkdown::pandoc_available()) return(invisible(TRUE))
  candidates <- c(Sys.getenv("RSTUDIO_PANDOC"),
    "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
    "C:/Program Files/RStudio/bin/pandoc")
  for (path in candidates[nzchar(candidates)]) {
    if (file.exists(file.path(path, "pandoc.exe")) || file.exists(file.path(path, "pandoc"))) {
      Sys.setenv(RSTUDIO_PANDOC = path)
      rmarkdown::find_pandoc(cache = FALSE)
      break
    }
  }
  invisible(rmarkdown::pandoc_available())
}
pdf_available <- function() nzchar(Sys.which("pdflatex"))
render_risk_report <- function(snapshot, output_file, format = "html") {
  configure_pandoc()
  if (!rmarkdown::pandoc_available()) stop("Pandoc is required. Install RStudio or Pandoc, then restart.")
  if (format == "pdf" && !pdf_available()) stop("PDF export requires a TeX installation. Use HTML and your browser's Print to PDF, or install TinyTeX.")
  work <- tempfile("risk-report-")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  template <- file.path(work, "risk_report.Rmd")
  file.copy("reports/risk_report.Rmd", template)
  env <- new.env(parent = environment(render_risk_report))
  env$snapshot <- snapshot
  kind <- if (format == "pdf") rmarkdown::pdf_document(toc = TRUE) else
    rmarkdown::html_document(toc = TRUE, self_contained = TRUE, theme = "default")
  generated <- rmarkdown::render(template, output_format = kind, envir = env, quiet = TRUE)
  if (!file.copy(generated, output_file, overwrite = TRUE)) stop("Could not save the report.")
  invisible(output_file)
}
report_outputs <- function(input, output, session, s) {
  snapshot <- reactive({
    b <- s$backtest()
    list(holdings = s$valuation()$data, value = s$valuation()$portfolio_value,
      metrics = portfolio_metrics(s$r()), risk = s$risk(), scenarios = s$scenarios(),
      stress = s$stress(), selected_scenario = input$scenario,
      correlation = s$corr(), backtest = b,
      returns = data.frame(Date = as.Date(zoo::index(s$aligned())), Return = s$r()),
      confidence = s$confidence(), draws = length(s$mc()), seed = input$seed,
      source = s$loaded()$mode, requested_dates = s$loaded()$dates, fetched = s$loaded()$fetched,
      window = input$window, generated = Sys.time())
  })
  output$report_capabilities <- renderText({
    configure_pandoc()
    paste("HTML:", if (rmarkdown::pandoc_available()) "ready." else "requires Pandoc.",
      "PDF:", if (pdf_available()) "TeX executable detected; a configured installation is required." else "requires TeX; alternatively print the HTML report to PDF.")
  })
  handler <- function(format) downloadHandler(
    filename = function() paste0("portfolio-risk-", Sys.Date(), ".", format),
    content = function(file) {
      tryCatch(render_risk_report(snapshot(), file, format),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error", duration = 15)
          stop(e)
        })
    })
  output$report_html <- handler("html")
  output$report_pdf <- handler("pdf")
  output$holdings_csv <- downloadHandler(filename = "valued-holdings.csv",
    content = function(file) write.csv(s$valuation()$data, file, row.names = FALSE))
}



