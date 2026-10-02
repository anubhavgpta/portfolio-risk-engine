library(shiny)
# Keep calculation modules local to this application instance.
for (file in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(file, local = TRUE, encoding = "UTF-8")
ui <- dashboard_ui()
server <- function(input, output, session) {
  state <- dashboard_state(input, output, session)
  dashboard_outputs(input, output, session, state)
}
shinyApp(ui, server)

# UI uses the standard Shiny Bootstrap theme.

