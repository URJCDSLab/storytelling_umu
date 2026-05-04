library(shiny)
library(bslib)

ui <- page_navbar(
  title = "Mi cuadro de mandos",
  sidebar = "Menú lateral",
  nav_panel("Wine testing", "Área de contenido principal 1"),
  nav_panel("Wine reviews", "Área de contenido principal 2")
  
)

shinyApp(ui, function(input, output) {})