library(shiny)
library(bslib)
library(DBI)
library(duckdb)
library(dplyr)
library(ggplot2)
library(DT)
library(broom)

con  <- dbConnect(duckdb(), dbdir = "data/wine.db", read_only = TRUE)
wine <- tbl(con, "tasting")

# Compute top levels once at startup from a minimal query
raw <- wine |> select(country, variety, winery) |> collect()

top_countries <- names(sort(table(raw$country), decreasing = TRUE))[1:20]
top_varieties <- names(sort(table(raw$variety), decreasing = TRUE))[1:20]
top_wineries  <- names(sort(table(raw$winery),  decreasing = TRUE))[1:20]

rm(raw)

lump <- function(x, top) if_else(x %in% top, x, "Other")

paises  <- c(sort(top_countries), "Other")
colores <- c("fct_country", "fct_variety")

ui <- page_navbar(
  theme = bs_theme(bootswatch = "cosmo"),
  title = "El vino y yo",
  sidebar = sidebar(title = "Menú lateral",
                    selectizeInput("ipais",
                                   label = "País",
                                   choices = paises),
                    checkboxInput("ilog", label = "Logaritmo"),
                    selectizeInput("icolormodelo",
                                   label = "Color puntos",
                                   choices = colores)),
  nav_panel("Exploración",
            "Análisis por país",
            plotOutput("ohistprecio")),
  nav_panel("Modelo de precio",
            "Estimación del precio",
            navset_card_underline(
              title = "Modelización",
              nav_panel("Gráfico de dispersión",
                        plotOutput("odisp")),
              nav_panel("Coeficientes",
                        dataTableOutput("ocoeficientes")),
              nav_panel("Análisis",
                        verbatimTextOutput("oanalisis"))
            ))
)

server <- function(input, output) {

  # Query only selected country's price data
  datos_pais <- reactive({
    q <- if (input$ipais == "Other") {
      wine |> filter(!country %in% !!top_countries)
    } else {
      wine |> filter(country == !!input$ipais)
    }
    q |>
      select(price) |>
      collect() |>
      mutate(y = if (input$ilog) log(price, 10) else price)
  })

  # Query only columns needed for scatter plot
  datos_disp <- reactive({
    col_color <- sub("fct_", "", input$icolormodelo)  # country / variety
    wine |>
      select(points, price, all_of(col_color)) |>
      collect() |>
      rename(color = 3) |>
      mutate(
        y     = if (input$ilog) log(price, 10) else price,
        color = lump(color, if (col_color == "country") top_countries else top_varieties)
      )
  })

  # Query only columns needed for the model
  datos_modelo <- reactive({
    wine |>
      select(price, points, country, variety) |>
      collect() |>
      mutate(
        y           = if (input$ilog) log(price, 10) else price,
        fct_country = factor(lump(country, top_countries)),
        fct_variety = factor(lump(variety, top_varieties))
      )
  })

  modelo <- reactive({
    lm(y ~ points + fct_country + fct_variety, data = datos_modelo())
  })

  output$ohistprecio <- renderPlot({
    datos_pais() |>
      ggplot(aes(x = y)) +
      geom_histogram(bins = 15, col = "white", fill = "orange") +
      labs(x = if (input$ilog) "log10(precio)" else "precio")
  })

  output$odisp <- renderPlot({
    datos_disp() |>
      ggplot(aes(x = points, y = y, col = color)) +
      geom_point(alpha = 0.3) +
      labs(y   = if (input$ilog) "log10(precio)" else "precio",
           col = input$icolormodelo)
  })

  output$ocoeficientes <- renderDT({
    datatable(tidy(modelo())) |>
      formatRound(columns = 2:5, digits = 4)
  })

  output$oanalisis <- renderPrint({
    summary(modelo())
  })
}

shinyApp(ui, server)
