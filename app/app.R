library(shiny)
library(bslib)
library(DBI)
library(RSQLite)
library(dbplyr)
library(dplyr)
library(httr2)
library(ggplot2)
library(DT)
library(broom)
library(tidyr)
library(sf)
library(giscoR)
library(leaflet)

con  <- dbConnect(SQLite(), dbname = "data/wine.db", flags = SQLITE_RO)
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

# Country name harmonisation: DB name -> giscoR NAME_ENGL
country_recode <- c(
  "Czech Republic" = "Czechia",
  "England"        = "United Kingdom",
  "Macedonia"      = "North Macedonia",
  "Turkey"         = "Türkiye",
  "US"             = "United States"
)

interpretar_modelo <- function(summary_text) {
  api_key <- Sys.getenv("OPENROUTER_API_KEY")
  if (nchar(api_key) == 0) stop("OPENROUTER_API_KEY no configurada en .Renviron")

  request("https://openrouter.ai/api/v1/chat/completions") |>
    req_headers(Authorization = paste("Bearer", api_key),
                `Content-Type` = "application/json") |>
    req_body_json(list(
      model    = "openrouter/free",
      messages = list(
        list(role = "system",
             content = paste(
               "Eres un profesor de estadística. Interpreta el siguiente",
               "resumen de un modelo de regresión lineal en lenguaje sencillo",
               "para estudiantes de máster sin experiencia avanzada en estadística.",
               "Destaca los coeficientes más importantes, el R², y si el modelo",
               "es útil para predecir el precio del vino. Responde en español,",
               "en 3-4 párrafos breves."
             )),
        list(role = "user", content = summary_text)
      )
    )) |>
    req_perform() |>
    resp_body_json(simplifyVector = FALSE) |>
    (`[[`)("choices") |> (`[[`)(1) |> (`[[`)("message") |> (`[[`)("content")
}

# Build geo data once at startup
countries_geo <- gisco_get_countries()

geo_data <- wine |>
  group_by(country) |>
  summarise(
    med_price  = median(price, na.rm = TRUE),
    avg_points = mean(points,  na.rm = TRUE),
    n_resenas  = n(),
    .groups    = "drop"
  ) |>
  collect() |>
  mutate(country_geo = recode(country, !!!country_recode)) |>
  right_join(countries_geo |> select(NAME_ENGL, geometry),
             by = c("country_geo" = "NAME_ENGL")) |>
  st_as_sf()

ui <- page_navbar(
  id    = "nav",                           # ejercicio 2: exponer pestaña activa
  theme = bs_theme(bootswatch = "cosmo"),
  title = "El vino y yo",
  sidebar = sidebar(title = "Menú lateral",
                    selectizeInput("ipais",
                                   label = "País",
                                   choices = paises),
                    checkboxInput("ilog", label = "Logaritmo"),
                    conditionalPanel(
                      condition = "input.nav === 'Modelo de precio'",
                      selectizeInput("icolormodelo",
                                     label = "Color puntos",
                                     choices = colores)
                    ),
                    # ejercicio 1: selector de variable del mapa
                    # ejercicio 2: solo visible en la pestaña Mapa
                    conditionalPanel(
                      condition = "input.nav === 'Mapa'",
                      selectizeInput("imapavar",
                                     label   = "Mapa: variable",
                                     choices = c("Precio mediano"    = "med_price",
                                                 "Número de reseñas" = "n_resenas"))
                    )),
  nav_panel("Exploración",
            "Análisis por país",
            plotOutput("ohistprecio")),
  nav_panel("Mapa",
            leafletOutput("omapa", height = "500px"),
            dataTableOutput("otablamapa")),  # ejercicio 4
  nav_panel("Modelo de precio",
            "Estimación del precio",
            navset_card_underline(
              title = "Modelización",
              nav_panel("Gráfico de dispersión",
                        plotOutput("odisp")),
              nav_panel("Coeficientes",
                        dataTableOutput("ocoeficientes")),
              nav_panel("Análisis",
                        verbatimTextOutput("oanalisis"),
                        hr(),
                        actionButton("ibtnia", "Interpretar con IA",
                                     icon = icon("robot")),
                        uiOutput("ointerpretacion"))
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

  output$omapa <- renderLeaflet({
    var     <- input$imapavar
    valores <- geo_data[[var]]
    titulo  <- if (var == "med_price") "Precio mediano ($)" else "Nº reseñas"

    pal <- colorBin(
      "YlOrRd",
      domain   = valores,
      bins     = quantile(valores, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
      na.color = "#cccccc"
    )

    # ejercicio 3: tooltip dinámico según variable seleccionada
    etiqueta <- if (var == "med_price") {
      paste0(geo_data$country, ": $", round(geo_data$med_price, 1),
             " | ", round(geo_data$avg_points, 1), " pts",
             " (", geo_data$n_resenas, " reseñas)")
    } else {
      paste0(geo_data$country, ": ", geo_data$n_resenas, " reseñas",
             " | $", round(geo_data$med_price, 1),
             " | ", round(geo_data$avg_points, 1), " pts")
    }

    geo_data |>
      leaflet() |>
      addTiles() |>
      addPolygons(
        fillColor   = pal(valores),
        weight      = 1,
        opacity     = 1,
        color       = "white",
        dashArray   = "3",
        fillOpacity = 0.7,
        label       = etiqueta
      ) |>
      addLegend(
        pal      = pal,
        values   = valores,
        title    = titulo,
        position = "bottomright"
      )
  })

  # ejercicio 4: tabla resumen ordenada por variable seleccionada
  output$otablamapa <- renderDT({
    geo_data |>
      st_drop_geometry() |>
      drop_na(country) |>
      select(country, med_price, avg_points, n_resenas) |>
      arrange(desc(.data[[input$imapavar]])) |>
      datatable(
        colnames = c("País", "Precio mediano", "Puntuación media", "Nº reseñas"),
        options  = list(pageLength = 10)
      ) |>
      formatRound(columns = c("med_price", "avg_points"), digits = 1)
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

  interpretacion <- eventReactive(input$ibtnia, {
    summary_text <- paste(capture.output(summary(modelo())), collapse = "\n")
    withProgress(message = "Consultando IA...", interpretar_modelo(summary_text))
  })

  output$ointerpretacion <- renderUI({
    req(interpretacion())
    div(style = "margin-top:1rem; padding:1rem; background:#f8f9fa; border-radius:6px;",
        h5("Interpretación"),
        p(interpretacion()))
  })
}

shinyApp(ui, server)
