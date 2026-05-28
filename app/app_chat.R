library(shiny)
library(bslib)
library(DT)
library(httr2)
library(jsonlite)
library(dplyr)
library(promises)
library(future)

plan(multisession)

# ── Dataset ----------------------------------------------------------------
data_df <- mtcars |>
  tibble::rownames_to_column("model") |>
  dplyr::select(model, mpg, cyl, hp, wt, gear)

system_prompt <- paste0(
  "You are a helpful data analyst assistant working with the mtcars dataset ",
  "(Motor Trend Car Road Tests, 1974). It has ", nrow(data_df), " cars with columns: ",
  "model (car name), mpg (miles/gallon), cyl (cylinders), hp (horsepower), ",
  "wt (weight in 1000 lbs), gear (number of gears).\n",
  "Use the provided tools to query the data before answering. ",
  "Always call a tool when the user asks about specific values, comparisons, or subsets. ",
  "Be concise."
)

# ── Tool definitions (JSON schema sent to the model) ----------------------
tools <- list(
  list(
    type = "function",
    `function` = list(
      name        = "filter_cars",
      description = "Filter cars by column conditions. Returns matching rows as a table.",
      parameters  = list(
        type       = "object",
        properties = list(
          cyl    = list(type = "integer", description = "Filter by number of cylinders (4, 6, or 8)"),
          min_hp = list(type = "number",  description = "Minimum horsepower"),
          max_hp = list(type = "number",  description = "Maximum horsepower"),
          min_mpg= list(type = "number",  description = "Minimum miles per gallon"),
          gear   = list(type = "integer", description = "Filter by number of gears (3, 4, or 5)")
        ),
        required   = list()
      )
    )
  ),
  list(
    type = "function",
    `function` = list(
      name        = "summarise_by",
      description = "Compute mean mpg, hp, and wt grouped by a column (cyl or gear).",
      parameters  = list(
        type       = "object",
        properties = list(
          group_by = list(type = "string", description = "Column to group by: 'cyl' or 'gear'")
        ),
        required   = list("group_by")
      )
    )
  ),
  list(
    type = "function",
    `function` = list(
      name        = "sort_cars",
      description = "Return the top N cars sorted by a column.",
      parameters  = list(
        type       = "object",
        properties = list(
          by         = list(type = "string",  description = "Column to sort by: mpg, hp, wt, cyl, gear"),
          descending = list(type = "boolean", description = "Sort descending? Default true"),
          n          = list(type = "integer", description = "Number of rows to return, default 5")
        ),
        required   = list("by")
      )
    )
  ),
  list(
    type = "function",
    `function` = list(
      name        = "correlate",
      description = "Compute the Pearson correlation between two numeric columns.",
      parameters  = list(
        type       = "object",
        properties = list(
          col_x = list(type = "string", description = "First column: mpg, hp, wt, cyl, gear"),
          col_y = list(type = "string", description = "Second column: mpg, hp, wt, cyl, gear")
        ),
        required   = list("col_x", "col_y")
      )
    )
  )
)

# ── Tool implementations ---------------------------------------------------
run_tool <- function(name, args_json) {
  args <- fromJSON(args_json, simplifyVector = TRUE)

  if (name == "filter_cars") {
    df <- data_df
    if (!is.null(args$cyl))     df <- df |> filter(cyl    == args$cyl)
    if (!is.null(args$gear))    df <- df |> filter(gear   == args$gear)
    if (!is.null(args$min_hp))  df <- df |> filter(hp     >= args$min_hp)
    if (!is.null(args$max_hp))  df <- df |> filter(hp     <= args$max_hp)
    if (!is.null(args$min_mpg)) df <- df |> filter(mpg    >= args$min_mpg)
    if (nrow(df) == 0) return("No cars match those filters.")
    toJSON(df, auto_unbox = TRUE)

  } else if (name == "summarise_by") {
    col <- args$group_by
    if (!col %in% c("cyl", "gear")) return("group_by must be 'cyl' or 'gear'")
    result <- data_df |>
      group_by(.data[[col]]) |>
      summarise(
        n        = n(),
        mean_mpg = round(mean(mpg), 2),
        mean_hp  = round(mean(hp),  2),
        mean_wt  = round(mean(wt),  2),
        .groups  = "drop"
      )
    toJSON(result, auto_unbox = TRUE)

  } else if (name == "sort_cars") {
    col  <- args$by
    desc <- if (is.null(args$descending)) TRUE else args$descending
    n    <- if (is.null(args$n)) 5L else as.integer(args$n)
    if (!col %in% names(data_df)) return(paste("Unknown column:", col))
    df <- if (desc) data_df |> arrange(desc(.data[[col]])) else
                    data_df |> arrange(.data[[col]])
    toJSON(head(df, n), auto_unbox = TRUE)

  } else if (name == "correlate") {
    cx <- args$col_x; cy <- args$col_y
    valid <- c("mpg", "hp", "wt", "cyl", "gear")
    if (!cx %in% valid || !cy %in% valid)
      return(paste("Columns must be one of:", paste(valid, collapse = ", ")))
    r <- cor(data_df[[cx]], data_df[[cy]])
    sprintf('{"col_x":"%s","col_y":"%s","pearson_r":%.4f}', cx, cy, r)

  } else {
    paste("Unknown tool:", name)
  }
}

# ── OpenRouter call with tool-use loop ------------------------------------
# Returns the final assistant text after resolving all tool calls.
chat_with_tools <- function(messages, model) {
  api_key <- Sys.getenv("OPENROUTER_API_KEY")
  if (nchar(api_key) == 0) stop("OPENROUTER_API_KEY not set in .Renviron")

  msgs <- messages  # local mutable copy

  repeat {
    body <- list(model = model, messages = msgs, tools = tools)

    resp <- request("https://openrouter.ai/api/v1/chat/completions") |>
      req_headers(
        Authorization  = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ) |>
      req_body_json(body) |>
      req_perform()

    result  <- resp_body_json(resp, simplifyVector = FALSE)
    choice  <- result$choices[[1]]
    message <- choice$message

    # No tool calls — return the text content
    if (is.null(message$tool_calls) || length(message$tool_calls) == 0) {
      return(message$content %||% "(no response)")
    }

    # Append assistant message with tool_calls
    msgs <- c(msgs, list(message))

    # Execute each tool call and append results
    for (tc in message$tool_calls) {
      tool_result <- tryCatch(
        run_tool(tc$`function`$name, tc$`function`$arguments),
        error = function(e) paste("Tool error:", conditionMessage(e))
      )
      msgs <- c(msgs, list(list(
        role         = "tool",
        tool_call_id = tc$id,
        content      = tool_result
      )))
    }
  }
}

`%||%` <- function(a, b) if (!is.null(a) && nchar(a) > 0) a else b

# ── UI --------------------------------------------------------------------
ui <- page_sidebar(
  title = "Chat with mtcars",
  theme = bs_theme(bootswatch = "cosmo"),

  sidebar = sidebar(
    title = "Settings",
    width = 280,
    p(class = "text-muted small",
      "Model: OpenRouter free router — picks the best available free model automatically."),
    hr(),
    actionButton("clear", "Clear chat", class = "btn-outline-secondary btn-sm w-100")
  ),

  layout_columns(
    col_widths = c(7, 5),

    card(
      card_header("Chat"),
      uiOutput("chat_messages"),
      card_footer(
        layout_columns(
          col_widths = c(10, 2),
          textAreaInput("user_input", NULL,
            placeholder = "Ask something about the data...",
            rows = 2
          ),
          actionButton("send", "Send", class = "btn-primary w-100 h-100")
        )
      )
    ),

    card(
      card_header("Data: mtcars"),
      DTOutput("data_table")
    )
  )
)

# ── Server ----------------------------------------------------------------
server <- function(input, output, session) {

  # list of list(role, content) — tool messages excluded from display
  history     <- reactiveVal(list())
  is_thinking <- reactiveVal(FALSE)

  output$data_table <- renderDT({
    datatable(data_df, options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })

  render_bubble <- function(m) {
    is_user <- m$role == "user"
    div(
      class = if (is_user) "d-flex justify-content-end mb-2"
              else         "d-flex justify-content-start mb-2",
      div(
        class = if (is_user) "p-2 rounded bg-primary text-white"
                else         "p-2 rounded bg-light border",
        style = "max-width: 85%; white-space: pre-wrap;",
        m$content
      )
    )
  }

  output$chat_messages <- renderUI({
    msgs <- history()

    bubbles <- if (length(msgs) == 0) {
      list(p("No messages yet. Ask something about the data!", class = "text-muted p-3"))
    } else {
      lapply(msgs, render_bubble)
    }

    if (is_thinking()) {
      bubbles <- c(bubbles, list(
        div(class = "d-flex justify-content-start mb-2",
          div(class = "p-2 rounded bg-light border text-muted fst-italic",
              "Thinking...")
        )
      ))
    }

    div(
      id    = "chat_scroll",
      style = "height: 420px; overflow-y: auto; padding: 8px;",
      tagList(bubbles)
    )
  })

  observeEvent(input$send, {
    req(!is_thinking(), nchar(trimws(input$user_input)) > 0)

    user_text <- trimws(input$user_input)
    updateTextAreaInput(session, "user_input", value = "")

    current <- c(history(), list(list(role = "user", content = user_text)))
    history(current)
    is_thinking(TRUE)

    api_messages <- c(
      list(list(role = "system", content = system_prompt)),
      current
    )

    future_promise({
      chat_with_tools(api_messages, "openrouter/free")
    }) %...>% (function(reply) {
      history(c(current, list(list(role = "assistant", content = reply))))
      is_thinking(FALSE)
    }) %...!% (function(err) {
      history(c(current, list(list(role = "assistant",
                                   content = paste("Error:", conditionMessage(err))))))
      is_thinking(FALSE)
    })

    NULL
  })

  observeEvent(input$clear, {
    history(list())
  })
}

shinyApp(ui, server)
