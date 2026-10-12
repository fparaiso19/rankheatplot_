library(shiny)
library(rankheatplot)

example_csv <- paste(
  readLines(system.file("extdata", "example_rankheat.csv", package = "rankheatplot")),
  collapse = "\n"
)

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: 'Helvetica Neue', Arial, sans-serif; }
    .well { background: #f7f9fa; }
    .help-block { font-size: 12px; }
    #plot img { max-width: 100%; height: auto; }
  "))),
  titlePanel("Rank-heat plot", windowTitle = "Rank-heat plot"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      tabsetPanel(
        tabPanel(
          "Data",
          br(),
          radioButtons("source", NULL, inline = TRUE,
                       choices = c("Upload file" = "file", "Paste table" = "paste",
                                   "Example" = "example"),
                       selected = "example"),
          conditionalPanel(
            "input.source == 'file'",
            fileInput("file", NULL, accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls")),
            helpText("First column: treatments. One column per outcome with SUCRA",
                     "or P-score values (0-100 or 0-1). Leave a cell empty when a",
                     "treatment has no data for that outcome.")
          ),
          conditionalPanel(
            "input.source == 'paste'",
            textAreaInput("pasted", NULL, value = example_csv, rows = 10, resize = "vertical"),
            helpText("Comma, semicolon or tab separated, with a header row.",
                     "You can paste straight from Excel.")
          ),
          downloadLink("template", "Download a data template (CSV)")
        ),
        tabPanel(
          "Layout",
          br(),
          selectInput("treatment_order", "Treatment order (clockwise)",
                      c("As in the data" = "data", "Mean value, highest first" = "mean",
                        "Alphabetical" = "alphabetical")),
          uiOutput("outcome_order_ui"),
          textInput("outcome_title", "Outcome heading", "Outcome"),
          sliderInput("inner_radius", "Inner hole", 0, 0.7, 0.35, 0.01),
          sliderInput("sector_gap", "Gap between sectors (degrees)", 0, 8, 2, 0.5)
        ),
        tabPanel(
          "Colours & text",
          br(),
          selectInput("palette", "Colour scale",
                      c("Original (red - yellow - green)" = "veroniki",
                        "Red / yellow / green" = "ryg",
                        "ColorBrewer RdYlGn" = "RdYlGn",
                        "Grey scale" = "grey",
                        "Viridis (colour-blind safe)" = "viridis")),
          checkboxInput("reverse", "Reverse colours", FALSE),
          checkboxInput("show_values", "Show values in sectors", TRUE),
          fluidRow(
            column(6, numericInput("digits", "Decimals", 0, 0, 3, 1)),
            column(6, textInput("na_label", "Label for missing", ""))
          ),
          fluidRow(
            column(6, textInput("legend_title", "Legend title", "Score")),
            column(6, selectInput("legend_position", "Legend",
                                  c("Bottom left" = "bottomleft", "Bottom" = "bottom",
                                    "Right" = "right", "None" = "none")))
          ),
          selectInput("family", "Font", c("Sans serif" = "sans", "Serif" = "serif",
                                          "Monospace" = "mono")),
          sliderInput("treatment_size", "Treatment names", 1.5, 6, 3.0, 0.1),
          sliderInput("outcome_size", "Outcome names", 1, 5, 2.0, 0.1),
          sliderInput("value_size", "Values", 1, 5, 2.3, 0.1)
        ),
        tabPanel(
          "Download",
          br(),
          selectInput("format", "Format",
                      c("TIFF (LZW)" = "tiff", "PNG" = "png", "PDF" = "pdf",
                        "SVG" = "svg", "EPS" = "eps")),
          fluidRow(
            column(4, numericInput("width", "Width", 7, 1, 30, 0.5)),
            column(4, numericInput("height", "Height", 7, 1, 30, 0.5)),
            column(4, selectInput("units", "Units", c("in", "cm", "mm")))
          ),
          numericInput("dpi", "Resolution (dpi, TIFF/PNG)", 600, 72, 1200, 50),
          downloadButton("download", "Download plot")
        )
      )
    ),
    mainPanel(
      width = 8,
      uiOutput("message"),
      plotOutput("plot", width = "700px", height = "700px")
    )
  )
)

server <- function(input, output, session) {
  raw <- reactive({
    switch(input$source,
      example = example_rankheat,
      file = {
        req(input$file)
        ext <- tolower(tools::file_ext(input$file$name))
        path <- paste0(input$file$datapath, ".", ext)
        file.copy(input$file$datapath, path, overwrite = TRUE)
        read_rankheat(path)
      },
      paste = {
        req(nzchar(trimws(input$pasted)))
        tmp <- tempfile(fileext = ".csv")
        writeLines(input$pasted, tmp)
        read_rankheat(tmp)
      }
    )
  })

  data <- reactive({
    out <- tryCatch(rankheat_data(raw()), error = function(e) e)
    validate(need(!inherits(out, "error"),
                  if (inherits(out, "error")) conditionMessage(out) else ""))
    out
  })

  output$outcome_order_ui <- renderUI({
    outs <- levels(data()$outcome)
    selectizeInput("outcome_order", "Outcomes, outermost ring first",
                   choices = outs, selected = outs, multiple = TRUE,
                   options = list(plugins = list("drag_drop", "remove_button")))
  })

  plot_obj <- reactive({
    d <- data()
    outs <- input$outcome_order
    if (is.null(outs) || !length(outs) || !all(outs %in% levels(d$outcome))) {
      outs <- levels(d$outcome)
    }
    rankheatplot(
      d,
      treatment_order = input$treatment_order,
      outcome_order = outs,
      palette = input$palette,
      reverse_palette = input$reverse,
      show_values = input$show_values,
      digits = input$digits %||% 0,
      na_label = input$na_label,
      outcome_title = input$outcome_title,
      legend_title = input$legend_title,
      legend_position = input$legend_position,
      inner_radius = input$inner_radius,
      sector_gap = input$sector_gap,
      treatment_size = input$treatment_size,
      outcome_size = input$outcome_size,
      value_size = input$value_size,
      family = input$family
    )
  })

  output$plot <- renderPlot(plot_obj(), width = 700, height = 700, res = 100)

  output$download <- downloadHandler(
    filename = function() paste0("rankheatplot.", input$format),
    content = function(file) {
      if (input$format == "svg" && !requireNamespace("svglite", quietly = TRUE)) {
        stop("SVG export needs the 'svglite' package.")
      }
      tmp <- paste0(tempfile(), ".", input$format)
      save_rankheatplot(plot_obj(), tmp, width = input$width, height = input$height,
                        units = input$units, dpi = input$dpi)
      file.copy(tmp, file, overwrite = TRUE)
    }
  )

  output$template <- downloadHandler(
    filename = function() "rankheatplot_template.csv",
    content = function(file) writeLines(example_csv, file)
  )
}

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

shinyApp(ui, server)
