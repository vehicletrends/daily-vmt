library(shiny)
library(bslib)
library(echarts4r)
library(tidyverse)

# Load data
data_url <- "https://cdn.jsdelivr.net/gh/vehicletrends/data@main/data/quantiles_dvmt.csv"
data <- read.csv(data_url)

ui <- page_sidebar(
  title = "DVMT Quantiles by Vehicle Type",
  sidebar = sidebar(
    radioButtons(
      "facet_by",
      "Facet by:",
      choices = c("Powertrain" = "powertrain", "Vehicle Type" = "vehicle_type"),
      selected = "powertrain"
    ),
    checkboxGroupInput(
      "vehicle_types",
      "Vehicle Types:",
      choices = unique(data$vehicle_type),
      selected = unique(data$vehicle_type)
    ),
    checkboxGroupInput(
      "powertrains",
      "Powertrains:",
      choices = unique(data$powertrain),
      selected = unique(data$powertrain)
    )
  ),
  card(
    card_header("Legend"),
    echarts4rOutput("legend_chart", height = "100px")
  ),
  uiOutput("charts")
)

server <- function(input, output, session) {

  filtered_data <- reactive({
    data %>%
      filter(
        vehicle_type %in% input$vehicle_types,
        powertrain %in% input$powertrains
      )
  })

  # Create a minimal chart that shows only the legend
  output$legend_chart <- renderEcharts4r({
    req(nrow(filtered_data()) > 0)

    # Determine which variable represents the lines
    if (input$facet_by == "powertrain") {
      line_var <- "vehicle_type"
    } else {
      line_var <- "powertrain"
    }

    # Create a chart with minimal data just to generate the legend
    # We'll take just 2 points per series to make it tiny
    legend_data <- filtered_data() %>%
      group_by(.data[[line_var]]) %>%
      slice(1:2)

    legend_data %>%
      group_by(.data[[line_var]]) %>%
      e_charts(dvmt) %>%
      e_line(quantile, symbol = "none", lineStyle = list(opacity = 0)) %>%
      e_legend(
        show = TRUE,
        orient = "horizontal",
        left = "center",
        top = "center",
        itemGap = 20
      ) %>%
      e_x_axis(show = FALSE) %>%
      e_y_axis(show = FALSE) %>%
      e_grid(
        left = 0,
        right = 0,
        top = 0,
        bottom = 0,
        show = FALSE
      ) %>%
      e_tooltip(show = FALSE)
  })

  output$charts <- renderUI({
    req(nrow(filtered_data()) > 0)

    # Determine which variable to facet by
    if (input$facet_by == "powertrain") {
      facet_values <- sort(unique(filtered_data()$powertrain))
    } else {
      facet_values <- sort(unique(filtered_data()$vehicle_type))
    }

    chart_outputs <- lapply(facet_values, function(fv) {
      output_id <- paste0("chart_", gsub("[^A-Za-z0-9]", "_", fv))

      card(
        card_header(fv),
        echarts4rOutput(output_id, height = "300px")
      )
    })

    layout_column_wrap(
      width = 1/2,
      !!!chart_outputs
    )
  })

  observe({
    req(nrow(filtered_data()) > 0)

    # Determine which variable to facet by and which to use for lines
    if (input$facet_by == "powertrain") {
      facet_values <- sort(unique(filtered_data()$powertrain))
      facet_var <- "powertrain"
      line_var <- "vehicle_type"
    } else {
      facet_values <- sort(unique(filtered_data()$vehicle_type))
      facet_var <- "vehicle_type"
      line_var <- "powertrain"
    }

    lapply(facet_values, function(fv) {
      output_id <- paste0("chart_", gsub("[^A-Za-z0-9]", "_", fv))

      local({
        fv_local <- fv
        output[[output_id]] <- renderEcharts4r({
          chart_data <- filtered_data() %>%
            filter(.data[[facet_var]] == fv_local)

          chart_data %>%
            group_by(.data[[line_var]]) %>%
            e_charts(dvmt) %>%
            e_line(quantile, symbol = "none", smooth = FALSE) %>%
            e_tooltip(trigger = "axis") %>%
            e_x_axis(name = "DVMT") %>%
            e_y_axis(name = "Quantile") %>%
            e_legend(show = FALSE) %>%
            e_color(background = "transparent") %>%
            e_toolbox_feature(feature = "saveAsImage")
        })
      })
    })
  })
}

shinyApp(ui, server)
