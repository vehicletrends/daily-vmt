# Simplified Daily VMT Dashboard
# Single page app showing only Daily Vehicle Miles Traveled distributions

# Load required libraries
library(shiny)
library(plotly)

# Load data from jsDelivr CDN (CORS-enabled for Shinylive)
data_url <- "https://cdn.jsdelivr.net/gh/jhelvy/vehicletrends@main/data/quantiles_dvmt.csv"
quantiles_dvmt <- read.csv(data_url)

# Define UI
ui <- fluidPage(
  titlePanel("Daily VMT Distribution"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      h5("Powertrain Type:", style = "font-weight: bold;"),
      checkboxGroupInput(
        "fuel_types",
        NULL,
        choices = list(
          "BEV" = "bev",
          "Conventional" = "cv",
          "Diesel" = "diesel",
          "Flex Fuel" = "flex",
          "Hybrid" = "hev",
          "PHEV" = "phev"
        ),
        selected = c("bev", "cv", "hev")
      ),

      h5("Vehicle Type:", style = "font-weight: bold; margin-top: 15px;"),
      checkboxGroupInput(
        "vehicle_types",
        NULL,
        choices = list(
          "Car" = "car",
          "CUV" = "cuv",
          "Minivan" = "minivan",
          "Pickup" = "pickup",
          "SUV" = "suv"
        ),
        selected = c("car", "cuv", "minivan", "pickup", "suv")
      ),

      hr(),

      sliderInput(
        "dvmt_range",
        "DVMT Range (miles):",
        min = 0,
        max = 100,
        value = c(0, 100)
      ),

      radioButtons(
        "plot_type",
        "Plot Type:",
        choices = list(
          "CDF" = "cdf",
          "PDF" = "pdf",
          "Histogram" = "hist"
        ),
        selected = "cdf"
      ),

      checkboxInput(
        "show_aggregated",
        "Show Aggregated Line",
        value = FALSE
      )
    ),

    mainPanel(
      width = 9,
      plotlyOutput("cdf_plot", height = "700px")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Load VMT data from GitHub
  sample_data <- reactive({
    data <- quantiles_dvmt[
      quantiles_dvmt$powertrain != "all" & quantiles_dvmt$vehicle_type != "all",
    ]
    return(data)
  })

  # Filtered data based on user inputs
  filtered_data <- reactive({
    data <- sample_data()

    # Get filter values with defaults
    fuel_types <- if (
      is.null(input$fuel_types) || length(input$fuel_types) == 0
    ) {
      unique(data$powertrain)
    } else {
      input$fuel_types
    }
    fuel_types <- intersect(fuel_types, unique(data$powertrain))

    vehicle_types <- if (
      is.null(input$vehicle_types) || length(input$vehicle_types) == 0
    ) {
      unique(data$vehicle_type)
    } else {
      input$vehicle_types
    }

    dvmt_range <- if (is.null(input$dvmt_range)) {
      c(0, max(data$dvmt, na.rm = TRUE))
    } else {
      input$dvmt_range
    }

    data <- data[
      data$powertrain %in%
        fuel_types &
        data$vehicle_type %in% vehicle_types &
        data$dvmt >= dvmt_range[1] &
        data$dvmt <= dvmt_range[2],
    ]

    return(data)
  })

  # Render the main plot
  output$cdf_plot <- renderPlotly({
    tryCatch(
      {
        data <- filtered_data()

        if (nrow(data) == 0) {
          return(
            plot_ly() %>%
              add_annotations(
                text = "No data available for selected filters",
                xref = "paper",
                yref = "paper",
                x = 0.5,
                y = 0.5,
                showarrow = FALSE,
                font = list(size = 16)
              )
          )
        }

        # Remove NA values
        data <- data[
          !is.na(data$dvmt) &
            !is.na(data$quantile) &
            !is.na(data$powertrain) &
            !is.na(data$vehicle_type),
        ]

        if (nrow(data) == 0) {
          return(
            plot_ly() %>%
              add_annotations(
                text = "No valid data after cleaning",
                xref = "paper",
                yref = "paper",
                x = 0.5,
                y = 0.5,
                showarrow = FALSE,
                font = list(size = 16)
              )
          )
        }

        # Get plot type
        plot_type <- if (is.null(input$plot_type)) "cdf" else input$plot_type

        # Get unique powertrains and vehicle types
        powertrains <- unique(data$powertrain)
        vehicle_types <- unique(data$vehicle_type)

        # Create color palette
        colors <- viridisLite::viridis(length(vehicle_types))
        color_map <- setNames(colors, vehicle_types)

        # --- CDF PLOT ---
        if (plot_type == "cdf") {
          plots <- lapply(powertrains, function(pt) {
            pt_data <- data[data$powertrain == pt, ]

            p <- plot_ly()

            for (vt in vehicle_types) {
              vt_data <- pt_data[pt_data$vehicle_type == vt, ]
              if (nrow(vt_data) > 0) {
                p <- p %>%
                  add_trace(
                    data = vt_data,
                    x = ~dvmt,
                    y = ~quantile,
                    type = 'scatter',
                    mode = 'lines',
                    name = vt,
                    line = list(color = color_map[vt], width = 2.5),
                    hovertemplate = paste0(
                      "<b>",
                      vt,
                      "</b><br>",
                      "DVMT: %{x:.1f} miles<br>",
                      "Quantile: %{y:.1%}<br>",
                      "<extra></extra>"
                    ),
                    showlegend = (pt == powertrains[1])
                  )
              }
            }

            # Add aggregated line if requested
            if (
              !is.null(input$show_aggregated) &&
                input$show_aggregated &&
                length(vehicle_types) > 1
            ) {
              agg_data <- aggregate(
                quantile ~ dvmt,
                data = pt_data,
                FUN = mean,
                na.rm = TRUE
              )
              agg_data <- agg_data[order(agg_data$dvmt), ]

              if (nrow(agg_data) > 0) {
                p <- p %>%
                  add_trace(
                    data = agg_data,
                    x = ~dvmt,
                    y = ~quantile,
                    type = 'scatter',
                    mode = 'lines',
                    name = 'Aggregated',
                    line = list(color = 'black', width = 3, dash = 'dash'),
                    hovertemplate = paste0(
                      "<b>Aggregated</b><br>",
                      "DVMT: %{x:.1f} miles<br>",
                      "Quantile: %{y:.1%}<br>",
                      "<extra></extra>"
                    ),
                    showlegend = (pt == powertrains[1])
                  )
              }
            }

            p <- p %>%
              layout(
                xaxis = list(title = "DVMT", gridcolor = "#e9ecef"),
                yaxis = list(
                  title = "Cumulative Probability",
                  gridcolor = "#e9ecef"
                ),
                plot_bgcolor = "#fafbfc",
                paper_bgcolor = "#ffffff",
                annotations = list(
                  x = 0.5,
                  y = 1.08,
                  text = paste("<b>", toupper(pt), "</b>"),
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE,
                  font = list(size = 14, color = "#2c3e50")
                )
              )

            return(p)
          })

          fig <- subplot(
            plots,
            nrows = ceiling(length(powertrains) / 3),
            shareX = FALSE,
            shareY = TRUE,
            titleX = TRUE,
            titleY = TRUE,
            margin = 0.06
          ) %>%
            layout(
              title = list(
                text = "<b>Cumulative Distribution Function (CDF) of Daily VMT</b><br><sub style='color:#6c757d;'>Distribution of daily vehicle miles traveled by powertrain and vehicle type</sub>",
                font = list(size = 18, color = "#2c3e50")
              ),
              hovermode = 'closest',
              showlegend = TRUE,
              legend = list(
                orientation = "h",
                x = 0.5,
                xanchor = "center",
                y = -0.12,
                bgcolor = "rgba(255,255,255,0.9)",
                bordercolor = "#dee2e6",
                borderwidth = 1,
                font = list(size = 12)
              ),
              margin = list(t = 100, b = 80, l = 60, r = 40)
            ) %>%
            config(
              displayModeBar = TRUE,
              displaylogo = FALSE,
              modeBarButtonsToRemove = c('lasso2d', 'select2d', 'autoScale2d'),
              toImageButtonOptions = list(
                format = 'png',
                filename = 'dvmt_cdf_plot',
                height = 1200,
                width = 1800,
                scale = 2
              )
            )

          return(fig)

          # --- PDF PLOT ---
        } else if (plot_type == "pdf") {
          plots <- lapply(powertrains, function(pt) {
            pt_data <- data[data$powertrain == pt, ]

            p <- plot_ly()

            for (vt in vehicle_types) {
              vt_data <- pt_data[pt_data$vehicle_type == vt, ]
              vt_data <- vt_data[order(vt_data$dvmt), ]

              if (nrow(vt_data) > 1) {
                pdf_vals <- c(diff(vt_data$quantile) / diff(vt_data$dvmt), NA)
                pdf_data <- data.frame(
                  dvmt = vt_data$dvmt,
                  pdf = pdf_vals
                )
                pdf_data <- pdf_data[!is.na(pdf_data$pdf) & pdf_data$pdf >= 0, ]

                if (nrow(pdf_data) > 0) {
                  p <- p %>%
                    add_trace(
                      data = pdf_data,
                      x = ~dvmt,
                      y = ~pdf,
                      type = 'scatter',
                      mode = 'lines',
                      name = vt,
                      line = list(color = color_map[vt], width = 2),
                      fill = 'tozeroy',
                      fillcolor = paste0(substr(color_map[vt], 1, 7), "30"),
                      hovertemplate = paste0(
                        "<b>",
                        vt,
                        "</b><br>",
                        "DVMT: %{x:.1f} miles<br>",
                        "Density: %{y:.4f}<br>",
                        "<extra></extra>"
                      ),
                      showlegend = (pt == powertrains[1])
                    )
                }
              }
            }

            # Add aggregated line if requested
            if (
              !is.null(input$show_aggregated) &&
                input$show_aggregated &&
                length(vehicle_types) > 1
            ) {
              agg_cdf <- aggregate(
                quantile ~ dvmt,
                data = pt_data,
                FUN = mean,
                na.rm = TRUE
              )
              agg_cdf <- agg_cdf[order(agg_cdf$dvmt), ]

              if (nrow(agg_cdf) > 1) {
                pdf_vals <- c(diff(agg_cdf$quantile) / diff(agg_cdf$dvmt), NA)
                agg_pdf <- data.frame(
                  dvmt = agg_cdf$dvmt,
                  pdf = pdf_vals
                )
                agg_pdf <- agg_pdf[!is.na(agg_pdf$pdf) & agg_pdf$pdf >= 0, ]

                if (nrow(agg_pdf) > 0) {
                  p <- p %>%
                    add_trace(
                      data = agg_pdf,
                      x = ~dvmt,
                      y = ~pdf,
                      type = 'scatter',
                      mode = 'lines',
                      name = 'Aggregated',
                      line = list(color = 'black', width = 3, dash = 'dash'),
                      hovertemplate = paste0(
                        "<b>Aggregated</b><br>",
                        "DVMT: %{x:.1f} miles<br>",
                        "Density: %{y:.4f}<br>",
                        "<extra></extra>"
                      ),
                      showlegend = (pt == powertrains[1])
                    )
                }
              }
            }

            p <- p %>%
              layout(
                xaxis = list(title = "DVMT", gridcolor = "#e9ecef"),
                yaxis = list(
                  title = "Probability Density",
                  gridcolor = "#e9ecef"
                ),
                plot_bgcolor = "#fafbfc",
                annotations = list(
                  x = 0.5,
                  y = 1.05,
                  text = paste("<b>", toupper(pt), "</b>"),
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE,
                  font = list(size = 12)
                )
              )

            return(p)
          })

          fig <- subplot(
            plots,
            nrows = ceiling(length(powertrains) / 3),
            shareX = FALSE,
            shareY = FALSE,
            titleX = TRUE,
            titleY = TRUE,
            margin = 0.06
          ) %>%
            layout(
              title = list(
                text = "<b>Probability Density Function (PDF) of Daily VMT</b><br><sub>Density distribution of daily vehicle miles traveled</sub>",
                font = list(size = 16)
              ),
              hovermode = 'closest',
              showlegend = TRUE,
              legend = list(
                orientation = "h",
                x = 0.5,
                xanchor = "center",
                y = -0.15
              )
            ) %>%
            config(
              displayModeBar = TRUE,
              displaylogo = FALSE,
              modeBarButtonsToRemove = c('lasso2d', 'select2d'),
              toImageButtonOptions = list(
                format = 'png',
                filename = 'dvmt_pdf_plot'
              )
            )

          return(fig)

          # --- HISTOGRAM PLOT ---
        } else if (plot_type == "hist") {
          plots <- lapply(powertrains, function(pt) {
            pt_data <- data[data$powertrain == pt, ]

            p <- plot_ly()

            for (vt in vehicle_types) {
              vt_data <- pt_data[pt_data$vehicle_type == vt, ]

              if (nrow(vt_data) > 0) {
                p <- p %>%
                  add_trace(
                    data = vt_data,
                    x = ~dvmt,
                    type = 'histogram',
                    name = vt,
                    marker = list(
                      color = color_map[vt],
                      line = list(color = 'white', width = 1)
                    ),
                    opacity = 0.7,
                    hovertemplate = paste0(
                      "<b>",
                      vt,
                      "</b><br>",
                      "DVMT: %{x}<br>",
                      "Count: %{y}<br>",
                      "<extra></extra>"
                    ),
                    showlegend = (pt == powertrains[1])
                  )
              }
            }

            # Add aggregated density line if requested
            if (
              !is.null(input$show_aggregated) &&
                input$show_aggregated &&
                length(vehicle_types) > 1
            ) {
              agg_data <- aggregate(
                quantile ~ dvmt,
                data = pt_data,
                FUN = mean,
                na.rm = TRUE
              )
              agg_data <- agg_data[order(agg_data$dvmt), ]

              if (nrow(agg_data) > 1) {
                pdf_vals <- c(diff(agg_data$quantile) / diff(agg_data$dvmt), NA)
                agg_pdf <- data.frame(
                  dvmt = agg_data$dvmt,
                  pdf = pdf_vals
                )
                agg_pdf <- agg_pdf[!is.na(agg_pdf$pdf) & agg_pdf$pdf >= 0, ]

                if (nrow(agg_pdf) > 0) {
                  max_pdf <- max(agg_pdf$pdf, na.rm = TRUE)
                  if (max_pdf > 0) {
                    scale_factor <- nrow(pt_data) *
                      (max(pt_data$dvmt) - min(pt_data$dvmt)) /
                      30
                    agg_pdf$pdf_scaled <- agg_pdf$pdf * scale_factor

                    p <- p %>%
                      add_trace(
                        data = agg_pdf,
                        x = ~dvmt,
                        y = ~pdf_scaled,
                        type = 'scatter',
                        mode = 'lines',
                        name = 'Aggregated',
                        line = list(color = 'black', width = 3, dash = 'dash'),
                        yaxis = 'y2',
                        hovertemplate = paste0(
                          "<b>Aggregated Density</b><br>",
                          "DVMT: %{x:.1f} miles<br>",
                          "Density: %{y:.2f}<br>",
                          "<extra></extra>"
                        ),
                        showlegend = (pt == powertrains[1])
                      )
                  }
                }
              }
            }

            layout_config <- list(
              xaxis = list(title = "DVMT", gridcolor = "#e9ecef"),
              yaxis = list(title = "Count", gridcolor = "#e9ecef"),
              barmode = 'overlay',
              plot_bgcolor = "#fafbfc",
              annotations = list(
                x = 0.5,
                y = 1.05,
                text = paste("<b>", toupper(pt), "</b>"),
                xref = "paper",
                yref = "paper",
                showarrow = FALSE,
                font = list(size = 12)
              )
            )

            # Add secondary y-axis if aggregated line is shown
            if (
              !is.null(input$show_aggregated) &&
                input$show_aggregated &&
                length(vehicle_types) > 1
            ) {
              layout_config$yaxis2 <- list(
                overlaying = 'y',
                side = 'right',
                title = 'Density',
                showgrid = FALSE
              )
            }

            p <- p %>% layout(layout_config)

            return(p)
          })

          fig <- subplot(
            plots,
            nrows = ceiling(length(powertrains) / 3),
            shareX = FALSE,
            shareY = FALSE,
            titleX = TRUE,
            titleY = TRUE,
            margin = 0.06
          ) %>%
            layout(
              title = list(
                text = "<b>Histogram of Daily VMT</b><br><sub>Frequency distribution of daily vehicle miles traveled</sub>",
                font = list(size = 16)
              ),
              hovermode = 'closest',
              showlegend = TRUE,
              legend = list(
                orientation = "h",
                x = 0.5,
                xanchor = "center",
                y = -0.15
              )
            ) %>%
            config(
              displayModeBar = TRUE,
              displaylogo = FALSE,
              modeBarButtonsToRemove = c('lasso2d', 'select2d'),
              toImageButtonOptions = list(
                format = 'png',
                filename = 'dvmt_histogram_plot'
              )
            )

          return(fig)
        }
      },
      error = function(e) {
        plot_ly() %>%
          add_annotations(
            text = paste("Error:", e$message),
            xref = "paper",
            yref = "paper",
            x = 0.5,
            y = 0.5,
            showarrow = FALSE,
            font = list(size = 14, color = "red")
          )
      }
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
