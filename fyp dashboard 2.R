library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(nycflights13)

# Data Preparation
flights_weather <- flights %>% 
  mutate(date = make_date(year, month, day)) %>%
  left_join(weather, by = c("origin", "time_hour")) %>%
  mutate(
    weather_type = case_when(
      is.na(precip) ~ "Unknown",
      precip > 0.2 ~ "Rainy",
      visib < 3 ~ "Low Visibility",
      wind_speed > 15 ~ "Windy",
      TRUE ~ "Clear"
    ),
    month_name = factor(month(date, label = TRUE), levels = month.abb),
    hour = hour(time_hour),
    day_of_month = day(date),
    flight_id = row_number()
  )

# Helper function for styled summary tiles
valueBox <- function(title, value, color) {
  div(
    style = paste0(
      "background-color:", color, 
      "; color:white; padding:20px; border-radius:10px; text-align:center; margin-bottom:10px;"
    ),
    h4(title),
    h2(value)
  )
}

# UI
ui <- fluidPage(
  titlePanel("NYC Flight Delays Dashboard (2013)"),
  
  tabsetPanel(
    tabPanel("Dashboard",
             sidebarLayout(
               sidebarPanel(
                 selectInput("month", "Month", choices = levels(flights_weather$month_name)),
                 selectInput("day", "Day of Month", choices = c("All", 1:31), selected = "All"),
                 selectInput("airline", "Airline", choices = c("All", sort(unique(flights_weather$carrier))), selected = "All"),
                 selectInput("weather", "Weather Type", choices = c("All", sort(unique(flights_weather$weather_type))), selected = "All")
               ),
               mainPanel(
                 h4("Summary Statistics"),
                 fluidRow(
                   column(4, uiOutput("tile_total")),
                   column(4, uiOutput("tile_cancelled")),
                   column(4, uiOutput("tile_early"))
                 ),
                 fluidRow(
                   column(6, uiOutput("tile_avg_dep")),
                   column(6, uiOutput("tile_avg_arr"))
                 ),
                 br(),
                 h4("Delay Histograms"),
                 fluidRow(
                   column(6, plotOutput("dep_hist", height = "300px")),
                   column(6, plotOutput("arr_hist", height = "300px"))
                 ),
                 h4("Arrival Delay vs. Weather Variables"),
                 fluidRow(
                   column(3, plotlyOutput("vs_precip", height = "250px")),
                   column(3, plotlyOutput("vs_visib", height = "250px")),
                   column(3, plotlyOutput("vs_wind", height = "250px")),
                   column(3, plotlyOutput("vs_temp", height = "250px"))
                 )
               )
             )
    ),
    
    tabPanel("When's the best time to fly?",
             sidebarLayout(
               sidebarPanel(
                 selectInput("best_month", "Select Month", choices = c("All", levels(flights_weather$month_name))),
                 selectInput("best_day", "Select Day", choices = c("All", 1:31)),
                 selectInput("best_airline", "Select Airline", choices = c("All", sort(unique(flights_weather$carrier)))),
                 selectInput("best_origin", "Select Origin Airport", choices = c("All", sort(unique(flights_weather$origin)))),
                 radioButtons("delay_metric", "Delay Metric", choices = c("Average", "Median"), selected = "Average")
               ),
               mainPanel(
                 plotlyOutput("best_time_plot", height = "500px")
               )
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  filtered_data <- reactive({
    df <- flights_weather %>% filter(month_name == input$month)
    if (input$day != "All") df <- df %>% filter(day_of_month == as.integer(input$day))
    if (input$airline != "All") df <- df %>% filter(carrier == input$airline)
    if (input$weather != "All") df <- df %>% filter(weather_type == input$weather)
    df
  })
  
  # Summary Tiles
  output$tile_total <- renderUI({ valueBox("Total Flights", nrow(filtered_data()), "#2980b9") })
  
  output$tile_cancelled <- renderUI({
    val <- sum(is.na(filtered_data()$dep_time))
    pct <- round(100 * val / nrow(filtered_data()), 1)
    color <- ifelse(pct > 2, "#e74c3c", ifelse(pct > 1.4, "#f39c12", "#27ae60"))
    valueBox("Cancelled Flights", paste0(val, " (", pct, "%)"), color)
  })
  
  output$tile_early <- renderUI({
    val <- sum(filtered_data()$dep_delay < 0, na.rm = TRUE)
    valueBox("Early Departures", val, "#27ae60")
  })
  
  output$tile_avg_dep <- renderUI({
    val <- round(mean(filtered_data()$dep_delay, na.rm = TRUE), 1)
    color <- if (val <= 5) "#27ae60" else if (val <= 20) "#f1c40f" else "#e74c3c"
    valueBox("Avg Departure Delay", paste0(val, " min"), color)
  })
  
  output$tile_avg_arr <- renderUI({
    val <- round(mean(filtered_data()$arr_delay, na.rm = TRUE), 1)
    color <- if (val <= 5) "#27ae60" else if (val <= 20) "#f1c40f" else "#e74c3c"
    valueBox("Avg Arrival Delay", paste0(val, " min"), color)
  })
  
  output$dep_hist <- renderPlot({
    ggplot(filtered_data(), aes(x = dep_delay)) +
      geom_histogram(bins = 40, fill = "#f39c12", color = "white") +
      labs(title = "Departure Delay Distribution", x = "Departure Delay (min)", y = "Flights") +
      xlim(0, 200)
  })
  
  output$arr_hist <- renderPlot({
    ggplot(filtered_data(), aes(x = arr_delay)) +
      geom_histogram(bins = 40, fill = "#3498db", color = "white") +
      labs(title = "Arrival Delay Distribution", x = "Arrival Delay (min)", y = "Flights") +
      xlim(0, 200)
  })
  
  make_scatter <- function(var_name) {
    df <- filtered_data()
    p <- ggplot(df, aes_string(x = var_name, y = "arr_delay", text = "paste('Dest: ', dest, '<br>Temp: ', temp, '<br>Precip: ', precip, '<br>Wind: ', wind_speed, '<br>Vis: ', visib)")) +
      geom_point(color = "#e74c3c", alpha = 0.6) +
      geom_smooth(method = "loess", color = "black") +
      labs(title = paste("Arrival Delay vs.", var_name), x = var_name, y = "Arrival Delay (min)")
    ggplotly(p, tooltip = "text")
  }
  
  output$vs_precip <- renderPlotly({ make_scatter("precip") })
  output$vs_visib  <- renderPlotly({ make_scatter("visib") })
  output$vs_wind   <- renderPlotly({ make_scatter("wind_speed") })
  output$vs_temp   <- renderPlotly({ make_scatter("temp") })
  
  # Best Time to Fly Plot
  output$best_time_plot <- renderPlotly({
    df <- flights_weather
    if (input$best_month != "All") df <- df %>% filter(month_name == input$best_month)
    if (input$best_day != "All") df <- df %>% filter(day_of_month == as.integer(input$best_day))
    if (input$best_airline != "All") df <- df %>% filter(carrier == input$best_airline)
    if (input$best_origin != "All") df <- df %>% filter(origin == input$best_origin)
    
    df <- df %>%
      mutate(time_of_day = case_when(
        hour >= 6 & hour < 12 ~ "Morning",
        hour >= 12 & hour < 18 ~ "Afternoon",
        hour >= 18 ~ "Evening",
        TRUE ~ "Night"
      ))
    
    metric_fun <- if (input$delay_metric == "Average") mean else median
    
    summary_df <- df %>%
      group_by(time_of_day) %>%
      summarise(
        avg_dep = metric_fun(dep_delay, na.rm = TRUE),
        avg_arr = metric_fun(arr_delay, na.rm = TRUE),
        flight_count = n(),
        .groups = "drop"
      ) %>%
      mutate(
        time_of_day = factor(time_of_day, levels = c("Morning", "Afternoon", "Evening", "Night")),
        tooltip = paste0(
          "Time: ", time_of_day, "<br>",
          "Dep Delay: ", round(avg_dep, 1), "<br>",
          "Arr Delay: ", round(avg_arr, 1), "<br>",
          "Flights: ", flight_count
        )
      ) %>%
      pivot_longer(cols = c(avg_dep, avg_arr), names_to = "type", values_to = "delay")
    
    plot_ly(
      data = summary_df,
      x = ~time_of_day,
      y = ~delay,
      color = ~type,
      text = ~tooltip,
      type = "bar",
      hoverinfo = "text",
      barmode = "group",
      colors = c("avg_dep" = "#f39c12", "avg_arr" = "#3498db")
    ) %>%
      layout(
        title = paste(input$delay_metric, "Delays by Time of Day"),
        xaxis = list(title = "Time of Day"),
        yaxis = list(title = paste(input$delay_metric, " Delay (min)")),
        legend = list(title = list(text = "Delay Type"))
      )
  })
}

# Run App
shinyApp(ui = ui, server = server)
