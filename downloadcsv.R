library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)

# Read CSV file into a data frame named 'data'
data <- read.csv("air_quality_data.csv", stringsAsFactors = FALSE)

# Make sure the date is in the correct format
data$date.local <- ymd_hms(data$date.local)

# UI
ui <- fluidPage(
  titlePanel("Air Quality"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Choose a country:", choices = unique(data$country)),
      sliderInput("year_range", "Choose a year range:",
                  min = min(year(data$date.local), na.rm = TRUE),
                  max = max(year(data$date.local), na.rm = TRUE),
                  value = c(min(year(data$date.local), na.rm = TRUE), max(year(data$date.local), na.rm = TRUE))
      )
    ),
    mainPanel(
      plotOutput("linePlot")
    )
  )
)

# Server
server <- function(input, output, session) {
  output$linePlot <- renderPlot({
    filtered_data <- data %>%
      filter(country == input$country,
             year(date.local) >= input$year_range[1],
             year(date.local) <= input$year_range[2])
    
    ggplot(filtered_data, aes(x = date.local, y = value)) +
      geom_line() +
      ggtitle(paste("Air Quality in", input$country)) +
      xlab("Date") +
      ylab("Value (µg/m³)")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
