#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(httr)
library(jsonlite)
library(plotly)
library(htmltools)
library(tidyverse)
library(lubridate)

ckanSQL <- function(url) {
  # Make the Request
  r <- RETRY("GET", URLencode(url))
  # Extract Content
  c <- content(r, "text")
  # Basic gsub to make NA's consistent with R
  json <- gsub('NaN', 'NA', c, perl = TRUE)
  # Create Dataframe
  data.frame(jsonlite::fromJSON(json)$result$records)
}



# df <- read_csv("66cdcd57-6c92-4aaa-8800-0ed9d8f03e22.csv") %>%
#   mutate(Month = month(mdy(Date)), Day = day(mdy(Date)), Year = year(mdy(Date)))
# 
# group_by(df, Year, Month, Day) %>%
#   count()
# group_by(df, Year, Month) %>%
#   summarise(avg_age = mean(get('Current Age')))


# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Counts of 311 Jail Census"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dates",
                     "Select Dates",
                     start = Sys.Date()-30,
                     end = Sys.Date(),)),
    mainPanel(
      plotlyOutput("count")
    )))


# Define server logic required to draw a histogram
server <- function(input, output) {
  loadjail <- reactive({
    # Build API Query with proper encodes
    
    input$dates[1] <- as.Date(input$dates[1], origin = "1970-01-01")
    input$dates[2] <- as.Date(input$dates[2], origin = "1970-01-01")
    url <- paste0("SELECT%20*%20FROM%20%2266cdcd57-6c92-4aaa-8800-0ed9d8f03e22%22%20WHERE%20%20%27Date%27%20%3E%3D%20%27", input$dates[1], "%27%20AND%20%27Date%27%20%3C%3D%20%27", input$dates[2], "%27")
    
    # Load and clean data
    jail <- ckanSQL(url) %>%
      na.omit() %>%
      mutate(date = as.Date(Date))
      return(jail)
  })
  
  #plot # inmates counted in census
  output$count <- renderPlotly({
    # data for chart
    table <- loadjail %>%
      group_by(Date) %>%
      summarise(count = n())

    ggplot(table, aes(x = date, y = count)) + geom_bar()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
    