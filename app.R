library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(shinythemes)

# Read the data
data <- read.csv("videogame_sales.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Video Game Sales Analysis", windowTitle = "Video Game Sales Analysis"),
  theme = shinytheme("flatly"),
  navbarPage(
    "",
    
    tabPanel(
      "Sales Analysis", icon = icon("chart-bar"),
      fluidRow(
        column(6,
               div(valueBoxOutput("region_total"), style="color:white;"),
               div(valueBoxOutput("global_total"), style="color:white;"),
               div(valueBoxOutput("region_percentage"), style="color:white;"),style="background-color:#2C3E50;"
        ),
        br(),
        br(),
        column(12,
               sidebarPanel(
                 selectInput("sales_region", "Select Sales Region",
                             choices = c(
                               "North American Sales" = "NA_Sales",
                               "European Sales" = "EU_Sales",
                               "Japanese Sales" = "JP_Sales",
                               "Rest of World Sales" = "Other_Sales"
                             ),
                             selected = "NA_Sales"
                 )
               ),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               
               fluidRow(
                 column(6, plotlyOutput("pie_chart")),
                 column(6, plotlyOutput("bar_chart"))
               )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$region_total <- renderValueBox({
    selected_column <- input$sales_region
    sales_sum <- sum(data[[selected_column]], na.rm = TRUE)
    title_vb <- switch(input$sales_region,
                       "NA_Sales" = "North America",
                       "EU_Sales" = "Europe",
                       "JP_Sales" = "Japan",
                       "Other_Sales" = "Rest of World")
   
    valueBox(
      value = sales_sum,
      subtitle = paste("Total sales for", title_vb, "(Millions)")
    )
  })
  
  output$global_total <- renderValueBox({
    global_sales_sum <- sum(data$Global_Sales, na.rm = TRUE)
    valueBox(
      value = global_sales_sum,
      subtitle = "Total Global Sales (Millions)"
    )
  })
  
  output$region_percentage <- renderValueBox({
    selected_column <- input$sales_region
    sales_in_region <- sum(data[[selected_column]], na.rm = TRUE)
    global_sales <- sum(data$Global_Sales, na.rm = TRUE)
    percentage <- round(sales_in_region / global_sales * 100, 2)
    valueBox(
      value = scales::percent(percentage / 100, accuracy = 0.01),
      subtitle = paste("Accounts for ", scales::percent(percentage / 100, accuracy = 0.01), " of Global Sales"),
      color = #343a40
    )
  })
  
  output$pie_chart <- renderPlotly({
    genre_sales <- data %>%
      group_by(Genre) %>%
      summarize(Total_Sales = sum(get(input$sales_region), na.rm = TRUE))
    title_pie <- switch(input$sales_region,
                        "NA_Sales" = "North America",
                        "EU_Sales" = "Europe",
                        "JP_Sales" = "Japan",
                        "Other_Sales" = "Rest of World"
    )
    plot_ly(data = genre_sales, labels = ~Genre, values = ~Total_Sales, type = 'pie', textinfo = 'percent+label') %>%
      layout(
        title = paste("All-Time Sales by Genre in", title_pie),
        height = 500
      )
  })
  
  output$bar_chart <- renderPlotly({
    genre_sales <- data %>%
      group_by(Genre) %>%
      summarize(Sales = sum(get(input$sales_region), na.rm = TRUE))
    title_bar <- switch(input$sales_region,
                        "NA_Sales" = "North America",
                        "EU_Sales" = "Europe",
                        "JP_Sales" = "Japan",
                        "Other_Sales" = "Rest of World"
    )
    total_global_sales <- data %>%
      group_by(Genre) %>%
      summarize(Total_Global_Sales = sum(Global_Sales, na.rm = TRUE))
    comparison_data <- merge(genre_sales, total_global_sales, by = "Genre")
    plot_ly(data = comparison_data, x = ~Genre, y = ~Sales, type = 'bar', text = ~paste(Sales, "$"), name = title_bar) %>%
      add_trace(y = ~Total_Global_Sales, text = ~paste(Total_Global_Sales, "M"), name = 'Global Sales') %>%
      layout(
        title = paste(title_bar, "vs Global Sales by Genre"),
        height = 500
      )
  })
}

shinyApp(ui, server)
