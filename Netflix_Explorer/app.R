
library(shiny)
library(shinydashboard)
library(bslib)
library(tidyverse)

# UI 
library(shinydashboard)

ui <- dashboardPage(
  skin = "black", 
  dashboardHeader(
    title = span("Netflix App",
                 style = "color:#E50914; font-family:'Bebas Neue', sans-serif")
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("home")),
      menuItem("Genre Analysis", tabName = "genre", icon = icon("film")),
      menuItem("Country Analysis", tabName = "country", icon = icon("globe"))
    )
  ),
  dashboardBody(
    tabItems(
      
      # Overview page
      tabItem(tabName = "overview",
              h2("Overview Section"),
              fluidRow(
                box(
                  title = "Top Genres", 
                  width = 8,
                  plotOutput("genrePlot")
                ),
                box(
                  title = "Inputs",
                  width = 4,
                  selectInput("gg", "Category 1", choices = c("1", "2")),
                  selectInput("gs", "Category 2", choices = c("1", "2"))
                )
              ),
              fluidRow(
                box(
                  title = "Plot 2",
                  width = 12,
                  plotOutput("plot2")
                )
              )
      ),
      
      # Genre page
      tabItem(tabName = "genre",
              h2("Genre Analysis"),
              plotOutput("genrePlot")
      ),
      
      # Country page
      tabItem(tabName = "country",
              h2("Country Analysis"),
              plotOutput("countryPlot")
      )
    )
  )
)


server <- function(input, output, session) {
  # Your plotting logic
  output$genrePlot <- renderPlot({ plot(1:10, 1:10, col = "red") })
  output$plot1 <- renderPlot({ hist(rnorm(100)) })
  output$plot2 <- renderPlot({ plot(cars) })
  output$countryPlot <- renderPlot({ barplot(table(mtcars$cyl)) })
}

# Run the application 
shinyApp(ui = ui, server = server)
