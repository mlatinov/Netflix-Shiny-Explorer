library(shiny)
library(shinydashboard)
library(timetk)
library(plotly)
library(DT)
library(bslib)
library(tidyverse)

## Source Functions and Data
setwd("C:/Users/Huawei/OneDrive/Competitions/Netflix-Shiny-Explorer/")
source("functions/ts_functions.R")
source("functions/helper_functions.R")
stock_data <- tar_read(netflix_stock_price)

# UI 
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
      menuItem("Country Analysis", tabName = "country", icon = icon("globe")),
      
      #### Time Series Menu with Subtabs
      menuItem("Time Series Analysis", icon = icon("calendar"),
               menuSubItem("Exploratory Analysis", tabName = "eda_ts"),
               menuSubItem("Model Forecast", tabName = "forecast_ts"),
               menuSubItem("More Information",tabName = "information_ts")
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      #### Overview
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
      
      #### Genre
      tabItem(tabName = "genre",
              h2("Genre Analysis"),
              plotOutput("genrePlot")
      ),
      
      #### Country
      tabItem(tabName = "country",
              h2("Country Analysis"),
              plotOutput("countryPlot")
      ),
      
      #### Time Series EDA Tab ####
      tabItem(tabName = "eda_ts",
              h2("Exploratory Time Series Analysis"),
              ## Summary Statistics Table ##
              fluidRow(
                box(
                  title = "Summary Statistics",
                  width = 12,
                  DTOutput(outputId = "ts_eda_summary_stat")
                )
              ),
              ## Plot the original TS data 
              fluidRow(
                box(
                  title = "Netflix Stock Data",
                  width = 12,
                  plotlyOutput(outputId = "netflix_stock_viz", height = "400px")
                )
              ),
              ## Plot STL Diagnostics
              fluidRow(
                box(
                  title = "STL Diagnostics",
                  width = 12,
                  plotlyOutput(outputId = "stl_stock_viz", height = "400px")
                )
              ),
              ## Plot Autocorrelation and Partial Autocorrelation
              fluidRow(
                box(
                  title = "Autocorrelation",
                  width = 6,
                  plotlyOutput(outputId = "autocor_ts")
                ),
                # Plot Anomaly Detection
                box(
                  title = "Anomaly Detection",
                  width = 6,
                  plotlyOutput(outputId = "annomaly_detect_plot")
                )
              )
      ),
      #### Time Series Information Tab ####
      tabItem(tabName = "information_ts",
              h2("Understanding Time Series Plots"),
              fluidRow(
                h3("Understanding STL"),
                box(
                  title = "What is STL",
                  width = 12,
                  solidHeader = TRUE,
                  status = "info",
                    HTML(
                      "<p><strong>STL (Seasonal-Trend decomposition using Loess)</strong> helps us understand how different components contribute to a time series. The decomposition separates the data into <em>Trend</em>, <em>Seasonality</em>, and <em>Remainder</em>. Here's how to interpret each part:</p>
  <ul>
    <li><strong>Observed:</strong> This is the original time series. Use it as a reference to understand what the full data looks like before decomposition.</li>
    
    <li><strong>Trend:</strong> This smooth line shows the long-term direction in the data. Look for consistent upward or downward patterns. For example, a rising trend might indicate growing user engagement or stock prices.</li>
    
    <li><strong>Seasonal:</strong> Repeating, cyclical patterns that occur over a fixed time (e.g., weekly, monthly). These may highlight customer behavior, daily or weekly activity cycles. If this component is strong, seasonality plays an important role in the time series.</li>
    
    <li><strong>Remainder (Noise):</strong> What’s left after removing trend and seasonality. This component includes randomness, outliers, or one-time events. 
    
    <br><strong>Tip:</strong> Large spikes or dips here may suggest anomalies — such as sudden news, outages, or unusual spikes in usage.</li>
  </ul>
  <p><strong>How to Use It:</strong> If the <em>seasonal</em> component is small or flat, your data may not have strong periodic patterns. If the <em>trend</em> changes directions often, your series may be unstable. If the <em>remainder</em> has large variation, consider anomaly detection or using robust models.</p>"
                      )
                  )
                )
      ),
      #### Forecast Tab ####
      tabItem(tabName = "forecast_ts",
              h2("Forecast Models"),
              p("Getting There "),
              ## Model ACC Table ##
              fluidRow(
                box(
                  title = "Model Accuracy Table",
                  dataTableOutput(outputId = "ts_model_acc_table"),
                  width = 12 
                )
              ),
              ## User Selects the Future Forecast ##
              fluidRow(
                box(
                  title = "Pick Forecast Range",
                  selectInput(
                    inputId = "forecast_future",
                    label = "Forecast",
                    choices = list(
                      "1 Month Forecast" ="1 month",
                      "3 Months Forecast" = "3 months",
                      "6 Months Forecast" ="6 months",
                      "1 Year Forecast" = "1 year",
                      "3 years" = "3 Years Forecast"
                      ),
                    # Default = 1 month
                    selected = "1 month"
                    ),
                  # User selects the Models or Ensemble for forecasting
                  selectInput(
                    inputId = "ts_models_ensemble",
                    label = "Method",
                    choices = list(
                      "Individual Models" = "models",
                      "Ensemble" = "ensemble"
                      )
                    ),
                  width = 3
                  )
                )
              )
      )
  )
)

# Server
server <- function(input, output, session) {
  
  #### Time Series Section ####
  
  ## TS EDA Section ##
  
  # Plot datatable with TS summary Statistics
  output$ts_eda_summary_stat <- renderDataTable({
    tk_summary_diagnostics(stock_data, .date_var = date)
  })
  # Plot the General Time Series data with Time tk for the TS Explanatory tab  ####
  output$netflix_stock_viz <- renderPlotly({
    plot_time_series(.data = stock_data, .date_var = date, .value = close)
  })
  # Plot STL Diagnostics for the TS Explanatory tab
  output$stl_stock_viz <- renderPlotly({
   diag_p <- plot_stl_diagnostics(
     .data        = stock_data,
     .date_var    = date,
     .value       = close,
     .feature_set = c("trend", "remainder"),
     .frequency   = "auto",
     .trend       = "auto",
     .interactive = TRUE  
    )
   netflix_plotly_theme(diag_p)
  })
  ## Plot Autocorrelation and Partial Autocorrelation
  output$autocor_ts <- renderPlotly({
    autocor_p <- plot_acf_diagnostics(stock_data, .date_var = date, .value = close, .lags = 30)
    netflix_plotly_theme(autocor_p)
  })
  ## Plot Anomaly Detection
  output$annomaly_detect_plot <- renderPlotly({
    anomaly_p <- plot_anomaly_diagnostics(.data = stock_data,.date_var = date,.value = close)
    netflix_plotly_theme(anomaly_p)
  })
  
  ## TS Forecast Section ##
  
  ## Modeltime Accuracy Table ## ts_model_acc_table
  
  # User selects the future period for forecasting
  stock_data_future <- reactive({
    # Check if the stock_data and the User input are available
    req(stock_data,input$forecast_future,input$ts_models_ensemble)
    # Extent the stock data
    future_frame(
      .data = stock_data,
      .date_var = date,
      .length_out = input$forecast_future # <-- User input 
      )
  })
  # User selects the Models or Ensemble for forecasting
  
  ## Forecast the future data ##
  
}

# Run the app
shinyApp(ui = ui, server = server)
