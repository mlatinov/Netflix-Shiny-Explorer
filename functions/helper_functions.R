

#### Function for data cleaning ####

## Load and Clean TS ##
ts_clean <- function(file){
  
  ## Load the data
  data <- read.csv(file)
  
  # Clean the data 
  data_clean <- data %>%
    # Change the colanmes to lower case
    rename_with(.fn = tolower) %>%
    # Change the date type to date
    mutate(
      date = ymd(date)
    )
  
  # Retrun 
  return(data_clean)
}


clean_data <- function(data){
  
  # Clean the data 
  data <- data %>%
    
    # Add Nas whenever emthy
    mutate(across(where(is.character), ~ na_if(., ""))) %>%
    
    # Convert the date in ISO ymd date format 
    mutate(date_added = str_trim(date_added)) %>%
    mutate(
      date_added = case_when(
        str_detect(date_added, "^[a-zA-Z]") ~mdy(date_added),
        str_detect(date_added, "^[0-9]") ~dmy(date_added)) 
      ) 
# Return 
  return(data)
}

# Netflix Ploly Theme 
netflix_plotly_theme <- function(p) {
  layout(
    p,
    plot_bgcolor = "#FFFFFF",  # White background for the plot
    paper_bgcolor = "#FFFFFF", # White background for the full chart area
    font = list(
      color = "#E50914",       # Netflix red for all text
      family = "Bebas Neue",   # Same Netflix font
      size = 14
    ),
    xaxis = list(
      title = "",
      color = "#E50914",
      gridcolor = "#DDDDDD",   # Light grey grid lines
      zerolinecolor = "#DDDDDD",
      tickfont = list(color = "#222222")
    ),
    yaxis = list(
      title = "",
      color = "#E50914",
      gridcolor = "#DDDDDD",
      zerolinecolor = "#DDDDDD",
      tickfont = list(color = "#222222")
    ),
    legend = list(
      bgcolor = "#FFFFFF",
      bordercolor = "#E50914",
      font = list(color = "#222222")
    )
  )
}
##### Visualization Functions ####

# #### Heatmap ####
# v <- netflix_films_clean %>%
#         separate_rows(genres, sep = ", ") %>%
#         count(genres,sort = TRUE)
# 





