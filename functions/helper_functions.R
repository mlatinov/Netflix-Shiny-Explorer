

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

##### Visualization Functions ####

# #### Heatmap ####
# v <- netflix_films_clean %>%
#         separate_rows(genres, sep = ", ") %>%
#         count(genres,sort = TRUE)
# 





