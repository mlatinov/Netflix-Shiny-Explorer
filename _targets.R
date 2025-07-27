
#### Libraries ####

## Organization ##
library(targets)

## Date wrangling ##
library(timetk)
library(tidyverse)

## ML ##
library(tidymodels)
library(finetune)
library(modeltime)
library(modeltime.resample)
library(modeltime.ensemble)

## Viz ##
library(plotly)

#### Source Functions ####
source("functions/helper_functions.R")
source("functions/ts_functions.R")

#### Pipeline ####
list(
  
  #### Load the datasets ####
  
  ## Netflix Films ##
  tar_target(
    name = netflix_films_data,
    command = read.csv("data/NetFlix.csv")
    ),
  ## Netflix Subs prices ##
  tar_target(
    name = netflix_prices,
    command = read.csv("data/netflix price in different countries.csv")
    ),
  ## Netflix Stock Prices ##
  tar_target(
    name = netflix_stock_price,
    command = ts_clean(file = "data/NFLX.csv")
    ),
  ## Netflix IMB Movies scores ##
  tar_target(
    name = netflix_imdb_scores,
    command = read.csv("data/Netflix TV Shows and Movies.csv")
  ),
  #### Clean the data ####
  tar_target(
    name = netflix_films_clean,
    command = clean_data(data = netflix_films_data)
  ),
  
  #### Time Series ####
  
  ### Make training and Testing set ### 
  tar_target(
    name = ts_split,
    command = time_series_split(data = netflix_stock_price,assess = "1 year",cumulative = TRUE)
    ),
  ## Extend data 1 Year into the future ##
  tar_target(
    name = ts_extent,
    command = future_frame(.data = netflix_stock_price,.date_var = date,.length_out = "1 year")
  ),
  ## Recipe ##
  tar_target(
    name = ts_recipe,
    command = prepare_ts(data = ts_split)
    ),
  
  ### TS Models ###
  
  ## ARIMA Model ##
  tar_target(
    name = arima_boost_model,
    command = arima_model_function(ts_split = ts_split ,ts_recipe = ts_recipe)
  ),
  ## PROPHET model ##
  tar_target(
    name = prophet_boost_model,
    command = prophet_model_function
  ),
  ## Deep Ar ##
  ## GP Forecaster ##

  ## Modeltime Table ##
  tar_target(
    name = modeltime_table,
    command = modeltime_table(
      arima_boost_model,
      prophet_boost_model
    )
  ),
  ## Ensemble ##
  tar_target(
    name = ts_ensemble,
    command =  ensemble_average(
      object = modeltime_table,
      type = "mean"
      )
    ),
## Calibration ##
tar_target(
  name = calibration_table,
  command = modeltime_calibrate(
    object = modeltime_table,
    new_data = testing(ts_split)
    )
  ),
## Ensemble Calibration ##
tar_target(
  name = calibration_table_ensemble,
  command = modeltime_calibrate(
    object = ts_ensemble,
    new_data = testing(ts_split)
  )
),
  ## Forecast vs Test Set ##
  tar_target(
    name = forecast_test_set_check,
    command = modeltime_forecast(
      object = calibration_table,
      new_data = testing(ts_split),
      actual_data = netflix_stock_price,
      conf_interval = 0.95
      )
    ),
## Forecast vs Test Set Ensemble  ##
tar_target(
  name = forecast_test_set_check_ensemble,
  command = modeltime_forecast(
    object = calibration_table_ensemble,
    new_data = testing(ts_split),
    actual_data = netflix_stock_price,
    conf_interval = 0.95
    )
),
  ## Refit to Full Dataset & Forecast Forward ##
  tar_target(
    name = refit,
    command = modeltime_refit(
      object = calibration_table,
      data = netflix_stock_price
      )
    ),
## Refit Ensemble ##
tar_target(
  name = refit_ensemble,
  command = modeltime_refit(
    object = calibration_table_ensemble,
    data = netflix_stock_price
  )
),
## Forecast Forward ##
tar_target(
  name = forecast_forward,
  command = modeltime_forecast(
    object = refit,
    new_data = ts_extent,
    actual_data = netflix_stock_price,
    conf_interval = 0.95
    )
  ),
## Forecast Forward Ensemble ##
tar_target(
  name = forecast_forward_ensemble,
  command = modeltime_forecast(
    object = refit_ensemble,
    new_data = ts_extent,
    actual_data = netflix_stock_price,
    conf_interval = 0.95
    )
  ),
)



