
#### Modeling Function ####

#### Prepare Data ####
prepare_ts <- function(data){
  
  ## Create a recipe for preprocesing ##
  recipe <- recipe(close ~ .,data = training(data)) %>%
    
    # Convert date to time-based features
    step_timeseries_signature(date) %>%
    
    # Add lag Features 
    step_lag(close, lag = 1:12) %>%
    
    # Add MA
    step_slidify(
      close,
      .f = ~mean(.x, na.rm = TRUE),
      period = 20,
      align = "right",
      partial = TRUE
    )
  
  # Return the recipe 
  return(recipe)
}

#### Model Function ####

## Arima Model ##
arima_model_function <- function(ts_split,ts_recipe){
  
  ## Arima Spec ##
  arima_spec <- arima_boost(
    mode = "regression",
    mtry = tune(),
    min_n = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    trees = tune()
    ) %>%
    set_engine("arima_xgboost")
  
  # Arima Workflow 
  arima_wf <- workflow() %>%
    add_model(arima_spec) %>%
    add_recipe(recipe = ts_recipe)
    
  # Resamples
  resamples_ts <- time_series_cv(
    data = training(ts_split),
    date_var = date,
    initial = 365 * 3,   
    assess = 365 * 1,    
    slice_limit = 10     
  )
  
  # Tune Grid 
  tune_grid <- parameters(arima_spec) %>%
    update(
      min_n      = min_n(range = c(2, 40)),
      learn_rate = learn_rate(range = c(1e-10, 1e-1), trans = log10_trans()),
      mtry       = mtry(range = c(1, 40)),
      trees      = trees(range = c(500, 2500)),
      tree_depth = tree_depth(range = c(1, 20)),
    )
  
  # MBO Control 
  control <- control_bayes(
    seed = 123,
    save_pred = TRUE,
    no_improve = 20,
    save_workflow = TRUE
    )
  
  # Metric 
   metric <- metric_set(rmse)
   
  # MBO tune 
  tune_mbo <- tune_bayes(
    arima_wf,
    resamples = resamples_ts,
    metrics = metric,
    param_info = tune_grid,
    iter = 50,
    initial = 20,
    control = control
    )
  
  # Best Params
  best_params <- select_best(tune_mbo)
  
  # Finalize the workflow
  final_wf <- finalize_workflow(arima_wf, best_params)
  
  # Return the workflow 
  return(final_wf)
}

## Prophet Model ## 
prophet_model_function <- function(ts_split,ts_recipe){
  
  # Prophet Spec
  prophet_spec <- prophet_boost(
    mode = "regression",
    mtry = tune(),
    min_n = tune(),
    tree_depth = tune(),
    trees = tune(),
    learn_rate = tune(),
    changepoint_num = tune(),
    changepoint_range = tune()  
    ) %>%
    set_engine("prophet_xgboost")
  # Prophet Workflow 
  prophet_workflow <- workflow() %>%
    add_model(prophet_spec) %>%
    add_recipe(ts_recipe)
  # Resamples
  resamples_ts <- time_series_cv(
    data = training(ts_split),
    date_var = date,
    initial = 365 * 3,   
    assess = 365 * 1,    
    slice_limit = 10     
  )
  ## LHC Tune Grid
  # Define a param set 
  param_set <- parameters(
    mtry(range = c(5, 30)),
    min_n(range = c(10, 80)),
    tree_depth(range = c(1, 10)),
    trees(range = c(500, 1500)),
    learn_rate(range = c(1e-10, 1e-1), trans = log10_trans()),
    changepoint_num(range = c(5, 50)),
    changepoint_range(range = c(0.5, 0.95))
  )
  # Generate Latin Hypercube grid
  lch_tune_grid <- grid_space_filling(
    param_set,
    size = 35
  )
  # Metric 
  metric <- metric_set(rmse)
  # Initial tuning
  initial_results <- tune_grid(
    prophet_workflow,
    resamples = resamples_ts,
    grid = lch_tune_grid,
    metrics = metric,
    control = control_grid(save_pred = TRUE)
  )
  # MBO Control 
  control <- control_bayes(
    no_improve = 20,
    seed = 123,
    save_pred = TRUE,
    save_workflow = TRUE
    )
  # Tune MBO
  tune_mbo <- tune_bayes(
    prophet_workflow,
    resamples = resamples_ts,
    iter = 50,
    param_info = param_set,
    metrics = metric,
    initial = initial_results,
    control = control
  )
  # Select Best 
  best_tune <- select_best(tune_mbo)
  # Finalize the workflow 
  prophet_final_wf <- finalize_workflow(prophet_workflow,parameters = best_tune)
  # Return final workflow 
  return(prophet_final_wf)
}

