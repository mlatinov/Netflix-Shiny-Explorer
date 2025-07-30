
#### Clean the data ####

clean_churn <- function(data){
  
  data %>%
    # Rename the columns 
    rename(
      sub_lenght_months = `Subscription Length (Months)`,
      customer_satisfaction = `Customer Satisfaction Score (1-10)`,
      daily_watch_time_h = `Daily Watch Time (Hours)`,
      engagement_rate  = `Engagement Rate (1-10)`,
      device = `Device Used Most Often`,
      genre_prefence = `Genre Preference`,
      region = Region,
      payment_history = `Payment History (On-Time/Delayed)`,
      sub_plan = `Subscription Plan`,
      churn = `Churn Status (Yes/No)`,
      support_queries = `Support Queries Logged`,
      age = Age,
      montly_income = `Monthly Income ($)`,
      promo_offers_used = `Promotional Offers Used`,
      num_profiles_created = `Number of Profiles Created`,
      customer_id = `Customer ID`
      ) %>%
    # Change the types
    mutate(across(where(is.character), as.factor))%>%
    # Encome Churn Status 0 = NO 1 = YES
    mutate(
      churn = as.factor(ifelse(churn == "No",yes = 0,no = 1))
    )
}

#### EDA Function #### 
eda_churn <- function(data, plot_type, x, y,z,stat_test){
  
  #### Plots ####
  
  # Netflix modern white-based theme
  theme_netflix_modern <- function(base_size = 14, base_family = "Helvetica") {
    theme_minimal(base_size = base_size, base_family = base_family) %+replace%
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "#e6e6e6"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "#111111",),
        axis.title = element_text(color = "#111111", face = "bold"),
        plot.title = element_text(color = "#E50914", face = "bold", size = 15, hjust = 0),
        plot.subtitle = element_text(color = "#444444", size = 14),
        plot.caption = element_text(color = "#888888", size = 10),
        legend.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA),
        legend.text = element_text(color = "#111111"),
        legend.title = element_text(color = "#111111", face = "bold")
      )
  }
  # Column Plot
  if (plot_type == "col_plot") {
    plot <- data %>%
      mutate(x_reordered = fct_reorder(.f = .data[[x]], .x = .data[[y]], .fun = mean)) %>%
      ggplot(aes(x = x_reordered, y = .data[[y]], fill = .data[[x]])) +
      geom_col() +
      scale_fill_viridis_d(option = "F") +
      theme_netflix_modern()+
      labs(
        title = paste0(str_to_title(y), " by ", x, " category"),
        x = str_to_title(x),
        y = str_to_title(y),
        fill = str_to_title(x)
      )
    # Heatmap
  }else if(plot_type == "heatmap"){
    plot <- data %>%
      mutate(x_reordered = fct_reorder(.f = .data[[x]], .x = .data[[y]], .fun = mean)) %>%
      ggplot(aes(x = x_reordered,y = .data[[y]],fill = .data[[z]]))+
      geom_tile()+
      scale_fill_viridis_c(option = "F")+
      theme_netflix_modern()+
      labs(
        title = paste0(str_to_title(y), " and ", x , " by ", z),
        x = str_to_title(x),
        y = str_to_title(y),
        fill = str_to_title(z)
        )
    # Density Distribution by Category
  }else if(plot_type == "density"){
    plot <- data %>%
      ggplot(aes(x = .data[[x]], y = .data[[y]],fill = .data[[y]])) +
      geom_density_ridges()+
      scale_fill_viridis_d(option = "F")+
      theme_netflix_modern()+
      labs(
        title = paste0(str_to_title(y), " by ", x),
        x = str_to_title(x),
        y = str_to_title(y),
        fill = str_to_title(y)
      )
    # Boxplot 
  }else if(plot_type == "boxplot"){
    plot <- data %>%
      ggplot(aes(x = .data[[x]]))+
      geom_boxplot(colour = "#E50914")+
      theme_netflix_modern()+
      labs(
        title = paste0(str_to_title(x), " boxplot "),
        x = str_to_title(x)
      )
    # Barplot 
  }else if(plot_type == "barplot"){
    plot <- data %>%
      count(.data[[x]]) %>%
      mutate(percent = n / sum(n) * 100) %>%
      ggplot(aes(x = fct_reorder(.data[[x]], percent), y = percent)) +
      geom_col(fill = "firebrick") +  
      coord_flip() +                   
      labs(
        x = str_to_title(x),
        y = "Percentage (%)",
        title = paste0(str_to_title(x), " Distribution in %")
        ) +
      theme_netflix_modern()
    # Geo-Map by Numerical Value
  }else if(plot_type == "geo"){
    # Calculate Scores 
    region_scores <- data %>%
      group_by(region) %>%
      summarise(x_mean = mean(.data[[x]], na.rm = TRUE))
    
    # Get world map
    world <- ne_countries(scale = "medium", returnclass = "sf")
    
    # Join 
    world_joined <- world %>%
      left_join(region_scores, by = c("region_un" = "region"))
    
    # Plot the map 
    plot <- 
      ggplot(world_joined) +
      geom_sf(aes(fill = x_mean), color = "white", size = 0.2) +
      scale_fill_viridis_c(
        option = "F",
        name = paste0("Mean",str_to_title(x)),
        na.value = "gray90"
      ) +
      labs(
        title = "Continent Scores",
        subtitle = paste0("Mean " ,str_to_title(x) , " by Region"),
        fill = "Hours"
      ) +
      theme_minimal()
  }
  return(plot)
}


#### Modeling Function ####

## Recipe function ##
recipe_churn_make <- function(churn_splits){
  
  # Recipe 
  recipe <- recipe(churn ~ .,data = training(churn_splits)) %>%
    
    # Step id 
    update_role(customer_id,new_role = "id") %>%
    
    # Remove Near Zero Variance Features
    step_nzv(all_predictors()) %>%
    
    # Apply Transformation
    step_YeoJohnson(all_numeric_predictors()) %>%
    
    # Scale Numerical Features
    step_scale(all_numeric_predictors())%>%
    
    # Encode Nominal Features
    step_dummy(all_nominal_predictors())
  
  # Return 
  return(recipe)
    
}

#### Random Forest Function ####
random_forest_churn <- function(churn_split,recipe_churn){
  
  # Model Specification 
  rf_model <- rand_forest(
    mode = "classification",
    mtry = tune(),
    trees = tune(),
    min_n = tune()
    ) %>%
    set_engine("ranger")
  
  # Workflow 
  rf_workflow <- workflow()%>%
    add_model(rf_model)%>%
    add_recipe(recipe_churn)
  
  # Resampling
  resamples_churn <- vfold_cv(data = training(churn_split),v = 10)
  
  # Metric 
  metric <- metric_set(roc_auc)
  
  # Tune Grid LHC
  tune_grid <- parameters(
    mtry(range = c(2,16)),
    trees(range = c(200,600)),
    min_n(range = c(2,60))
    ) %>%
    grid_space_filling(size = 30)
  
  # Initial tuning
  initial_tuned <- tune_grid(
    rf_workflow,
    resamples = resamples,
    metrics = metric,
    grid = tune_grid,
    control = control_grid(save_pred = TRUE,save_workflow = TRUE)
  )
  ## MBO ##
  control_bo <- control_bayes(
    seed = 123,
    save_pred = TRUE,
    save_workflow = TRUE,
    no_improve = 20
    )
  
  ## Apply Bayes Optimization ##
  mbo <- tune_bayes(
    rf_workflow,
    resamples = resamples_churn,
    iter = 50,
    metrics = metric,
    initial = initial_tuned,
    param_info = tune_grid,
    control = control_bo
  )
  
  # Select the best 
  best_fit <- select_best(mbo)
  
  # Finalize the workflow 
  random_forest_final_wf <- finalize_workflow(best_fit)
  
  # Fit the model 
  random_forest_fit <- fit(random_forest_final_wf,training(churn_split))
  
  # Return 
  return(random_forest_fit)

}






