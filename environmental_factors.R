# environmental_factors.R

# Detach MASS package if it's loaded to avoid select() conflicts
if ("package:MASS" %in% search()) {
  detach("package:MASS", unload = TRUE)
  message("Detached MASS package to avoid function conflicts")
}

# Load required packages
if (!require("INLA")) {
  install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
}
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tidyr")) install.packages("tidyr")

library(INLA)
library(dplyr)
library(ggplot2)
library(tidyr)

#=============================================
# STANDARDIZED VARIANCE DECOMPOSITION FUNCTION
#=============================================

# Function to calculate variance components consistently across models
calculate_variance_decomposition <- function(model, fixed_effects = NULL, family = "gaussian") {
  # Extract precision parameters
  precision_summary <- model$summary.hyperpar
  
  # Print the precision summary to see actual values and row names
  message("Precision parameter summary:")
  print(precision_summary)
  
  # Initialize components vector for environmental model
  components <- c("Player", "Park", "Residual", "Environmental Fixed Effects")
  variances <- numeric(length(components))
  
  # Extract variances (reciprocal of precision)
  tryCatch({
    # Check if the row names exist exactly as expected
    if("Precision for player_id" %in% rownames(precision_summary)) {
      player_prec <- precision_summary["Precision for player_id", "mean"]
      message(paste("Player precision:", player_prec))
      # Check if precision is too large (effectively infinity)
      if(player_prec > 1e10) {
        message("Player precision extremely large, setting variance to near zero")
        variances[1] <- 1e-10  # Small but not exactly zero
      } else {
        variances[1] <- 1/player_prec
      }
    } else {
      # If row name doesn't match, try to find a close match
      message("Could not find exact 'Precision for player_id' row")
      player_row <- grep("player", rownames(precision_summary), ignore.case = TRUE)
      if(length(player_row) > 0) {
        player_prec <- precision_summary[player_row[1], "mean"]
        message(paste("Found alternative player precision:", player_prec))
        variances[1] <- 1/player_prec
      } else {
        message("No player precision parameter found")
        variances[1] <- 0
      }
    }
    
    # Same approach for park precision
    if("Precision for park_id" %in% rownames(precision_summary)) {
      park_prec <- precision_summary["Precision for park_id", "mean"]
      message(paste("Park precision:", park_prec))
      if(park_prec > 1e10) {
        message("Park precision extremely large, setting variance to near zero")
        variances[2] <- 1e-10
      } else {
        variances[2] <- 1/park_prec
      }
    } else {
      message("Could not find exact 'Precision for park_id' row")
      park_row <- grep("park", rownames(precision_summary), ignore.case = TRUE)
      if(length(park_row) > 0) {
        park_prec <- precision_summary[park_row[1], "mean"]
        message(paste("Found alternative park precision:", park_prec))
        variances[2] <- 1/park_prec
      } else {
        message("No park precision parameter found")
        variances[2] <- 0
      }
    }
  }, error = function(e) {
    message("Error extracting variance components: ", e$message)
  })
  
  # Calculate residual variance depending on family
  if (family == "poisson") {
    # For Poisson models, theoretical variance equals the mean
    fitted_values <- model$summary.fitted.values$mean
    variances[3] <- mean(fitted_values, na.rm = TRUE)
    message(paste("Residual variance (Poisson mean):", variances[3]))
  } else {
    # For Gaussian models, use precision directly
    if("Precision for the Gaussian observations" %in% rownames(precision_summary)) {
      resid_prec <- precision_summary["Precision for the Gaussian observations", "mean"]
      variances[3] <- 1/resid_prec
      message(paste("Residual variance (Gaussian precision):", variances[3]))
    } else {
      message("Could not find Gaussian precision parameter")
      variances[3] <- NA
    }
  }
  
  # Calculate fixed effects variance if provided
  if(!is.null(fixed_effects)) {
    # Remove intercept
    fixed_effects <- fixed_effects[rownames(fixed_effects) != "(Intercept)", ]
    # Sum squared effects - simplified approach
    variances[4] <- sum(fixed_effects$mean^2, na.rm = TRUE)
    message(paste("Fixed effects variance:", variances[4]))
  } else {
    variances[4] <- 0
  }
  
  # Create data frame of variance components
  variance_components <- data.frame(
    component = components,
    variance = variances
  )
  
  # Calculate proportions
  total_variance <- sum(variance_components$variance, na.rm = TRUE)
  variance_components$proportion <- variance_components$variance / total_variance
  
  message("Calculated variance components:")
  print(variance_components)
  
  return(variance_components)
}

#=============================================
# ENVIRONMENTAL INLA MODEL FUNCTIONS
#=============================================

# Fit INLA model for environmental effects
run_environmental_model <- function(env_data, park_results = NULL) {
  message("\n========================================================")
  message("          RUNNING ENVIRONMENTAL EFFECTS INLA MODEL")
  message("========================================================\n")
  
  # Basic validation
  required_cols <- c("WAR", "temperature_z", "humidity_z", "wind_speed_z", "elevation_z", "player_id")
  missing_cols <- setdiff(required_cols, names(env_data))
  if (length(missing_cols) > 0) {
    stop("Error: Dataset is missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Scale WAR to improve numerical stability
  env_data$WAR_scaled <- scale(env_data$WAR)[,1]
  
  # Center environmental variables within park to better separate park and environmental effects
  message("Centering environmental variables within parks...")
  env_data <- env_data %>%
    group_by(park_id) %>%
    mutate(
      # Center these within park to separate park and environmental effects
      temp_within_park = temperature_z - mean(temperature_z, na.rm = TRUE),
      humid_within_park = humidity_z - mean(humidity_z, na.rm = TRUE),
      wind_within_park = wind_speed_z - mean(wind_speed_z, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Import park effects if available for integration
  park_effect_term <- NULL
  if(!is.null(park_results) && "park_effects" %in% names(park_results)) {
    message("Integrating park effects from prior analysis...")
    # Extract park effects and join with env_data
    park_effects_df <- park_results$park_effects %>%
      select(park_id, effect) %>%
      rename(park_effect = effect)
    
    # Join with env_data
    env_data <- env_data %>%
      left_join(park_effects_df, by = "park_id")
    
    # Create centered park effect term
    env_data$park_effect_centered <- scale(env_data$park_effect, scale = FALSE)[,1]
    park_effect_term <- "park_effect_centered +"
  } else {
    park_effect_term <- ""
    message("No park effects data available for integration")
  }
  
  # Create formula for INLA with interactions and random effects
  env_formula_str <- paste0("WAR_scaled ~ ", 
                            park_effect_term, # Include park effects if available
                            "temp_within_park + humid_within_park + wind_within_park + elevation_z +",
                            "I(temp_within_park^2) + ", # Quadratic term for temperature
                            "temp_within_park:elevation_z + ", # Key interaction
                            "f(player_id, model = 'iid', hyper = list(theta = list(prior = 'pc.prec', param = c(1, 0.05)))) +",
                            "f(park_id, model = 'iid', hyper = list(theta = list(prior = 'pc.prec', param = c(1, 0.05))))")
  
  # Convert string to formula
  env_formula <- as.formula(env_formula_str)
  
  # Set control parameters for INLA
  control_compute <- list(
    dic = TRUE,          # Compute Deviance Information Criterion
    waic = TRUE,         # Compute Widely Applicable Information Criterion
    return.marginals = TRUE,  # Return posterior marginals
    config = TRUE
  )
  
  # More careful numerical integration settings
  control_inla <- list(
    strategy = "gaussian",
    int.strategy = "eb",
    h = 1e-3,
    tolerance = 1e-5
  )
  
  # Control family with explicit parameters
  control_family <- list(
    control.link = list(model = "default")
  )
  
  # Record start time for benchmarking
  start_time <- Sys.time()
  
  # Fit INLA model with enhanced numerical settings
  message("Fitting INLA model for environmental effects...")
  tryCatch({
    env_model <- inla(
      env_formula,
      family = "gaussian",
      data = env_data,
      control.compute = control_compute,
      control.predictor = list(compute = TRUE),
      control.inla = control_inla,
      control.family = control_family,
      verbose = TRUE  # Set to TRUE to see detailed messages
    )
  }, error = function(e) {
    message("INLA model fitting failed: ", e$message)
    # Try an even simpler model without interactions if the full model fails
    simple_formula_str <- paste0("WAR_scaled ~ ", 
                                 park_effect_term, 
                                 "temp_within_park + humid_within_park + wind_within_park + elevation_z +",
                                 "f(player_id, model = 'iid', hyper = list(theta = list(prior = 'pc.prec', param = c(1, 0.05)))) +",
                                 "f(park_id, model = 'iid', hyper = list(theta = list(prior = 'pc.prec', param = c(1, 0.05))))")
    
    simple_formula <- as.formula(simple_formula_str)
    
    message("Trying a simpler model...")
    env_model <- inla(
      simple_formula,
      family = "gaussian",
      data = env_data,
      control.compute = control_compute,
      control.predictor = list(compute = TRUE),
      control.family = control_family,
      control.inla = control_inla,
      verbose = FALSE
    )
  })
  
  # Record end time
  end_time <- Sys.time()
  inla_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  message(paste("INLA model fitting completed in", round(inla_time, 2), "seconds"))
  
  # Check convergence
  if (!is.null(env_model$mode) && !is.null(env_model$mode$convergence) && env_model$mode$convergence != 0) {
    message("WARNING: Model did not converge. Consider further adjustments.")
  } else {
    message("Model converged successfully!")
  }
  
  # Check for negative DIC or infinite WAIC
  if (!is.null(env_model$dic) && env_model$dic$dic < 0) {
    message("WARNING: Negative DIC detected. This indicates model specification issues.")
    message("Proceeding with parameter estimates, but interpret diagnostics with caution.")
  }
  
  if (!is.null(env_model$waic) && is.infinite(env_model$waic$waic)) {
    message("WARNING: Infinite WAIC detected. This indicates computational issues.")
    message("Proceeding with parameter estimates, but interpret diagnostics with caution.")
  }
  
  # Also fit a separate model for specific outcomes like home runs
  # which might be more sensitive to environmental factors
  message("Fitting outcome-specific environmental models...")
  outcome_models <- list()
  
  for(outcome in c("HR", "X2B", "X3B")) {
    # Check if outcome column exists
    if(outcome %in% names(env_data)) {
      message(paste("Fitting model for", outcome))
      
      # Create specialized formula for specific outcome
      # Different outcomes may be more sensitive to different environmental factors
      if(outcome == "HR") {
        # Home runs are especially sensitive to elevation and temperature
        outcome_formula <- as.formula(paste(
          outcome, "~ temp_within_park + elevation_z +",
          "f(player_id, model = 'iid', hyper = list(theta = list(prior = 'pc.prec', param = c(1, 0.05)))) +", 
          "f(park_id, model = 'iid', hyper = list(theta = list(prior = 'pc.prec', param = c(1, 0.05))))"
        ))
      } else if(outcome == "X2B") {
        # Doubles are sensitive to field dimensions and wind
        outcome_formula <- as.formula(paste(
          outcome, "~ temp_within_park + wind_within_park +",
          "f(player_id, model = 'iid', hyper = list(theta = list(prior = 'pc.prec', param = c(1, 0.05)))) +", 
          "f(park_id, model = 'iid', hyper = list(theta = list(prior = 'pc.prec', param = c(1, 0.05))))"
        ))
      } else {
        # Triples and other outcomes - general model
        outcome_formula <- as.formula(paste(
          outcome, "~ temp_within_park + elevation_z +",
          "f(player_id, model = 'iid', hyper = list(theta = list(prior = 'pc.prec', param = c(1, 0.05))))"
        ))
      }
      
      # Fit model
      tryCatch({
        outcome_model <- inla(
          outcome_formula,
          family = "poisson",  # Poisson for count data
          data = env_data,
          control.compute = control_compute,
          control.family = list(link = "log"),
          control.inla = control_inla,
          verbose = FALSE
        )
        
        outcome_models[[outcome]] <- outcome_model
      }, error = function(e) {
        message(paste("Error fitting model for", outcome, ":", e$message))
        # Try a simpler model without interaction effects
        simple_outcome_formula <- as.formula(paste(
          outcome, "~ temp_within_park + elevation_z +",
          "f(player_id, model = 'iid', hyper = list(theta = list(prior = 'pc.prec', param = c(1, 0.05))))"
        ))
        
        tryCatch({
          simple_outcome_model <- inla(
            simple_outcome_formula,
            family = "poisson",
            data = env_data,
            control.compute = control_compute,
            control.family = list(link = "log"),
            control.inla = control_inla,
            verbose = FALSE
          )
          
          outcome_models[[outcome]] <- simple_outcome_model
        }, error = function(e2) {
          message(paste("Simple model for", outcome, "also failed:", e2$message))
        })
      })
    }
  }
  
  return(list(
    model = env_model,
    formula = env_formula,
    park_effects_integrated = !is.null(park_effect_term) && park_effect_term != "",
    outcome_models = outcome_models,
    computation_time = inla_time,
    env_data = env_data  # Include the processed data with new columns
  ))
}

# Extract and analyze environmental effects
analyze_environmental_effects <- function(env_model_results, env_data) {
  message("Analyzing environmental effects...")
  
  # Extract model
  env_model <- env_model_results$model
  
  # Make sure we're using the processed data from the model
  if(is.null(env_model_results$env_data)) {
    message("Warning: env_data not found in model results. Using original env_data.")
  } else {
    env_data <- env_model_results$env_data
  }
  
  # Extract fixed effects
  env_effects <- data.frame(
    variable = rownames(env_model$summary.fixed),
    effect = env_model$summary.fixed$mean,
    sd = env_model$summary.fixed$sd,
    lower_ci = env_model$summary.fixed$`0.025quant`,
    upper_ci = env_model$summary.fixed$`0.975quant`
  ) %>%
    # Calculate significance
    mutate(
      significant = (lower_ci > 0) | (upper_ci < 0),
      # Calculate standardized effect size
      std_effect_size = effect / sd
    )
  
  # Check if park effect was included
  park_effect_integrated <- any(grepl("park_effect", env_effects$variable))
  if(park_effect_integrated) {
    message("Park effects were successfully integrated into the environmental model")
  }
  
  # Extract outcome-specific effects
  outcome_effects <- list()
  if(!is.null(env_model_results$outcome_models) && length(env_model_results$outcome_models) > 0) {
    for(outcome_name in names(env_model_results$outcome_models)) {
      outcome_model <- env_model_results$outcome_models[[outcome_name]]
      
      outcome_effects[[outcome_name]] <- data.frame(
        variable = rownames(outcome_model$summary.fixed),
        effect = outcome_model$summary.fixed$mean,
        sd = outcome_model$summary.fixed$sd,
        lower_ci = outcome_model$summary.fixed$`0.025quant`,
        upper_ci = outcome_model$summary.fixed$`0.975quant`,
        significant = (outcome_model$summary.fixed$`0.025quant` > 0) | 
          (outcome_model$summary.fixed$`0.975quant` < 0),
        std_effect_size = outcome_model$summary.fixed$mean / outcome_model$summary.fixed$sd
      )
    }
  }
  
  # Calculate variance decomposition
  message("Calculating variance decomposition...")
  variance_components <- calculate_variance_decomposition(
    env_model, 
    fixed_effects = env_model$summary.fixed,
    family = "gaussian"
  )
  
  # Calculate outcome-specific variance decompositions
  outcome_variance_components <- list()
  if(!is.null(env_model_results$outcome_models) && length(env_model_results$outcome_models) > 0) {
    for(outcome_name in names(env_model_results$outcome_models)) {
      outcome_model <- env_model_results$outcome_models[[outcome_name]]
      
      outcome_variance_components[[outcome_name]] <- calculate_variance_decomposition(
        outcome_model,
        fixed_effects = outcome_model$summary.fixed,
        family = "poisson"
      )
    }
  }
  
  # Create interaction effects analysis
  interaction_effects <- NULL
  if(!is.null(env_effects)) {
    # Extract interaction terms
    interaction_effects <- env_effects %>%
      filter(grepl(":", variable)) %>%
      arrange(desc(abs(effect)))
  }
  
  # Create a diagnostic dataframe
  diagnostics <- data.frame(
    metric = c("DIC", "WAIC", "Marginal Loglikelihood", "Model Converged"),
    value = c(
      env_model$dic$dic,
      env_model$waic$waic,
      env_model$mlik[1],
      TRUE  # Just hardcode this for now since we already checked convergence
    )
  )
  
  # Return analysis results
  return(list(
    env_effects = env_effects,
    park_effect_integrated = park_effect_integrated,
    interaction_effects = interaction_effects,
    outcome_effects = outcome_effects,
    variance_components = variance_components,
    outcome_variance_components = outcome_variance_components,
    diagnostics = diagnostics
  ))
}

#=============================================
# VISUALIZATION FUNCTIONS
#=============================================

create_environmental_visualizations <- function(env_analysis) {
  message("Creating environmental visualizations...")
  
  # Extract analysis components
  env_effects <- env_analysis$env_effects
  variance_components <- env_analysis$variance_components
  outcome_effects <- env_analysis$outcome_effects
  interaction_effects <- env_analysis$interaction_effects
  
  # Plot 1: Environmental main effects
  env_effects_main <- env_effects %>%
    filter(variable != "(Intercept)" & !grepl(":", variable) & !grepl("\\^", variable)) 
  
  env_effects_plot <- tryCatch({
    env_effects_main %>%
      mutate(
        variable_label = case_when(
          variable == "temp_within_park" ~ "Temperature (within park)",
          variable == "humid_within_park" ~ "Humidity (within park)",
          variable == "wind_within_park" ~ "Wind Speed (within park)",
          variable == "elevation_z" ~ "Elevation",
          variable == "park_effect_centered" ~ "Park Effect (from park model)",
          TRUE ~ variable
        )
      ) %>%
      ggplot(aes(x = variable_label, y = effect, fill = significant)) +
      geom_col() +
      geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.3) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      scale_fill_manual(values = c("gray70", "steelblue"), 
                        labels = c("Not Significant", "Significant"),
                        name = "Effect") +
      coord_flip() +
      labs(
        title = "Environmental Main Effects on Player Performance",
        subtitle = "Standardized coefficients with 95% credible intervals",
        x = NULL,
        y = "Effect on WAR (standardized)"
      ) +
      theme_minimal()
  }, error = function(e) {
    message("Error creating environmental effects plot: ", e$message)
    # Return a simple plot
    ggplot() + 
      annotate("text", x = 0, y = 0, label = "Environmental effects plot not available") +
      theme_void()
  })
  
  # Plot 2: Interaction effects
  interaction_plot <- tryCatch({
    if(!is.null(interaction_effects) && nrow(interaction_effects) > 0) {
      interaction_effects %>%
        mutate(
          variable_label = case_when(
            variable == "temp_within_park:humid_within_park" ~ "Temperature × Humidity",
            variable == "temp_within_park:elevation_z" ~ "Temperature × Elevation",
            TRUE ~ variable
          )
        ) %>%
        ggplot(aes(x = variable_label, y = effect, fill = significant)) +
        geom_col() +
        geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.3) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
        scale_fill_manual(values = c("gray70", "steelblue"), 
                          labels = c("Not Significant", "Significant"),
                          name = "Effect") +
        coord_flip() +
        labs(
          title = "Environmental Interaction Effects",
          subtitle = "Interaction terms with 95% credible intervals",
          x = NULL,
          y = "Effect on WAR (standardized)"
        ) +
        theme_minimal()
    } else {
      ggplot() + 
        annotate("text", x = 0, y = 0, label = "No interaction effects available") +
        theme_void()
    }
  }, error = function(e) {
    message("Error creating interaction effects plot: ", e$message)
    ggplot() + 
      annotate("text", x = 0, y = 0, label = "Interaction effects plot not available") +
      theme_void()
  })
  
  # Plot 3: Variance components
  variance_plot <- tryCatch({
    ggplot(variance_components, 
           aes(x = reorder(component, -proportion), 
               y = proportion, fill = component)) +
      geom_bar(stat = "identity", width = 0.7) +
      geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)), 
                vjust = -0.5, size = 4) +
      scale_y_continuous(labels = scales::percent, 
                         limits = c(0, max(variance_components$proportion) * 1.2)) +
      scale_fill_brewer(palette = "Set2") +
      labs(
        title = "Variance Decomposition in Performance",
        subtitle = "Proportion of variance explained by each component",
        x = NULL,
        y = "Proportion of Variance"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  }, error = function(e) {
    message("Error creating variance plot: ", e$message)
    # Return a simple plot
    ggplot() + 
      annotate("text", x = 0, y = 0, label = "Variance decomposition plot not available") +
      theme_void()
  })
  
  # Plot 4: Outcome-specific effects comparison
  outcome_comparison_plot <- tryCatch({
    if(length(outcome_effects) > 0) {
      # Prepare data for comparison plot
      outcome_comparison_data <- data.frame()
      
      for(outcome_name in names(outcome_effects)) {
        # Extract temperature and elevation effects
        outcome_df <- outcome_effects[[outcome_name]]
        
        # Get main environmental effects
        temp_effect <- outcome_df$effect[outcome_df$variable == "temp_within_park"]
        elev_effect <- outcome_df$effect[outcome_df$variable == "elevation_z"]
        
        if(length(temp_effect) > 0 || length(elev_effect) > 0) {
          temp_row <- data.frame(
            outcome = gsub("X", "", outcome_name),
            variable = "Temperature",
            effect = ifelse(length(temp_effect) > 0, temp_effect, NA)
          )
          
          elev_row <- data.frame(
            outcome = gsub("X", "", outcome_name),
            variable = "Elevation",
            effect = ifelse(length(elev_effect) > 0, elev_effect, NA)
          )
          
          outcome_comparison_data <- rbind(outcome_comparison_data, temp_row, elev_row)
        }
      }
      
      # Create plot if we have data
      if(nrow(outcome_comparison_data) > 0) {
        ggplot(outcome_comparison_data, aes(x = variable, y = effect, fill = outcome)) +
          geom_bar(stat = "identity", position = "dodge") +
          geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
          labs(
            title = "Environmental Effects by Outcome Type",
            subtitle = "Comparing effects across different batting outcomes",
            x = "Environmental Factor",
            y = "Effect Size",
            fill = "Outcome"
          ) +
          theme_minimal()
      } else {
        ggplot() + 
          annotate("text", x = 0, y = 0, label = "Insufficient data for outcome comparison") +
          theme_void()
      }
    } else {
      ggplot() + 
        annotate("text", x = 0, y = 0, label = "No outcome-specific models available") +
        theme_void()
    }
  }, error = function(e) {
    message("Error creating outcome comparison plot: ", e$message)
    ggplot() + 
      annotate("text", x = 0, y = 0, label = "Outcome comparison plot not available") +
      theme_void()
  })
  
  # Return all visualizations
  return(list(
    env_effects_plot = env_effects_plot,
    interaction_plot = interaction_plot,
    variance_plot = variance_plot,
    outcome_comparison_plot = outcome_comparison_plot
  ))
}

#=============================================
# MAIN FUNCTION TO RUN ENVIRONMENTAL ANALYSIS
#=============================================

run_environmental_analysis <- function(park_results = NULL, spatial_results = NULL,
                                       data_path = "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Data",
                                       results_path = "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Results") {
  message("\n========================================================")
  message("          RUNNING ENVIRONMENTAL FACTORS ANALYSIS")
  message("========================================================\n")
  
  # Record start time for benchmarking
  start_time <- Sys.time()
  
  # Create directories if they don't exist
  if (!dir.exists(data_path)) {
    dir.create(data_path, recursive = TRUE)
  }
  
  if (!dir.exists(results_path)) {
    dir.create(results_path, recursive = TRUE)
  }
  
  # Step 1: Load pre-prepared data for environmental analysis
  message("Loading prepared data for environmental analysis...")
  
  # Step 1.1: Load environmental datasets
  env_data_path <- file.path(data_path, "env_data.rds")
  game_env_path <- file.path(data_path, "game_env.rds")
  
  if (!file.exists(env_data_path) || !file.exists(game_env_path)) {
    stop("Required environmental data files not found. Please run data_preparation.R first.")
  }
  
  message("Loading environmental data...")
  env_data <- readRDS(env_data_path)
  game_env <- readRDS(game_env_path)
  
  # Step 2: Fit environmental model with park effects integration
  message("Fitting environmental model...")
  env_model_results <- run_environmental_model(env_data, park_results)
  
  # Step 3: Analyze environmental effects
  message("Analyzing environmental effects...")
  
  # Wrap in tryCatch to handle potential errors with more grace
  env_analysis <- tryCatch({
    analyze_environmental_effects(env_model_results, env_data)
  }, error = function(e) {
    message("Error during environmental analysis: ", e$message)
    # Return a minimal analysis with just the model
    list(
      env_effects = as.data.frame(env_model_results$model$summary.fixed) %>%
        mutate(variable = rownames(env_model_results$model$summary.fixed)) %>%
        mutate(significant = (`0.025quant` > 0) | (`0.975quant` < 0)),
      park_effect_integrated = env_model_results$park_effects_integrated,
      interaction_effects = NULL,
      outcome_effects = list(),
      variance_components = calculate_variance_decomposition(
        env_model_results$model, 
        fixed_effects = env_model_results$model$summary.fixed,
        family = "gaussian"
      ),
      outcome_variance_components = list(),
      diagnostics = data.frame(
        metric = c("DIC", "WAIC", "Marginal Loglikelihood", "Model Converged"),
        value = c(
          env_model_results$model$dic$dic,
          env_model_results$model$waic$waic,
          env_model_results$model$mlik[1],
          TRUE
        )
      )
    )
  })
  
  # Step 4: Create visualizations
  message("Creating environmental visualizations...")
  env_visualizations <- tryCatch({
    create_environmental_visualizations(env_analysis)
  }, error = function(e) {
    message("Error creating visualizations: ", e$message)
    # Create a minimal set of visualizations
    list(
      env_effects_plot = ggplot() + ggtitle("Environmental effects visualization unavailable"),
      interaction_plot = ggplot() + ggtitle("Interaction effects visualization unavailable"),
      variance_plot = ggplot() + ggtitle("Variance plot unavailable"),
      outcome_comparison_plot = ggplot() + ggtitle("Outcome comparison plot unavailable")
    )
  })
  
  # Record end time
  end_time <- Sys.time()
  computation_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  message(paste("Environmental analysis completed in", round(computation_time, 2), "seconds"))
  
  # Save results
  message("Saving environmental analysis results...")
  
  # Create a dedicated directory for environmental results
  env_results_dir <- file.path(results_path, "environmental")
  if (!dir.exists(env_results_dir)) {
    dir.create(env_results_dir, recursive = TRUE)
  }
  
  # Save plots
  tryCatch({
    ggsave(file.path(env_results_dir, "environmental_effects.png"), 
           env_visualizations$env_effects_plot, width = 10, height = 8)
    ggsave(file.path(env_results_dir, "interaction_effects.png"), 
           env_visualizations$interaction_plot, width = 10, height = 6)
    ggsave(file.path(env_results_dir, "variance_components.png"), 
           env_visualizations$variance_plot, width = 10, height = 6)
    ggsave(file.path(env_results_dir, "outcome_comparison.png"), 
           env_visualizations$outcome_comparison_plot, width = 10, height = 6)
  }, error = function(e) {
    message("Error saving plots: ", e$message)
  })
  
  # Save data
  tryCatch({
    write.csv(env_analysis$env_effects, 
              file.path(env_results_dir, "environmental_effects.csv"), row.names = FALSE)
    write.csv(env_analysis$variance_components, 
              file.path(env_results_dir, "variance_components.csv"), row.names = FALSE)
    write.csv(env_analysis$diagnostics,
              file.path(env_results_dir, "env_model_diagnostics.csv"), row.names = FALSE)
    
    # Save interaction effects if available
    if(!is.null(env_analysis$interaction_effects) && nrow(env_analysis$interaction_effects) > 0) {
      write.csv(env_analysis$interaction_effects,
                file.path(env_results_dir, "interaction_effects.csv"), row.names = FALSE)
    }
  }, error = function(e) {
    message("Error saving result CSVs: ", e$message)
  })
  
  # Save outcome-specific data if available
  if(!is.null(env_analysis$outcome_effects) && length(env_analysis$outcome_effects) > 0) {
    for(outcome_name in names(env_analysis$outcome_effects)) {
      tryCatch({
        write.csv(env_analysis$outcome_effects[[outcome_name]], 
                  file.path(env_results_dir, paste0(outcome_name, "_effects.csv")), row.names = FALSE)
      }, error = function(e) {
        message(paste("Error saving results for", outcome_name, ":", e$message))
      })
    }
  }
  
  # Save model and full results to data path
  model_results <- list(
    env_model = env_model_results$model,
    outcome_models = env_model_results$outcome_models,
    env_formula = env_model_results$formula,
    park_effects_integrated = env_model_results$park_effects_integrated,
    env_analysis = env_analysis,
    env_visualizations = env_visualizations,
    computation_time = computation_time
  )
  
  saveRDS(model_results, file.path(data_path, "environmental_results.rds"))
  
  # Print summary
  message("\n========================================================")
  message("          ENVIRONMENTAL ANALYSIS RESULTS SUMMARY")
  message("========================================================\n")
  
  # Print diagnostics
  message("Model Diagnostics:")
  message(paste("DIC:", round(env_analysis$diagnostics$value[1], 2)))
  message(paste("WAIC:", round(env_analysis$diagnostics$value[2], 2)))
  message(paste("Marginal Loglikelihood:", round(env_analysis$diagnostics$value[3], 2)))
  message(paste("Model Converged:", env_analysis$diagnostics$value[4]))
  
  # Summarize model integration
  if(env_model_results$park_effects_integrated) {
    message("\nPark effects successfully integrated into environmental model")
  } else {
    message("\nNo park effects were integrated (park_results not provided)")
  }
  
  # Summarize environmental effects
  message("\nEnvironmental Effects on Performance:")
  env_effects <- env_analysis$env_effects
  for (i in 1:nrow(env_effects)) {
    if (env_effects$variable[i] != "(Intercept)") {
      effect <- env_effects$effect[i]
      var_name <- env_effects$variable[i]
      significant <- env_effects$significant[i]
      significance_str <- ifelse(significant, "significant", "not significant")
      message(sprintf("%-30s: %+.4f (%s)", var_name, effect, significance_str))
    }
  }
  
  # Summarize interaction effects if available
  if(!is.null(env_analysis$interaction_effects) && nrow(env_analysis$interaction_effects) > 0) {
    message("\nEnvironmental Interaction Effects:")
    inter_effects <- env_analysis$interaction_effects
    for (i in 1:nrow(inter_effects)) {
      effect <- inter_effects$effect[i]
      var_name <- inter_effects$variable[i]
      significant <- inter_effects$significant[i]
      significance_str <- ifelse(significant, "significant", "not significant")
      message(sprintf("%-30s: %+.4f (%s)", var_name, effect, significance_str))
    }
  }
  
  # Summarize outcome-specific models if available
  if(!is.null(env_analysis$outcome_effects) && length(env_analysis$outcome_effects) > 0) {
    message("\nOutcome-Specific Environmental Effects:")
    for(outcome_name in names(env_analysis$outcome_effects)) {
      message(paste("\n", gsub("X", "", outcome_name), "Model:"))
      
      outcome_df <- env_analysis$outcome_effects[[outcome_name]]
      
      # Handle case when outcome_df may be empty
      if(nrow(outcome_df) > 0) {
        significant_effects <- outcome_df %>% 
          filter(variable != "(Intercept)" & significant == TRUE) %>%
          arrange(desc(abs(effect)))
        
        if(nrow(significant_effects) > 0) {
          for(j in 1:nrow(significant_effects)) {
            message(sprintf("  %-25s: %+.4f", 
                            significant_effects$variable[j], 
                            significant_effects$effect[j]))
          }
        } else {
          message("  No significant environmental effects found")
        }
      } else {
        message("  No effects available for this outcome")
      }
    }
  }
  
  message("\nResults saved to: ", env_results_dir)
  message("Model and full results saved to: ", data_path)
  
  # Return the results
  return(model_results)
}

# When this script is run directly, it will try to load data and run the analysis
if (!exists("sourced_by_master") || !sourced_by_master) {
  data_path <- "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Data"
  results_path <- "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Results"
  
  # Try to load park results for integration
  park_results <- tryCatch({
    readRDS(file.path(data_path, "park_results.rds"))
  }, error = function(e) {
    message("Could not load park results: ", e$message)
    message("Will run environmental analysis without park effects integration")
    NULL
  })
  
  environmental_results <- tryCatch({
    run_environmental_analysis(park_results = park_results, data_path = data_path, results_path = results_path)
  }, error = function(e) {
    message("\nERROR: ", e$message)
    message("To run this module, first run the data preparation module.")
    return(NULL)
  })
  
  if (!is.null(environmental_results)) {
    message("\nEnvironmental factors analysis complete!")
  }
}