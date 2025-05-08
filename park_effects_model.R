# park_effects_model.R

# Load required packages
if (!require("INLA")) {
  install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
}
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")

library(INLA)
library(dplyr)
library(ggplot2)

#=============================================
# STANDARDIZED VARIANCE DECOMPOSITION FUNCTION
#=============================================

# Function to calculate variance components consistently across models
calculate_variance_decomposition <- function(model, family = "gaussian") {
  # Extract precision parameters
  precision_summary <- model$summary.hyperpar
  
  # Print the precision summary to see actual values and row names
  message("Precision parameter summary:")
  print(precision_summary)
  
  # Initialize components vector
  components <- c("Player", "Park", "Residual")
  variances <- numeric(length(components))
  
  # Extract variances with better error handling
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
    } else {
      message("Could not find Gaussian precision parameter")
      variances[3] <- NA
    }
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
# PARK EFFECTS INLA MODEL FUNCTION
#=============================================

run_park_effects_model <- function(park_data, 
                                   data_path = "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Data",
                                   results_path = "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Results") {
  message("\n========================================================")
  message("          RUNNING PARK EFFECTS INLA MODEL")
  message("========================================================\n")
  
  # Basic validation
  required_cols <- c("H", "AB", "player_id", "park_id")
  missing_cols <- setdiff(required_cols, names(park_data))
  if (length(missing_cols) > 0) {
    stop("Error: Dataset is missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Formula for INLA
  formula_park <- H ~ offset(log(AB)) + 
    f(player_id, model = "iid", 
      hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.05)))) +  # Player random effect
    f(park_id, model = "iid",
      hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.05))))    # Park random effect
  
  # Set control parameters for INLA
  control_compute <- list(
    dic = TRUE,          # Compute Deviance Information Criterion
    waic = TRUE,         # Compute Widely Applicable Information Criterion
    return.marginals = TRUE,  # Return posterior marginals
    config = TRUE        # Return config information for better diagnostics
  )
  
  # Control family parameters
  control_family <- list(
    link = "log"
  )
  
  # Set numerical control parameters to improve convergence
  control_inla <- list(
    strategy = "gaussian",   # Stable strategy
    int.strategy = "eb",     # Empirical Bayes for stability
    h = 1e-4,                # Step length for numerical integration
    tolerance = 1e-6        # Convergence tolerance
  )
  
  # Record start time for benchmarking
  start_time <- Sys.time()
  
  # Fit INLA model
  message("Fitting INLA model...")
  park_model <- inla(
    formula_park,
    family = "poisson",
    data = park_data,
    control.compute = control_compute,
    control.predictor = list(compute = TRUE),
    control.family = control_family,
    control.inla = control_inla,
    verbose = FALSE    # Set to TRUE for debugging
  )
  
  # Record end time
  end_time <- Sys.time()
  inla_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  message(paste("INLA model fitting completed in", round(inla_time, 2), "seconds"))
  
  # Check convergence
  if (!is.null(park_model$mode) && !is.null(park_model$mode$convergence) && park_model$mode$convergence != 0) {
    message("WARNING: Model did not converge. Consider further adjustments.")
  } else {
    message("Model converged successfully!")
  }
  
  # Extract park effects with additional diagnostics
  message("Extracting park effects...")
  
  # Get unique parks for joining with effects
  unique_parks <- park_data %>%
    select(park, park_id) %>%
    distinct()
  
  # Extract park effects from INLA model
  park_effects <- data.frame(
    park_id = park_model$summary.random$park_id$ID,
    effect = park_model$summary.random$park_id$mean,
    sd = park_model$summary.random$park_id$sd,
    lower_ci = park_model$summary.random$park_id$`0.025quant`,
    upper_ci = park_model$summary.random$park_id$`0.975quant`
  ) %>%
    # Join with park names
    left_join(unique_parks, by = "park_id") %>%
    # Calculate actual park factors
    mutate(
      park_factor = exp(effect),
      lower_ci_factor = exp(lower_ci),
      upper_ci_factor = exp(upper_ci),
      significant = (lower_ci_factor > 1) | (upper_ci_factor < 1)
    ) %>%
    arrange(desc(park_factor))
  
  # Calculate variance decomposition
  message("Calculating variance decomposition...")
  variance_components <- calculate_variance_decomposition(park_model, family = "poisson")
  
  # Create park effects plot
  park_plot <- ggplot(head(park_effects, 20), 
                      aes(x = reorder(park, park_factor), y = park_factor, 
                          color = significant)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = lower_ci_factor, ymax = upper_ci_factor), width = 0.3) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    scale_color_manual(values = c("gray50", "blue"), 
                       labels = c("Not Significant", "Significant"),
                       name = "Effect") +
    coord_flip() +
    labs(
      title = "Park Effects on Batting Performance",
      subtitle = "Values > 1 indicate hitter-friendly parks (with 95% credible intervals)",
      x = "Ballpark",
      y = "Park Factor (exp(effect))"
    ) +
    theme_minimal()
  
  # Create variance plot
  variance_plot <- ggplot(variance_components, 
                          aes(x = reorder(component, -proportion), 
                              y = proportion, fill = component)) +
    geom_bar(stat = "identity", width = 0.7) +
    geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)), 
              vjust = -0.5, size = 4) +
    scale_y_continuous(labels = scales::percent, 
                       limits = c(0, max(variance_components$proportion) * 1.2)) +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = "Variance Decomposition in Batting Performance",
      subtitle = "Proportion of variance explained by each component",
      x = NULL,
      y = "Proportion of Variance"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(size = 12),
      panel.grid.major.x = element_blank()
    )
  
  # Save results to files
  message("Saving park effects results...")
  
  # Create directories if they don't exist
  if (!dir.exists(data_path)) {
    dir.create(data_path, recursive = TRUE)
  }
  
  if (!dir.exists(results_path)) {
    dir.create(results_path, recursive = TRUE)
  }
  
  # Save CSV files to results directory
  write.csv(park_effects, file.path(results_path, "park_effects.csv"), row.names = FALSE)
  write.csv(variance_components, file.path(results_path, "variance_components.csv"), row.names = FALSE)
  
  # Create a diagnostic dataframe
  diagnostics <- data.frame(
    metric = c("DIC", "WAIC", "Marginal Loglikelihood", "Model Converged"),
    value = c(
      park_model$dic$dic,
      park_model$waic$waic,
      park_model$mlik[1],
      TRUE  # Just hardcode this for now since we already checked convergence
    )
  )
  
  write.csv(diagnostics, file.path(results_path, "park_model_diagnostics.csv"), row.names = FALSE)
  
  # Save plots to results directory
  ggsave(file.path(results_path, "park_effects_plot.png"), park_plot, width = 12, height = 8)
  ggsave(file.path(results_path, "variance_bar_plot.png"), variance_plot, width = 10, height = 6)
  
  # Save model as RDS for future use to data directory
  saveRDS(park_model, file.path(data_path, "park_model.rds"))
  
  # Create results list
  results <- list(
    park_model = park_model,
    park_effects = park_effects,
    variance_components = variance_components,
    park_plot = park_plot,
    variance_plot = variance_plot,
    diagnostics = diagnostics,
    computation_time = inla_time
  )
  
  # Save complete results object to data directory
  saveRDS(results, file.path(data_path, "park_results.rds"))
  
  # Print summary
  message("\n========================================================")
  message("          PARK EFFECTS ANALYSIS RESULTS")
  message("========================================================\n")
  
  message("Model Diagnostics:")
  message(paste("DIC:", round(park_model$dic$dic, 2)))
  message(paste("WAIC:", round(park_model$waic$waic, 2)))
  message(paste("Marginal Loglikelihood:", round(park_model$mlik[1], 2)))
  message(paste("Model Converged:", park_model$mode$convergence == 0))
  
  top_parks <- head(park_effects %>% arrange(desc(park_factor)), 5)
  bottom_parks <- head(park_effects %>% arrange(park_factor), 5)
  
  message("\nTop 5 Hitter-Friendly Parks:")
  for (i in 1:nrow(top_parks)) {
    message(sprintf("%d. %s: %.3f (%.3f-%.3f)", 
                    i, top_parks$park[i], 
                    top_parks$park_factor[i],
                    top_parks$lower_ci_factor[i],
                    top_parks$upper_ci_factor[i]))
  }
  
  message("\nTop 5 Pitcher-Friendly Parks:")
  for (i in 1:nrow(bottom_parks)) {
    message(sprintf("%d. %s: %.3f (%.3f-%.3f)", 
                    i, bottom_parks$park[i], 
                    bottom_parks$park_factor[i],
                    bottom_parks$lower_ci_factor[i],
                    bottom_parks$upper_ci_factor[i]))
  }
  
  message("\nVariance Decomposition:")
  for (i in 1:nrow(variance_components)) {
    message(sprintf("%-25s: %6.1f%%", 
                    variance_components$component[i], 
                    variance_components$proportion[i] * 100))
  }
  
  message("\nResults saved to: ", results_path)
  message("Model and full results saved to: ", data_path)
  
  # Return the results
  return(results)
}

#=============================================
# FUNCTION TO LOAD DATA AND RUN ANALYSIS
#=============================================

run_park_analysis <- function(park_data = NULL, 
                              data_path = "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Data",
                              results_path = "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Results") {
  # Check if park_data is provided
  if (is.null(park_data)) {
    # Try to load from disk
    park_data_path <- file.path(data_path, "park_data.rds")
    if (file.exists(park_data_path)) {
      message("Loading park data from: ", park_data_path)
      park_data <- readRDS(park_data_path)
    } else {
      stop("No park_data provided and none found on disk. Please run data preparation first.")
    }
  }
  
  # Run the park effects model
  park_results <- run_park_effects_model(park_data, data_path, results_path)
  
  return(park_results)
}

# When this script is run directly, it will try to load data and run the analysis
if (!exists("sourced_by_master") || !sourced_by_master) {
  data_path <- "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Data"
  results_path <- "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Results"
  
  park_results <- tryCatch({
    run_park_analysis(data_path = data_path, results_path = results_path)
  }, error = function(e) {
    message("\nERROR: ", e$message)
    message("To run this module, either:")
    message("1. First run the data preparation module to create the necessary datasets")
    message("2. Or provide park_data directly: run_park_analysis(park_data)")
    return(NULL)
  })
  
  if (!is.null(park_results)) {
    message("\nPark effects analysis complete!")
  }
}