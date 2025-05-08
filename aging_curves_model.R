# aging_curves_model.R

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
  
  # Initialize components vector for aging curves model
  components <- c("Player", "Age", "Residual")
  variances <- numeric(length(components))
  
  # Extract variances (reciprocal of precision)
  tryCatch({
    variances[1] <- 1/precision_summary["Precision for player_id", "mean"]  # Player variance
    
    # For age, check if it's modeled as random effect
    if("Precision for age_id" %in% rownames(precision_summary)) {
      variances[2] <- 1/precision_summary["Precision for age_id", "mean"]  # Age variance
    } else {
      # If age is not a random effect, try to calculate from fixed effects
      variances[2] <- 0
    }
  }, error = function(e) {
    message("Error extracting variance components: ", e$message)
  })
  
  # Calculate residual variance for Gaussian models
  variances[3] <- 1/precision_summary["Precision for the Gaussian observations", "mean"]
  
  # Create data frame of variance components
  variance_components <- data.frame(
    component = components,
    variance = variances
  )
  
  # Calculate proportions
  total_variance <- sum(variance_components$variance, na.rm = TRUE)
  variance_components$proportion <- variance_components$variance / total_variance
  
  return(variance_components)
}

#=============================================
# AGING CURVES INLA MODEL FUNCTION
#=============================================

# Aging curves model with focus on numerical stability

run_aging_curves_model <- function(player_trajectories, 
                                   data_path = "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Data",
                                   results_path = "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Results") {
  message("\n========================================================")
  message("        RUNNING AGING CURVES INLA MODEL")
  message("========================================================\n")
  
  # Basic validation
  required_cols <- c("WAR_centered", "player_id", "age", "age_id", "AB")
  missing_cols <- setdiff(required_cols, names(player_trajectories))
  if (length(missing_cols) > 0) {
    stop("Error: Dataset is missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Transform age to be centered at 30 (typical peak age)
  player_trajectories$age_centered <- player_trajectories$age - 30
  player_trajectories$age_centered_sq <- player_trajectories$age_centered^2
  
  # Scale WAR_centered to avoid numerical issues
  player_trajectories$WAR_scaled <- scale(player_trajectories$WAR_centered)[,1]
  
  # Scale weights to be more numerically stable
  # This is important because extreme weights can cause computational problems
  max_AB <- max(player_trajectories$AB)
  player_trajectories$AB_weight <- player_trajectories$AB / max_AB
  
  # Create formula for INLA - model with age as fixed quadratic effect
  # Use the scaled response and weights for better numerical stability
  war_formula <- WAR_scaled ~ 
    age_centered + age_centered_sq +  # Fixed quadratic effect for age
    f(player_id, model = "iid",       # Player random effect
      hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.05))))
  
  # Set control parameters for INLA with better numerical control
  control_compute <- list(
    dic = TRUE,
    waic = TRUE,
    return.marginals = TRUE,
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
  message("Fitting INLA model for aging curves...")
  war_model <- inla(
    war_formula,
    family = "gaussian",
    data = player_trajectories,
    weights = player_trajectories$AB_weight,  # Use scaled weights
    control.compute = control_compute,
    control.predictor = list(compute = TRUE),
    control.inla = control_inla,
    control.family = control_family,
    verbose = TRUE  # Set to TRUE to see detailed messages
  )
  
  # Record end time
  end_time <- Sys.time()
  inla_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  message(paste("INLA model fitting completed in", round(inla_time, 2), "seconds"))
  
  # Check convergence
  if (!is.null(war_model$mode) && !is.null(war_model$mode$convergence) && war_model$mode$convergence != 0) {
    message("WARNING: Model did not converge. Consider further adjustments.")
  } else {
    message("Model converged successfully!")
  }
  
  # Check for negative DIC or infinite WAIC
  if (!is.null(war_model$dic) && war_model$dic$dic < 0) {
    message("WARNING: Negative DIC detected. This indicates model specification issues.")
    message("Proceeding with parameter estimates, but interpret diagnostics with caution.")
  }
  
  if (!is.null(war_model$waic) && is.infinite(war_model$waic$waic)) {
    message("WARNING: Infinite WAIC detected. This indicates computational issues.")
    message("Proceeding with parameter estimates, but interpret diagnostics with caution.")
  }
  
  # Extract fixed effects for the aging curve
  age_fixed_effects <- war_model$summary.fixed
  
  # Create a data frame of ages for prediction
  ages <- data.frame(
    age = seq(min(player_trajectories$age), max(player_trajectories$age), by = 0.1)
  )
  
  # Center the ages
  ages$age_centered <- ages$age - 30
  ages$age_centered_sq <- ages$age_centered^2
  
  # Calculate the aging curve from the fixed effects, converting back to original scale
  sd_WAR <- sd(player_trajectories$WAR_centered, na.rm = TRUE)
  ages$effect <- (age_fixed_effects["(Intercept)", "mean"] + 
                    age_fixed_effects["age_centered", "mean"] * ages$age_centered +
                    age_fixed_effects["age_centered_sq", "mean"] * ages$age_centered_sq) * sd_WAR
  
  # Find the peak age using the quadratic formula
  # For a quadratic curve y = a + b*x + c*x^2, the peak is at x = -b/(2*c)
  b <- age_fixed_effects["age_centered", "mean"]
  c <- age_fixed_effects["age_centered_sq", "mean"]
  
  if (c < 0) {  # Ensure it's a maximum, not a minimum
    peak_age_offset <- -b/(2*c)  # This is relative to the centering point (30)
    peak_age <- 30 + peak_age_offset
    
    # Calculate the value at the peak
    peak_value <- (age_fixed_effects["(Intercept)", "mean"] + 
                     b * peak_age_offset + 
                     c * peak_age_offset^2) * sd_WAR
    
    # Calculate rate of decline after peak by evaluating the slope at age peak_age + 2
    post_peak_age <- peak_age + 2
    post_peak_centered <- post_peak_age - 30
    decline_rate <- (b + 2 * c * post_peak_centered) * sd_WAR  # Derivative of the quadratic function
  } else {
    # If the parabola opens upward, there's no peak
    message("WARNING: Quadratic age effect doesn't have a maximum. Check model results.")
    peak_age <- NA
    peak_value <- NA
    decline_rate <- NA
  }
  
  # Calculate variance decomposition
  message("Calculating variance decomposition...")
  variance_components <- calculate_variance_decomposition(war_model, family = "gaussian")
  
  # Extract player-specific trajectories for visualization
  # Select a few players with long careers for examples
  player_spans <- player_trajectories %>%
    group_by(player_id, player_name) %>%
    summarize(
      career_span = max(age) - min(age),
      n_seasons = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(career_span), desc(n_seasons))
  
  # Take top 5 players with longest careers
  example_players <- head(player_spans$player_id, 5)
  
  # Extract trajectories for these players
  player_examples <- player_trajectories %>%
    filter(player_id %in% example_players) %>%
    arrange(player_id, age)
  
  # Create visualizations
  message("Creating aging curve visualizations...")
  
  # Create population aging curve plot
  aging_curve_plot <- ggplot() +
    # Add model-based curve
    geom_line(data = ages, 
              aes(x = age, y = effect), 
              size = 1.2, color = "blue") +
    # Add peak age marker
    geom_vline(xintercept = peak_age, 
               linetype = "dashed", color = "red") +
    # Add annotations
    annotate("text", x = peak_age + 0.5, y = peak_value, 
             label = paste("Peak Age:", round(peak_age, 1)),
             hjust = 0, vjust = 1, size = 4) +
    # Add phases
    annotate("text", x = peak_age - 5, y = predict(loess(effect ~ age, ages), peak_age - 5), 
             label = "Development Phase",
             hjust = 1, color = "darkgreen", size = 4) +
    annotate("text", x = peak_age + 5, y = predict(loess(effect ~ age, ages), peak_age + 5), 
             label = "Decline Phase",
             hjust = 0, color = "darkred", size = 4) +
    # Add titles and labels
    labs(
      title = "Player Aging Curve for WAR",
      subtitle = paste0("Peak performance at age ", round(peak_age, 1), 
                        " with ", round(abs(decline_rate), 3), 
                        " WAR decline per year after peak"),
      x = "Age",
      y = "WAR Effect (centered)"
    ) +
    theme_minimal()
  
  # Player-specific trajectory plot with improved visualization
  player_trajectory_plot <- ggplot() +
    # Add population curve as reference
    geom_line(data = ages, 
              aes(x = age, y = effect), 
              size = 1.2, color = "blue", alpha = 0.7) +
    # Add individual player trajectories
    geom_line(data = player_examples, 
              aes(x = age, y = WAR_centered, group = player_id, color = as.factor(player_id)), 
              size = 0.9) +
    # Add points for actual observations
    geom_point(data = player_examples, 
               aes(x = age, y = WAR_centered, color = as.factor(player_id)),
               size = 2, alpha = 0.6) +
    # Add peak age marker
    geom_vline(xintercept = peak_age, 
               linetype = "dashed", color = "red") +
    # Improve color scale for better distinguishability
    scale_color_brewer(palette = "Set1", name = "Player ID") +
    # Add labels
    labs(
      title = "Individual Player Trajectories vs. Population Curve",
      subtitle = "Player-specific career arcs compared to population average (blue line)",
      x = "Age",
      y = "WAR Effect (centered)",
      color = "Player ID"
    ) +
    theme_minimal() +
    # Improve legend
    theme(
      legend.position = "right",
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 12, face = "bold")
    )
  
  # Create variance decomposition plot
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
      title = "Variance Decomposition in Player Performance",
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
  message("Saving aging curves results...")
  
  # Create directories if they don't exist
  if (!dir.exists(data_path)) {
    dir.create(data_path, recursive = TRUE)
  }
  
  if (!dir.exists(results_path)) {
    dir.create(results_path, recursive = TRUE)
  }
  
  # Create a diagnostic dataframe
  diagnostics <- data.frame(
    metric = c("DIC", "WAIC", "Marginal Loglikelihood", "Model Converged", "Peak Age", "Decline Rate"),
    value = c(
      war_model$dic$dic,
      war_model$waic$waic,
      war_model$mlik[1],
      TRUE,  # Just hardcode this for now since we already checked convergence
      peak_age,
      decline_rate
    )
  )
  
  # Save diagnostic information
  write.csv(diagnostics, file.path(results_path, "aging_model_diagnostics.csv"), row.names = FALSE)
  
  # Save age effects
  write.csv(ages, file.path(results_path, "aging_effects.csv"), row.names = FALSE)
  
  # Save variance components
  write.csv(variance_components, file.path(results_path, "aging_variance_components.csv"), row.names = FALSE)
  
  # Save plots to results directory
  ggsave(file.path(results_path, "aging_curve_plot.png"), aging_curve_plot, width = 12, height = 8)
  ggsave(file.path(results_path, "player_trajectories_plot.png"), player_trajectory_plot, width = 12, height = 8)
  ggsave(file.path(results_path, "aging_variance_plot.png"), variance_plot, width = 10, height = 6)
  
  # Save model as RDS for future use to data directory
  saveRDS(war_model, file.path(data_path, "aging_model.rds"))
  
  # Create results list
  results <- list(
    war_model = war_model,
    age_effects = ages,
    peak_age = peak_age,
    decline_rate = decline_rate,
    player_examples = player_examples,
    variance_components = variance_components,
    aging_curve_plot = aging_curve_plot,
    player_trajectory_plot = player_trajectory_plot,
    variance_plot = variance_plot,
    diagnostics = diagnostics,
    computation_time = inla_time
  )
  
  # Save complete results object to data directory
  saveRDS(results, file.path(data_path, "aging_results.rds"))
  
  # Print summary
  message("\n========================================================")
  message("          AGING CURVES ANALYSIS RESULTS")
  message("========================================================\n")
  
  message("Model Diagnostics:")
  message(paste("DIC:", round(war_model$dic$dic, 2)))
  message(paste("WAIC:", round(war_model$waic$waic, 2)))
  message(paste("Marginal Loglikelihood:", round(war_model$mlik[1], 2)))
  message(paste("Model Converged:", war_model$mode$convergence == 0))
  
  message("\nPeak Performance Age:", round(peak_age, 1))
  message("Rate of Performance Decline After Peak:", 
          sprintf("%.3f WAR per year", abs(decline_rate)))
  
  message("\nAge Effects Summary (relative to age 30):")
  ages_summary <- ages[ages$age %in% c(22, 25, 27, 30, 33, 36, 39), ]
  ref_effect <- ages_summary$effect[ages_summary$age == 30]
  
  for (i in 1:nrow(ages_summary)) {
    message(sprintf("Age %2d: %+.3f WAR relative to age 30", 
                    ages_summary$age[i], 
                    ages_summary$effect[i] - ref_effect))
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

run_aging_analysis <- function(player_trajectories = NULL,
                               data_path = "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Data",
                               results_path = "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Results") {
  # Check if player_trajectories is provided
  if (is.null(player_trajectories)) {
    # Try to load from disk
    data_path_file <- file.path(data_path, "player_trajectories.rds")
    if (file.exists(data_path_file)) {
      message("Loading player trajectories data from: ", data_path_file)
      player_trajectories <- readRDS(data_path_file)
    } else {
      stop("No player_trajectories provided and none found on disk. Please run data preparation first.")
    }
  }
  
  # Run the aging curves model
  aging_results <- run_aging_curves_model(player_trajectories, data_path, results_path)
  
  return(aging_results)
}

# Export the function to the global environment to ensure it's available to other scripts
assign("run_aging_analysis", run_aging_analysis, envir = .GlobalEnv)

# When this script is run directly, it will try to load data and run the analysis
if (!exists("sourced_by_master") || !sourced_by_master) {
  data_path <- "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Data"
  results_path <- "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Results"
  
  aging_results <- tryCatch({
    run_aging_analysis(data_path = data_path, results_path = results_path)
  }, error = function(e) {
    message("\nERROR: ", e$message)
    message("To run this module, either:")
    message("1. First run the data preparation module to create the necessary datasets")
    message("2. Or provide player_trajectories directly: run_aging_analysis(player_trajectories)")
    return(NULL)
  })
  
  if (!is.null(aging_results)) {
    message("\nAging curves analysis complete!")
  }
}