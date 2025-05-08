# master_analysis.R to remove spatial and environmental MCMC models

# Set this flag to prevent duplicate execution in modules
sourced_by_master <- TRUE

# Set the correct paths
code_path <- "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Code"
data_path <- "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Data"
results_path <- "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Results"

# Create directories if they don't exist
dir.create(data_path, recursive = TRUE, showWarnings = FALSE)
dir.create(results_path, recursive = TRUE, showWarnings = FALSE)

# Set analysis parameters
min_year <- 2000  # Only include data from this year forward
min_ab <- 50      # Minimum at-bats threshold

log_file <- file.path(results_path, "full_console_log.txt")
sink(log_file, append=FALSE, split=TRUE)

#=============================================
# STEP-BY-STEP ANALYSIS EXECUTION
#=============================================

# Display menu
message("\nAvailable analysis components:")
message("1. Data Preparation")
message("2. Park Effects Analysis (INLA)")
message("3. Aging Curves Analysis (INLA)")
message("4. Spatial Park Effects Analysis (INLA)")
message("5. Environmental Factors Analysis (INLA)")
message("6. Park Effects Analysis (MCMC)")
message("7. Aging Curves Analysis (MCMC)")
message("8. Computational Efficiency Comparison")
message("9. Dashboard Creation")
message("10. Run All INLA Components")
message("11. Run All MCMC Components")
message("12. Run Complete Analysis")

message("\nEnter the numbers of components to run (comma-separated), or one of the complete options (10, 11, 12):")
choice <- readline()

# Process choice
if (choice == "10") {
  components <- c("data", "park_inla", "aging_inla", "spatial_inla", "env_inla", "dashboard")
} else if (choice == "11") {
  components <- c("park_mcmc", "aging_mcmc", "efficiency", "dashboard")
} else if (choice == "12") {
  components <- c("data", "park_inla", "aging_inla", "spatial_inla", "env_inla", 
                  "park_mcmc", "aging_mcmc", "efficiency", "dashboard")
} else {
  # Parse the comma-separated numbers
  selected_numbers <- as.numeric(unlist(strsplit(choice, ",")))
  
  # Map numbers to component names
  components <- c()
  if (1 %in% selected_numbers) components <- c(components, "data")
  if (2 %in% selected_numbers) components <- c(components, "park_inla")
  if (3 %in% selected_numbers) components <- c(components, "aging_inla")
  if (4 %in% selected_numbers) components <- c(components, "spatial_inla")
  if (5 %in% selected_numbers) components <- c(components, "env_inla")
  if (6 %in% selected_numbers) components <- c(components, "park_mcmc")
  if (7 %in% selected_numbers) components <- c(components, "aging_mcmc")
  if (8 %in% selected_numbers) components <- c(components, "efficiency")
  if (9 %in% selected_numbers) components <- c(components, "dashboard")
}

# Output selected components
message("\nRunning baseball analysis with components: ", paste(components, collapse = ", "))

# Record start time
total_start <- Sys.time()

message("\n========================================================")
message("          BASEBALL ANALYTICS WITH INLA & MCMC")
message("          Step-by-Step Analysis Pipeline")
message("========================================================\n")

# Initialize results variables
datasets <- NULL
park_results <- NULL
aging_results <- NULL
spatial_results <- NULL
environmental_results <- NULL
park_mcmc_results <- NULL
aging_mcmc_results <- NULL
efficiency_results <- NULL
dashboard <- NULL

# Step 1: Data Preparation
if ("data" %in% components) {
  message("\n========================================================")
  message("STEP 1: DATA PREPARATION MODULE")
  message("========================================================\n")
  
  message("Sourcing data_preparation.R...")
  source(file.path(code_path, "data_preparation.R"))
  
  message("Running prepare_all_datasets function...")
  datasets <- prepare_all_datasets(min_year, min_ab, data_path, results_path)
  
  message("Data preparation step completed.\n")
  message("Press Enter to continue...")
  readline()
}

# Step 2: Park Effects Analysis (INLA)
if ("park_inla" %in% components) {
  message("\n========================================================")
  message("STEP 2: PARK EFFECTS ANALYSIS MODULE (INLA)")
  message("========================================================\n")
  
  message("Sourcing park_effects_model.R...")
  source(file.path(code_path, "park_effects_model.R"))
  
  message("Running park effects analysis...")
  if (!is.null(datasets) && "park_data" %in% names(datasets)) {
    park_results <- run_park_effects_model(datasets$park_data, data_path, results_path)
  } else {
    park_results <- run_park_effects_model(NULL, data_path, results_path)
  }
  
  message("Park effects analysis (INLA) step completed.\n")
  message("Press Enter to continue...")
  readline()
}

# Step 3: Aging Curves Analysis (INLA)
if ("aging_inla" %in% components) {
  message("\n========================================================")
  message("STEP 3: AGING CURVES ANALYSIS MODULE (INLA)")
  message("========================================================\n")
  
  message("Sourcing aging_curves_model.R...")
  source(file.path(code_path, "aging_curves_model.R"))
  
  message("Running run_aging_analysis function...")
  if (!is.null(datasets) && "player_trajectories" %in% names(datasets)) {
    aging_results <- run_aging_analysis(datasets$player_trajectories, data_path, results_path)
  } else {
    aging_results <- run_aging_analysis(NULL, data_path, results_path)
  }
  
  message("Aging curves analysis (INLA) step completed.\n")
  message("Press Enter to continue...")
  readline()
}

# Step 4: Spatial Analysis (INLA)
if ("spatial_inla" %in% components) {
  message("\n========================================================")
  message("STEP 4: SPATIAL PARK EFFECTS ANALYSIS MODULE (INLA)")
  message("========================================================\n")
  
  message("Sourcing spatial_analysis.R...")
  source(file.path(code_path, "spatial_analysis.R"))
  
  message("Running run_spatial_park_analysis function...")
  spatial_results <- run_spatial_park_analysis(
    park_results = park_results,
    data_path = data_path,
    results_path = results_path
  )
  
  message("Spatial park effects analysis (INLA) step completed.\n")
  message("Press Enter to continue...")
  readline()
}

# Step 5: Environmental Analysis (INLA)
if ("env_inla" %in% components) {
  message("\n========================================================")
  message("STEP 5: ENVIRONMENTAL FACTORS ANALYSIS MODULE (INLA)")
  message("========================================================\n")
  
  message("Sourcing environmental_factors.R...")
  source(file.path(code_path, "environmental_factors.R"))
  
  message("Running run_environmental_analysis function...")
  environmental_results <- run_environmental_analysis(
    park_results = park_results,
    spatial_results = spatial_results,
    data_path = data_path,
    results_path = results_path
  )
  
  message("Environmental factors analysis (INLA) step completed.\n")
  message("Press Enter to continue...")
  readline()
}

# Step 6: Park Effects Analysis (MCMC)
if ("park_mcmc" %in% components) {
  message("\n========================================================")
  message("STEP 6: PARK EFFECTS ANALYSIS MODULE (MCMC)")
  message("========================================================\n")
  
  message("Sourcing mcmc_models.R...")
  source(file.path(code_path, "mcmc_models.R"))
  
  message("Running park effects MCMC analysis...")
  if (!is.null(datasets) && "park_data" %in% names(datasets)) {
    park_mcmc_results <- run_park_effects_mcmc(datasets$park_data, data_path, results_path)
  } else {
    # Try to load park_data from disk
    park_data_path <- file.path(data_path, "park_data.rds")
    if (file.exists(park_data_path)) {
      park_data <- readRDS(park_data_path)
      park_mcmc_results <- run_park_effects_mcmc(park_data, data_path, results_path)
    } else {
      message("Cannot run park effects MCMC model: park_data not found")
    }
  }
  
  message("Park effects analysis (MCMC) step completed.\n")
  message("Press Enter to continue...")
  readline()
}

# Step 7: Aging Curves Analysis (MCMC)
if ("aging_mcmc" %in% components) {
  message("\n========================================================")
  message("STEP 7: AGING CURVES ANALYSIS MODULE (MCMC)")
  message("========================================================\n")
  
  message("Using mcmc_models.R...")
  if (!exists("run_aging_curves_mcmc")) {
    message("Sourcing mcmc_models.R...")
    source(file.path(code_path, "mcmc_models.R"))
  }
  
  message("Running aging curves MCMC analysis...")
  if (!is.null(datasets) && "player_trajectories" %in% names(datasets)) {
    aging_mcmc_results <- run_aging_curves_mcmc(datasets$player_trajectories, data_path, results_path)
  } else {
    # Try to load player_trajectories from disk
    trajectories_path <- file.path(data_path, "player_trajectories.rds")
    if (file.exists(trajectories_path)) {
      player_trajectories <- readRDS(trajectories_path)
      aging_mcmc_results <- run_aging_curves_mcmc(player_trajectories, data_path, results_path)
    } else {
      message("Cannot run aging curves MCMC model: player_trajectories not found")
    }
  }
  
  message("Aging curves analysis (MCMC) step completed.\n")
  message("Press Enter to continue...")
  readline()
}

# Step 8: Computational Efficiency Comparison
if ("efficiency" %in% components) {
  message("\n========================================================")
  message("STEP 8: COMPUTATIONAL EFFICIENCY COMPARISON")
  message("========================================================\n")
  
  message("Using mcmc_models.R...")
  if (!exists("compare_computational_efficiency")) {
    message("Sourcing mcmc_models.R...")
    source(file.path(code_path, "mcmc_models.R"))
  }
  
  message("Running computational efficiency comparison...")
  efficiency_results <- compare_computational_efficiency(
    data_path = data_path,
    results_path = results_path
  )
  
  message("Computational efficiency comparison step completed.\n")
  message("Press Enter to continue...")
  readline()
}

# Step 9: Create Dashboard
if ("dashboard" %in% components) {
  message("\n========================================================")
  message("STEP 9: DASHBOARD CREATION MODULE")
  message("========================================================\n")
  
  message("Sourcing dashboard_creation.R...")
  source(file.path(code_path, "dashboard_creation.R"))
  
  message("Running create_dashboard function...")
  dashboard <- create_dashboard(
    park_results = park_results, 
    aging_results = aging_results,
    spatial_results = spatial_results,
    park_mcmc_results = park_mcmc_results,
    aging_mcmc_results = aging_mcmc_results,
    efficiency_results = efficiency_results,
    data_path = data_path,
    results_path = results_path
  )
  
  message("Running print_summary function...")
  print_summary(
    park_results = park_results, 
    aging_results = aging_results,
    spatial_results = spatial_results,
    park_mcmc_results = park_mcmc_results,
    aging_mcmc_results = aging_mcmc_results,
    efficiency_results = efficiency_results,
    data_path = data_path,
    results_path = results_path
  )
  
  message("Dashboard creation step completed.\n")
}

# Run diagnostics for INLA models
if (any(c("park_inla", "aging_inla", "spatial_inla", "env_inla") %in% components)) {
  message("\n========================================================")
  message("RUNNING INLA MODEL DIAGNOSTICS")
  message("========================================================\n")
  
  message("Sourcing diagnostics.R...")
  source(file.path(code_path, "diagnostics.R"))
  
  message("Running model diagnostics...")
  inla_models_list <- list(
    park_effects = park_results,
    aging_curves = aging_results,
    spatial = spatial_results,
    environmental = environmental_results
  )
  
  inla_diagnostics <- run_model_diagnostics(inla_models_list, data_path, results_path)
  
  message("INLA model diagnostics step completed.\n")
}

# Calculate total execution time
total_end <- Sys.time()
total_time <- difftime(total_end, total_start, units = "mins")
message(paste("\nTotal analysis completed in", round(total_time, 2), "minutes"))

# Final summary
message("\n========================================================")
message("          ANALYSIS EXECUTION SUMMARY")
message("========================================================\n")

message("Complete analysis finished!")
message("All model data saved to:")
message(data_path)
message("All results and visualizations saved to:")
message(results_path)

# Print a summary of what was run
message("\nSummary of completed analysis components:")
if ("data" %in% components) message("✓ Data preparation completed")
if ("park_inla" %in% components) message("✓ Park effects analysis (INLA) completed")
if ("aging_inla" %in% components) message("✓ Aging curves analysis (INLA) completed")
if ("spatial_inla" %in% components) message("✓ Spatial park effects analysis (INLA) completed")
if ("env_inla" %in% components) message("✓ Environmental factors analysis (INLA) completed")
if ("park_mcmc" %in% components) message("✓ Park effects analysis (MCMC) completed")
if ("aging_mcmc" %in% components) message("✓ Aging curves analysis (MCMC) completed")
if ("efficiency" %in% components) message("✓ Computational efficiency comparison completed")
if ("dashboard" %in% components) message("✓ Dashboard creation completed")

# Print efficiency comparison if available
if (!is.null(efficiency_results) && "efficiency" %in% components) {
  message("\nComputational Efficiency Summary:")
  
  # Create a nicely formatted table
  eff_df <- efficiency_results$efficiency
  
  # Print header
  message(sprintf("%-20s | %-12s | %-12s | %-10s", 
                  "Model", "INLA (sec)", "MCMC (sec)", "Speedup"))
  message(paste(rep("-", 60), collapse = ""))
  
  # Print rows
  for (i in 1:nrow(eff_df)) {
    if (!is.na(eff_df$speedup_factor[i])) {
      message(sprintf("%-20s | %12.2f | %12.2f | %10.1fx", 
                      eff_df$model[i], 
                      eff_df$inla_time[i],
                      eff_df$mcmc_time[i],
                      eff_df$speedup_factor[i]))
    }
  }
  
  # Calculate average speedup
  avg_speedup <- mean(eff_df$speedup_factor, na.rm = TRUE)
  message(paste(rep("-", 60), collapse = ""))
  message(sprintf("%-20s | %12s | %12s | %10.1fx", 
                  "Average", "", "", avg_speedup))
}

sink()