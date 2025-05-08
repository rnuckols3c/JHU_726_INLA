# mcmc_models.R - Implementation of MCMC models for comparison with INLA
# This module implements equivalent Stan models for park effects and aging curves

# Load required packages
if (!require("rstan")) install.packages("rstan")
if (!require("parallel")) install.packages("parallel")
if (!require("bayesplot")) install.packages("bayesplot")
if (!require("bridgesampling")) install.packages("bridgesampling")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")

library(rstan)
library(parallel)
library(bayesplot)
library(bridgesampling)
library(ggplot2)
library(dplyr)

# Set Stan options for faster sampling
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

#=============================================
# PARK EFFECTS MCMC MODEL
#=============================================

run_park_effects_mcmc <- function(park_data, 
                                  data_path = "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Data",
                                  results_path = "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Results") {
  message("\n========================================================")
  message("          RUNNING PARK EFFECTS MCMC MODEL")
  message("========================================================\n")
  
  # Basic validation
  required_cols <- c("H", "AB", "player_id", "park_id")
  missing_cols <- setdiff(required_cols, names(park_data))
  if (length(missing_cols) > 0) {
    stop("Error: Dataset is missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Create Stan model code
  stan_code <- "
  data {
    int<lower=0> N;             // number of observations
    int<lower=0> N_player;      // number of players
    int<lower=0> N_park;        // number of parks
    int<lower=1> player_id[N];  // player IDs
    int<lower=1> park_id[N];    // park IDs
    int<lower=0> H[N];          // hits
    int<lower=0> AB[N];         // at-bats
  }
  parameters {
    real intercept;
    vector[N_player] player_effect;
    vector[N_park] park_effect;
    real<lower=0> sigma_player;
    real<lower=0> sigma_park;
  }
  model {
    // Priors
    player_effect ~ normal(0, sigma_player);
    park_effect ~ normal(0, sigma_park);
    sigma_player ~ gamma(1, 0.05);  // match INLA prior
    sigma_park ~ gamma(1, 0.05);    // match INLA prior
    
    // Likelihood
    for (i in 1:N) {
      H[i] ~ poisson(exp(intercept + player_effect[player_id[i]] + 
                       park_effect[park_id[i]]) * AB[i]);
    }
  }
  generated quantities {
    vector[N_park] park_factor;
    for (p in 1:N_park) {
      park_factor[p] = exp(park_effect[p]);
    }
  }
  "
  
  # Create data list for Stan
  stan_data <- list(
    N = nrow(park_data),
    N_player = length(unique(park_data$player_id)),
    N_park = length(unique(park_data$park_id)),
    player_id = as.array(as.numeric(factor(park_data$player_id, 
                                           levels = unique(park_data$player_id)))),
    park_id = as.array(as.numeric(factor(park_data$park_id, 
                                         levels = unique(park_data$park_id)))),
    H = as.array(park_data$H),
    AB = as.array(park_data$AB)
  )
  
  # Check park_id and player_id are properly formatted
  if (max(stan_data$player_id) != stan_data$N_player) {
    message("Warning: player_id not sequential. Remapping player IDs.")
    player_map <- data.frame(
      original = unique(park_data$player_id),
      stan_id = 1:stan_data$N_player
    )
    stan_data$player_id <- player_map$stan_id[match(park_data$player_id, player_map$original)]
  }
  
  if (max(stan_data$park_id) != stan_data$N_park) {
    message("Warning: park_id not sequential. Remapping park IDs.")
    park_map <- data.frame(
      original = unique(park_data$park_id),
      stan_id = 1:stan_data$N_park
    )
    stan_data$park_id <- park_map$stan_id[match(park_data$park_id, park_map$original)]
  }
  
  # Record start time for benchmarking
  start_time <- Sys.time()
  
  # Fit Stan model
  message("Fitting Stan model...")
  park_stanfit <- stan(
    model_code = stan_code,
    data = stan_data,
    iter = 2000,
    warmup = 1000,
    chains = 4,
    cores = parallel::detectCores(),
    control = list(adapt_delta = 0.9, max_treedepth = 12)
  )
  
  # Record end time
  end_time <- Sys.time()
  mcmc_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  message(paste("MCMC model fitting completed in", round(mcmc_time, 2), "seconds"))
  
  # Extract parameters
  message("Extracting park effects...")
  park_samples <- extract(park_stanfit)
  
  # Calculate park effects
  unique_parks <- unique(park_data$park_id)
  park_effects <- data.frame(
    park_id = unique_parks,
    effect = apply(park_samples$park_effect, 2, mean),
    sd = apply(park_samples$park_effect, 2, sd),
    lower_ci = apply(park_samples$park_effect, 2, function(x) quantile(x, 0.025)),
    upper_ci = apply(park_samples$park_effect, 2, function(x) quantile(x, 0.975))
  )
  
  # Get park names
  park_names <- park_data %>%
    select(park_id, park) %>%
    distinct()
  
  # Join with park names
  park_effects <- park_effects %>%
    left_join(park_names, by = "park_id") %>%
    # Calculate park factors
    mutate(
      park_factor = exp(effect),
      lower_ci_factor = exp(lower_ci),
      upper_ci_factor = exp(upper_ci),
      significant = (lower_ci_factor > 1) | (upper_ci_factor < 1)
    ) %>%
    arrange(desc(park_factor))
  
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
      title = "Park Effects on Batting Performance (MCMC)",
      subtitle = "Values > 1 indicate hitter-friendly parks (with 95% credible intervals)",
      x = "Ballpark",
      y = "Park Factor (exp(effect))"
    ) +
    theme_minimal()
  
  # Calculate MCMC diagnostics
  message("Calculating MCMC diagnostics...")
  rhats <- summary(park_stanfit)$summary[, "Rhat"]
  n_eff <- summary(park_stanfit)$summary[, "n_eff"]
  
  # Create diagnostic dataframe
  diagnostics <- data.frame(
    metric = c("Max Rhat", "Min n_eff", "Computation Time"),
    value = c(max(rhats, na.rm = TRUE), min(n_eff, na.rm = TRUE), mcmc_time)
  )
  
  # Save results to files
  message("Saving park effects MCMC results...")
  
  # Create directories if they don't exist
  if (!dir.exists(data_path)) {
    dir.create(data_path, recursive = TRUE)
  }
  
  if (!dir.exists(results_path)) {
    dir.create(results_path, recursive = TRUE)
  }
  
  # Create MCMC results directory
  mcmc_dir <- file.path(results_path, "mcmc")
  if (!dir.exists(mcmc_dir)) {
    dir.create(mcmc_dir, recursive = TRUE)
  }
  
  # Save CSV files to results directory
  write.csv(park_effects, file.path(mcmc_dir, "park_effects_mcmc.csv"), row.names = FALSE)
  write.csv(diagnostics, file.path(mcmc_dir, "park_mcmc_diagnostics.csv"), row.names = FALSE)
  
  # Save plots to results directory
  ggsave(file.path(mcmc_dir, "park_effects_mcmc_plot.png"), park_plot, width = 12, height = 8)
  
  # Save trace plots
  trace_plot <- mcmc_trace(park_stanfit, regex_pars = "sigma")
  ggsave(file.path(mcmc_dir, "park_mcmc_trace.png"), trace_plot, width = 10, height = 6)
  
  # Save model as RDS for future use
  saveRDS(park_stanfit, file.path(data_path, "park_mcmc.rds"))
  
  # Create results list
  results <- list(
    stanfit = park_stanfit,
    park_effects = park_effects,
    diagnostics = diagnostics,
    park_plot = park_plot,
    computation_time = mcmc_time
  )
  
  # Save complete results object
  saveRDS(results, file.path(data_path, "park_mcmc_results.rds"))
  
  # Print summary
  message("\n========================================================")
  message("          PARK EFFECTS MCMC ANALYSIS RESULTS")
  message("========================================================\n")
  
  message("MCMC Diagnostics:")
  message(paste("Max Rhat:", round(max(rhats, na.rm = TRUE), 3)))
  message(paste("Min n_eff:", round(min(n_eff, na.rm = TRUE), 1)))
  message(paste("Computation Time:", round(mcmc_time, 2), "seconds"))
  
  top_parks <- head(park_effects %>% arrange(desc(park_factor)), 5)
  bottom_parks <- head(park_effects %>% arrange(park_factor), 5)
  
  message("\nTop 5 Hitter-Friendly Parks (MCMC):")
  for (i in 1:nrow(top_parks)) {
    message(sprintf("%d. %s: %.3f (%.3f-%.3f)", 
                    i, top_parks$park[i], 
                    top_parks$park_factor[i],
                    top_parks$lower_ci_factor[i],
                    top_parks$upper_ci_factor[i]))
  }
  
  message("\nTop 5 Pitcher-Friendly Parks (MCMC):")
  for (i in 1:nrow(bottom_parks)) {
    message(sprintf("%d. %s: %.3f (%.3f-%.3f)", 
                    i, bottom_parks$park[i], 
                    bottom_parks$park_factor[i],
                    bottom_parks$lower_ci_factor[i],
                    bottom_parks$upper_ci_factor[i]))
  }
  
  message("\nResults saved to: ", mcmc_dir)
  
  # Return the results
  return(results)
}

#=============================================
# AGING CURVES MCMC MODEL
#=============================================

run_aging_curves_mcmc <- function(player_trajectories, 
                                  data_path = "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Data",
                                  results_path = "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Results") {
  message("\n========================================================")
  message("          RUNNING AGING CURVES MCMC MODEL")
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
  
  # Create Stan model code
  stan_code <- "
  data {
    int<lower=0> N;             // number of observations
    int<lower=0> N_player;      // number of players
    vector[N] WAR_centered;     // centered WAR
    vector[N] age_centered;     // centered age
    vector[N] age_centered_sq;  // centered age squared
    int<lower=1> player_id[N];  // player IDs
    vector[N] AB_weight;        // at-bats for weighting
  }
  parameters {
    real intercept;
    real beta_age;              // linear age effect
    real beta_age_sq;           // quadratic age effect
    vector[N_player] player_effect;
    real<lower=0> sigma_player;
    real<lower=0> sigma_resid;
  }
  model {
    // Priors
    player_effect ~ normal(0, sigma_player);
    sigma_player ~ gamma(1, 0.05);  // match INLA prior
    sigma_resid ~ gamma(1, 0.05);   // match INLA prior
    
    // Likelihood with weights
    for (i in 1:N) {
      target += normal_lpdf(WAR_centered[i] | 
                  intercept + beta_age * age_centered[i] + 
                  beta_age_sq * age_centered_sq[i] + 
                  player_effect[player_id[i]], 
                  sigma_resid / sqrt(AB_weight[i])) * AB_weight[i];
    }
  }
  generated quantities {
    // Calculate peak age
    real peak_age_offset = -beta_age / (2 * beta_age_sq);
    real peak_age = 30 + peak_age_offset;
    
    // Calculate peak value
    real peak_value = intercept + beta_age * peak_age_offset + 
                      beta_age_sq * (peak_age_offset)^2;
                      
    // Calculate predicted values for different ages
    vector[18] age_effects;
    for (i in 1:18) {
      real age_i = i + 21;  // Ages 22 to 39
      real centered_i = age_i - 30;
      age_effects[i] = intercept + beta_age * centered_i + 
                      beta_age_sq * centered_i^2;
    }
  }
  "
  
  # Create data list for Stan
  stan_data <- list(
    N = nrow(player_trajectories),
    N_player = length(unique(player_trajectories$player_id)),
    WAR_centered = player_trajectories$WAR_centered,
    age_centered = player_trajectories$age_centered,
    age_centered_sq = player_trajectories$age_centered_sq,
    player_id = as.array(as.numeric(factor(player_trajectories$player_id, 
                                           levels = unique(player_trajectories$player_id)))),
    AB_weight = player_trajectories$AB / max(player_trajectories$AB)  # Normalize weights
  )
  
  # Record start time for benchmarking
  start_time <- Sys.time()
  
  # Fit Stan model
  message("Fitting Stan model...")
  aging_stanfit <- stan(
    model_code = stan_code,
    data = stan_data,
    iter = 2000,
    warmup = 1000,
    chains = 4,
    cores = parallel::detectCores(),
    control = list(adapt_delta = 0.9, max_treedepth = 12)
  )
  
  # Record end time
  end_time <- Sys.time()
  mcmc_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  message(paste("MCMC model fitting completed in", round(mcmc_time, 2), "seconds"))
  
  # Extract samples
  aging_samples <- extract(aging_stanfit)
  
  # Create ages dataframe for population curve
  ages <- data.frame(
    age = seq(min(player_trajectories$age), max(player_trajectories$age), by = 0.1)
  )
  
  # Center the ages
  ages$age_centered <- ages$age - 30
  ages$age_centered_sq <- ages$age_centered^2
  
  # Calculate the aging curve
  intercept_mean <- mean(aging_samples$intercept)
  beta_age_mean <- mean(aging_samples$beta_age)
  beta_age_sq_mean <- mean(aging_samples$beta_age_sq)
  
  ages$effect <- intercept_mean + 
    beta_age_mean * ages$age_centered +
    beta_age_sq_mean * ages$age_centered_sq
  
  # Extract peak age and decline rate
  peak_age <- mean(aging_samples$peak_age)
  
  # Calculate decline rate
  post_peak_age <- peak_age + 2
  post_peak_centered <- post_peak_age - 30
  decline_rate <- beta_age_mean + 2 * beta_age_sq_mean * post_peak_centered
  
  # Create aging curve plot
  aging_curve_plot <- ggplot() +
    # Add model-based curve
    geom_line(data = ages, 
              aes(x = age, y = effect), 
              size = 1.2, color = "blue") +
    # Add peak age marker
    geom_vline(xintercept = peak_age, 
               linetype = "dashed", color = "red") +
    # Add annotations
    annotate("text", x = peak_age + 0.5, y = max(ages$effect), 
             label = paste("Peak Age:", round(peak_age, 1)),
             hjust = 0, vjust = 1, size = 4) +
    # Add titles and labels
    labs(
      title = "Player Aging Curve for WAR (MCMC)",
      subtitle = paste0("Peak performance at age ", round(peak_age, 1), 
                        " with ", round(abs(decline_rate), 3), 
                        " WAR decline per year after peak"),
      x = "Age",
      y = "WAR Effect (centered)"
    ) +
    theme_minimal()
  
  # Calculate MCMC diagnostics
  message("Calculating MCMC diagnostics...")
  rhats <- summary(aging_stanfit)$summary[, "Rhat"]
  n_eff <- summary(aging_stanfit)$summary[, "n_eff"]
  
  # Create diagnostic dataframe
  diagnostics <- data.frame(
    metric = c("Max Rhat", "Min n_eff", "Computation Time", "Peak Age", "Decline Rate"),
    value = c(max(rhats, na.rm = TRUE), min(n_eff, na.rm = TRUE), 
              mcmc_time, peak_age, decline_rate)
  )
  
  # Create age effects summary
  age_effects_summary <- data.frame(
    age = c(22, 25, 27, 30, 33, 36, 39),
    effect = sapply(c(22, 25, 27, 30, 33, 36, 39), function(a) {
      a_centered <- a - 30
      intercept_mean + beta_age_mean * a_centered + beta_age_sq_mean * a_centered^2
    })
  )
  
  # Save results to files
  message("Saving aging curves MCMC results...")
  
  # Create MCMC results directory
  mcmc_dir <- file.path(results_path, "mcmc")
  if (!dir.exists(mcmc_dir)) {
    dir.create(mcmc_dir, recursive = TRUE)
  }
  
  # Save CSV files to results directory
  write.csv(ages, file.path(mcmc_dir, "aging_effects_mcmc.csv"), row.names = FALSE)
  write.csv(diagnostics, file.path(mcmc_dir, "aging_mcmc_diagnostics.csv"), row.names = FALSE)
  write.csv(age_effects_summary, file.path(mcmc_dir, "age_effects_summary_mcmc.csv"), row.names = FALSE)
  
  # Save plots to results directory
  ggsave(file.path(mcmc_dir, "aging_curve_mcmc_plot.png"), aging_curve_plot, width = 12, height = 8)
  
  # Save trace plots
  trace_plot <- mcmc_trace(aging_stanfit, pars = c("intercept", "beta_age", "beta_age_sq", 
                                                   "sigma_player", "sigma_resid"))
  ggsave(file.path(mcmc_dir, "aging_mcmc_trace.png"), trace_plot, width = 10, height = 8)
  
  # Save model as RDS for future use
  saveRDS(aging_stanfit, file.path(data_path, "aging_mcmc.rds"))
  
  # Create results list
  results <- list(
    stanfit = aging_stanfit,
    ages = ages,
    peak_age = peak_age,
    decline_rate = decline_rate,
    diagnostics = diagnostics,
    aging_curve_plot = aging_curve_plot,
    computation_time = mcmc_time
  )
  
  # Save complete results object
  saveRDS(results, file.path(data_path, "aging_mcmc_results.rds"))
  
  # Print summary
  message("\n========================================================")
  message("          AGING CURVES MCMC ANALYSIS RESULTS")
  message("========================================================\n")
  
  message("MCMC Diagnostics:")
  message(paste("Max Rhat:", round(max(rhats, na.rm = TRUE), 3)))
  message(paste("Min n_eff:", round(min(n_eff, na.rm = TRUE), 1)))
  message(paste("Computation Time:", round(mcmc_time, 2), "seconds"))
  
  message("\nPeak Performance Age (MCMC):", round(peak_age, 1))
  message("Rate of Performance Decline After Peak:", 
          sprintf("%.3f WAR per year", abs(decline_rate)))
  
  message("\nAge Effects Summary (relative to age 30):")
  ref_effect <- age_effects_summary$effect[age_effects_summary$age == 30]
  
  for (i in 1:nrow(age_effects_summary)) {
    message(sprintf("Age %2d: %+.3f WAR relative to age 30", 
                    age_effects_summary$age[i], 
                    age_effects_summary$effect[i] - ref_effect))
  }
  
  message("\nResults saved to: ", mcmc_dir)
  
  # Return the results
  return(results)
}

#=============================================
# COMPUTATIONAL EFFICIENCY COMPARISON
#=============================================

compare_computational_efficiency <- function(data_path = "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Data",
                                             results_path = "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Results") {
  message("\n========================================================")
  message("          COMPARING INLA AND MCMC COMPUTATIONAL EFFICIENCY")
  message("========================================================\n")
  
  # Create directory for efficiency results
  efficiency_dir <- file.path(results_path, "efficiency")
  if (!dir.exists(efficiency_dir)) {
    dir.create(efficiency_dir, recursive = TRUE)
  }
  
  # List of models to compare
  model_names <- c("Park Effects", "Aging Curves")
  
  # Load INLA results
  message("Loading INLA results...")
  inla_results <- list()
  
  # Park effects
  if (file.exists(file.path(data_path, "park_results.rds"))) {
    inla_results$park <- readRDS(file.path(data_path, "park_results.rds"))
  } else {
    message("Park effects INLA results not found.")
  }
  
  # Aging curves
  if (file.exists(file.path(data_path, "aging_results.rds"))) {
    inla_results$aging <- readRDS(file.path(data_path, "aging_results.rds"))
  } else {
    message("Aging curves INLA results not found.")
  }
  
  # Load MCMC results
  message("Loading MCMC results...")
  mcmc_results <- list()
  
  # Park effects
  if (file.exists(file.path(data_path, "park_mcmc_results.rds"))) {
    mcmc_results$park <- readRDS(file.path(data_path, "park_mcmc_results.rds"))
  } else {
    message("Park effects MCMC results not found.")
  }
  
  # Aging curves
  if (file.exists(file.path(data_path, "aging_mcmc_results.rds"))) {
    mcmc_results$aging <- readRDS(file.path(data_path, "aging_mcmc_results.rds"))
  } else {
    message("Aging curves MCMC results not found.")
  }
  
  # Compare computational times
  message("Comparing computational efficiency...")
  
  # Initialize results dataframe
  efficiency_df <- data.frame(
    model = model_names,
    inla_time = NA,
    mcmc_time = NA,
    speedup_factor = NA
  )
  
  # Compare park effects
  if (!is.null(inla_results$park) && !is.null(mcmc_results$park)) {
    efficiency_df$inla_time[1] <- inla_results$park$computation_time
    efficiency_df$mcmc_time[1] <- mcmc_results$park$computation_time
    efficiency_df$speedup_factor[1] <- mcmc_results$park$computation_time / inla_results$park$computation_time
  }
  
  # Compare aging curves
  if (!is.null(inla_results$aging) && !is.null(mcmc_results$aging)) {
    efficiency_df$inla_time[2] <- inla_results$aging$computation_time
    efficiency_df$mcmc_time[2] <- mcmc_results$aging$computation_time
    efficiency_df$speedup_factor[2] <- mcmc_results$aging$computation_time / inla_results$aging$computation_time
  }
  
  # Create comparison plot
  efficiency_plot <- ggplot(efficiency_df, aes(x = model)) +
    geom_bar(aes(y = inla_time, fill = "INLA"), stat = "identity", position = "dodge", alpha = 0.7) +
    geom_bar(aes(y = mcmc_time, fill = "MCMC"), stat = "identity", position = "dodge", alpha = 0.7) +
    geom_text(aes(y = mcmc_time, label = paste0(round(speedup_factor, 1), "x")), 
              vjust = -0.5, color = "darkred", size = 4) +
    scale_fill_manual(values = c("INLA" = "blue", "MCMC" = "red"), 
                      name = "Method") +
    labs(
      title = "Computational Efficiency: INLA vs MCMC",
      subtitle = "Numbers indicate how many times faster INLA is compared to MCMC",
      x = "Model Type",
      y = "Computation Time (seconds)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top"
    )
  
  # Create log-scale version of the comparison plot
  efficiency_plot_log <- ggplot(efficiency_df, aes(x = model)) +
    geom_bar(aes(y = inla_time, fill = "INLA"), stat = "identity", position = "dodge", alpha = 0.7) +
    geom_bar(aes(y = mcmc_time, fill = "MCMC"), stat = "identity", position = "dodge", alpha = 0.7) +
    geom_text(aes(y = mcmc_time, label = paste0(round(speedup_factor, 1), "x")), 
              vjust = -0.5, color = "darkred", size = 4) +
    scale_fill_manual(values = c("INLA" = "blue", "MCMC" = "red"), 
                      name = "Method") +
    scale_y_log10() +
    labs(
      title = "Computational Efficiency: INLA vs MCMC (Log Scale)",
      subtitle = "Numbers indicate how many times faster INLA is compared to MCMC",
      x = "Model Type",
      y = "Computation Time (seconds, log scale)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top"
    )
  
  # Compare accuracy of results
  accuracy_comparison <- data.frame(
    model = character(),
    parameter = character(),
    inla_value = numeric(),
    mcmc_value = numeric(),
    diff = numeric(),
    rel_diff_pct = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Compare park effects
  if (!is.null(inla_results$park) && !is.null(mcmc_results$park)) {
    # Join park effects from both methods for comparison
    inla_parks <- inla_results$park$park_effects %>%
      select(park_id, park, inla_effect = effect, inla_factor = park_factor)
    
    mcmc_parks <- mcmc_results$park$park_effects %>%
      select(park_id, park, mcmc_effect = effect, mcmc_factor = park_factor)
    
    park_comparison <- inla_parks %>%
      inner_join(mcmc_parks, by = c("park_id", "park")) %>%
      mutate(
        effect_diff = inla_effect - mcmc_effect,
        factor_diff = inla_factor - mcmc_factor,
        effect_rel_diff = abs(effect_diff / ((inla_effect + mcmc_effect) / 2)) * 100,
        factor_rel_diff = abs(factor_diff / ((inla_factor + mcmc_factor) / 2)) * 100
      )
    
    # Add to accuracy comparison
    accuracy_comparison <- rbind(
      accuracy_comparison,
      data.frame(
        model = "Park Effects",
        parameter = "Mean Park Effect",
        inla_value = mean(inla_parks$inla_effect),
        mcmc_value = mean(mcmc_parks$mcmc_effect),
        diff = mean(park_comparison$effect_diff),
        rel_diff_pct = mean(park_comparison$effect_rel_diff)
      )
    )
  }
  
  # Compare aging curves
  if (!is.null(inla_results$aging) && !is.null(mcmc_results$aging)) {
    accuracy_comparison <- rbind(
      accuracy_comparison,
      data.frame(
        model = "Aging Curves",
        parameter = "Peak Age",
        inla_value = inla_results$aging$peak_age,
        mcmc_value = mcmc_results$aging$peak_age,
        diff = inla_results$aging$peak_age - mcmc_results$aging$peak_age,
        rel_diff_pct = abs((inla_results$aging$peak_age - mcmc_results$aging$peak_age) / 
                             ((inla_results$aging$peak_age + mcmc_results$aging$peak_age) / 2)) * 100
      ),
      data.frame(
        model = "Aging Curves",
        parameter = "Decline Rate",
        inla_value = inla_results$aging$decline_rate,
        mcmc_value = mcmc_results$aging$decline_rate,
        diff = inla_results$aging$decline_rate - mcmc_results$aging$decline_rate,
        rel_diff_pct = abs((inla_results$aging$decline_rate - mcmc_results$aging$decline_rate) / 
                             ((inla_results$aging$decline_rate + mcmc_results$aging$decline_rate) / 2)) * 100
      )
    )
  }
  
  # Save results
  message("Saving efficiency comparison results...")
  write.csv(efficiency_df, file.path(efficiency_dir, "computational_efficiency.csv"), row.names = FALSE)
  write.csv(accuracy_comparison, file.path(efficiency_dir, "accuracy_comparison.csv"), row.names = FALSE)
  
  # Save plots
  ggsave(file.path(efficiency_dir, "efficiency_comparison.png"), efficiency_plot, width = 10, height = 8)
  ggsave(file.path(efficiency_dir, "efficiency_comparison_log.png"), efficiency_plot_log, width = 10, height = 8)
  
  # Create accuracy plot
  if (nrow(accuracy_comparison) > 0) {
    accuracy_plot <- ggplot(accuracy_comparison, aes(x = model, y = rel_diff_pct, fill = parameter)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = "Accuracy Comparison: INLA vs MCMC",
        subtitle = "Percentage difference between INLA and MCMC estimates",
        x = "Model Type",
        y = "Relative Difference (%)"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top"
      )
    
    ggsave(file.path(efficiency_dir, "accuracy_comparison.png"), accuracy_plot, width = 10, height = 8)
  }
  
  # Print summary
  message("\n========================================================")
  message("          COMPUTATIONAL EFFICIENCY COMPARISON RESULTS")
  message("========================================================\n")
  
  # Print efficiency comparison
  message("Computational Efficiency Summary:")
  for (i in 1:nrow(efficiency_df)) {
    if (!is.na(efficiency_df$speedup_factor[i])) {
      message(sprintf("%-20s: INLA: %8.2f sec, MCMC: %8.2f sec, Speedup: %5.1fx", 
                      efficiency_df$model[i], 
                      efficiency_df$inla_time[i],
                      efficiency_df$mcmc_time[i],
                      efficiency_df$speedup_factor[i]))
    }
  }
  
  # Print accuracy comparison
  if (nrow(accuracy_comparison) > 0) {
    message("\nAccuracy Comparison Summary:")
    for (i in 1:nrow(accuracy_comparison)) {
      message(sprintf("%-20s - %-25s: INLA: %8.3f, MCMC: %8.3f, Diff: %5.1f%%", 
                      accuracy_comparison$model[i], 
                      accuracy_comparison$parameter[i],
                      accuracy_comparison$inla_value[i],
                      accuracy_comparison$mcmc_value[i],
                      accuracy_comparison$rel_diff_pct[i]))
    }
  }
  
  message("\nResults saved to: ", efficiency_dir)
  
  return(list(
    efficiency = efficiency_df,
    accuracy = accuracy_comparison,
    efficiency_plot = efficiency_plot,
    efficiency_plot_log = efficiency_plot_log
  ))
}