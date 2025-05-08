# Script to create comprehensive analysis of INLA vs MCMC comparison
# This script creates additional visualizations and summaries of the efficiency comparison

# Load required packages
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("reshape2")) install.packages("reshape2")

library(ggplot2)
library(dplyr)
library(gridExtra)
library(reshape2)

analyze_efficiency_comparison <- function(data_path = "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Data",
                                          results_path = "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Results") {
  
  message("\n========================================================")
  message("          ANALYZING INLA VS MCMC COMPARISON RESULTS")
  message("========================================================\n")
  
  # Create analysis directory
  analysis_dir <- file.path(results_path, "analysis")
  if (!dir.exists(analysis_dir)) {
    dir.create(analysis_dir, recursive = TRUE)
  }
  
  # Load necessary data
  efficiency_path <- file.path(results_path, "efficiency")
  efficiency_data_path <- file.path(efficiency_path, "computational_efficiency.csv")
  accuracy_data_path <- file.path(efficiency_path, "accuracy_comparison.csv")
  
  if (!file.exists(efficiency_data_path) || !file.exists(accuracy_data_path)) {
    # Try to load from data directory
    eff_results_path <- file.path(data_path, "efficiency_results.rds")
    if (file.exists(eff_results_path)) {
      message("Loading efficiency results from: ", eff_results_path)
      eff_results <- readRDS(eff_results_path)
      
      # Extract data frames
      if ("efficiency" %in% names(eff_results)) {
        efficiency_data <- eff_results$efficiency
      } else {
        stop("Could not find efficiency data in the loaded results")
      }
      
      if ("accuracy" %in% names(eff_results)) {
        accuracy_data <- eff_results$accuracy
      } else {
        stop("Could not find accuracy data in the loaded results")
      }
    } else {
      stop("Could not find efficiency comparison data. Please run the comparison first.")
    }
  } else {
    # Load from CSV files
    message("Loading efficiency data from: ", efficiency_data_path)
    efficiency_data <- read.csv(efficiency_data_path)
    
    message("Loading accuracy data from: ", accuracy_data_path)
    accuracy_data <- read.csv(accuracy_data_path)
  }
  
  # 1. Create enhanced efficiency visualization
  message("Creating enhanced efficiency visualization...")
  
  # Calculate mean and median speedup
  mean_speedup <- mean(efficiency_data$speedup_factor, na.rm = TRUE)
  median_speedup <- median(efficiency_data$speedup_factor, na.rm = TRUE)
  
  # Create enhanced plot with both linear and log scales
  efficiency_enhanced <- ggplot(efficiency_data, aes(x = reorder(model, -speedup_factor))) +
    geom_bar(aes(y = mcmc_time, fill = "MCMC"), stat = "identity", alpha = 0.7) +
    geom_bar(aes(y = inla_time, fill = "INLA"), stat = "identity", alpha = 0.7) +
    geom_text(aes(y = mcmc_time, label = paste0(round(speedup_factor, 1), "x")), 
              vjust = -0.5, size = 5, fontface = "bold") +
    geom_hline(yintercept = median(efficiency_data$inla_time, na.rm = TRUE), 
               linetype = "dashed", color = "blue", alpha = 0.7) +
    geom_hline(yintercept = median(efficiency_data$mcmc_time, na.rm = TRUE), 
               linetype = "dashed", color = "red", alpha = 0.7) +
    scale_fill_manual(values = c("INLA" = "blue", "MCMC" = "red"), 
                      name = "Method") +
    labs(
      title = "Computational Efficiency: INLA vs MCMC",
      subtitle = paste0("Average speedup: ", round(mean_speedup, 1), 
                        "x (median: ", round(median_speedup, 1), "x)"),
      x = "Model Type",
      y = "Computation Time (seconds)",
      caption = "Dashed lines represent median computation times"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      legend.position = "top",
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
  
  # Log scale version
  efficiency_enhanced_log <- efficiency_enhanced +
    scale_y_log10() +
    labs(
      title = "Computational Efficiency: INLA vs MCMC (Log Scale)",
      y = "Computation Time (seconds, log scale)"
    )
  
  # Save enhanced plots
  ggsave(file.path(analysis_dir, "efficiency_enhanced.png"), 
         efficiency_enhanced, width = 12, height = 8)
  ggsave(file.path(analysis_dir, "efficiency_enhanced_log.png"), 
         efficiency_enhanced_log, width = 12, height = 8)
  
  # 2. Create model-by-model efficiency comparison
  message("Creating model-by-model efficiency comparison...")
  
  # Prepare data for detailed comparison
  efficiency_melted <- melt(efficiency_data, 
                            id.vars = c("model", "speedup_factor"),
                            measure.vars = c("inla_time", "mcmc_time"),
                            variable.name = "method", 
                            value.name = "time")
  
  # Reformat method names
  efficiency_melted$method <- ifelse(efficiency_melted$method == "inla_time", "INLA", "MCMC")
  
  # Create comparison plot
  model_comparison <- ggplot(efficiency_melted, 
                             aes(x = model, y = time, fill = method)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = ifelse(method == "MCMC", 
                                 paste0(round(speedup_factor, 1), "x"), "")),
              position = position_dodge(width = 0.9),
              vjust = -0.5, size = 4) +
    scale_fill_manual(values = c("INLA" = "blue", "MCMC" = "red"), 
                      name = "Method") +
    labs(
      title = "Model-by-Model Efficiency Comparison",
      x = "Model Type",
      y = "Computation Time (seconds)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top"
    )
  
  # Save comparison plot
  ggsave(file.path(analysis_dir, "model_comparison.png"), 
         model_comparison, width = 12, height = 8)
  
  # 3. Create accuracy comparison visualization
  message("Creating comprehensive accuracy analysis...")
  
  # Check if accuracy data exists
  if (nrow(accuracy_data) > 0) {
    # Calculate mean difference by model
    model_accuracy <- accuracy_data %>%
      group_by(model) %>%
      summarize(
        mean_diff_pct = mean(rel_diff_pct, na.rm = TRUE),
        max_diff_pct = max(rel_diff_pct, na.rm = TRUE),
        min_diff_pct = min(rel_diff_pct, na.rm = TRUE),
        parameters = n()
      )
    
    # Create accuracy bar plot
    accuracy_by_model <- ggplot(model_accuracy, aes(x = reorder(model, mean_diff_pct), 
                                                    y = mean_diff_pct, fill = mean_diff_pct)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(round(mean_diff_pct, 1), "%")), 
                vjust = -0.5, size = 4) +
      geom_errorbar(aes(ymin = min_diff_pct, ymax = max_diff_pct), width = 0.2) +
      scale_fill_gradient(low = "green", high = "orange", name = "Mean Difference (%)") +
      labs(
        title = "Accuracy Comparison by Model",
        subtitle = "Average parameter difference between INLA and MCMC estimates",
        x = "Model Type",
        y = "Parameter Difference (%)",
        caption = "Error bars show min/max parameter differences"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    # Create detailed parameter comparison
    parameter_accuracy <- ggplot(accuracy_data, 
                                 aes(x = reorder(parameter, rel_diff_pct), 
                                     y = rel_diff_pct, fill = model)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(round(rel_diff_pct, 1), "%")), 
                hjust = -0.1, size = 3) +
      geom_hline(yintercept = mean(accuracy_data$rel_diff_pct), 
                 linetype = "dashed", color = "red") +
      annotate("text", x = 1, y = mean(accuracy_data$rel_diff_pct) * 1.1, 
               label = paste0("Mean: ", round(mean(accuracy_data$rel_diff_pct), 1), "%"), 
               color = "red", hjust = 0) +
      coord_flip() +
      labs(
        title = "Parameter-by-Parameter Accuracy Comparison",
        subtitle = "Percentage difference between INLA and MCMC estimates",
        x = "Parameter",
        y = "Difference (%)"
      ) +
      theme_minimal()
    
    # Save accuracy plots
    ggsave(file.path(analysis_dir, "accuracy_by_model.png"), 
           accuracy_by_model, width = 10, height = 8)
    ggsave(file.path(analysis_dir, "parameter_accuracy.png"), 
           parameter_accuracy, width = 12, height = 8)
    
    # 4. Create value comparison plots
    message("Creating parameter value comparison plots...")
    
    # Long format data for value comparison
    value_data <- accuracy_data %>%
      select(model, parameter, inla_value, mcmc_value) %>%
      melt(id.vars = c("model", "parameter"), 
           variable.name = "method", value.name = "value") %>%
      mutate(method = ifelse(method == "inla_value", "INLA", "MCMC"))
    
    # Create comparison plot
    value_comparison <- ggplot(value_data, aes(x = parameter, y = value, fill = method)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~model, scales = "free", ncol = 1) +
      scale_fill_manual(values = c("INLA" = "blue", "MCMC" = "red"), 
                        name = "Method") +
      labs(
        title = "Parameter Value Comparison",
        subtitle = "Absolute parameter values by method",
        x = NULL,
        y = "Parameter Value"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(fill = "lightgray"),
        strip.text = element_text(face = "bold")
      )
    
    # Save value comparison plot
    ggsave(file.path(analysis_dir, "value_comparison.png"), 
           value_comparison, width = 12, height = 10)
    
    # 5. Create scatter plot of parameter values
    value_wide <- value_data %>%
      dcast(model + parameter ~ method, value.var = "value") %>%
      mutate(
        abs_diff = abs(INLA - MCMC),
        rel_diff = abs_diff / ((INLA + MCMC) / 2) * 100
      )
    
    value_scatter <- ggplot(value_wide, aes(x = INLA, y = MCMC, color = model)) +
      geom_point(size = 3, alpha = 0.8) +
      geom_text(aes(label = parameter), vjust = -0.8, hjust = 0.5, size = 3) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
      labs(
        title = "INLA vs MCMC Parameter Values",
        subtitle = "Perfect agreement would fall on the dashed line",
        x = "INLA Parameter Value",
        y = "MCMC Parameter Value"
      ) +
      theme_minimal() +
      scale_color_brewer(palette = "Set1", name = "Model")
    
    # Save scatter plot
    ggsave(file.path(analysis_dir, "value_scatter.png"), 
           value_scatter, width = 10, height = 8)
  }
  
  # 6. Create combined efficiency and accuracy metrics
  message("Creating combined efficiency-accuracy analysis...")
  
  # Calculate model-level metrics
  if (nrow(accuracy_data) > 0) {
    combined_metrics <- efficiency_data %>%
      select(model, speedup_factor) %>%
      left_join(
        accuracy_data %>%
          group_by(model) %>%
          summarize(mean_diff_pct = mean(rel_diff_pct, na.rm = TRUE)),
        by = "model"
      )
    
    # Create combined plot
    combined_plot <- ggplot(combined_metrics, 
                            aes(x = speedup_factor, y = mean_diff_pct, color = model)) +
      geom_point(size = 5, alpha = 0.8) +
      geom_text(aes(label = model), vjust = -1.2, hjust = 0.5, size = 3.5) +
      labs(
        title = "Efficiency vs Accuracy Trade-off",
        subtitle = "Ideal models have high speedup (x-axis) and low difference (y-axis)",
        x = "Speedup Factor (×)",
        y = "Mean Parameter Difference (%)"
      ) +
      theme_minimal() +
      scale_color_brewer(palette = "Set1", name = "Model") +
      scale_x_continuous(expand = c(0.1, 0)) +
      scale_y_continuous(expand = c(0.1, 0))
    
    # Save combined plot
    ggsave(file.path(analysis_dir, "efficiency_accuracy_tradeoff.png"), 
           combined_plot, width = 10, height = 8)
  }
  
  # 7. Create summary report table
  message("Creating summary report...")
  
  # Overall summary statistics
  summary_stats <- data.frame(
    metric = c(
      "Average Speedup Factor",
      "Maximum Speedup Factor",
      "Minimum Speedup Factor",
      "Average INLA Computation Time (sec)",
      "Average MCMC Computation Time (sec)"
    ),
    value = c(
      mean(efficiency_data$speedup_factor, na.rm = TRUE),
      max(efficiency_data$speedup_factor, na.rm = TRUE),
      min(efficiency_data$speedup_factor, na.rm = TRUE),
      mean(efficiency_data$inla_time, na.rm = TRUE),
      mean(efficiency_data$mcmc_time, na.rm = TRUE)
    )
  )
  
  if (nrow(accuracy_data) > 0) {
    accuracy_stats <- data.frame(
      metric = c(
        "Average Parameter Difference (%)",
        "Maximum Parameter Difference (%)",
        "Minimum Parameter Difference (%)",
        "Number of Parameters Compared"
      ),
      value = c(
        mean(accuracy_data$rel_diff_pct, na.rm = TRUE),
        max(accuracy_data$rel_diff_pct, na.rm = TRUE),
        min(accuracy_data$rel_diff_pct, na.rm = TRUE),
        nrow(accuracy_data)
      )
    )
    
    summary_stats <- rbind(summary_stats, accuracy_stats)
  }
  
  # Save summary stats to CSV
  write.csv(summary_stats, file.path(analysis_dir, "summary_statistics.csv"), 
            row.names = FALSE)
  
  # Create model-specific summary
  model_summary <- efficiency_data %>%
    select(model, inla_time, mcmc_time, speedup_factor)
  
  if (nrow(accuracy_data) > 0) {
    model_accuracy_summary <- accuracy_data %>%
      group_by(model) %>%
      summarize(
        mean_diff_pct = mean(rel_diff_pct, na.rm = TRUE),
        max_diff_pct = max(rel_diff_pct, na.rm = TRUE),
        parameters = n()
      )
    
    model_summary <- model_summary %>%
      left_join(model_accuracy_summary, by = "model")
  }
  
  # Save model summary to CSV
  write.csv(model_summary, file.path(analysis_dir, "model_summary.csv"), 
            row.names = FALSE)
  
  # 8. Print text summary to console
  message("\n========================================================")
  message("          EFFICIENCY AND ACCURACY SUMMARY")
  message("========================================================\n")
  
  message("Efficiency Summary:")
  for (i in 1:nrow(summary_stats)) {
    if (grepl("Speedup|Time", summary_stats$metric[i])) {
      message(sprintf("%-35s: %10.2f", 
                      summary_stats$metric[i], 
                      summary_stats$value[i]))
    }
  }
  
  if (nrow(accuracy_data) > 0) {
    message("\nAccuracy Summary:")
    for (i in 1:nrow(summary_stats)) {
      if (grepl("Difference|Parameters", summary_stats$metric[i])) {
        if (grepl("Difference", summary_stats$metric[i])) {
          message(sprintf("%-35s: %10.2f%%", 
                          summary_stats$metric[i], 
                          summary_stats$value[i]))
        } else {
          message(sprintf("%-35s: %10.0f", 
                          summary_stats$metric[i], 
                          summary_stats$value[i]))
        }
      }
    }
  }
  
  message("\nModel-Specific Summary:")
  for (i in 1:nrow(model_summary)) {
    message(sprintf("\n%s:", model_summary$model[i]))
    message(sprintf("  INLA Time: %.2f sec, MCMC Time: %.2f sec", 
                    model_summary$inla_time[i], 
                    model_summary$mcmc_time[i]))
    message(sprintf("  Speedup Factor: %.1fx", 
                    model_summary$speedup_factor[i]))
    
    if (!is.null(model_summary$mean_diff_pct) && !is.na(model_summary$mean_diff_pct[i])) {
      message(sprintf("  Mean Parameter Difference: %.2f%%", 
                      model_summary$mean_diff_pct[i]))
    }
  }
  
  message("\nAnalysis results saved to: ", analysis_dir)
  
  # Return analysis results
  invisible(list(
    summary_stats = summary_stats,
    model_summary = model_summary,
    plots = list(
      efficiency_enhanced = efficiency_enhanced,
      efficiency_enhanced_log = efficiency_enhanced_log,
      model_comparison = model_comparison,
      accuracy_by_model = if(exists("accuracy_by_model")) accuracy_by_model else NULL,
      parameter_accuracy = if(exists("parameter_accuracy")) parameter_accuracy else NULL,
      value_comparison = if(exists("value_comparison")) value_comparison else NULL,
      value_scatter = if(exists("value_scatter")) value_scatter else NULL,
      combined_plot = if(exists("combined_plot")) combined_plot else NULL
    )
  ))
}

# Run the analysis if executed directly
if (interactive()) {
  # Specify paths
  data_path <- "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Data"
  results_path <- "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Results"
  
  # Run analysis
  results <- analyze_efficiency_comparison(data_path, results_path)
}