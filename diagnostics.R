# diagnostics.R - Comprehensive Model diagnostics functionality for Baseball INLA Analysis

# Load required packages
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")

library(ggplot2)
library(dplyr)

#=============================================
# MODEL DIAGNOSTICS FUNCTION
#=============================================

run_model_diagnostics <- function(models_list, 
                                  data_path = "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Data",
                                  results_path = "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Results") {
  
  message("\n========================================================")
  message("          RUNNING COMPREHENSIVE MODEL DIAGNOSTICS")
  message("========================================================\n")
  
  # Create diagnostics directory
  diag_path <- file.path(results_path, "diagnostics")
  dir.create(diag_path, recursive = TRUE, showWarnings = FALSE)
  
  # Results storage
  all_diagnostics <- list()
  
  # For each model in the list
  for(model_name in names(models_list)) {
    if(is.null(models_list[[model_name]])) {
      message(paste("No model available for", model_name))
      next
    }
    
    message(paste("Running diagnostics for", model_name))
    model <- NULL
    
    # Extract the right model object based on structure
    if("model" %in% names(models_list[[model_name]])) {
      model <- models_list[[model_name]]$model
    } else if(!is.null(class(models_list[[model_name]])) && class(models_list[[model_name]])[1] == "inla") {
      model <- models_list[[model_name]]
    } else if("park_model" %in% names(models_list[[model_name]])) {
      model <- models_list[[model_name]]$park_model
    } else if("war_model" %in% names(models_list[[model_name]])) {
      model <- models_list[[model_name]]$war_model
    } else {
      message(paste("Could not find model object for", model_name))
      next
    }
    
    # Get basic model information with robust error checking
    model_info <- data.frame(
      model_name = model_name,
      dic = ifelse(!is.null(model$dic) && !is.null(model$dic$dic), model$dic$dic, NA),
      waic = ifelse(!is.null(model$waic) && !is.null(model$waic$waic), model$waic$waic, NA),
      mlik = ifelse(!is.null(model$mlik) && length(model$mlik) > 0, model$mlik[1], NA),
      family = ifelse(!is.null(model$family) && length(model$family) > 0, model$family[1], "unknown"),
      converged = !is.null(model$mode$convergence) && model$mode$convergence == 0,
      stringsAsFactors = FALSE
    )
    
    # Extract hyperparameters (precision parameters)
    if(!is.null(model$summary.hyperpar)) {
      hyperpars <- as.data.frame(model$summary.hyperpar)
      hyperpars$parameter <- rownames(model$summary.hyperpar)
      
      # Create a diagnostic plot for hyperparameters
      hyper_plot <- ggplot(hyperpars, aes(x = parameter, y = mean)) +
        geom_point(size = 3) +
        geom_errorbar(aes(ymin = `0.025quant`, ymax = `0.975quant`), width = 0.2) +
        coord_flip() +
        labs(
          title = paste("Hyperparameter Estimates for", model_name),
          subtitle = "Precision parameters with 95% credible intervals",
          x = NULL,
          y = "Precision (higher = less variance)"
        ) +
        theme_minimal()
      
      # Save plot
      ggsave(file.path(diag_path, paste0(model_name, "_hyperparameters.png")), 
             hyper_plot, width = 10, height = 8)
    }
    
    # Check fixed effects if available
    if(!is.null(model$summary.fixed)) {
      fixed_effects <- as.data.frame(model$summary.fixed)
      fixed_effects$parameter <- rownames(model$summary.fixed)
      
      # Filter out intercept for better scaling
      fixed_effects_no_int <- fixed_effects[fixed_effects$parameter != "(Intercept)", ]
      
      if(nrow(fixed_effects_no_int) > 0) {
        # Create a plot for fixed effects
        fixed_plot <- ggplot(fixed_effects_no_int, aes(x = parameter, y = mean)) +
          geom_point(size = 3) +
          geom_errorbar(aes(ymin = `0.025quant`, ymax = `0.975quant`), width = 0.2) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
          coord_flip() +
          labs(
            title = paste("Fixed Effects Estimates for", model_name),
            subtitle = "Coefficients with 95% credible intervals",
            x = NULL,
            y = "Effect Size"
          ) +
          theme_minimal()
        
        # Save plot
        ggsave(file.path(diag_path, paste0(model_name, "_fixed_effects.png")), 
               fixed_plot, width = 10, height = 8)
      }
    }
    
    # Check residuals if fitted values are available
    if(!is.null(model$summary.fitted.values)) {
      # Calculate Pearson residuals for all model types
      if(!is.null(model$summary.fitted.values$mean) && !is.null(model$summary.fitted.values$sd)) {
        # Get observations
        observations <- NULL
        
        # Extract observations based on model type/structure
        if(!is.null(model$data)) {
          if("H" %in% names(model$data)) {
            observations <- model$data$H
          } else if("WAR" %in% names(model$data)) {
            observations <- model$data$WAR
          } else if("WAR_centered" %in% names(model$data)) {
            observations <- model$data$WAR_centered
          } else if("outcome_count" %in% names(model$data)) {
            observations <- model$data$outcome_count
          }
        } 
        
        # Create residual data if we have observations
        if(!is.null(observations) && length(observations) > 0) {
          # Get fitted values
          fitted_vals <- model$summary.fitted.values$mean
          
          # Ensure fitted values are the same length as observations
          if(length(fitted_vals) == length(observations)) {
            sd_fitted <- model$summary.fitted.values$sd
            
            # Calculate residuals
            resids <- as.numeric(observations) - fitted_vals
            
            # Create residual plot data
            resid_data <- data.frame(
              fitted = fitted_vals,
              residual = resids,
              pearson_residual = resids / sd_fitted,
              observation = observations
            )
            
            # Create residual plots
            resid_plot <- ggplot(resid_data, aes(x = fitted, y = residual)) +
              geom_point(alpha = 0.5) +
              geom_smooth(method = "loess", se = FALSE, color = "red") +
              geom_hline(yintercept = 0, linetype = "dashed") +
              labs(
                title = paste("Residual Diagnostics for", model_name),
                subtitle = "Residuals vs Fitted Values",
                x = "Fitted Values",
                y = "Residuals"
              ) +
              theme_minimal()
            
            pearson_plot <- ggplot(resid_data, aes(x = fitted, y = pearson_residual)) +
              geom_point(alpha = 0.5) +
              geom_smooth(method = "loess", se = FALSE, color = "red") +
              geom_hline(yintercept = 0, linetype = "dashed") +
              labs(
                title = paste("Pearson Residual Diagnostics for", model_name),
                subtitle = "Pearson Residuals vs Fitted Values",
                x = "Fitted Values",
                y = "Pearson Residuals"
              ) +
              theme_minimal() +
              ylim(-5, 5) # Limit outliers for better visualization
            
            # Create a residual histogram
            resid_hist <- ggplot(resid_data, aes(x = pearson_residual)) +
              geom_histogram(bins = 30, fill = "steelblue") +
              geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
              labs(
                title = paste("Distribution of Pearson Residuals for", model_name),
                x = "Pearson Residual",
                y = "Count"
              ) +
              theme_minimal() +
              xlim(-5, 5) # Limit outliers for better visualization
            
            # Save plots
            ggsave(file.path(diag_path, paste0(model_name, "_residuals.png")), 
                   resid_plot, width = 10, height = 8)
            ggsave(file.path(diag_path, paste0(model_name, "_pearson_residuals.png")), 
                   pearson_plot, width = 10, height = 8)
            ggsave(file.path(diag_path, paste0(model_name, "_residual_hist.png")), 
                   resid_hist, width = 10, height = 8)
          }
        }
      }
    }
    
    # Extract random effects if available
    random_effects_data <- data.frame()
    
    if(!is.null(model$summary.random)) {
      random_effects_summary <- list()
      
      for(random_effect in names(model$summary.random)) {
        if(!is.null(model$summary.random[[random_effect]])) {
          random_effect_df <- as.data.frame(model$summary.random[[random_effect]])
          
          # Add random effect name
          random_effect_df$effect_name <- random_effect
          random_effect_df$ID_factor <- as.factor(random_effect_df$ID)
          
          # Append to the random effects data if it has rows
          if(nrow(random_effect_df) > 0) {
            random_effects_data <- rbind(random_effects_data, random_effect_df)
            
            # Calculate summary statistics for this random effect
            random_effects_summary[[random_effect]] <- list(
              mean_variance = var(random_effect_df$mean, na.rm = TRUE),
              median_abs_effect = median(abs(random_effect_df$mean), na.rm = TRUE),
              max_abs_effect = max(abs(random_effect_df$mean), na.rm = TRUE)
            )
          }
        }
      }
      
      # Create a summary plot of random effects variances
      if(length(random_effects_summary) > 0) {
        # Convert summary to data frame
        random_var_summary <- data.frame(
          effect_name = names(random_effects_summary),
          mean_variance = sapply(random_effects_summary, function(x) x$mean_variance),
          median_abs_effect = sapply(random_effects_summary, function(x) x$median_abs_effect),
          max_abs_effect = sapply(random_effects_summary, function(x) x$max_abs_effect)
        )
        
        # Create plot
        random_var_plot <- ggplot(random_var_summary, 
                                  aes(x = reorder(effect_name, mean_variance), 
                                      y = mean_variance, fill = effect_name)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          labs(
            title = paste("Random Effects Variance for", model_name),
            subtitle = "Empirical variance of random effects by type",
            x = NULL,
            y = "Variance"
          ) +
          theme_minimal() +
          theme(legend.position = "none")
        
        # Save plot
        ggsave(file.path(diag_path, paste0(model_name, "_random_effects_variance.png")), 
               random_var_plot, width = 10, height = 8)
      }
    }
    
    # Store diagnostics
    all_diagnostics[[model_name]] <- list(
      model_info = model_info,
      hyperparameters = if(exists("hyperpars") && !is.null(hyperpars)) hyperpars else NULL,
      fixed_effects = if(exists("fixed_effects") && !is.null(fixed_effects)) fixed_effects else NULL,
      random_effects_summary = if(exists("random_var_summary") && !is.null(random_var_summary)) random_var_summary else NULL
    )
  }
  
  # Create summary table
  if(length(all_diagnostics) > 0) {
    # Extract model_info dataframes from each diagnostic result
    model_summaries <- lapply(all_diagnostics, function(x) x$model_info)
    
    # Combine into a single dataframe
    model_summary <- do.call(rbind, model_summaries)
    
    # Write to CSV
    write.csv(model_summary, file.path(diag_path, "model_summary.csv"), row.names = FALSE)
    
    # Create comparison plot for DIC (if we have multiple models)
    if(nrow(model_summary) > 1 && sum(!is.na(model_summary$dic)) > 1) {
      # Filter out NA DIC values
      model_summary_valid <- model_summary[!is.na(model_summary$dic),]
      
      comparison_plot <- ggplot(model_summary_valid, 
                                aes(x = reorder(model_name, dic), y = dic, fill = converged)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_manual(values = c("red", "green"), name = "Converged") +
        labs(
          title = "Model Comparison by DIC",
          subtitle = "Lower DIC indicates better model fit",
          x = NULL,
          y = "DIC (Deviance Information Criterion)"
        ) +
        theme_minimal()
      
      ggsave(file.path(diag_path, "model_comparison_dic.png"), comparison_plot, width = 10, height = 8)
      
      # Also create WAIC comparison if available
      if(sum(!is.na(model_summary$waic)) > 1) {
        # Filter out NA WAIC values
        model_summary_waic <- model_summary[!is.na(model_summary$waic),]
        
        waic_plot <- ggplot(model_summary_waic, 
                            aes(x = reorder(model_name, waic), y = waic, fill = converged)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          scale_fill_manual(values = c("red", "green"), name = "Converged") +
          labs(
            title = "Model Comparison by WAIC",
            subtitle = "Lower WAIC indicates better model fit",
            x = NULL,
            y = "WAIC (Widely Applicable Information Criterion)"
          ) +
          theme_minimal()
        
        ggsave(file.path(diag_path, "model_comparison_waic.png"), waic_plot, width = 10, height = 8)
      }
    }
  }
  
  # Generate comprehensive HTML report
  html_content <- c(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    "<title>INLA Baseball Analytics Model Diagnostics</title>",
    "<style>",
    "body { font-family: Arial, sans-serif; max-width: 1200px; margin: 0 auto; padding: 20px; }",
    "h1, h2, h3 { color: #333366; }",
    "table { border-collapse: collapse; width: 100%; margin-bottom: 20px; }",
    "th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }",
    "th { background-color: #f2f2f2; }",
    "tr:nth-child(even) { background-color: #f9f9f9; }",
    ".model-section { margin-bottom: 40px; padding: 20px; border: 1px solid #ddd; border-radius: 5px; }",
    ".img-container { margin: 20px 0; text-align: center; }",
    ".img-container img { max-width: 100%; height: auto; border: 1px solid #ddd; }",
    "</style>",
    "</head>",
    "<body>",
    "<h1>INLA Baseball Analytics Model Diagnostics</h1>",
    paste("<p>Report generated on:", Sys.time(), "</p>")
  )
  
  # Add model summary table
  if(exists("model_summary") && nrow(model_summary) > 0) {
    html_content <- c(html_content,
                      "<h2>Model Comparison Summary</h2>",
                      "<table>",
                      "<tr><th>Model</th><th>DIC</th><th>WAIC</th><th>Family</th><th>Converged</th></tr>")
    
    for(i in 1:nrow(model_summary)) {
      html_content <- c(html_content,
                        paste0("<tr>",
                               "<td>", model_summary$model_name[i], "</td>",
                               "<td>", round(model_summary$dic[i], 2), "</td>",
                               "<td>", round(model_summary$waic[i], 2), "</td>",
                               "<td>", model_summary$family[i], "</td>",
                               "<td>", ifelse(model_summary$converged[i], "Yes", "No"), "</td>",
                               "</tr>"))
    }
    
    html_content <- c(html_content, "</table>")
    
    # Add comparison plots
    if(file.exists(file.path(diag_path, "model_comparison_dic.png"))) {
      html_content <- c(html_content,
                        "<div class='img-container'>",
                        paste0("<img src='model_comparison_dic.png' alt='DIC Comparison'>"),
                        "</div>")
    }
    
    if(file.exists(file.path(diag_path, "model_comparison_waic.png"))) {
      html_content <- c(html_content,
                        "<div class='img-container'>",
                        paste0("<img src='model_comparison_waic.png' alt='WAIC Comparison'>"),
                        "</div>")
    }
  }
  
  # Add individual model details
  for(model_name in names(all_diagnostics)) {
    model_diag <- all_diagnostics[[model_name]]
    
    html_content <- c(html_content,
                      paste0("<div class='model-section'>"),
                      paste0("<h2>Model: ", model_name, "</h2>"))
    
    # Add hyperparameters table
    if(!is.null(model_diag$hyperparameters) && nrow(model_diag$hyperparameters) > 0) {
      html_content <- c(html_content,
                        "<h3>Hyperparameters (Precision Parameters)</h3>",
                        "<table>",
                        "<tr><th>Parameter</th><th>Mean</th><th>2.5% Quantile</th><th>97.5% Quantile</th></tr>")
      
      for(i in 1:nrow(model_diag$hyperparameters)) {
        html_content <- c(html_content,
                          paste0("<tr>",
                                 "<td>", model_diag$hyperparameters$parameter[i], "</td>",
                                 "<td>", round(model_diag$hyperparameters$mean[i], 4), "</td>",
                                 "<td>", round(model_diag$hyperparameters$`0.025quant`[i], 4), "</td>",
                                 "<td>", round(model_diag$hyperparameters$`0.975quant`[i], 4), "</td>",
                                 "</tr>"))
      }
      
      html_content <- c(html_content, "</table>")
      
      # Add hyperparameter plot
      html_content <- c(html_content,
                        "<div class='img-container'>",
                        paste0("<img src='", model_name, "_hyperparameters.png' alt='Hyperparameters'>"),
                        "</div>")
    }
    
    # Add fixed effects table
    if(!is.null(model_diag$fixed_effects) && nrow(model_diag$fixed_effects) > 0) {
      html_content <- c(html_content,
                        "<h3>Fixed Effects</h3>",
                        "<table>",
                        "<tr><th>Parameter</th><th>Mean</th><th>2.5% Quantile</th><th>97.5% Quantile</th></tr>")
      
      for(i in 1:nrow(model_diag$fixed_effects)) {
        html_content <- c(html_content,
                          paste0("<tr>",
                                 "<td>", model_diag$fixed_effects$parameter[i], "</td>",
                                 "<td>", round(model_diag$fixed_effects$mean[i], 4), "</td>",
                                 "<td>", round(model_diag$fixed_effects$`0.025quant`[i], 4), "</td>",
                                 "<td>", round(model_diag$fixed_effects$`0.975quant`[i], 4), "</td>",
                                 "</tr>"))
      }
      
      html_content <- c(html_content, "</table>")
      
      # Add fixed effects plot if it exists
      if(file.exists(file.path(diag_path, paste0(model_name, "_fixed_effects.png")))) {
        html_content <- c(html_content,
                          "<div class='img-container'>",
                          paste0("<img src='", model_name, "_fixed_effects.png' alt='Fixed Effects'>"),
                          "</div>")
      }
    }
    
    # Add residual plots if they exist
    if(file.exists(file.path(diag_path, paste0(model_name, "_residuals.png")))) {
      html_content <- c(html_content,
                        "<h3>Residual Diagnostics</h3>",
                        "<div class='img-container'>",
                        paste0("<img src='", model_name, "_residuals.png' alt='Residuals'>"),
                        "</div>")
    }
    
    if(file.exists(file.path(diag_path, paste0(model_name, "_pearson_residuals.png")))) {
      html_content <- c(html_content,
                        "<div class='img-container'>",
                        paste0("<img src='", model_name, "_pearson_residuals.png' alt='Pearson Residuals'>"),
                        "</div>")
    }
    
    if(file.exists(file.path(diag_path, paste0(model_name, "_residual_hist.png")))) {
      html_content <- c(html_content,
                        "<div class='img-container'>",
                        paste0("<img src='", model_name, "_residual_hist.png' alt='Residual Histogram'>"),
                        "</div>")
    }
    
    # Add random effects variance plot if it exists
    if(file.exists(file.path(diag_path, paste0(model_name, "_random_effects_variance.png")))) {
      html_content <- c(html_content,
                        "<h3>Random Effects Variance</h3>",
                        "<div class='img-container'>",
                        paste0("<img src='", model_name, "_random_effects_variance.png' alt='Random Effects Variance'>"),
                        "</div>")
    }
    
    html_content <- c(html_content, "</div>")
  }
  
  # Close HTML
  html_content <- c(html_content, "</body>", "</html>")
  
  # Write HTML report
  writeLines(html_content, file.path(diag_path, "diagnostics_report.html"))
  
  message("Model diagnostics complete. Results saved to:", diag_path)
  message("HTML report generated at:", file.path(diag_path, "diagnostics_report.html"))
  
  return(all_diagnostics)
}

# Export the function to the global environment
assign("run_model_diagnostics", run_model_diagnostics, envir = .GlobalEnv)

# Example usage in master_analysis.R - add after all models are run:
# models_list <- list(
#   park_effects = park_results,
#   aging_curves = aging_results,
#   spatial = spatial_results,
#   environmental = environmental_results
# )
# diagnostics <- run_model_diagnostics(models_list, data_path, results_path)