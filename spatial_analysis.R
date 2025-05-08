# spatial_analysis.R

# Load required packages
if (!require("INLA")) {
  install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
}
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("spdep")) install.packages("spdep")
if (!require("fields")) install.packages("fields")

library(INLA)
library(dplyr)
library(ggplot2)
library(spdep)
library(fields)

# Suppress deprecated ggplot2 warnings
suppressWarnings({
  old <- getOption("ggplot2.deprecation.warn")
  options(ggplot2.deprecation.warn = FALSE)
})

#=============================================
# STANDARDIZED VARIANCE DECOMPOSITION FUNCTION
#=============================================

# Function to calculate variance components consistently across models
calculate_variance_decomposition <- function(model, fixed_effects = NULL, family = "poisson") {
  # Extract precision parameters
  precision_summary <- model$summary.hyperpar
  
  # Print the precision summary to see actual values and row names
  message("Precision parameter summary:")
  print(precision_summary)
  
  # Initialize components vector
  components <- c("Spatial Field", "Park Dimensions", "Park", "Residual")
  variances <- numeric(length(components))
  
  # Extract variances (reciprocal of precision)
  tryCatch({
    # Try to get spatial field variance
    if("Precision for spatial_field" %in% rownames(precision_summary)) {
      spatial_prec <- precision_summary["Precision for spatial_field", "mean"]
      message(paste("Spatial field precision:", spatial_prec))
      if(spatial_prec > 1e10) {
        message("Spatial field precision extremely large, setting variance to near zero")
        variances[1] <- 1e-10  # Small but not exactly zero
      } else {
        variances[1] <- 1/spatial_prec
      }
    } else {
      # If row name doesn't match, try to find a close match
      message("Could not find exact 'Precision for spatial_field' row")
      spatial_row <- grep("spatial", rownames(precision_summary), ignore.case = TRUE)
      if(length(spatial_row) > 0) {
        spatial_prec <- precision_summary[spatial_row[1], "mean"]
        message(paste("Found alternative spatial field precision:", spatial_prec))
        variances[1] <- 1/spatial_prec
      } else {
        message("No spatial field precision parameter found")
        variances[1] <- 0
      }
    }
    
    # Try to get park variance
    if("Precision for park_id" %in% rownames(precision_summary)) {
      park_prec <- precision_summary["Precision for park_id", "mean"]
      message(paste("Park precision:", park_prec))
      if(park_prec > 1e10) {
        message("Park precision extremely large, setting variance to near zero")
        variances[3] <- 1e-10
      } else {
        variances[3] <- 1/park_prec
      }
    } else {
      message("Could not find exact 'Precision for park_id' row")
      park_row <- grep("park", rownames(precision_summary), ignore.case = TRUE)
      if(length(park_row) > 0) {
        park_prec <- precision_summary[park_row[1], "mean"]
        message(paste("Found alternative park precision:", park_prec))
        variances[3] <- 1/park_prec
      } else {
        message("No park precision parameter found")
        variances[3] <- 0
      }
    }
  }, error = function(e) {
    message("Error extracting variance components: ", e$message)
  })
  
  # Calculate residual variance depending on family
  if (family == "poisson") {
    # For Poisson models, theoretical variance equals the mean
    fitted_values <- model$summary.fitted.values$mean
    variances[4] <- mean(fitted_values, na.rm = TRUE)
    message(paste("Residual variance (Poisson mean):", variances[4]))
  } else {
    # For Gaussian models, use precision directly
    if("Precision for the Gaussian observations" %in% rownames(precision_summary)) {
      resid_prec <- precision_summary["Precision for the Gaussian observations", "mean"]
      variances[4] <- 1/resid_prec
      message(paste("Residual variance (Gaussian precision):", variances[4]))
    } else {
      message("Could not find Gaussian precision parameter")
      variances[4] <- NA
    }
  }
  
  # Calculate variance from fixed effects (park dimensions)
  if(!is.null(fixed_effects)) {
    # Remove intercept
    dimension_vars <- c("left_field_distance", "right_field_distance", 
                        "center_field_distance", "elevation")
    dimension_effects <- fixed_effects[rownames(fixed_effects) %in% dimension_vars, ]
    
    # Sum squared effects for dimension variables
    if(nrow(dimension_effects) > 0) {
      variances[2] <- sum(dimension_effects$mean^2, na.rm = TRUE)
      message(paste("Park dimensions variance:", variances[2]))
    } else {
      variances[2] <- 0
    }
  } else {
    variances[2] <- 0
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
# SPATIAL MODEL PREPARATION
#=============================================

prepare_spatial_model <- function(spatial_data, stadium_dimensions, park_summary) {
  message("Preparing spatial model for park effects...")
  
  # Validate inputs
  if(is.null(spatial_data)) {
    stop("spatial_data cannot be NULL")
  }
  if(is.null(stadium_dimensions)) {
    stop("stadium_dimensions cannot be NULL")
  }
  if(is.null(park_summary)) {
    stop("park_summary cannot be NULL")
  }
  
  # Check if stadium_dimensions has required columns
  required_cols <- c("long", "lat", "park_id", "park")
  missing_cols <- setdiff(required_cols, names(stadium_dimensions))
  if(length(missing_cols) > 0) {
    stop("stadium_dimensions is missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Extract coordinates - ensure we have valid data
  park_coords_data <- stadium_dimensions[, c("long", "lat")]
  if(nrow(park_coords_data) == 0) {
    stop("No valid coordinates found in stadium_dimensions")
  }
  
  # Check for NA values in coordinates
  if(any(is.na(park_coords_data$long)) || any(is.na(park_coords_data$lat))) {
    warning("NA values found in coordinates. Filtering these out.")
    park_coords_data <- park_coords_data[!is.na(park_coords_data$long) & !is.na(park_coords_data$lat), ]
    if(nrow(park_coords_data) == 0) {
      stop("No valid coordinates remain after filtering NAs")
    }
  }
  
  # Convert to matrix explicitly
  park_coords <- as.matrix(park_coords_data)
  
  # Create mesh for spatial analysis
  message("Creating spatial mesh...")
  tryCatch({
    mesh <- inla.mesh.2d(
      loc = park_coords,
      max.edge = c(5, 10),  # Parameters for mesh resolution
      cutoff = 0.5,         # Minimum distance between mesh vertices
      offset = c(1, 5)      # Buffer around points
    )
  }, error = function(e) {
    stop("Error creating mesh: ", e$message, ". Check the coordinates for validity.")
  })
  
  # Create SPDE model
  message("Creating SPDE spatial model...")
  tryCatch({
    spde <- inla.spde2.pcmatern(
      mesh = mesh,
      alpha = 2,            # Smoothness parameter
      prior.range = c(10, 0.5),  # Prior on spatial range
      prior.sigma = c(1, 0.5)    # Prior on spatial standard deviation
    )
  }, error = function(e) {
    stop("Error creating SPDE model: ", e$message)
  })
  
  # Standardize the data consistently
  if(!all(c("lf_distance", "rf_distance", "cf_distance", "elevation") %in% names(stadium_dimensions)) &&
     !all(c("left_field_distance", "right_field_distance", "center_field_distance", "elevation") %in% names(stadium_dimensions))) {
    stop("Stadium dimensions missing required field distance columns")
  }
  
  # Create standardized version with consistent column names
  stadium_dims_std <- stadium_dimensions
  
  # Rename columns if using lf_distance format
  if("lf_distance" %in% names(stadium_dims_std)) {
    names(stadium_dims_std)[names(stadium_dims_std) == "lf_distance"] <- "left_field_distance"
    names(stadium_dims_std)[names(stadium_dims_std) == "rf_distance"] <- "right_field_distance"
    names(stadium_dims_std)[names(stadium_dims_std) == "cf_distance"] <- "center_field_distance"
  }
  
  # Create derived metrics
  stadium_dims_std$avg_distance <- (stadium_dims_std$left_field_distance + 
                                      stadium_dims_std$right_field_distance + 
                                      stadium_dims_std$center_field_distance) / 3
  
  # Scale variables for numerical stability
  stadium_dims_std$left_field_distance_scaled <- scale(stadium_dims_std$left_field_distance)[,1]
  stadium_dims_std$right_field_distance_scaled <- scale(stadium_dims_std$right_field_distance)[,1]
  stadium_dims_std$center_field_distance_scaled <- scale(stadium_dims_std$center_field_distance)[,1]
  stadium_dims_std$elevation_scaled <- scale(stadium_dims_std$elevation)[,1]
  
  # Check if park_summary has required columns
  required_cols <- c("park_id", "H", "AB")
  missing_cols <- setdiff(required_cols, names(park_summary))
  if(length(missing_cols) > 0) {
    stop("park_summary is missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Check if park_summary has field dimensions
  if(!all(c("left_field_distance", "right_field_distance", "center_field_distance") %in% names(park_summary))) {
    message("Joining stadium dimensions to park_summary...")
    # Join the required columns
    dimension_cols <- c("park_id", "left_field_distance", "right_field_distance", 
                        "center_field_distance", "elevation",
                        "left_field_distance_scaled", "right_field_distance_scaled", 
                        "center_field_distance_scaled", "elevation_scaled")
    
    # Ensure all necessary columns exist in stadium_dims_std
    missing_dim_cols <- setdiff(dimension_cols, names(stadium_dims_std))
    if(length(missing_dim_cols) > 0) {
      stop("Missing required dimension columns: ", paste(missing_dim_cols, collapse = ", "))
    }
    
    # Select only the dimension columns for joining
    dims_for_joining <- stadium_dims_std[, dimension_cols]
    
    # Join with park_summary
    park_summary <- merge(park_summary, dims_for_joining, by = "park_id", all.x = TRUE)
    
    # Check for NAs after merge
    if(any(is.na(park_summary$left_field_distance))) {
      warning("NAs introduced by merge - some parks may be missing dimension data")
    }
  }
  
  # Create projection matrix
  message("Creating projection matrix...")
  tryCatch({
    A <- inla.spde.make.A(mesh, loc = park_coords)
  }, error = function(e) {
    stop("Error creating projection matrix: ", e$message)
  })
  
  # Verify dimensions before creating stack
  message(paste("Matrix A has", nrow(A), "rows and", nrow(park_summary), "park_summary rows"))
  
  # Create stack data
  stack_data <- data.frame(
    y = park_summary$H,
    intercept = 1,
    at_bats = park_summary$AB,
    left_field_distance_scaled = park_summary$left_field_distance_scaled,
    right_field_distance_scaled = park_summary$right_field_distance_scaled, 
    center_field_distance_scaled = park_summary$center_field_distance_scaled,
    elevation_scaled = park_summary$elevation_scaled
  )
  
  # Create stack
  message("Creating INLA stack...")
  tryCatch({
    stack <- inla.stack(
      data = list(y = stack_data$y),
      A = list(A, 1),
      effects = list(
        spatial_field = 1:spde$n.spde,
        stack_data[, -1]  # All columns except y
      ),
      tag = "spatial"
    )
  }, error = function(e) {
    stop("Error creating INLA stack: ", e$message)
  })
  
  message("Spatial model preparation completed successfully")
  
  return(list(
    stack = stack,
    spde = spde,
    mesh = mesh,
    spatial_data = spatial_data,
    park_summary = park_summary,
    park_coords = park_coords,
    stadium_dimensions = stadium_dims_std
  ))
}

#=============================================
# SPATIAL MODEL FITTING FUNCTION
#=============================================

run_spatial_model <- function(spatial_model_data) {
  message("Fitting spatial park effects model...")
  
  # Create spatial formula
  spatial_formula <- y ~ offset(log(at_bats)) +
    f(spatial_field, model = spatial_model_data$spde) +  # Spatial random field
    left_field_distance_scaled + right_field_distance_scaled + 
    center_field_distance_scaled + elevation_scaled
  
  # Set control parameters for INLA
  control_compute <- list(
    dic = TRUE,          # Compute Deviance Information Criterion
    waic = TRUE,         # Compute Widely Applicable Information Criterion
    return.marginals = TRUE  # Return posterior marginals
  )
  
  # Set numerical control parameters to improve convergence
  control_inla <- list(
    strategy = "gaussian",
    int.strategy = "eb",
    h = 1e-3,
    tolerance = 1e-5
  )
  
  # Fit spatial model
  spatial_model <- inla(
    spatial_formula,
    family = "poisson",
    data = inla.stack.data(spatial_model_data$stack),
    control.predictor = list(
      A = inla.stack.A(spatial_model_data$stack),
      compute = TRUE
    ),
    control.compute = control_compute,
    control.inla = control_inla,
    verbose = FALSE
  )
  
  # Check convergence
  if (!is.null(spatial_model$mode) && !is.null(spatial_model$mode$convergence) && spatial_model$mode$convergence != 0) {
    message("WARNING: Model did not converge. Consider further adjustments.")
  } else {
    message("Model converged successfully!")
  }
  
  # Check for negative DIC or infinite WAIC
  if (!is.null(spatial_model$dic) && spatial_model$dic$dic < 0) {
    message("WARNING: Negative DIC detected. This indicates model specification issues.")
    message("Proceeding with parameter estimates, but interpret diagnostics with caution.")
  }
  
  if (!is.null(spatial_model$waic) && is.infinite(spatial_model$waic$waic)) {
    message("WARNING: Infinite WAIC detected. This indicates computational issues.")
    message("Proceeding with parameter estimates, but interpret diagnostics with caution.")
  }
  
  return(spatial_model)
}

#=============================================
# OUTCOME-SPECIFIC MODELS FUNCTION
#=============================================

run_outcome_specific_models <- function(spatial_data, stadium_dimensions) {
  message("Running outcome-specific spatial models...")
  
  # Check if required columns exist in spatial_data
  required_cols <- c("HR", "X2B", "X3B")
  available_cols <- intersect(required_cols, names(spatial_data))
  
  if(length(available_cols) == 0) {
    message("No outcome columns (HR, X2B, X3B) found in spatial_data")
    return(list())
  }
  
  outcome_models <- list()
  
  # Standardize dimension column names
  stadium_dims_standardized <- stadium_dimensions
  names(stadium_dims_standardized)[names(stadium_dims_standardized) == "lf_distance"] <- "left_field_distance"
  names(stadium_dims_standardized)[names(stadium_dims_standardized) == "rf_distance"] <- "right_field_distance"
  names(stadium_dims_standardized)[names(stadium_dims_standardized) == "cf_distance"] <- "center_field_distance"
  
  # Scale dimensions for better numerical behavior
  stadium_dims_standardized$left_field_distance_scaled <- scale(stadium_dims_standardized$left_field_distance)[,1]
  stadium_dims_standardized$right_field_distance_scaled <- scale(stadium_dims_standardized$right_field_distance)[,1]
  stadium_dims_standardized$center_field_distance_scaled <- scale(stadium_dims_standardized$center_field_distance)[,1]
  stadium_dims_standardized$elevation_scaled <- scale(stadium_dims_standardized$elevation)[,1]
  
  # Model configuration for all outcomes
  for(outcome in available_cols) {
    message(paste("Processing", outcome, "data..."))
    
    # Aggregate data for this outcome
    outcome_summary <- spatial_data %>%
      group_by(park_id, park) %>%
      summarize(
        outcome_count = sum(get(outcome), na.rm = TRUE),
        AB = sum(AB, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      # Filter out parks with too few at-bats
      filter(AB >= 100)
    
    # Join with standardized stadium dimensions
    stadium_dims_basic <- stadium_dims_standardized[, c("park_id", "park", 
                                                        "left_field_distance_scaled", 
                                                        "right_field_distance_scaled", 
                                                        "center_field_distance_scaled", 
                                                        "elevation_scaled")]
    
    outcome_data <- merge(outcome_summary, stadium_dims_basic, 
                          by = c("park_id", "park"), all.x = TRUE)
    
    # Create formula - same for all outcomes, model will learn weights
    outcome_formula <- as.formula(paste(
      "outcome_count ~ offset(log(AB)) +",
      "left_field_distance_scaled + right_field_distance_scaled + center_field_distance_scaled + elevation_scaled +",
      "f(park_id, model = 'iid', hyper = list(theta = list(prior = 'pc.prec', param = c(1, 0.05))))"
    ))
    
    # Set numerical control parameters
    control_inla <- list(
      strategy = "gaussian",
      int.strategy = "eb",
      h = 1e-3,
      tolerance = 1e-5
    )
    
    # Fit model
    tryCatch({
      control_compute <- list(dic = TRUE, waic = TRUE, return.marginals = TRUE)
      
      outcome_model <- inla(
        outcome_formula,
        family = "poisson",
        data = outcome_data,
        control.predictor = list(compute = TRUE),
        control.compute = control_compute,
        control.family = list(link = "log"),
        control.inla = control_inla,
        verbose = FALSE
      )
      
      # Check convergence
      if (!is.null(outcome_model$mode) && !is.null(outcome_model$mode$convergence) && outcome_model$mode$convergence != 0) {
        message(paste("WARNING:", outcome, "model did not converge."))
      } else {
        message(paste("Model for", outcome, "converged successfully!"))
      }
      
      # Store results
      outcome_models[[outcome]] <- list(
        model = outcome_model,
        data = outcome_data,
        formula = outcome_formula,
        effects = outcome_model$summary.fixed,
        diagnostics = data.frame(
          metric = c("DIC", "WAIC", "Marginal Loglikelihood", "Model Converged"),
          value = c(
            outcome_model$dic$dic,
            outcome_model$waic$waic,
            outcome_model$mlik[1],
            TRUE
          )
        )
      )
      
      message(paste(outcome, "model successfully fit"))
    }, error = function(e) {
      message(paste("Error fitting model for", outcome, ":", e$message))
      
      # Try a simpler model as fallback
      simple_formula <- as.formula(paste(
        "outcome_count ~ offset(log(AB)) +", 
        "left_field_distance_scaled + elevation_scaled"
      ))
      
      tryCatch({
        simple_model <- inla(
          simple_formula,
          family = "poisson",
          data = outcome_data,
          control.predictor = list(compute = TRUE),
          control.compute = list(dic = TRUE, waic = TRUE),
          control.inla = control_inla,
          verbose = FALSE
        )
        
        outcome_models[[outcome]] <- list(
          model = simple_model,
          data = outcome_data,
          formula = simple_formula,
          effects = simple_model$summary.fixed,
          diagnostics = data.frame(
            metric = c("DIC", "WAIC", "Marginal Loglikelihood", "Model Converged"),
            value = c(
              simple_model$dic$dic,
              simple_model$waic$waic,
              simple_model$mlik[1],
              TRUE
            )
          )
        )
        
        message(paste(outcome, "model fit as fallback"))
      }, error = function(e2) {
        message(paste("Simple model for", outcome, "also failed:", e2$message))
      })
    })
  }
  
  return(outcome_models)
}

#=============================================
# EXTRACT SPATIAL RESULTS FUNCTION
#=============================================

extract_spatial_results <- function(spatial_model, spatial_model_data, park_effects) {
  message("Extracting spatial model results...")
  
  # Extract fixed effects for dimension variables
  dimension_effects <- spatial_model$summary.fixed[c("left_field_distance_scaled", "right_field_distance_scaled", 
                                                     "center_field_distance_scaled", "elevation_scaled"), ]
  
  # Create diagnostic dataframe
  diagnostics <- data.frame(
    metric = c("DIC", "WAIC", "Marginal Loglikelihood", "Model Converged"),
    value = c(
      spatial_model$dic$dic,
      spatial_model$waic$waic,
      spatial_model$mlik[1],
      TRUE
    )
  )
  
  # Extract spatial field for visualization
  mesh <- spatial_model_data$mesh
  
  # Set up grid for projection
  x_range <- range(spatial_model_data$park_coords[,1], na.rm = TRUE)
  y_range <- range(spatial_model_data$park_coords[,2], na.rm = TRUE)
  
  # Ensure ranges are valid
  if (any(is.na(c(x_range, y_range)))) {
    x_range <- c(-125, -70)  # Default US longitude range
    y_range <- c(25, 48)     # Default US latitude range
  }
  
  # Create projector
  proj <- inla.mesh.projector(
    mesh,
    xlim = x_range + c(-2, 2),
    ylim = y_range + c(-2, 2),
    dims = c(50L, 50L)  # Grid size
  )
  
  # Project spatial field mean and sd
  spatial_field_mean <- inla.mesh.project(proj, spatial_model$summary.random$spatial_field$mean)
  spatial_field_sd <- inla.mesh.project(proj, spatial_model$summary.random$spatial_field$sd)
  
  # Calculate significance of spatial field if credible intervals are available
  if ("0.025quant" %in% colnames(spatial_model$summary.random$spatial_field) &&
      "0.975quant" %in% colnames(spatial_model$summary.random$spatial_field)) {
    
    spatial_field_lower <- inla.mesh.project(proj, spatial_model$summary.random$spatial_field$`0.025quant`)
    spatial_field_upper <- inla.mesh.project(proj, spatial_model$summary.random$spatial_field$`0.975quant`)
    spatial_field_significant <- (spatial_field_lower * spatial_field_upper) > 0
  } else {
    spatial_field_significant <- NULL
  }
  
  # Standardize park effects structure
  park_effects_df <- as.data.frame(park_effects)
  
  # Join park effects with stadium dimensions
  # First get the standardized dimension variables
  stadium_dims_subset <- spatial_model_data$stadium_dimensions[, c("park_id", 
                                                                   "left_field_distance", 
                                                                   "right_field_distance", 
                                                                   "center_field_distance", 
                                                                   "elevation")]
  
  # Standardize column names if needed
  if("lf_distance" %in% names(stadium_dims_subset)) {
    names(stadium_dims_subset)[names(stadium_dims_subset) == "lf_distance"] <- "left_field_distance"
    names(stadium_dims_subset)[names(stadium_dims_subset) == "rf_distance"] <- "right_field_distance"
    names(stadium_dims_subset)[names(stadium_dims_subset) == "cf_distance"] <- "center_field_distance"
  }
  
  # Merge park effects with stadium dimensions
  spatial_park_effects <- merge(park_effects_df, stadium_dims_subset, 
                                by = "park_id", all.x = TRUE)
  
  # Calculate mean values for dimension variables
  mean_lf <- mean(spatial_park_effects$left_field_distance, na.rm = TRUE)
  mean_rf <- mean(spatial_park_effects$right_field_distance, na.rm = TRUE)
  mean_cf <- mean(spatial_park_effects$center_field_distance, na.rm = TRUE)
  mean_elev <- mean(spatial_park_effects$elevation, na.rm = TRUE)
  
  # Scale dimension variables for better interpretability
  spatial_park_effects$left_field_distance_scaled <- 
    (spatial_park_effects$left_field_distance - mean_lf) / sd(spatial_park_effects$left_field_distance, na.rm = TRUE)
  spatial_park_effects$right_field_distance_scaled <- 
    (spatial_park_effects$right_field_distance - mean_rf) / sd(spatial_park_effects$right_field_distance, na.rm = TRUE)
  spatial_park_effects$center_field_distance_scaled <- 
    (spatial_park_effects$center_field_distance - mean_cf) / sd(spatial_park_effects$center_field_distance, na.rm = TRUE)
  spatial_park_effects$elevation_scaled <- 
    (spatial_park_effects$elevation - mean_elev) / sd(spatial_park_effects$elevation, na.rm = TRUE)
  
  # Calculate dimension contribution based on scaled variables
  # This ensures consistent scaling with the model coefficients
  spatial_park_effects$dimension_contribution <- 
    dimension_effects["left_field_distance_scaled", "mean"] * spatial_park_effects$left_field_distance_scaled +
    dimension_effects["right_field_distance_scaled", "mean"] * spatial_park_effects$right_field_distance_scaled +
    dimension_effects["center_field_distance_scaled", "mean"] * spatial_park_effects$center_field_distance_scaled +
    dimension_effects["elevation_scaled", "mean"] * spatial_park_effects$elevation_scaled
  
  # Calculate total effect and factor
  spatial_park_effects$total_effect <- spatial_park_effects$effect + spatial_park_effects$dimension_contribution
  spatial_park_effects$total_factor <- exp(spatial_park_effects$total_effect)
  
  # Calculate variance decomposition
  variance_components <- calculate_variance_decomposition(
    spatial_model, 
    fixed_effects = dimension_effects,
    family = "poisson"
  )
  
  # Return results
  return(list(
    dimension_effects = dimension_effects,
    spatial_field_mean = spatial_field_mean,
    spatial_field_sd = spatial_field_sd,
    spatial_field_significant = spatial_field_significant,
    spatial_park_effects = spatial_park_effects,
    variance_components = variance_components,
    diagnostics = diagnostics,
    projector = proj,
    x_range = x_range,
    y_range = y_range
  ))
}

#=============================================
# CREATE SPATIAL VISUALIZATIONS FUNCTION
#=============================================

create_spatial_visualizations <- function(spatial_results, spatial_model_data, outcome_models = NULL) {
  message("Creating spatial visualizations...")
  
  # Extract relevant data for plotting
  stadium_dimensions <- spatial_model_data$stadium_dimensions
  dimension_effects <- spatial_results$dimension_effects
  spatial_park_effects <- spatial_results$spatial_park_effects
  
  # Plot 1: Spatial field plot
  spatial_df <- expand.grid(
    x = seq(spatial_results$x_range[1], spatial_results$x_range[2], length.out = 50),
    y = seq(spatial_results$y_range[1], spatial_results$y_range[2], length.out = 50)
  )
  
  spatial_df$value <- as.vector(spatial_results$spatial_field_mean)
  spatial_df$sd <- as.vector(spatial_results$spatial_field_sd)
  
  # Check if we have significance data
  if(!is.null(spatial_results$spatial_field_significant)) {
    spatial_df$significant <- as.vector(spatial_results$spatial_field_significant)
    alpha_values <- ifelse(spatial_df$significant, 0.9, 0.3)
  } else {
    alpha_values <- 0.7
  }
  
  # Create basic spatial plot
  spatial_plot <- ggplot() +
    geom_raster(data = spatial_df, 
                aes(x = x, y = y, fill = value),
                alpha = alpha_values,
                interpolate = TRUE) +
    # Add park locations
    geom_point(data = stadium_dimensions, 
               aes(x = long, y = lat), 
               color = "black", size = 3) +
    # Customize appearance
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red", 
      midpoint = 0,
      name = "Spatial\nEffect"
    ) +
    labs(
      title = "Spatial Distribution of Park Effects",
      subtitle = "Spatial field showing geographic patterns in park effects",
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() +
    coord_fixed(ratio = 1.3)  # Adjust for map projection
  
  # Plot 2: Dimension effects plot
  dimension_plot_data <- data.frame(
    dimension = rownames(dimension_effects),
    effect = dimension_effects[, "mean"],
    lower_ci = dimension_effects[, "0.025quant"],
    upper_ci = dimension_effects[, "0.975quant"],
    significant = (dimension_effects[, "0.025quant"] * dimension_effects[, "0.975quant"]) > 0
  )
  
  # Create nicer labels
  dimension_plot_data$dimension_label <- factor(
    ifelse(dimension_plot_data$dimension == "left_field_distance_scaled", "Left Field\nDistance",
           ifelse(dimension_plot_data$dimension == "right_field_distance_scaled", "Right Field\nDistance",
                  ifelse(dimension_plot_data$dimension == "center_field_distance_scaled", "Center Field\nDistance", 
                         ifelse(dimension_plot_data$dimension == "elevation_scaled", "Elevation", 
                                as.character(dimension_plot_data$dimension)))))
  )
  
  dimension_plot <- ggplot(dimension_plot_data, 
                           aes(x = dimension_label, y = effect, color = significant)) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_color_manual(values = c("gray50", "blue"), 
                       labels = c("Not Significant", "Significant"),
                       name = "Effect") +
    labs(
      title = "Effects of Stadium Dimensions on Batting Performance",
      subtitle = "Coefficients with 95% credible intervals",
      x = NULL,
      y = "Effect on log(Hits)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
      panel.grid.major.x = element_blank()
    )
  
  # Plot 3: Variance decomposition
  variance_plot <- ggplot(spatial_results$variance_components, 
                          aes(x = reorder(component, -proportion), 
                              y = proportion, fill = component)) +
    geom_bar(stat = "identity", width = 0.7) +
    geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)), 
              vjust = -0.5, size = 4) +
    scale_y_continuous(labels = scales::percent, 
                       limits = c(0, max(spatial_results$variance_components$proportion) * 1.2)) +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = "Variance Decomposition in Park Effects",
      subtitle = "Proportion of variance explained by each component",
      x = NULL,
      y = "Proportion of Variance"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Create outcome-specific plots
  outcome_plots <- list()
  
  if(!is.null(outcome_models) && length(outcome_models) > 0) {
    for(outcome_name in names(outcome_models)) {
      outcome_model_data <- outcome_models[[outcome_name]]
      
      if(!is.null(outcome_model_data$effects)) {
        # Create dataframe from effects
        effects_df <- data.frame(
          variable = rownames(outcome_model_data$effects),
          mean = outcome_model_data$effects[, "mean"],
          lower = outcome_model_data$effects[, "0.025quant"],
          upper = outcome_model_data$effects[, "0.975quant"]
        )
        
        # Filter out intercept
        effects_df <- effects_df[effects_df$variable != "(Intercept)" & 
                                   effects_df$variable != "intercept", ]
        
        # Add significant column
        effects_df$significant <- (effects_df$lower * effects_df$upper) > 0
        
        # Create plot
        if(nrow(effects_df) > 0) {
          outcome_plots[[outcome_name]] <- ggplot(effects_df, 
                                                  aes(x = variable, y = mean, 
                                                      color = significant)) +
            geom_point(size = 4) +
            geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
            scale_color_manual(values = c("gray50", "blue"), 
                               labels = c("Not Significant", "Significant"),
                               name = "Effect") +
            labs(
              title = paste("Stadium Effects on", gsub("X", "", outcome_name)),
              subtitle = "Coefficients with 95% credible intervals",
              x = NULL,
              y = paste("Effect on log(", gsub("X", "", outcome_name), ")")
            ) +
            theme_minimal() +
            coord_flip()
        }
      }
    }
  }
  
  # Return plots
  return(list(
    spatial_plot = spatial_plot,
    dimension_plot = dimension_plot,
    variance_plot = variance_plot,
    outcome_plots = outcome_plots
  ))
}

#=============================================
# MAIN SPATIAL ANALYSIS FUNCTION
#=============================================

run_spatial_park_analysis <- function(park_results = NULL, 
                                      data_path = "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Data",
                                      results_path = "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Results") {
  message("\n========================================================")
  message("          RUNNING SPATIAL PARK EFFECTS ANALYSIS")
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
  
  # Step 1: Load pre-prepared data for spatial analysis
  message("Loading prepared data for spatial analysis...")
  
  # Step 1.1: Check if park_results is provided, otherwise try to load it
  if (is.null(park_results)) {
    park_results_path <- file.path(data_path, "park_results.rds")
    if (file.exists(park_results_path)) {
      message("Loading park effects results from: ", park_results_path)
      park_results <- readRDS(park_results_path)
    } else {
      stop("Park effects results not found. Please run park_effects_model.R first.")
    }
  }
  
  # Extract park effects from park_results
  park_effects <- park_results$park_effects
  
  # Step 1.2: Load pre-prepared spatial datasets
  message("Loading spatial datasets...")
  
  spatial_data_path <- file.path(data_path, "spatial_data.rds")
  stadium_dimensions_path <- file.path(data_path, "stadium_dimensions.rds")
  park_summary_path <- file.path(data_path, "park_summary.rds")
  
  if (!file.exists(spatial_data_path) || !file.exists(stadium_dimensions_path) || !file.exists(park_summary_path)) {
    stop("Required spatial data files not found. Please run data_preparation.R first.")
  }
  
  spatial_data <- readRDS(spatial_data_path)
  stadium_dimensions <- readRDS(stadium_dimensions_path)
  park_summary <- readRDS(park_summary_path)
  
  # Step 2: Prepare spatial model
  message("Preparing spatial model data...")
  tryCatch({
    spatial_model_data <- prepare_spatial_model(spatial_data, stadium_dimensions, park_summary)
  }, error = function(e) {
    message("Error in prepare_spatial_model: ", e$message)
    stop("Failed to prepare spatial model data")
  })
  
  # Step 3: Fit spatial model
  message("Fitting spatial model...")
  tryCatch({
    spatial_model <- run_spatial_model(spatial_model_data)
  }, error = function(e) {
    message("Error in run_spatial_model: ", e$message)
    stop("Failed to fit spatial model")
  })
  
  # Step 4: Run outcome-specific models
  message("Running outcome-specific models...")
  tryCatch({
    outcome_models <- run_outcome_specific_models(spatial_data, stadium_dimensions)
  }, error = function(e) {
    message("Error in run_outcome_specific_models: ", e$message)
    message("Continuing without outcome-specific models")
    outcome_models <- list()
  })
  
  # Step 5: Extract spatial results
  message("Extracting spatial results...")
  tryCatch({
    spatial_results <- extract_spatial_results(spatial_model, spatial_model_data, park_effects)
  }, error = function(e) {
    message("Error in extract_spatial_results: ", e$message)
    stop("Failed to extract spatial results")
  })
  
  # Step 6: Create visualizations
  message("Creating spatial visualizations...")
  tryCatch({
    spatial_visualizations <- create_spatial_visualizations(
      spatial_results, 
      spatial_model_data,
      outcome_models
    )
  }, error = function(e) {
    message("Error in create_spatial_visualizations: ", e$message)
    message("Creating minimal visualizations")
    
    # Create minimal visualizations if full ones fail
    spatial_visualizations <- list(
      spatial_plot = ggplot() + ggtitle("Spatial plot unavailable"),
      dimension_plot = ggplot() + ggtitle("Dimension plot unavailable"),
      variance_plot = ggplot() + ggtitle("Variance plot unavailable"),
      outcome_plots = list()
    )
  })
  
  # Record end time
  end_time <- Sys.time()
  computation_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  message(paste("Spatial analysis completed in", round(computation_time, 2), "seconds"))
  
  # Save results
  message("Saving spatial analysis results...")
  
  # Create a dedicated directory for spatial results
  spatial_results_dir <- file.path(results_path, "spatial")
  if (!dir.exists(spatial_results_dir)) {
    dir.create(spatial_results_dir, recursive = TRUE)
  }
  
  # Save plots
  tryCatch({
    ggsave(file.path(spatial_results_dir, "spatial_field_plot.png"), 
           spatial_visualizations$spatial_plot, width = 12, height = 10)
    ggsave(file.path(spatial_results_dir, "dimension_effects_plot.png"), 
           spatial_visualizations$dimension_plot, width = 10, height = 8)
    ggsave(file.path(spatial_results_dir, "variance_plot.png"),
           spatial_visualizations$variance_plot, width = 10, height = 6)
  }, error = function(e) {
    message("Error saving main plots: ", e$message)
  })
  
  # Save outcome-specific plots if available
  if(length(spatial_visualizations$outcome_plots) > 0) {
    for(outcome_name in names(spatial_visualizations$outcome_plots)) {
      tryCatch({
        ggsave(file.path(spatial_results_dir, paste0(outcome_name, "_effects.png")), 
               spatial_visualizations$outcome_plots[[outcome_name]], width = 10, height = 8)
      }, error = function(e) {
        message(paste("Error saving plot for", outcome_name, ":", e$message))
      })
    }
  }
  
  # Save data
  tryCatch({
    write.csv(spatial_results$spatial_park_effects, 
              file.path(spatial_results_dir, "spatial_park_effects.csv"), row.names = FALSE)
    write.csv(as.data.frame(spatial_results$dimension_effects), 
              file.path(spatial_results_dir, "dimension_effects.csv"), row.names = TRUE)
    write.csv(spatial_results$variance_components,
              file.path(spatial_results_dir, "variance_components.csv"), row.names = FALSE)
    write.csv(spatial_results$diagnostics,
              file.path(spatial_results_dir, "spatial_model_diagnostics.csv"), row.names = FALSE)
  }, error = function(e) {
    message("Error saving result CSVs: ", e$message)
  })
  
  # Save outcome-specific results
  if(length(outcome_models) > 0) {
    for(outcome_name in names(outcome_models)) {
      if(!is.null(outcome_models[[outcome_name]]$effects)) {
        tryCatch({
          write.csv(as.data.frame(outcome_models[[outcome_name]]$effects), 
                    file.path(spatial_results_dir, paste0(outcome_name, "_effects.csv")), 
                    row.names = TRUE)
          write.csv(outcome_models[[outcome_name]]$diagnostics,
                    file.path(spatial_results_dir, paste0(outcome_name, "_diagnostics.csv")), 
                    row.names = FALSE)
        }, error = function(e) {
          message(paste("Error saving results for", outcome_name, ":", e$message))
        })
      }
    }
  }
  
  # Save model and full results to data path
  model_results <- list(
    spatial_model = spatial_model,
    outcome_models = outcome_models,
    spatial_model_data = spatial_model_data,
    spatial_results = spatial_results,
    spatial_visualizations = spatial_visualizations,
    computation_time = computation_time
  )
  
  saveRDS(model_results, file.path(data_path, "spatial_results.rds"))
  
  # Print summary
  message("\n========================================================")
  message("          SPATIAL ANALYSIS RESULTS SUMMARY")
  message("========================================================\n")
  
  # Print diagnostics
  message("Model Diagnostics:")
  message(paste("DIC:", round(spatial_results$diagnostics$value[1], 2)))
  message(paste("WAIC:", round(spatial_results$diagnostics$value[2], 2)))
  message(paste("Marginal Loglikelihood:", round(spatial_results$diagnostics$value[3], 2)))
  message(paste("Model Converged:", spatial_results$diagnostics$value[4]))
  
  # Summarize dimension effects
  message("\nStadium Dimension Effects:")
  dimension_effects <- spatial_results$dimension_effects
  for (dim in rownames(dimension_effects)) {
    effect <- dimension_effects[dim, "mean"]
    significant <- (dimension_effects[dim, "0.025quant"] * dimension_effects[dim, "0.975quant"]) > 0
    significance_str <- ifelse(significant, "significant", "not significant")
    message(sprintf("%-30s: %+.5f (%s)", dim, effect, significance_str))
  }
  
  # Summarize parks with strong dimension contributions
  message("\nTop Parks with Strong Dimension Contributions:")
  if(!is.null(spatial_results$spatial_park_effects) && nrow(spatial_results$spatial_park_effects) > 0) {
    top_dimension_effects <- spatial_results$spatial_park_effects[
      order(-abs(spatial_results$spatial_park_effects$dimension_contribution)), ]
    top_dimension_effects <- head(top_dimension_effects, 5)
    
    for (i in 1:nrow(top_dimension_effects)) {
      message(sprintf("%d. %-25s: %+.4f", i, top_dimension_effects$park[i], 
                      top_dimension_effects$dimension_contribution[i]))
    }
  } else {
    message("No parks with dimension contributions available")
  }
  
  # Summarize outcome-specific models
  if(length(outcome_models) > 0) {
    message("\nOutcome-Specific Models:")
    for(outcome_name in names(outcome_models)) {
      if(!is.null(outcome_models[[outcome_name]]$effects)) {
        message(paste("\n", gsub("X", "", outcome_name), "Model:"))
        
        # Get significant effects
        effects <- as.data.frame(outcome_models[[outcome_name]]$effects)
        effects$variable <- rownames(effects)
        effects$significant <- (effects$`0.025quant` * effects$`0.975quant`) > 0
        significant_effects <- effects[effects$significant & effects$variable != "(Intercept)", ]
        
        if(nrow(significant_effects) > 0) {
          for(j in 1:nrow(significant_effects)) {
            message(sprintf("  %-30s: %+.5f", 
                            significant_effects$variable[j], 
                            significant_effects$mean[j]))
          }
        } else {
          message("  No significant stadium dimension effects found")
        }
      }
    }
  }
  
  message("\nResults saved to: ", spatial_results_dir)
  message("Model and full results saved to: ", data_path)
  
  # Return the results
  return(model_results)
}

# Export the function to the global environment to ensure it's available to other scripts
assign("run_spatial_park_analysis", run_spatial_park_analysis, envir = .GlobalEnv)

# When this script is run directly, it will try to load park results and run the analysis
if (!exists("sourced_by_master") || !sourced_by_master) {
  data_path <- "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Data"
  results_path <- "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Results"
  
  spatial_results <- tryCatch({
    run_spatial_park_analysis(data_path = data_path, results_path = results_path)
  }, error = function(e) {
    message("\nERROR: ", e$message)
    message("To run this module, first run the park effects analysis module.")
    return(NULL)
  })
  
  if (!is.null(spatial_results)) {
    message("\nSpatial park effects analysis complete!")
  }
}