# Data Preparation Module for Baseball INLA Analysis
# This module handles all data preparation and returns clean datasets for all analyses

# Load required packages
if (!require("dplyr")) install.packages("dplyr")
if (!require("Lahman")) install.packages("Lahman")

library(dplyr)
library(Lahman)

#=============================================
# DATA PREPARATION FUNCTIONS
#=============================================

prepare_master_dataset <- function(min_year, min_ab) {
  message("Preparing master baseball dataset...")
  
  # Load base tables from Lahman
  batting <- Lahman::Batting
  people <- Lahman::People
  teams <- Lahman::Teams
  
  # Create master player-level dataset
  master_data <- batting %>%
    # Filter for recent years
    filter(yearID >= min_year) %>%
    # Filter for minimum at-bats
    filter(AB >= min_ab) %>%
    # Select all important batting columns
    select(playerID, yearID, teamID, stint, G, AB, R, H, X2B, X3B, HR, RBI, SB, CS, BB, SO, IBB, HBP, SH, SF, GIDP) %>%
    # Join with player info
    left_join(people %>% select(playerID, birthYear, birthMonth, birthDay, nameFirst, nameLast), 
              by = "playerID") %>%
    # Calculate player age
    mutate(
      age = yearID - birthYear,
      player_name = paste(nameFirst, nameLast)
    ) %>%
    # Join with team info to get park
    left_join(teams %>% select(yearID, teamID, name, park), 
              by = c("yearID", "teamID"))
  
  # Calculate batting metrics - verify these columns exist before calculation
  master_data <- master_data %>%
    mutate(
      # Calculate traditional batting stats
      batting_avg = H / AB,
      # Calculate WAR approximation (simplified)
      WAR = (H * 0.47 + X2B * 0.78 + X3B * 1.09 + HR * 1.40 + BB * 0.33 - AB * 0.098) / 100
    )
  
  # Create numeric IDs for INLA
  master_data <- master_data %>%
    mutate(
      player_id = as.numeric(factor(playerID)),
      team_id = as.numeric(factor(teamID)),
      park_id = as.numeric(factor(park)),
      is_home_park = TRUE  # Simplified assumption
    )
  
  # Print data summary
  message("Master dataset summary:")
  message(paste("Number of observations:", nrow(master_data)))
  message(paste("Years included:", min(master_data$yearID), "to", max(master_data$yearID)))
  message(paste("Number of unique players:", n_distinct(master_data$playerID)))
  
  return(master_data)
}

prepare_park_effects_data <- function(master_data) {
  message("Preparing park effects data...")
  
  # Create aggregated data at player-park level
  park_data <- master_data %>%
    group_by(playerID, player_id, park, park_id, teamID, team_id, yearID) %>%
    summarize(
      AB = sum(AB, na.rm = TRUE),
      H = sum(H, na.rm = TRUE),
      HR = sum(HR, na.rm = TRUE),
      X2B = sum(X2B, na.rm = TRUE),
      X3B = sum(X3B, na.rm = TRUE),
      BB = sum(BB, na.rm = TRUE),
      WAR = sum(WAR, na.rm = TRUE),
      batting_avg = H / AB,
      is_home_park = first(is_home_park),
      player_name = first(player_name),
      .groups = "drop"
    ) %>%
    # Only keep records with sufficient sample size
    filter(AB >= 20)
  
  # Add game ID for INLA
  park_data$game_id <- 1:nrow(park_data)
  
  # Diagnostic check for park effects data
  essential_columns <- c("playerID", "player_id", "park", "park_id", "AB", "H", 
                         "HR", "X2B", "X3B", "BB", "WAR", "game_id")
  missing_columns <- setdiff(essential_columns, names(park_data))
  
  if (length(missing_columns) > 0) {
    warning("WARNING: Park data is missing these essential columns: ", 
            paste(missing_columns, collapse = ", "))
  } else {
    message("All essential columns for park effects analysis created successfully.")
  }
  
  message(paste("Park effects data prepared with", nrow(park_data), "observations"))
  return(park_data)
}

prepare_aging_curves_data <- function(master_data) {
  message("Preparing aging curves data...")
  
  # Create player aging data with direct aggregation
  aging_data <- master_data %>%
    # Group by player and age
    group_by(playerID, player_id, age) %>%
    filter(sum(AB, na.rm = TRUE) >= 30) %>%  # Minimum AB threshold
    summarize(
      WAR = sum(WAR, na.rm = TRUE),
      AB = sum(AB, na.rm = TRUE),
      H = sum(H, na.rm = TRUE),
      X2B = sum(X2B, na.rm = TRUE),
      X3B = sum(X3B, na.rm = TRUE),
      HR = sum(HR, na.rm = TRUE),
      BB = sum(BB, na.rm = TRUE),
      batting_avg = H / AB,
      player_name = first(player_name),
      .groups = "drop"
    ) %>%
    # Create age ID for INLA
    mutate(age_id = as.integer(age))
  
  # Create player trajectories with career centering
  player_trajectories <- aging_data %>%
    # Only include players with substantial careers
    group_by(playerID) %>%
    filter(n() >= 5) %>%  # Must have at least 5 seasons
    filter(max(age) - min(age) >= 6) %>%  # Must span at least 6 years
    # Normalize WAR for each player
    mutate(
      career_avg_WAR = mean(WAR, na.rm = TRUE),
      WAR_centered = WAR - career_avg_WAR
    ) %>%
    ungroup()
  
  # Diagnostic check for aging curves data
  essential_columns <- c("playerID", "player_id", "age", "age_id", "AB", "WAR", 
                         "WAR_centered", "X2B", "X3B", "HR", "BB")
  missing_columns <- setdiff(essential_columns, names(player_trajectories))
  
  if (length(missing_columns) > 0) {
    warning("WARNING: Player trajectories data is missing these essential columns: ", 
            paste(missing_columns, collapse = ", "))
  } else {
    message("All essential columns for aging curves analysis created successfully.")
  }
  
  message(paste("Aging curves data prepared with", nrow(player_trajectories), "observations"))
  return(player_trajectories)
}

prepare_spatial_data <- function(park_data) {
  message("Preparing spatial analysis data...")
  
  # Create stadium dimensions dataset
  set.seed(123)  # For reproducibility
  
  # Get unique parks
  unique_parks <- park_data %>%
    select(park, park_id) %>%
    distinct()
  
  # Create simulated stadium dimensions
  stadium_dimensions <- unique_parks %>%
    mutate(
      lf_distance = sample(330:350, n(), replace = TRUE),
      rf_distance = sample(330:350, n(), replace = TRUE),
      cf_distance = sample(400:420, n(), replace = TRUE),
      elevation = case_when(
        grepl("COL|DEN", park) ~ 5280,    # Coors Field
        grepl("ARI|PHO", park) ~ 1100,    # Chase Field
        grepl("ATL", park) ~ 1050,        # Truist Park
        TRUE ~ sample(0:800, 1)           # Other parks
      ),
      # Create simulated coordinates for parks
      lat = runif(n(), 25, 48),   # Latitude range for US
      long = runif(n(), -125, -70)  # Longitude range for US
    )
  
  # Join park data with stadium dimensions
  spatial_data <- park_data %>%
    left_join(stadium_dimensions, by = c("park_id", "park"))
  
  # Summarize data at park level for spatial analysis
  park_summary <- spatial_data %>%
    group_by(park_id, park) %>%
    summarise(
      H = sum(H, na.rm = TRUE),
      AB = sum(AB, na.rm = TRUE),
      left_field = first(lf_distance),
      right_field = first(rf_distance),
      center_field = first(cf_distance),
      elevation = first(elevation),
      lat = first(lat),
      long = first(long),
      .groups = 'drop'
    )
  
  # Check for essential columns
  essential_spatial_columns <- c("park_id", "park", "H", "AB", "lf_distance", "rf_distance", 
                                 "cf_distance", "elevation", "lat", "long")
  missing_spatial_columns <- setdiff(essential_spatial_columns, 
                                     c(names(spatial_data), "lf_distance", "rf_distance", "cf_distance"))
  
  if (length(missing_spatial_columns) > 0) {
    warning("WARNING: Spatial data is missing these essential columns: ", 
            paste(missing_spatial_columns, collapse = ", "))
  } else {
    message("All essential columns for spatial analysis created successfully.")
  }
  
  # Return both datasets
  return(list(
    spatial_data = spatial_data,
    stadium_dimensions = stadium_dimensions,
    park_summary = park_summary
  ))
}

prepare_environmental_data <- function(park_data, spatial_data) {
  message("Creating environmental data...")
  
  # Create game-level environmental data
  set.seed(789)  # For reproducibility
  
  # Extract unique games from park_data
  games <- park_data %>%
    select(game_id) %>%
    distinct()
  
  # Get park information for each game
  game_parks <- park_data %>%
    select(game_id, park_id, park) %>%
    distinct()
  
  # Join with stadium dimensions
  stadium_dimensions <- spatial_data$stadium_dimensions
  
  # Join games with park information and dimensions
  game_env <- game_parks %>%
    left_join(stadium_dimensions, by = c("park_id", "park"))
  
  # Generate environmental conditions for each game
  game_env <- game_env %>%
    mutate(
      # Temperature varies by park location and elevation
      temperature = case_when(
        elevation > 3000 ~ runif(n(), 60, 75),  # Higher elevation tends to be cooler
        lat < 35 ~ runif(n(), 75, 95),          # Southern parks tend to be warmer
        TRUE ~ runif(n(), 65, 85)               # Northern parks have more moderate temps
      ),
      
      # Humidity varies by geography
      humidity = case_when(
        grepl("MIA|FLA|TB|HOU", park) ~ runif(n(), 60, 90), # Coastal and southern
        grepl("ARI|PHO|COL|DEN", park) ~ runif(n(), 20, 40), # Desert parks are dry
        TRUE ~ runif(n(), 40, 70)                           # Others moderate
      ),
      
      # Wind speed varies somewhat randomly
      wind_speed = runif(n(), 0, 20),
      
      # Wind direction (in degrees, 0 = from north, 90 = from east, etc.)
      wind_direction = sample(0:359, n(), replace = TRUE),
      
      # Precipitation (0 = none, 1-5 = light to heavy)
      precipitation = sample(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), n(), replace = TRUE),
      
      # Cloud cover (0-10, 0 = clear, 10 = overcast)
      cloud_cover = sample(0:10, n(), replace = TRUE),
      
      # Air pressure (normalized around average sea level pressure)
      air_pressure = rnorm(n(), 1013, 5)
    )
  
  # Add advanced environmental metrics
  game_env <- game_env %>%
    mutate(
      # Heat index (simplified combination of temperature and humidity)
      heat_index = temperature + 0.05 * humidity * (temperature - 80),
      
      # Altitude effect (combination of elevation and air pressure)
      altitude_effect = elevation / 1000 * (1013 - air_pressure) / 10,
      
      # Wind effect (combination of speed and direction - simplified)
      wind_effect = wind_speed * cos(wind_direction * pi/180)
    )
  
  # Standardize variables for modeling
  game_env <- game_env %>%
    mutate(
      temperature_z = scale(temperature)[,1],
      humidity_z = scale(humidity)[,1],
      wind_speed_z = scale(wind_speed)[,1],
      elevation_z = scale(elevation)[,1],
      air_pressure_z = scale(air_pressure)[,1],
      heat_index_z = scale(heat_index)[,1],
      altitude_effect_z = scale(altitude_effect)[,1],
      wind_effect_z = scale(wind_effect)[,1]
    )
  
  # Join environmental data back to park data
  env_data <- park_data %>%
    left_join(game_env, by = c("game_id", "park_id", "park"))
  
  # Add interaction terms that will be needed for analysis
  env_data <- env_data %>%
    mutate(
      `temperature_z:humidity_z` = temperature_z * humidity_z,
      `elevation_z:temperature_z` = elevation_z * temperature_z
    )
  
  # Check for essential columns
  essential_env_columns <- c("park_id", "park", "H", "AB", "WAR", "temperature", 
                             "humidity", "wind_speed", "elevation", "temperature_z",
                             "humidity_z", "wind_speed_z", "elevation_z", 
                             "temperature_z:humidity_z", "elevation_z:temperature_z")
  
  missing_env_columns <- setdiff(essential_env_columns, names(env_data))
  
  if (length(missing_env_columns) > 0) {
    warning("WARNING: Environmental data is missing these essential columns: ", 
            paste(missing_env_columns, collapse = ", "))
  } else {
    message("All essential columns for environmental analysis created successfully.")
  }
  
  message(paste("Created environmental data for", nrow(game_env), "games at", n_distinct(game_env$park), "parks"))
  
  return(list(
    env_data = env_data,
    game_env = game_env
  ))
}

#=============================================
# SAVE PREPARED DATASETS
#=============================================

save_datasets <- function(datasets, data_path, results_path) {
  # Create directories if they don't exist
  if (!dir.exists(data_path)) {
    dir.create(data_path, recursive = TRUE)
  }
  
  if (!dir.exists(results_path)) {
    dir.create(results_path, recursive = TRUE)
  }
  
  # Save each dataset as an RDS file to the data directory
  saveRDS(datasets$master_data, file.path(data_path, "master_data.rds"))
  saveRDS(datasets$park_data, file.path(data_path, "park_data.rds"))
  saveRDS(datasets$player_trajectories, file.path(data_path, "player_trajectories.rds"))
  saveRDS(datasets$spatial_data, file.path(data_path, "spatial_data.rds"))
  saveRDS(datasets$stadium_dimensions, file.path(data_path, "stadium_dimensions.rds"))
  saveRDS(datasets$park_summary, file.path(data_path, "park_summary.rds"))
  saveRDS(datasets$env_data, file.path(data_path, "env_data.rds"))
  saveRDS(datasets$game_env, file.path(data_path, "game_env.rds"))
  
  # Create a summary dataframe
  summary_df <- data.frame(
    dataset = c("master_data", "park_data", "player_trajectories", "spatial_data", 
                "stadium_dimensions", "park_summary", "env_data", "game_env"),
    observations = c(
      nrow(datasets$master_data),
      nrow(datasets$park_data),
      nrow(datasets$player_trajectories),
      nrow(datasets$spatial_data),
      nrow(datasets$stadium_dimensions),
      nrow(datasets$park_summary),
      nrow(datasets$env_data),
      nrow(datasets$game_env)
    ),
    columns = c(
      ncol(datasets$master_data),
      ncol(datasets$park_data),
      ncol(datasets$player_trajectories),
      ncol(datasets$spatial_data),
      ncol(datasets$stadium_dimensions),
      ncol(datasets$park_summary),
      ncol(datasets$env_data),
      ncol(datasets$game_env)
    ),
    creation_date = rep(Sys.Date(), 8)
  )
  
  # Save summary to results directory
  write.csv(summary_df, file.path(results_path, "dataset_summary.csv"), row.names = FALSE)
  
  message("Datasets saved to: ", data_path)
  message("Dataset summary saved to: ", results_path)
}

#=============================================
# MAIN FUNCTION TO PREPARE ALL DATASETS
#=============================================

prepare_all_datasets <- function(min_year, min_ab, data_path, results_path) {
  message("\n========================================================")
  message("          PREPARING DATA FOR INLA ANALYSIS")
  message("========================================================\n")
  
  # Verify that parameters are properly passed
  message("Using parameters:")
  message(paste("- Minimum year:", min_year))
  message(paste("- Minimum at-bats:", min_ab))
  message(paste("- Data path:", data_path))
  message(paste("- Results path:", results_path))
  
  # 1. Create master dataset
  master_data <- prepare_master_dataset(min_year, min_ab)
  
  # 2. Park effects data
  park_data <- prepare_park_effects_data(master_data)
  
  # 3. Aging curves data
  player_trajectories <- prepare_aging_curves_data(master_data)
  
  # 4. Spatial analysis data
  spatial_results <- prepare_spatial_data(park_data)
  spatial_data <- spatial_results$spatial_data
  stadium_dimensions <- spatial_results$stadium_dimensions
  park_summary <- spatial_results$park_summary
  
  # 5. Environmental data
  env_results <- prepare_environmental_data(park_data, spatial_results)
  env_data <- env_results$env_data
  game_env <- env_results$game_env
  
  # 6. Run full validation across all datasets
  validate_all_datasets(
    master_data, park_data, player_trajectories, 
    spatial_data, stadium_dimensions, park_summary,
    env_data, game_env
  )
  
  # Create datasets list
  datasets <- list(
    master_data = master_data,
    park_data = park_data,
    player_trajectories = player_trajectories,
    spatial_data = spatial_data,
    stadium_dimensions = stadium_dimensions,
    park_summary = park_summary,
    env_data = env_data,
    game_env = game_env
  )
  
  # Save datasets to the specified paths
  save_datasets(datasets, data_path, results_path)
  
  # Return datasets for immediate use
  return(datasets)
}

#=============================================
# VALIDATION FUNCTION
#=============================================

validate_all_datasets <- function(master_data, park_data, player_trajectories, 
                                  spatial_data, stadium_dimensions, park_summary,
                                  env_data, game_env) {
  message("\nValidating all datasets for required columns...")
  
  # Check master dataset
  master_required <- c("playerID", "player_id", "teamID", "team_id", "AB", "H", 
                       "X2B", "X3B", "HR", "BB", "WAR", "age", "park", "park_id")
  master_missing <- setdiff(master_required, names(master_data))
  if (length(master_missing) > 0) {
    warning("WARNING: Master dataset missing columns: ", paste(master_missing, collapse = ", "))
  } else {
    message("✓ Master dataset has all required columns")
  }
  
  # Check park effects dataset
  park_required <- c("playerID", "player_id", "park", "park_id", "AB", "H", 
                     "HR", "X2B", "X3B", "BB", "WAR", "game_id")
  park_missing <- setdiff(park_required, names(park_data))
  if (length(park_missing) > 0) {
    warning("WARNING: Park effects dataset missing columns: ", paste(park_missing, collapse = ", "))
  } else {
    message("✓ Park effects dataset has all required columns")
  }
  
  # Check player trajectories dataset
  traj_required <- c("playerID", "player_id", "age", "age_id", "AB", "WAR", 
                     "WAR_centered", "X2B", "X3B", "HR", "BB")
  traj_missing <- setdiff(traj_required, names(player_trajectories))
  if (length(traj_missing) > 0) {
    warning("WARNING: Player trajectories dataset missing columns: ", paste(traj_missing, collapse = ", "))
  } else {
    message("✓ Player trajectories dataset has all required columns")
  }
  
  # Check spatial dataset
  spatial_required <- c("park_id", "park", "lf_distance", "rf_distance", 
                        "cf_distance", "elevation", "lat", "long")
  spatial_missing <- setdiff(spatial_required, 
                             c(names(spatial_data), "lf_distance", "rf_distance", "cf_distance"))
  if (length(spatial_missing) > 0) {
    warning("WARNING: Spatial dataset missing columns: ", paste(spatial_missing, collapse = ", "))
  } else {
    message("✓ Spatial dataset has all required columns")
  }
  
  # Check environmental dataset
  env_required <- c("park_id", "park", "H", "AB", "WAR", "temperature", 
                    "humidity", "wind_speed", "elevation", "temperature_z",
                    "humidity_z", "wind_speed_z", "elevation_z", 
                    "temperature_z:humidity_z", "elevation_z:temperature_z")
  env_missing <- setdiff(env_required, names(env_data))
  if (length(env_missing) > 0) {
    warning("WARNING: Environmental dataset missing columns: ", paste(env_missing, collapse = ", "))
  } else {
    message("✓ Environmental dataset has all required columns")
  }
  
  message("Dataset validation complete!\n")
}

# This section only runs when the script is executed directly, not when sourced
if (!exists("sourced_by_master") || !sourced_by_master) {
  # Default parameters
  default_min_year <- 2010
  default_min_ab <- 50
  default_data_path <- "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Data"
  default_results_path <- "C:/Users/Richard Nuckols/Desktop/Desktop/Personal/JHU/Theory of Stats II/Module 13/Results"
  
  message("Running data preparation with default parameters:")
  message(paste("- Minimum year:", default_min_year))
  message(paste("- Minimum at-bats:", default_min_ab))
  
  datasets <- prepare_all_datasets(
    min_year = default_min_year, 
    min_ab = default_min_ab, 
    data_path = default_data_path, 
    results_path = default_results_path
  )
  
  message("\nData preparation complete!")
  message("You can now run the park effects or aging curves modules")
}