# Baseball Analytics with INLA and MCMC

This repository contains code for the research paper "Computational Efficiency and Parameter Estimation in Baseball Analytics: A Comparison of INLA and MCMC Methods". It implements a comprehensive analysis of baseball statistics using both Integrated Nested Laplace Approximation (INLA) and Markov Chain Monte Carlo (MCMC) methods.

## Research Overview

This research evaluates the application of Integrated Nested Laplace Approximation (INLA) to baseball analytics, comparing its performance against traditional Markov Chain Monte Carlo (MCMC) methods. The study focused on two primary objectives:

1. Implementing INLA for baseball outcome prediction through four progressive models
2. Comparing INLA with MCMC in terms of computational efficiency and parameter estimation accuracy

### Key Findings

- **Computational Efficiency**: INLA demonstrated average speedups of 283.2x over MCMC, with aging curves models showing a 419.7x improvement.
- **Parameter Accuracy**: Despite massive computational advantages, INLA produced remarkably similar estimates to MCMC, with minimal differences in park effects rankings and aging curve parameters (peak age difference of 0.5 years).
- **Baseball Insights**: Both methods identified Coors Field, Dolphin Stadium, and Enron Field as the most hitter-friendly parks. Player peak performance was identified at age 27.9 (INLA) or 27.4 (MCMC).
- **Extended Analytics**: INLA's efficiency enabled spatial and environmental analyses that would be computationally prohibitive with MCMC, revealing the impact of left-field distance on triples and temperature effects across different hit types.

## Code Structure

The repository is organized into several R scripts that implement different components of the analysis:

### Core Modules

- `master_analysis.R` - The main script that orchestrates the entire analysis pipeline
- `data_preparation.R` - Prepares baseball data for analysis from the Lahman Baseball Database
- `park_effects_model.R` - Implements the park effects model using INLA
- `aging_curves_model.R` - Implements the aging curves model using INLA
- `spatial_analysis.R` - Implements the spatial analysis of park effects using INLA
- `environmental_factors.R` - Implements the environmental factors model using INLA
- `mcmc_models.R` - Implements the MCMC versions of the park effects and aging curves models
- `dashboard_creation.R` - Creates visualizations and dashboards of the results
- `analyze_efficiency_comparison.R` - Analyzes the efficiency comparison between INLA and MCMC

### Models Implemented

1. **Park Effects Model**: Quantifies how different stadiums impact offensive performance
2. **Aging Curves Model**: Characterizes the relationship between player age and performance
3. **Spatial Park Effects Model**: Analyzes how specific stadium dimensions influence different types of hits
4. **Environmental Factors Model**: Examines the impact of temperature, humidity, and other environmental conditions on player performance

## Data

The analysis uses baseball data from 2000 to 2023, sourced from:
- The Lahman Baseball Database
- Baseball-Reference.com
- Statcast

The dataset includes:
- Player performance metrics (batting average, OBP, SLG, HR, etc.)
- Game-level data
- Player biographical information
- Stadium characteristics
- Environmental factors for each game

## Requirements

The following R packages are required:

```r
# Core packages
install.packages(c("dplyr", "ggplot2", "gridExtra", "reshape2", "tidyr"))

# INLA (install from repository)
install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

# For MCMC
install.packages(c("rstan", "parallel", "bayesplot", "bridgesampling"))

# For spatial analysis
install.packages(c("spdep", "fields"))

# Data source
install.packages("Lahman")
```

## How to Run

1. Clone this repository
2. Set up the directory structure:
   ```
   /Data         # For storing data files
   /Results      # For storing results and visualizations
   /Code         # The R scripts
   ```
3. Run the master analysis script with your preferred components:
   ```r
   source("master_analysis.R")
   ```
   
   The master script will present a menu of available analysis components:
   
   ```
   Available analysis components:
   1. Data Preparation
   2. Park Effects Analysis (INLA)
   3. Aging Curves Analysis (INLA)
   4. Spatial Park Effects Analysis (INLA)
   5. Environmental Factors Analysis (INLA)
   6. Park Effects Analysis (MCMC)
   7. Aging Curves Analysis (MCMC)
   8. Computational Efficiency Comparison
   9. Dashboard Creation
   10. Run All INLA Components
   11. Run All MCMC Components
   12. Run Complete Analysis
   ```

4. Select the components you want to run by entering the corresponding numbers (comma-separated), or one of the complete options (10, 11, or 12).

## Results

The analysis produces the following outputs in the Results directory:

- **Baseball Analytics Dashboard**: A comprehensive visualization of all analysis results
- **INLA vs. MCMC Comparison Dashboard**: A comparison of the INLA and MCMC results
- **Individual Model Results**: CSV files and plots for each model component
- **Efficiency Comparison**: Analysis of computational efficiency and parameter accuracy

## Citation

If you use this code in your research, please cite:

```
Nuckols, R. (2025). Computational Efficiency and Parameter Estimation in Baseball Analytics: A Comparison of INLA and MCMC Methods.
```

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- The Lahman Baseball Database for providing the historical baseball statistics
- INLA team for developing the efficient Bayesian inference method
- Stan Development Team for the Stan probabilistic programming language
