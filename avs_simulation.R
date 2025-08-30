# =======================================================================================
#
# AVS Economic Security Simulation: A Dynamic Risk Model
# 
# Author: Gianmarco Terenzi
#
#
# Overview:
# This script models the economic security of a hypothetical Actively Validated Service (AVS)
# built on a restaking protocol. The model analyzes the decision-making of a rational, 
# profit-maximizing validator to determine the conditions under which the AVS is vulnerable 
# to an economic attack.
#
# The simulation is dynamic, allowing to incorporate things like the systemic risk of underlying asset volatility 
# (a "bear market"), which can erode the core security capital of the system over time.
#
# The final output is a comparative "Security Surface" heatmap, visualizing AVS resilience
# across a range of security levels and threat models under different governance policies.
#
# =======================================================================================


# --- Phase 1: Environment Setup & Core Simulation Engine ---

# Load required libraries for data manipulation, plotting, and string formatting.
library(tidyverse) # Core suite of packages for data manipulation and plotting (ggplot2, dplyr, etc.).
library(glue)      # Used for robust and readable string interpolation, especially for plot captions.
library(scales)    # Provides functions to format numbers as percentages, dollars, etc., for clear plot labels.

#' @title Run Dynamic AVS Security Simulation
#' @description Simulates the daily economic security of an AVS over a 100-day period,
#'              incorporating the price decay of both the staked asset (ETH) and the
#'              rewards token (AVS token).
#' @param total_eth_restaked The total amount of ETH providing economic security to the AVS.
#' @param profit_from_corruption The potential one-time profit an attacker could gain from exploiting the AVS.
#' @param eth_price The initial price of ETH at the start of the simulation.
#' @param slashing_penalty The percentage of a validator's stake that is forfeited if they are caught attacking.
#' @param yield_rate The annualized yield (APY) earned by restakers, paid in the AVS's native token.
#' @param eth_price_drift The daily multiplicative factor for ETH's price decay (e.g., 0.995 for -0.5% daily).
#' @param avs_token_price_initial The initial price of the AVS's native reward token.
#' @param avs_token_price_drift The daily multiplicative factor for the AVS token's price decay.
#' @return A tibble with 100 rows, where each row represents a day and contains the security
#'         status (1 for secure, 0 for insecure).
run_dynamic_simulation <- function(total_eth_restaked, profit_from_corruption, eth_price, 
                                   slashing_penalty, yield_rate, eth_price_drift,
                                   avs_token_price_initial, avs_token_price_drift) {
  
  # --- Simulation Parameters ---
  timesteps <- 100 # The simulation runs for 100 days.
  
  # --- Initialize State Variables ---
  # These variables will be updated at the end of each daily loop.
  avs_token_price <- avs_token_price_initial 
  
  # Pre-allocate a tibble (data frame) for results. This is more computationally
  # efficient than growing the data frame inside the loop.
  results <- tibble(
    timestep = 1:timesteps,
    is_secure = 0
  )
  
  # --- The Simulation Loop ---
  # Iterate through each day to calculate the security status.
  for (day in 1:timesteps) {
    # 'Cost of Corruption' is the total value that would be slashed if an attack is detected.
    # This value erodes over time as the price of ETH falls.
    cost_of_corruption <- total_eth_restaked * eth_price * slashing_penalty
    
    # 'Honest Profit Value' is the opportunity cost for the attacker. It represents the
    # rewards they would forfeit by attacking. This also decays as the AVS token price falls.
    honest_profit_value <- (total_eth_restaked * yield_rate) * avs_token_price
    
    # The core security condition: A rational attacker will only attack if the potential
    # profit from the attack is greater than the combined cost of being slashed and
    # the honest rewards they give up.
    if (profit_from_corruption > cost_of_corruption + honest_profit_value) {
      is_secure <- 0 # System is vulnerable.
    } else {
      is_secure <- 1 # System is secure.
    }
    
    # Record the security state for the current day.
    results$is_secure[day] <- is_secure
    
    # Update state variables for the start of the next day, simulating market decay.
    avs_token_price <- avs_token_price * avs_token_price_drift
    eth_price <- eth_price * eth_price_drift
  }
  
  return(results)
}


# =======================================================================================
# --- Phase 2: Scenario Design & Execution ---
#
# This is the single "control panel" for the entire analysis.
# To test a new hypothesis, simply change the values in this block and re-run the script.
#
# =======================================================================================

all_scenarios <- crossing(
  # --- Independent Variables ---
  # These define the axes and facets of our final plot. We test all combinations.
  total_eth_restaked = seq(from = 15000, to = 55000, by = 10000),
  profit_from_corruption = seq(from = 1e7, to = 4e7, by = 0.5e7),
  slashing_penalty = c(0.50, 0.25), # The key governance policy we are comparing.
  
  # --- Constant Assumptions ---
  # These parameters are held constant across all simulations in this particular run.
  eth_price_initial = 3000,      # Initial ETH price at Day 0.
  eth_price_drift = 0.995,       # A -0.5% daily price decay to simulate a slow bear market.
  
  yield_rate = 0.05,             # A constant 5% APY for restakers.
  avs_token_price_initial = 2.0,   # Initial price of the AVS reward token.
  avs_token_price_drift = 0.98     # A -2% daily price decay for the AVS token (often more volatile).
)

# --- Execute Simulations ---
# Use the pmap() function to apply the `run_dynamic_simulation` to every row of the
# `all_scenarios` tibble. This is a clean and efficient "row-wise" operation.
simulation_results <- all_scenarios %>%
  rename(eth_price = eth_price_initial) %>% # Rename for clarity to match function arguments.
  mutate(
    simulation_results = pmap(., run_dynamic_simulation)
  ) %>%
  unnest(simulation_results) # The result is a nested list column; unnest() expands it into proper rows.


# =======================================================================================
# --- Phase 3: Analysis & Visualization ---
# =======================================================================================

# --- Summarize Raw Data ---
# Aggregate the 100-day simulation data for each scenario into a single metric:
# the first day the system was found to be insecure.
summary_results <- simulation_results %>%
  group_by(total_eth_restaked, profit_from_corruption, slashing_penalty) %>%
  summarise(
    # `which(is_secure == 0)` finds all days of failure. `first()` gets the earliest one.
    first_day_of_failure = first(which(is_secure == 0)),
    # If a scenario was never insecure, `first()` returns infinity (Inf). We replace
    # Inf with a value of 101 to signify "Always Secure" within the 100-day window.
    first_day_of_failure = ifelse(is.infinite(first_day_of_failure), 101, first_day_of_failure),
    .groups = 'drop' # Ungroup the data after summarising.
  )

# --- Prepare Data for Plotting ---
# To ensure the plot axes are ordered correctly and have clean labels, we create
# formatted character labels and then convert them to ordered factors. This is a
# robust technique that avoids common ggplot sorting issues.
plot_data <- summary_results %>%
  mutate(
    x_label = paste0(total_eth_restaked / 1e6, "M ETH"),
    y_label = paste0("$", profit_from_corruption / 1e9, "B")
  ) %>%
  mutate(
    x_label = factor(x_label, levels = unique(x_label[order(total_eth_restaked)])),
    y_label = factor(y_label, levels = unique(y_label[order(profit_from_corruption)]))
  )

# --- Dynamic Labels & Captions ---

# A custom labeller function for the plot facets. This creates the "Slashing Penalty: 50%" titles.
slashing_labels <- function(penalty) {
  # This conversion is a defensive measure. ggplot can sometimes pass facet values
  # as characters, which would cause `percent()` to fail.
  penalty_numeric <- as.numeric(as.character(penalty))
  paste("Slashing Penalty:", percent(penalty_numeric, accuracy = 1))
}

# Use glue() to dynamically create a caption based on the "Constant Assumptions"
# defined in the control panel. This ensures the plot caption always matches the data.
caption_text <- glue(
  "Constant Assumptions: ",
  "Initial ETH Price: {dollar(all_scenarios$eth_price_initial[1])} | ",
  "ETH Price Decay: {percent(1 - all_scenarios$eth_price_drift[1], accuracy = 0.1)} daily | ",
  "Initial AVS Token Price: {dollar(all_scenarios$avs_token_price_initial[1])} | ",
  "AVS Token Yield (APY): {percent(all_scenarios$yield_rate[1])}"
)

# --- Create the Final, Comparative Heatmap ---
ggplot(plot_data, aes(x = x_label, y = y_label)) +
  
  # The heatmap itself: colored tiles with a white border for clarity.
  geom_tile(aes(fill = first_day_of_failure), color = "white", linewidth = 0.5) +
  
  # Overlay the numeric result on each tile.
  geom_text(aes(label = first_day_of_failure), color = "black", size = 3.5) +
  
  # Create a separate plot for each slashing penalty value, using our custom labeller.
  facet_wrap(~ slashing_penalty, labeller = as_labeller(slashing_labels)) +
  
  # Define the color gradient: red (insecure) -> yellow (mid) -> green (secure).
  # The midpoint is set to 50 days. The limits ensure the color bar is consistent.
  scale_fill_gradient2(
    low = "#F8766D", mid = "#fee08b", high = "#74C476", midpoint = 50,
    name = "First Day of Failure\n(101 = Always Secure)", limits = c(1, 101)
  ) +
  
  # Add professional titles, subtitles, and the dynamic caption.
  labs(
    title = "AVS Security Surface: The Impact of Slashing Penalties",
    subtitle = "A dynamic simulation of validator incentives and system risk",
    x = "Total ETH Restaked (Economic Security)",
    y = "Potential Profit from Corruption (Threat Model)",
    caption = caption_text
  ) +
  
  # Apply a clean, minimal theme for a professional look.
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid.major = element_blank(), # Remove major grid lines.
    panel.grid.minor = element_blank(), # Remove minor grid lines.
    strip.text = element_text(face = "bold", size = 12), # Style the facet titles.
    axis.text.x = element_text(angle = 45, hjust = 1),   # Rotate x-axis labels to prevent overlap.
    plot.caption = element_text(hjust = 0.5) # Center the plot caption.
  )