# AVS Economic Security Simulation: A Dynamic Risk Model

---

## Overview

This project presents a dynamic economic model written in R to simulate the security of an Actively Validated Service (AVS) built on a restaking protocol. The model analyzes the decision-making of a rational, profit-maximizing validator to determine the conditions under which an AVS becomes vulnerable to a malicious attack.

Unlike static models, this simulation incorporates the systemic risk of asset volatility by modeling the daily price decay of both the staked asset (ETH) and the AVS's native reward token. This allows for a more realistic "stress test" of an AVS's resilience.

The final output is a series of "Security Surface" heatmaps that visualize system security across a range of threat models and capitalization levels, directly comparing the impact of key governance decisions.

## Methodology

The core of the simulation is the rational actor security condition. An attack is considered profitable (and therefore likely) if the potential gain from corruption is greater than the combined economic deterrents.

**`Profit from Corruption > Cost of Corruption + Honest Profit Value`**

Where:
*   **Profit from Corruption:** The potential illicit profit an attacker could gain (The Threat Model).
*   **Cost of Corruption:** The value of the attacker's staked assets that would be destroyed via slashing penalty. This is the primary economic deterrent.
*   **Honest Profit Value:** The opportunity cost of an attack, representing the future honest rewards the attacker forfeits.

The simulation runs for a 100-day period, where the `Cost of Corruption` and `Honest Profit Value` change daily according to a pre-defined market drift, modeling their erosion during a market downturn or other assumptions. The heatmap cells represent the **first day** that the security condition fails. A value of **101** indicates the system remained secure for the entire simulation window.

## How to Use This Model

The entire simulation is controlled from a single `all_scenarios` code block in the R script. To generate new scenarios, modify the parameters within this block and re-run the script.

```R
all_scenarios <- crossing(
  # --- Independent Variables (defines plot axes and facets) ---
  total_eth_restaked = seq(from = 1500000, to = 5500000, by = 1000000),
  profit_from_corruption = seq(from = 1e9, to = 4e9, by = 0.5e9),
  slashing_penalty = c(0.5, 0.1), 
  
  # --- Constant Assumptions (held true for the run) ---
  eth_price_initial = 3000,
  eth_price_drift = 0.995, # -0.5% daily decay
  yield_rate = 0.05,
  avs_token_price_initial = 2.0,
  avs_token_price_drift = 0.98 # -2.0% daily decay
)
```

**Required Libraries:** `tidyverse`, `glue`, `scales`

---

## Key Findings & Scenarios

### Scenario 1: Baseline - Impact of Slashing in a Slow Bear Market

This scenario models a large-scale AVS during a "grinding" bear market with a slow, -0.5% daily price decay. It compares a low **10%** slashing penalty against a high **50%** penalty.

![Scenario 1](scenario1.png)

**Insight:** The results are stark. With a 10% slashing penalty, the system is almost universally insecure from Day 1. In contrast, a 50% penalty provides a robust security surface, protecting the AVS against multi-billion dollar threats, especially at higher levels of restaked ETH. This demonstrates that the slashing penalty is a primary and highly effective lever for ensuring system security.

### Scenario 2: Stress Test - Resilience During a Sharp Market Crash

This scenario models the same AVS but under a much more aggressive market crash, simulated with a -2.0% daily price decay. It compares high slashing penalties of **50%** and **75%** to see how they hold up under extreme stress.

![Scenario 2](scenario2.png)

**Insight:** Market velocity is a critical risk factor. Compared to the baseline, the security surface has significantly eroded. Scenarios that were previously secure for over 80-90 days now fail in 30-60 days. This highlights that economic security is not static; it is highly sensitive to the rate of collateral price decay. Even a very high 75% slashing penalty cannot fully insulate the system from rapid, systemic market risk.

### Scenario 3: Analysis of a Small-Scale AVS

This scenario tests the security dynamics for a smaller, emerging AVS where the financial stakes are lower (threat models in the tens of millions). It compares a moderate **25%** slashing penalty against a **50%** penalty.

![Scenario 3](scenario3.png)

**Insight:** The core principle holds true even at a smaller scale. Doubling the slashing penalty from 25% to 50% dramatically expands the secure design space for the AVS. Further tests (not pictured) confirmed that even tripling the validator yield incentives had no material impact on the outcome, as the slashing penalty remains the dominant factor in the attacker's economic calculation.

## Core Conclusion

Across all tested scenarios, the **slashing penalty** is overwhelmingly the most significant factor in maintaining AVS economic security. While market conditions and the total amount of restaked capital are very important, the deterrent of a significant slashing penalty is the most powerful governance tool for securing a protocol against rational economic attacks.
