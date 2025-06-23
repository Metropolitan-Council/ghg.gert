# Logistic growth function with normalization and constraints
logisticGrowth <- function(t, K, r, t0, start_year, end_year) {
  # Standard logistic growth
  raw_growth <- K / (1 + exp(-r * (t - t0)))
  # Normalize to ensure it starts at 0 and ends at K
  growth_start <- K / (1 + exp(-r * (start_year - t0)))
  growth_end <- K / (1 + exp(-r * (end_year - t0)))
  normalized_growth <- (raw_growth - growth_start) / (growth_end - growth_start) * K
  return(normalized_growth)
}
