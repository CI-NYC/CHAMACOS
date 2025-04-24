library(JuliaCall)
library(readr)
library(tidyverse)

set.seed(5)
for (years in c("1", 
                "1_5", 
                "2"
))
{
  # reading in covariates and outcomes data
  covars_outcome <- readRDS(paste0("data/covars_outcome_time_1_years_obs_", years, "_last_2_exposures.rds"))
  
  # reading data, 445 observations non-missing
  pol <- readRDS(paste0("data/exposures_7_classes_time_1_years_obs_", years, "_last_2_exposures.rds"))
  
  pol <- pol |>
    select(newid,
           starts_with("op_") |
             starts_with("pyr_") |
             starts_with("carb_") |
             starts_with("neo_") |
             starts_with("mn_") |
             starts_with("gly_") |
             starts_with("paraq_")
    ) |>
    na.omit() |> # this should not remove any additional
    mutate(across(op_kg_2_year_10_5y:paraq_kg_2_year_10_5y, list(p95 = ~ quantile(., 0.95)))) |> # calculate 95th percentile for each variable
    # truncate any exposures above the 95th percentile for that exposure
    mutate(op_kg_2_year_10_5y = ifelse(op_kg_2_year_10_5y <= op_kg_2_year_10_5y_p95, op_kg_2_year_10_5y, op_kg_2_year_10_5y_p95),
           pyr_kg_2_year_10_5y = ifelse(pyr_kg_2_year_10_5y <= pyr_kg_2_year_10_5y_p95, pyr_kg_2_year_10_5y, pyr_kg_2_year_10_5y_p95),
           carb_kg_2_year_10_5y = ifelse(carb_kg_2_year_10_5y <= carb_kg_2_year_10_5y_p95, carb_kg_2_year_10_5y, carb_kg_2_year_10_5y_p95),
           neo_kg_2_year_10_5y = ifelse(neo_kg_2_year_10_5y <= neo_kg_2_year_10_5y_p95, neo_kg_2_year_10_5y, neo_kg_2_year_10_5y_p95),
           gly_kg_2_year_10_5y = ifelse(gly_kg_2_year_10_5y <= gly_kg_2_year_10_5y_p95, gly_kg_2_year_10_5y, gly_kg_2_year_10_5y_p95),
           mn_kg_2_year_10_5y = ifelse(mn_kg_2_year_10_5y <= mn_kg_2_year_10_5y_p95, mn_kg_2_year_10_5y, mn_kg_2_year_10_5y_p95),
           paraq_kg_2_year_10_5y = ifelse(paraq_kg_2_year_10_5y <= paraq_kg_2_year_10_5y_p95, paraq_kg_2_year_10_5y, paraq_kg_2_year_10_5y_p95)
    ) |>
    select(-c(ends_with("p95"))) # remove 95th percentile variables
  
  # Normalizing between 0 and 1
  norm01 <- function(x) {
    (x - min(x)) / (max(x) - min(x))
  }
  
  # combining exposures and covariates
  original_data <- pol |>
    left_join(covars_outcome)
  
  pol <- pol |>
    select(-newid)
  
  pol_unnormalized <- pol
  
  # normalizing exposures between 0 and 1
  pol <- mutate(pol, across(everything(), norm01))
  pol <- mutate(pol, across(everything(), \(x) round(x, 3)))
  
  nrow(pol)
  summary(pol) # should be between 0 and 1
  
  # Change to correct path for user
  options(JULIA_HOME = "/Users/si2426/.juliaup/bin")
  
  julia <- julia_setup()
  
  julia_library("MTPConvexHull")
  
  # Create the convex hull of the pollutants
  ch <- julia_call("ConvexHull", pol)
  
  # Should return the exact same values since the 'point' is in the hull
  round(julia_call("boundary", ch, unlist(pol[1, ])), 3) == round(unlist(pol[1, ]), 3)
  
  # MULTIPLICATIVE SHIFT: Assume we want to perform a 20% shift decrease
  shifted_mult <- as.matrix(mutate(pol, across(c("gly_kg_2_year_10_5y", "paraq_kg_2_year_10_5y"), \(x) x * 0.8)))
  shifted_mult_feasible <- shifted_mult
  
  for (i in 1:nrow(pol)) {
    shifted_mult_feasible[i, ] <- julia_call("boundary", ch, unlist(shifted_mult[i, ]))
  }
  
  # calculating percent change of original to feasible shift
  perc_change <- as.matrix(shifted_mult_feasible - pol)/as.matrix(pol) |>
    as.data.frame()
  
  # turning Inf -> NA -> 0 for cleaning
  is.na(perc_change)<-sapply(perc_change, is.infinite)
  perc_change[is.na(perc_change)] <- 0
  
  # average percent change within each exposure getting shifted_mult
  colMeans(perc_change) * 100
  
  # number of observations where shift goes above observed values
  colSums(perc_change > 0) 
  
  # median percent chage within each exposure getting shifted_mult
  unlist(lapply(perc_change, median)) * 100
  
  # proportion the feasible shift equal to the desired shift
  mean(shifted_mult_feasible == shifted_mult) # 54.3% for 20% reduction
  
  # summary statistics about the true shift
  # NA's have to do with dividing by a zero, can be ignored
  true_shift_mult <- shifted_mult_feasible / as.matrix(pol)
  true_shift_mult[!is.finite(true_shift_mult)] <- NA_real_
  summary(true_shift_mult)
  
  # returning shifts to original scale
  #shifted_mult_feasible_unnormalized <- (shifted_mult_feasible * (sapply(pol_unnormalized,function(col) max(col)) - sapply(pol_unnormalized,function(col) min(col)))) + sapply(pol_unnormalized,function(col) min(col))
  
  shifted_mult_feasible_unnormalized <- shifted_mult_feasible
  
  # joining shifts
  shifted_mult_data <- shifted_mult_feasible_unnormalized |>
    as.data.frame() |>
    cbind(original_data[, c(1, 9:ncol(original_data))]) |>
    relocate(newid, .before = ) # make it the first column
  
  # summary statistics about 1). those getting shifted above observed and 2). those getting shifted below observed forlast 2 exposures
  
  # shifted above observed
  
  row_indices_gly <- which(shifted_mult_feasible[, 6] > as.matrix(pol)[, 6])
  shifted_mult_data[row_indices_gly, ] |> select(-newid) |> summary() 
  
  row_indices_paraq <- which(shifted_mult_feasible[, 7] > as.matrix(pol)[, 7])
  shifted_mult_data[row_indices_paraq, ] |> select(-newid) |> summary() 
  
  # shifted below observed
  
  row_indices_gly <- which(shifted_mult_feasible[, 6] < as.matrix(pol)[, 6])
  shifted_mult_data[row_indices_gly, ] |> select(-newid) |> summary() 
  
  row_indices_paraq <- which(shifted_mult_feasible[, 7] < as.matrix(pol)[, 7])
  shifted_mult_data[row_indices_paraq, ] |> select(-newid) |> summary() 
  
  # ADDITIVE SHIFT: Assume we want to perform a 10 unit (scaled) decrease
  
  # calculating 10 unit shift on normalized scale
  additive_scale <- sapply(pol_unnormalized, function(col) (10 - min(col)) / (max(col) - min(col)))
  
  additive_scale <- additive_scale[6:7]
  
  shifted_add <- as.matrix(sweep(pol[6:7], 2, additive_scale, "-"))
  
  shifted_add <- cbind(as.matrix(pol[1:5]), shifted_add)
  shifted_add_feasible <- shifted_add
  
  for (i in 1:nrow(pol)) {
    shifted_add_feasible[i, ] <- julia_call("boundary", ch, unlist(shifted_add[i, ]))
  }
  
  units_change <- as.matrix(shifted_add_feasible - pol) |>
    as.data.frame()
  
  # turning Inf -> NA -> 0
  is.na(units_change)<-sapply(units_change, is.infinite)
  units_change[is.na(units_change)]<-0
  
  # unscaling the additive shift for summary statistics
  units_change_unscaled <- (units_change * (sapply(pol_unnormalized,function(col) max(col)) - sapply(pol_unnormalized,function(col) min(col)))) + sapply(pol_unnormalized,function(col) min(col))
  
  # mean unit change within each exposure getting shifted_add
  colMeans(units_change_unscaled)
  
  # median unit change within each exposure getting shifted_add
  unlist(lapply(units_change_unscaled, median))
  
  # proportion the feasible shift equaled the desired shift
  mean(shifted_add_feasible == shifted_add) # 0% for 10 unit shift
  
  # summary statistics about the true shift
  # NA's have to do with dividing by a zero, can be ignored
  true_shift_add <- shifted_add_feasible / as.matrix(pol)
  true_shift_add[!is.finite(true_shift_add)] <- NA_real_
  summary(true_shift_add)
  
  # joining shifts
  shifted_add_feasible_unnormalized_units_changed <- (units_change * (sapply(pol_unnormalized,function(col) max(col)) - sapply(pol_unnormalized,function(col) min(col)))) + sapply(pol_unnormalized,function(col) min(col))
  summary(shifted_add_feasible_unnormalized_units_changed)
  
  # returning shifts to original scale
  #shifted_add_feasible_unnormalized <- (shifted_add_feasible * (sapply(pol_unnormalized,function(col) max(col)) - sapply(pol_unnormalized,function(col) min(col)))) + sapply(pol_unnormalized,function(col) min(col))
  
  shifted_add_feasible_unnormalized <- shifted_add_feasible
  
  shifted_add_data <- shifted_add_feasible_unnormalized |>
    as.data.frame() |>
    cbind(original_data[, c(1, 9:ncol(original_data))]) |>
    relocate(newid, .before = ) # make it the first column
  
  # summary statistics about 1). those getting shifted above observed and 2). those getting shifted below observed forlast 2 exposures
  
  # shifted above observed
  
  row_indices_gly <- which(shifted_add_feasible[, 6] > as.matrix(pol)[, 6]) # X shifted above
  shifted_add_data[row_indices_gly, ] |> select(-newid) |> summary()
  
  row_indices_paraq <- which(shifted_add_feasible[, 7] > as.matrix(pol)[, 7]) # X shifted above
  shifted_add_data[row_indices_paraq, ] |> select(-newid) |> summary()
  
  # shifted below observed
  
  row_indices_gly <- which(shifted_add_feasible[, 6] < as.matrix(pol)[, 6]) # X shifted below
  shifted_add_data[row_indices_gly, ] |> select(-newid) |> summary()
  
  row_indices_paraq <- which(shifted_add_feasible[, 7] < as.matrix(pol)[, 7]) # X shifted below
  shifted_add_data[row_indices_paraq, ] |> select(-newid) |> summary()
  
  #shifted_add_extrapolate <- original_data[, 2:8]
  #shifted_add_extrapolate_df <- sweep(shifted_add_extrapolate, 2, additive_scale, "-")
  
  saveRDS(shifted_mult_data, paste0("data/shifted_data_80_percent_time_1_years_obs_", years, "_last_2_exposures.rds"))
  saveRDS(shifted_add_data, paste0("data/shifted_data_10_scaled_units_time_1_years_obs_", years, "_last_2_exposures.rds"))
  saveRDS(original_data, paste0("data/original_data_time_1_years_obs_", years, "_last_2_exposures.rds"))
}


