library(JuliaCall)
library(readr)
library(tidyverse)

set.seed(5)

# reading in covariates and outcomes data
covars_outcome <- readRDS(paste0("data/longitudinal_data_aligned.rds"))

# lists to save results 
shifted <- list()
observed <- list()
prop_in_convex_hull <- list()
R_statistic <- list()

# looping through time points -- applying convex hull at each point independently 
for (t in 1:5)
{
  set.seed(5)
  
  # reading data, 359 observations non-missing at initial time
  pol_unnormalized <- covars_outcome |>
    select(newid,
           paste0("op_kg_2_year_time_", t),
           paste0("pyr_kg_2_year_time_", t),
           paste0("carb_kg_2_year_time_", t),
           paste0("neo_kg_2_year_time_", t),
           paste0("mn_kg_2_year_time_", t),
           paste0("gly_kg_2_year_time_", t),
           paste0("paraq_kg_2_year_time_", t)
    ) |>
    na.omit()
  
  # scaling between 0 and 1
  norm01 <- function(x) {
    (x - min(x)) / (max(x) - min(x))
  }
  
  pol <- pol_unnormalized |>
    select(-newid)
  
  # scaling exposures between 0 and 1
  pol <- mutate(pol, across(everything(), norm01))
  #pol <- mutate(pol, across(everything(), \(x) round(x, 3)))
  
  nrow(pol)
  
  psych::describe(pol) # should be between 0 and 1
  
  par(mfrow = c(3, 3))  
  for (name in names(pol)) {
    hist(pol[[name]], main = name, xlab = "", col = "gray", breaks = 20)
  }
  
  # Change to correct path for user
  options(JULIA_HOME = "/Users/si2426/.juliaup/bin")
  
  julia <- julia_setup()
  
  julia_library("MTPConvexHull")
  
  # Create the convex hull of the pollutants
  ch <- julia_call("ConvexHull", pol)
  
  # Should return the exact same values since the 'point' is in the hull (might not be equal due to numerical issues but should be close)
  julia_call("boundary", ch, unlist(pol[1, ])) == unlist(pol[1, ])
  
  abs(julia_call("boundary", ch, unlist(pol[1, ])) - unlist(pol[1, ]))
  
  # add in some numerical tolerance (0.005 per variable? 5% percent?)
  
  # MULTIPLICATIVE SHIFT: Assume we want to perform 20% shift decreases on glyphosate and paraquat
  #shifted_mult <- as.matrix(mutate(pol, across(c(paste0("gly_kg_2_year_time_", t), paste0("paraq_kg_2_year_time_", t)), \(x) x * 0.8)))
  shifted_mult <- as.matrix(mutate(pol, across(c(paste0("neo_kg_2_year_time_", t)), \(x) x * 0.8)))
  #shifted_mult <- as.matrix(mutate(pol, across(everything(), \(x) x * 0.8)))
  
  shifted_mult_feasible <- shifted_mult
  
  for (i in 1:nrow(pol)) {
    shifted_mult_feasible[i, ] <- julia_call("boundary", ch, unlist(shifted_mult[i, ]))
  }
  
  # if input is 0 then post-correct to 0 
  #shifted_mult_feasible[pol == 0] <- 0
  
  # proportion of the feasible shift "equal" (allow for some error) to the desired shift
  within_range <- (abs(shifted_mult_feasible - shifted_mult) <= 0.005)
  
  within_range[is.na(within_range)] <- TRUE
  
  # if any value is FALSE in a row, then all values in that row set to FALSE
  within_range[] <- !rowSums(!within_range)
  
  prop <- mean(within_range[, 1])
  
  prop_in_convex_hull[[t]] <- prop
  
  # if within tolerance, then use the ideal shift (feasible shift is likely numeric error)
  shifted_mult_feasible <- ifelse(within_range, shifted_mult, shifted_mult_feasible)
  
  # summary statistics about the true shift
  # NA's have to do with dividing by a zero, can be ignored
  true_shift_mult <- shifted_mult_feasible / as.matrix(pol)
  true_shift_mult[!is.finite(true_shift_mult)] <- NA_real_
  summary(true_shift_mult)
  
  # calculating percent change of original to feasible shift
  perc_change <- as.matrix(shifted_mult_feasible - pol)/as.matrix(pol) |>
    as.data.frame()
  
  # turning Inf -> NA -> 0 for cleaning
  is.na(perc_change) <- sapply(perc_change, is.infinite)
  perc_change[is.na(perc_change)] <- 0
  
  # average percent change within each exposure getting shifted_mult
  colMeans(perc_change) * 100
  
  # number of observations where shift goes above observed values
  colSums(perc_change > 0) 
  
  # median percent change within each exposure getting shifted_mult
  unlist(lapply(perc_change, median)) * 100
  
  # returning shifts to original scale
  #shifted_mult_feasible_unnormalized <- (shifted_mult_feasible * (sapply(pol_unnormalized,function(col) max(col)) - sapply(pol_unnormalized,function(col) min(col)))) + sapply(pol_unnormalized,function(col) min(col))
  
  # getting Ri statistic
  
  numerator_components <- (shifted_mult_feasible - shifted_mult)^2
  denominator_components <- (pol - shifted_mult)^2
  
  R_numerator <- sqrt(rowSums(numerator_components))
  
  R_denominator <- sqrt(rowSums(denominator_components))
  
  R <- R_numerator/R_denominator
  R[!is.finite(R)] <- 0
  
  # get R for each pesticide
  R_components <- sqrt(numerator_components)/sqrt(denominator_components)
  R_components <- R_components |> mutate(across(where(is.numeric), ~ ifelse(is.finite(.), ., 0)))
  
  colnames(numerator_components) <- paste0(colnames(numerator_components), "_numerator")
  colnames(denominator_components) <- paste0(colnames(denominator_components), "_denominator")
  
  R_statistic_df <- data.frame(R = R,
                               R_numerator = R_numerator,
                               R_denominator = R_denominator) |>
    cbind(R_components) |>
    cbind(numerator_components, denominator_components)
  
  # only returning shifted for those in the convex hull (within the specified range); for those that fall outside, use observed treatment values
  shifted_final <- ifelse(within_range, shifted_mult, as.matrix(pol)) # or should it be shifted_mult_feasible?
  
  # joining shifts
  shifted_mult_data <- shifted_final |>
    as.data.frame() |>
    cbind(pol_unnormalized$newid) |>
    rename("newid" = "pol_unnormalized$newid") |>
    relocate(newid, .before = ) # make it the first column
  
  obs_data <- pol |>
    as.data.frame() |>
    cbind(pol_unnormalized$newid) |>
    rename("newid" = "pol_unnormalized$newid") |>
    relocate(newid, .before = )
  
  # summary statistics about 1). those getting shifted above observed and 2). those getting shifted below observed forlast 2 exposures
  
  # shifted above observed
  
  # row_indices_gly <- which(shifted_mult_feasible[, 6] > as.matrix(pol)[, 6])
  # shifted_mult_data[row_indices_gly, ] |> select(-newid) |> summary() 
  # 
  # row_indices_paraq <- which(shifted_mult_feasible[, 7] > as.matrix(pol)[, 7])
  # shifted_mult_data[row_indices_paraq, ] |> select(-newid) |> summary() 
  # 
  # # shifted below observed
  # 
  # row_indices_gly <- which(shifted_mult_feasible[, 6] < as.matrix(pol)[, 6])
  # shifted_mult_data[row_indices_gly, ] |> select(-newid) |> summary() 
  # 
  # row_indices_paraq <- which(shifted_mult_feasible[, 7] < as.matrix(pol)[, 7])
  # shifted_mult_data[row_indices_paraq, ] |> select(-newid) |> summary() 
  
  # ADDITIVE SHIFT: Assume we want to perform a 10 unit (scaled) decrease
  
  # # calculating 10 unit shift on normalized scale
  # additive_scale <- sapply(pol_unnormalized, function(col) (10 - min(col)) / (max(col) - min(col)))
  # 
  # additive_scale <- additive_scale[c(5, 7)]
  # 
  # shifted_add <- as.matrix(sweep(pol[c(5, 7)], 2, additive_scale, "-"))
  # 
  # shifted_add <- cbind(as.matrix(pol[c(1, 2, 3, 4, 6)]), shifted_add)
  # shifted_add_feasible <- shifted_add
  # 
  # for (i in 1:nrow(pol)) {
  #   shifted_add_feasible[i, ] <- julia_call("boundary", ch, unlist(shifted_add[i, ]))
  # }
  # 
  # units_change <- as.matrix(shifted_add_feasible - pol) |>
  #   as.data.frame()
  # 
  # # turning Inf -> NA -> 0
  # is.na(units_change)<-sapply(units_change, is.infinite)
  # units_change[is.na(units_change)]<-0
  # 
  # # unscaling the additive shift for summary statistics
  # units_change_unscaled <- (units_change * (sapply(pol_unnormalized,function(col) max(col)) - sapply(pol_unnormalized,function(col) min(col)))) + sapply(pol_unnormalized,function(col) min(col))
  # 
  # # mean unit change within each exposure getting shifted_add
  # colMeans(units_change_unscaled)
  # 
  # # median unit change within each exposure getting shifted_add
  # unlist(lapply(units_change_unscaled, median))
  # 
  # # proportion the feasible shift equaled the desired shift
  # mean(shifted_add_feasible == shifted_add) # 0% for 10 unit shift
  # 
  # # summary statistics about the true shift
  # # NA's have to do with dividing by a zero, can be ignored
  # true_shift_add <- shifted_add_feasible / as.matrix(pol)
  # true_shift_add[!is.finite(true_shift_add)] <- NA_real_
  # summary(true_shift_add)
  # 
  # # joining shifts
  # shifted_add_feasible_unnormalized_units_changed <- (units_change * (sapply(pol_unnormalized,function(col) max(col)) - sapply(pol_unnormalized,function(col) min(col)))) + sapply(pol_unnormalized,function(col) min(col))
  # summary(shifted_add_feasible_unnormalized_units_changed)
  # 
  # # returning shifts to original scale
  # #shifted_add_feasible_unnormalized <- (shifted_add_feasible * (sapply(pol_unnormalized,function(col) max(col)) - sapply(pol_unnormalized,function(col) min(col)))) + sapply(pol_unnormalized,function(col) min(col))
  # 
  # shifted_add_feasible <- round(shifted_add_feasible, 3)
  # 
  # shifted_add_data <- shifted_add_feasible |>
  #   as.data.frame() |>
  #   cbind(covars_outcome) |>
  #   relocate(newid, .before = ) # make it the first column
  # 
  # # summary statistics about 1). those getting shifted above observed and 2). those getting shifted below observed forlast 2 exposures
  # 
  # # shifted above observed
  # 
  # row_indices_gly <- which(shifted_add_feasible[, 6] > as.matrix(pol)[, 6]) # X shifted above
  # shifted_add_data[row_indices_gly, ] |> select(-newid) |> summary()
  # 
  # row_indices_paraq <- which(shifted_add_feasible[, 7] > as.matrix(pol)[, 7]) # X shifted above
  # shifted_add_data[row_indices_paraq, ] |> select(-newid) |> summary()
  # 
  # # shifted below observed
  # 
  # row_indices_gly <- which(shifted_add_feasible[, 6] < as.matrix(pol)[, 6]) # X shifted below
  # shifted_add_data[row_indices_gly, ] |> select(-newid) |> summary()
  # 
  # row_indices_paraq <- which(shifted_add_feasible[, 7] < as.matrix(pol)[, 7]) # X shifted below
  # shifted_add_data[row_indices_paraq, ] |> select(-newid) |> summary()
  
  #shifted_add_extrapolate <- original_data[, 2:8]
  #shifted_add_extrapolate_df <- sweep(shifted_add_extrapolate, 2, additive_scale, "-")
  
  #saveRDS(shifted_add_data, paste0("data/shifted_data_convex_add_time_1.rds"))
  
  shifted[[t]] <- shifted_mult_data
  observed[[t]] <- obs_data
  R_statistic[[t]] <- R_statistic_df
}

merged_shifted <- reduce(shifted, left_join, by = "newid")
merged_observed <- reduce(observed, left_join, by = "newid")

covars_outcome <- covars_outcome |>
  select(-c(starts_with("op_kg_2_year_time_"),
            starts_with("pyr_kg_2_year_time_"),
            starts_with("carb_kg_2_year_time_"),
            starts_with("neo_kg_2_year_time_"),
            starts_with("mn_kg_2_year_time_"),
            starts_with("gly_kg_2_year_time_"),
            starts_with("paraq_kg_2_year_time_")))

shifted_mult_final <- covars_outcome |>
  left_join(merged_shifted, by = c("newid" = "newid"))

observed_final <- covars_outcome |>
  left_join(merged_observed, by = c("newid" = "newid"))

saveRDS(shifted_mult_final, paste0("data/shifted_data_convex_mult_neo_shift.rds"))
saveRDS(prop_in_convex_hull, paste0("data/percent_in_convex_hull_neo_shift.rds"))
saveRDS(R_statistic, paste0("data/R_statistic_neo_shift.rds"))


