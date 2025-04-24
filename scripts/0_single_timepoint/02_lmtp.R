#remotes::install_github("nt-williams/lmtp@local-riesznet")
library(lmtp)
library(mlr3extralearners)
library(earth)
library(ranger)
library(xgboost)
library(tidyverse)

norm01 <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

for (years in c("1"#, 
                #"1_5"#,
                #"2"
                ))
{
for (time in c("10_5y"#, 
  #"12y"#, "14y", "16y", "18y", 
  #"mc1" #mc1 doesn't work with 2 years due to low censoring counts
))
{
data_original <- readRDS(here::here(paste0("data/original_data_time_1_years_obs_", years, "_last_2_exposures.rds"))) |>
  mutate(censor_10_5y = ifelse(is.na(mhtn_10_5y), 0, 1),
         censor_12y = ifelse(is.na(mhtn_12y), 0, 1),
         censor_14y = ifelse(is.na(mhtn_14y), 0, 1),
         censor_16y = ifelse(is.na(mhtn_16y), 0, 1),
         censor_18y = ifelse(is.na(mhtn_18y), 0, 1),
         censor_mc1 = ifelse(is.na(mhtn_mc1), 0, 1)) |>
  as.data.frame() |>
  mutate(conditional_10_5y = ifelse(d_gly_kg_2_year_10_5y == 1 | d_paraq_kg_2_year_10_5y == 1, 1, 0))

data_shifted_mult <- readRDS(here::here(paste0("data/shifted_data_80_percent_time_1_years_obs_", years, "_last_2_exposures.rds"))) |>
  mutate(censor_10_5y = 1,
         censor_12y = 1,
         censor_14y = 1,
         censor_16y = 1,
         censor_18y = 1,
         censor_mc1 = 1) |>
  as.data.frame() |>
  mutate(conditional_10_5y = ifelse(d_gly_kg_2_year_10_5y == 1 | d_paraq_kg_2_year_10_5y == 1, 1, 0))

data_shifted_add <- readRDS(here::here(paste0("data/shifted_data_10_scaled_units_time_1_years_obs_", years, "_last_2_exposures.rds"))) |>
  mutate(censor_10_5y = 1,
         censor_12y = 1,
         censor_14y = 1,
         censor_16y = 1,
         censor_18y = 1,
         censor_mc1 = 1) |>
  as.data.frame() |>
  mutate(conditional_10_5y = ifelse(d_gly_kg_2_year_10_5y == 1 | d_paraq_kg_2_year_10_5y == 1, 1, 0))

A <- list(c("op_kg_2_year_10_5y",
       "pyr_kg_2_year_10_5y",
       "carb_kg_2_year_10_5y",
       "neo_kg_2_year_10_5y",
       "mn_kg_2_year_10_5y",
       "gly_kg_2_year_10_5y",
       "paraq_kg_2_year_10_5y"
       ))

data_shifted_mult_all <- readRDS(paste0("data/original_data_time_1_years_obs_", years, "_last_2_exposures.rds")) |>
  mutate(across(all_of(c("gly_kg_2_year_10_5y", "paraq_kg_2_year_10_5y")), ~ . * 0.8)) |>
  mutate(censor_10_5y = 1,
         censor_12y = 1,
         censor_14y = 1,
         censor_16y = 1,
         censor_18y = 1,
         censor_mc1 = 1) |>
  as.data.frame() |>
  mutate(conditional_10_5y = ifelse(d_gly_kg_2_year_10_5y == 1 | d_paraq_kg_2_year_10_5y == 1, 1, 0))

data_shifted_add_all <- readRDS(paste0("data/original_data_time_1_years_obs_", years, "_last_2_exposures.rds")) |>
  mutate(across(all_of(unlist(A)), list(copy = ~ .))) |>
  mutate(across(all_of(c("gly_kg_2_year_10_5y", "paraq_kg_2_year_10_5y")), ~ . - 10)) |>
  mutate(censor_10_5y = 1,
         censor_12y = 1,
         censor_14y = 1,
         censor_16y = 1,
         censor_18y = 1,
         censor_mc1 = 1) |>
  # if shift takes below 0, keep at observed levels
  mutate(op_kg_2_year_10_5y = ifelse(op_kg_2_year_10_5y > 0, op_kg_2_year_10_5y, op_kg_2_year_10_5y_copy),
         pyr_kg_2_year_10_5y = ifelse(pyr_kg_2_year_10_5y > 0, pyr_kg_2_year_10_5y, pyr_kg_2_year_10_5y_copy),
         carb_kg_2_year_10_5y = ifelse(carb_kg_2_year_10_5y > 0, carb_kg_2_year_10_5y, carb_kg_2_year_10_5y_copy),
         neo_kg_2_year_10_5y = ifelse(neo_kg_2_year_10_5y > 0, neo_kg_2_year_10_5y, neo_kg_2_year_10_5y_copy),
         mn_kg_2_year_10_5y = ifelse(mn_kg_2_year_10_5y > 0, mn_kg_2_year_10_5y, mn_kg_2_year_10_5y_copy),
         gly_kg_2_year_10_5y = ifelse(gly_kg_2_year_10_5y > 0, gly_kg_2_year_10_5y, gly_kg_2_year_10_5y_copy),
         paraq_kg_2_year_10_5y = ifelse(paraq_kg_2_year_10_5y > 0, paraq_kg_2_year_10_5y, paraq_kg_2_year_10_5y_copy)) |>
  as.data.frame() |>
  select(-c(ends_with("_copy"))) |>
  mutate(conditional_10_5y = ifelse(d_gly_kg_2_year_10_5y == 1 | d_paraq_kg_2_year_10_5y == 1, 1, 0))

# try including lagged variables?
W <- c("cham", 
"momdl_age2", 
"educat_bl_2", 
"educat_bl_3", 
"hbp_bl", 
"diab_bl", 
"born_in_usa"#,
#"diabage_bl"#,
# "age_9y",
# #"marstat_9y",
# "marstat_9y_2",
# "marstat_9y_3",
# "marstat_9y_4",
# "marstat_9y_5",
# "marstat_9y_6",
# "marcat_9y",
# #"ipovcat_9y",
# "ipovcat_9y_2",
# "ipovcat_9y_3",
# "hhagwork_9y",
# #"work_cat_9y",
# "work_cat_9y_1",
# "work_cat_9y_2",
# "work_cat_9y_3"
)

L <- list(c(
"age_10_5y",
#"marstat_10_5y",
"marstat_10_5y_2",
"marstat_10_5y_3",
"marstat_10_5y_4",
"marstat_10_5y_5",
"marstat_10_5y_6",
"marcat_10_5y",
#"ipovcat_10_5y",
"ipovcat_10_5y_2",
"hhagwork_10_5y",
#"work_cat_10_5y",
"work_cat_10_5y_1",
"work_cat_10_5y_2",
"work_cat_10_5y_3")) 

learners <- list("mean", 
                 "glm",
                 "earth",
                 "xgboost",
                 list("xgboost", 
                      min_child_weight = 5, 
                      id = "xgboost1"),
                 "ranger",
                 list("ranger", 
                      num.trees = 1000, 
                      id = "ranger1")
)

run_lmtp <- function(data = data_original, outcome_timepoint = "10_5y", shifted = NULL, conditional = NULL)
{
  conditional_matrix <- data |>
    select(conditional) |>
    mutate(across(all_of(conditional), ~ as.logical(.))) |>
    as.matrix()
  
    res <- lmtp_tmle(data, 
             trt = A,
             outcome = paste0("mhtn_", outcome_timepoint), 
             baseline = W, 
             time_vary = L,
             cens = paste0("censor_", outcome_timepoint), 
             conditional = conditional_matrix, 
             shifted = shifted, 
             mtp = TRUE, 
             outcome_type = c("binomial"),
             learners_outcome = learners,
             learners_trt = learners,
             folds = 10, # change to 20 for final run
             control = lmtp_control(.learners_outcome_folds = 5,
                                    .learners_trt_folds = 5,
                                    .learners_conditional_folds = 5,
                                    .trim = 0.95,
                                    .patience = 10,
                                    .epochs = 50L,
                                    .batch_size = 8,
                                    .learning_rate = 0.01,
                                    .weight_decay = 1
                                    ))
    
    res
}

set.seed(5)
mult <- run_lmtp(data = data_original |>
                   mutate(across(unlist(A), norm01)),
                 outcome_timepoint = time, shifted = data_shifted_mult, conditional = "conditional_10_5y")
saveRDS(mult, here::here(paste0("results/", "mhtn_mult_convex_hull_time_", time, "_years_observed_",  years, ".rds")))

set.seed(5)
mult_all <- run_lmtp(outcome_timepoint = time, shifted = data_shifted_mult_all, conditional = "conditional_10_5y")
saveRDS(mult_all, here::here(paste0("results/", "mhtn_mult_", time, "_years_observed_",  years, ".rds")))

set.seed(5)
add <- run_lmtp(data = data_original |>
                  mutate(across(unlist(A), norm01)),
                outcome_timepoint = time, shifted = data_shifted_add, conditional = "conditional_10_5y")
saveRDS(add, here::here(paste0("results/", "mhtn_add_convex_hull_time_", time, "_years_observed_",  years, ".rds")))

set.seed(5)
add_all <-run_lmtp(outcome_timepoint = time, shifted = data_shifted_add_all, conditional = "conditional_10_5y")
saveRDS(add_all, here::here(paste0("results/", "mhtn_add_", time, "_years_observed_",  years, ".rds")))

set.seed(5)
obs <- run_lmtp(data = data_original |>
                  mutate(across(unlist(A), norm01)),
                outcome_timepoint = time, shifted = NULL, conditional = "conditional_10_5y")
saveRDS(obs, here::here(paste0("results/", "mhtn_obs_", time, "_years_observed_",  years, ".rds")))

set.seed(5)
obs_all <- run_lmtp(outcome_timepoint = time, shifted = NULL, conditional = "conditional_10_5y")
saveRDS(obs_all, here::here(paste0("results/", "mhtn_obs_", time, "_years_observed_",  years, ".rds")))

print(paste0("years observed >= ", years, " years, truncate >= 95th percentile, outcome timepoint: ", time))
print(lmtp_contrast(mult, ref = obs))
print(lmtp_contrast(mult_all, ref = obs_all))
print(lmtp_contrast(add, ref = obs))
print(lmtp_contrast(add_all, ref = obs_all))
}
}

