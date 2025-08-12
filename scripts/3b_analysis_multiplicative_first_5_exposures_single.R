#remotes::install_github("nt-williams/lmtp@mlr3superlearner") -- use personal version

library(lmtp)
library(mlr3extralearners)
library(earth)
library(ranger)
library(xgboost)
library(tidyverse)
library(data.table)

norm01 <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

data_original <- readRDS(here::here(paste0("data/observed_data.rds"))) |>
  mutate(censor_time_5 = case_when(mhtn_time_4 == 1 ~ 1,
                                   is.na(censor_time_5) ~ 0,
                                   TRUE ~ censor_time_5))

A <- list(c("op_kg_2_year_time_1",
            "pyr_kg_2_year_time_1",
            "carb_kg_2_year_time_1",
            "neo_kg_2_year_time_1",
            "mn_kg_2_year_time_1",
            "gly_kg_2_year_time_1",
            "paraq_kg_2_year_time_1")
)

# data_original <- data_original |>
#   mutate(across(
#     all_of(unlist(A)),
#     ~ pmin(., quantile(., 0.95, na.rm = TRUE))
#   ))

data_shifted_mult_all <- readRDS(here::here("data/shifted_data_convex_mult_first_5_shift.rds")) |>
  mutate(censor_time_1 = 1,
         censor_time_2 = 1,
         censor_time_3 = 1,
         censor_time_4 = 1,
         censor_time_5 = 1) |>
  as.data.frame()

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

L <- list(
  c(
    "age_time_1",
    #"marstat_time_1",
    "marstat_2_time_1",
    "marstat_3_time_1",
    "marstat_4_time_1",
    "marstat_5_time_1",
    "marstat_6_time_1",
    "marcat_time_1",
    #"ipovcat_time_1",
    "ipovcat_2_time_1",
    "hhagwork_time_1",
    "work_cat_time_1")
) 

learners <- list("mean", 
                 "glm",
                 "earth",
                 "xgboost",
                 list("xgboost", 
                      min_child_weight = 5, 
                      id = "xgboost1"),
                 list("xgboost", 
                      lambda = 5, 
                      id = "xgboost1"),
                 list("xgboost", 
                      lambda = 10, 
                      id = "xgboost1"),
                 "ranger"
)

data_original <- data_original |>
  select(unlist(A), starts_with("mhtn_time_"), unlist(L), W, starts_with("censor_time_")) |>
  as.data.frame()

data_shifted_mult_all <- data_shifted_mult_all |>
  select(unlist(A), starts_with("mhtn_time_"), unlist(L), W, starts_with("censor_time_")) |>
  as.data.frame()

run_lmtp <- function(data = data_original, shifted = NULL)
{
  res <- lmtp_tmle(data, 
                   trt = A,
                   outcome = paste0("mhtn_", c("time_5")), 
                   baseline = W, 
                   time_vary = L,
                   cens = paste0("censor_", c("time_5")), 
                   outcome_type  = "binomial",
                   shifted = shifted, 
                   mtp = TRUE, 
                   learners_outcome = learners,
                   learners_trt = learners,
                   folds = 10,
                   control = lmtp_control(#.learners_outcome_folds = NULL,
                     #.learners_trt_folds = NULL,
                     .trim = 0.99
                   ))
  
  res
}

set.seed(5)
mult_all <- run_lmtp(shifted = data_shifted_mult_all)
saveRDS(mult_all, here::here(paste0("results/", "mhtn_mult_shifting_first_5_20percent_single.rds")))

set.seed(5)
obs_all <- run_lmtp(shifted = NULL)
saveRDS(obs_all, here::here(paste0("results/", "mhtn_obs_shifting_first_5_20percent_single.rds")))

# set.seed(5)
# add_all <-run_lmtp(outcome_timepoint = time, shifted = data_shifted_add_all)
# saveRDS(add_all, here::here(paste0("results_longitudinal/", "mhtn_add_", time, "_years_observed_", ".rds")))