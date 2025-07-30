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

data_original <- readRDS(here::here(paste0("data/observed_data.rds"))) 

A <- list(c("op_kg_2_year_time_1",
            "pyr_kg_2_year_time_1",
            "carb_kg_2_year_time_1",
            "neo_kg_2_year_time_1",
            "mn_kg_2_year_time_1",
            "gly_kg_2_year_time_1",
            "paraq_kg_2_year_time_1"),
          c("op_kg_2_year_time_2",
            "pyr_kg_2_year_time_2",
            "carb_kg_2_year_time_2",
            "neo_kg_2_year_time_2",
            "mn_kg_2_year_time_2",
            "gly_kg_2_year_time_2",
            "paraq_kg_2_year_time_2"),
          c("op_kg_2_year_time_3",
            "pyr_kg_2_year_time_3",
            "carb_kg_2_year_time_3",
            "neo_kg_2_year_time_3",
            "mn_kg_2_year_time_3",
            "gly_kg_2_year_time_3",
            "paraq_kg_2_year_time_3"),
          c("op_kg_2_year_time_4",
            "pyr_kg_2_year_time_4",
            "carb_kg_2_year_time_4",
            "neo_kg_2_year_time_4",
            "mn_kg_2_year_time_4",
            "gly_kg_2_year_time_4",
            "paraq_kg_2_year_time_4"),
          c("op_kg_2_year_time_5",
            "pyr_kg_2_year_time_5",
            "carb_kg_2_year_time_5",
            "neo_kg_2_year_time_5",
            "mn_kg_2_year_time_5",
            "gly_kg_2_year_time_5",
            "paraq_kg_2_year_time_5")
)

# data_original <- data_original |>
#   mutate(across(
#     all_of(unlist(A)),
#     ~ pmin(., quantile(., 0.95, na.rm = TRUE))
#   ))

data_shifted_mult_all <- readRDS(here::here("data/shifted_data_convex_mult.rds")) |>
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
    "work_cat_time_1"),
  c(
    "age_time_2",
    #"marstat_time_2",
    "marstat_2_time_2",
    "marstat_3_time_2",
    "marstat_4_time_2",
    "marstat_5_time_2",
    "marstat_6_time_2",
    "marcat_time_2",
    #"ipovcat_time_2",
    "ipovcat_2_time_2",
    "ipovcat_3_time_2",
    "hhagwork_time_2",
    "work_cat_time_2"
  ),
  c(
    "age_time_3",
    #"marstat_time_3",
    "marstat_2_time_3",
    "marstat_3_time_3",
    "marstat_4_time_3",
    "marstat_5_time_3",
    "marstat_6_time_3",
    "marcat_time_3",
    #"ipovcat_time_3",
    "ipovcat_2_time_3",
    "ipovcat_3_time_3",
    "hhagwork_time_3",
    "work_cat_time_3"),
  c("age_time_4",
    #"marstat_time_4",
    "marstat_2_time_4",
    "marstat_3_time_4",
    "marstat_4_time_4",
    "marstat_5_time_4",
    "marstat_6_time_4",
    "marcat_time_4",
    #"ipovcat_time_4",
    "ipovcat_2_time_4",
    "ipovcat_3_time_4",
    "hhagwork_time_4",
    #"work_cat_time_4",
    "work_cat_time_4"),
  c(
    "age_time_5",
    #"marstat_time_5",
    "marstat_2_time_5",
    "marstat_3_time_5",
    "marstat_4_time_5",
    "marstat_5_time_5",
    "marstat_6_time_5",
    "marcat_time_5",
    #"ipovcat_time_5",
    "ipovcat_2_time_5",
    "ipovcat_3_time_5",
    "hhagwork_time_5",
    "work_cat_time_5")
) 

learners <- list("mean", 
                 "glm",
                 "earth",
                 "xgboost",
                 list("xgboost", 
                      min_child_weight = 5, 
                      id = "xgboost1"),
                 list("xgboost", 
                      lambda = 2, 
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
                   trt = A[1:i],
                   outcome = paste0("mhtn_", c("time_1", "time_2", "time_3", "time_4", "time_5"))[1:i], 
                   baseline = W, 
                   time_vary = L[1:i],
                   cens = paste0("censor_", c("time_1", "time_2", "time_3", "time_4", "time_5"))[1:i], 
                   outcome_type  = ifelse(i == 1, "binomial", "survival"),
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

for (i in 3:1)
{
  
  set.seed(5)
  mult_all <- run_lmtp(shifted = data_shifted_mult_all)
  saveRDS(mult_all, here::here(paste0("results_072225/", "mhtn_mult_t_", i, "_shifting_all_20percent.rds")))

  set.seed(5)
  obs_all <- run_lmtp(shifted = NULL)
  saveRDS(obs_all, here::here(paste0("results_072225/", "mhtn_obs_t_", i, "_shifting_all_20percent.rds")))

  # set.seed(5)
  # add_all <-run_lmtp(outcome_timepoint = time, shifted = data_shifted_add_all)
  # saveRDS(add_all, here::here(paste0("results_longitudinal/", "mhtn_add_", time, "_years_observed_", ".rds")))
}


