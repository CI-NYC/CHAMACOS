#remotes::install_github("nt-williams/lmtp@mlr3superlearner") -- use personal version
library(lmtp)
library(mlr3extralearners)
library(earth)
library(ranger)
library(xgboost)
library(tidyverse)
library(data.table)

# reading in the observed data 
data_original <- readRDS(here::here(paste0("data/observed_data.rds"))) 

# longitudinal exposures
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

# baseline covariates
W <- c("cham", 
       "momdl_age2", 
       "educat_bl_2", 
       "educat_bl_3", 
       "hbp_bl", 
       "diab_bl", 
       "born_in_usa"
)

# time varying covariates
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

# learners list -- is flexible 
learners <- list("mean", 
                 "glm",
                 "earth",
                 "cv_glmnet",
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

for (s in c("all", "first_5", "last_2", "paraq")) # the different shifts of interest
{

# shifted data must have censoring set to 1 (observed) for all observations
if (s != "all")
{
data_shifted_mult <- readRDS(here::here(paste0("data/shifted_data_convex_mult_", s, "_shift.rds"))) |>
  mutate(censor_time_1 = 1,
         censor_time_2 = 1,
         censor_time_3 = 1,
         censor_time_4 = 1,
         censor_time_5 = 1) |>
  as.data.frame()
} else{
  data_shifted_mult <- readRDS(here::here("data/shifted_data_convex_mult.rds")) |>
    mutate(censor_time_1 = 1,
           censor_time_2 = 1,
           censor_time_3 = 1,
           censor_time_4 = 1,
           censor_time_5 = 1) |>
    as.data.frame()
}

# only selecting columns we need for the analysis
data_original <- data_original |>
  select(unlist(A), starts_with("mhtn_time_"), unlist(L), W, starts_with("censor_time_")) |>
  as.data.frame()

data_shifted_mult <- data_shifted_mult |>
  select(unlist(A), starts_with("mhtn_time_"), unlist(L), W, starts_with("censor_time_")) |>
  as.data.frame()

print(identical(data_original |> select(starts_with("mhtn_time_"), unlist(L), W), data_shifted_mult |> select(starts_with("mhtn_time_"), unlist(L), W)))

# function to longitudinally run TMLE lmtp code
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
                   mtp = TRUE, # must be set to TRUE for continuous exposure
                   learners_outcome = learners,
                   learners_trt = learners,
                   folds = 20,
                   control = lmtp_control(#.learners_outcome_folds = NULL,
                                          #.learners_trt_folds = NULL,
                                          .discrete = FALSE,
                                          .trim = 0.99
                   ))
  
  res
}

for (i in 5:1)
{
  
  # running shift
  set.seed(5)
  mult_all <- run_lmtp(shifted = data_shifted_mult)
  saveRDS(mult_all, here::here(paste0("results/", "mhtn_mult_t_", i, "_shifting_", s, "_20percent.rds")))

  # running observed
  if (s == "all")
  {
  set.seed(5)
  obs_all <- run_lmtp(shifted = NULL)
  saveRDS(obs_all, here::here(paste0("results/", "mhtn_obs_t_", i, "_shifting_all_20percent.rds")))
  }
}
}

