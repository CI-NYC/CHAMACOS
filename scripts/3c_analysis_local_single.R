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

data_original <- readRDS(paste0("data/longitudinal_data_aligned.rds")) |>
  mutate(censor_time_5 = case_when(mhtn_time_4 == 1 ~ 1,
                                   is.na(censor_time_5) ~ 0,
                                   TRUE ~ censor_time_5)) |>
  mutate(conditional_time_1 = case_when(gly_kg_2_year_time_1 >= 25 & paraq_kg_2_year_time_1 >= 5 ~ 1, 
                                        TRUE ~ 0)) 

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

data_shifted_mult_all <- readRDS(paste0("data/longitudinal_data_aligned.rds")) |>
  mutate(conditional_time_1 = case_when(gly_kg_2_year_time_1 >= 25 & paraq_kg_2_year_time_1 >= 5 ~ 1, 
                                        TRUE ~ 0))  |>
  mutate(gly_kg_2_year_time_1 = gly_kg_2_year_time_1 * 0.8, # shifting gly down by 20% 
         paraq_kg_2_year_time_1 = paraq_kg_2_year_time_1 * 0.8# shifting paraq down by 20% 
    
  ) |>
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
       "born_in_usa"
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
  select(unlist(A), starts_with("mhtn_time_"), unlist(L), W, starts_with("censor_time_"), "conditional_time_1") |>
  as.data.frame()

data_shifted_mult_all <- data_shifted_mult_all |>
  select(unlist(A), starts_with("mhtn_time_"), unlist(L), W, starts_with("censor_time_"), "conditional_time_1") |>
  as.data.frame()

run_lmtp <- function(data = data_original, shifted = NULL)
{
  conditional_df <- data |>
    select(conditional_time_1,# paste0("censor_time_", 1:i)
    ) |>
    mutate(conditional_time_1 = as.logical(conditional_time_1))
  
  n <- nrow(data)
  
  conditional_matrix <- as.matrix(conditional_df)
  
  res <- lmtp_tmle(data, 
                   trt = A,
                   outcome = "mhtn_time_5", 
                   baseline = W, 
                   time_vary = L,
                   cens = "censor_time_5", 
                   conditional = conditional_matrix, 
                   outcome_type  = "binomial",
                   shifted = shifted, 
                   mtp = TRUE, 
                   learners_outcome = learners,
                   learners_trt = learners,
                   folds = 10,
                   control = lmtp_control(#.learners_outcome_folds = 5,
                     #.learners_trt_folds = 5,
                     #.learners_conditional_folds = 5,
                     .trim = 0.99,
                     .patience = 10,
                     .epochs = 50L,
                     .batch_size = 8,
                     .learning_rate = 0.01,
                     .weight_decay = 1
                   ))
  
  res
}


set.seed(5)
mult_all <- run_lmtp(shifted = data_shifted_mult_all)
saveRDS(mult_all, here::here(paste0("results/", "local_mult.rds")))

set.seed(5)
obs_all <- run_lmtp(shifted = NULL)
saveRDS(obs_all, here::here(paste0("results/", "local_obs.rds")))

obs_all <- readRDS(here::here(paste0("results/", "local_obs.rds")))
mult_all <- readRDS(here::here(paste0("results/", "local_mult.rds")))

lmtp_contrast(mult_all, ref = obs_all)

