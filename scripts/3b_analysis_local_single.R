# See https://beyondtheate.com for more information

### NOTE: MUST INSTALL LOCAL-RIESZNET VERSION OF LMTP

#remotes::install_github("nt-williams/lmtp@local-riesznet")
library(lmtp)
library(mlr3extralearners)
library(earth)
library(ranger)
library(xgboost)
library(tidyverse)

# constructing our conditionals (to whom do we want to target our effect?)
get_conditional <- readRDS(here::here("data/longitudinal_data_aligned.rds"))  |>
  mutate(conditional_time_1 = case_when(gly_kg_2_year_time_1 >= 25 | paraq_kg_2_year_time_1 >= 5 ~ 1, 
                                        TRUE ~ 0),
         conditional2_time_1 = case_when(gly_kg_2_year_time_1 >= 25 & paraq_kg_2_year_time_1 >= 5 ~ 1, 
                                        TRUE ~ 0)) 

# reading in our data and setting up for a single timepoint exposure and outcome
data_original <- readRDS(here::here(paste0("data/observed_data.rds"))) |>
  mutate(censor_time_5 = case_when(mhtn_time_4 == 1 ~ 1,
                                   is.na(censor_time_5) ~ 0,
                                   TRUE ~ censor_time_5)) |>
  mutate(conditional_time_1 = get_conditional$conditional_time_1,
         conditional2_time_1 = get_conditional$conditional2_time_1)

# we should check what percentile these values correspond to in the data
gly_func <- ecdf(data_original$gly_kg_2_year_time_1)
gly_func(25)

paraq_func <- ecdf(data_original$paraq_kg_2_year_time_1)
paraq_func(5)

# exposures
A <- list(c("op_kg_2_year_time_1",
            "pyr_kg_2_year_time_1",
            "carb_kg_2_year_time_1",
            "neo_kg_2_year_time_1",
            "mn_kg_2_year_time_1",
            "gly_kg_2_year_time_1",
            "paraq_kg_2_year_time_1")
)

# our shifted data
data_shifted_mult_all <- readRDS(here::here("data/shifted_data_convex_mult_last_2_shift.rds")) |>
  mutate(conditional_time_1 = get_conditional$conditional_time_1,
         conditional2_time_1 = get_conditional$conditional2_time_1) |>
  mutate(censor_time_1 = 1,
         censor_time_2 = 1,
         censor_time_3 = 1,
         censor_time_4 = 1,
         censor_time_5 = 1) |>
  as.data.frame()

# baseline covariates
W <- c("cham", 
       "momdl_age2", 
       "educat_bl_2", 
       "educat_bl_3", 
       "hbp_bl", 
       "diab_bl", 
       "born_in_usa"
)

# time-varying covariates
L <- list(
  c("marcat_time_1",
    #"ipovcat_time_1",
    "ipovcat_2_time_1",
    "ipovcat_3_time_1",
    "hhagwork_time_1",
    "work_cat_time_1")
) 

# learners list -- is flexible 
learners <- list("mean", 
                 "glm",
                 "earth",
                 "cv_glmnet",
                 list("lightgbm", 
                      min_sum_hessian_in_leaf = 10, 
                      id = "lightgbm1"),
                 list("lightgbm", 
                      lambda_l2 = 10, 
                      id = "lightgbm2"),
                 list("lightgbm", 
                      lambda_l1 = 10, 
                      id = "lightgbm3"),
                 "ranger"
)

data_original <- data_original |>
  select(unlist(A), starts_with("mhtn_time_"), unlist(L), W, starts_with("censor_time_"), starts_with("conditional")) |>
  as.data.frame()

data_shifted_mult_all <- data_shifted_mult_all |>
  select(unlist(A), starts_with("mhtn_time_"), unlist(L), W, starts_with("censor_time_"), starts_with("conditional")) |>
  as.data.frame()

# function to run TMLE  local lmtp code
run_lmtp <- function(data = data_original, shifted = NULL, conditional = "")
{
  if (conditional == "")
  {
  conditional_matrix <- data |>
    select(conditional_time_1) |>
    mutate(conditional_time_1 = as.logical(conditional_time_1)) |>
    as.matrix()
  } else if (conditional == "_and")
  {
    conditional_matrix <- data |>
      select(conditional2_time_1) |>
      mutate(conditional2_time_1 = as.logical(conditional2_time_1)) |>
      as.matrix()
  }
  
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
                   folds = 20,
                   control = lmtp_control(#.learners_outcome_folds = 5,
                     #.learners_trt_folds = 5,
                     #.learners_conditional_folds = 5,
                     .trim = 0.99,
                     .patience = 10,
                     .epochs = 50L,
                     .batch_size = 8,
                     .learning_rate = 0.001,
                     .weight_decay = 0.0001
                   ))
  
  res
}

for (i in c(""))
{
set.seed(5)
mult_all <- run_lmtp(shifted = data_shifted_mult_all, conditional = i)
saveRDS(mult_all, here::here(paste0("results/", "local_mult", i, ".rds")))

set.seed(5)
obs_all <- run_lmtp(shifted = NULL, conditional = i)
saveRDS(obs_all, here::here(paste0("results/", "local_obs", i, ".rds")))

obs_all <- readRDS(here::here(paste0("results/", "local_obs", i, ".rds")))
mult_all <- readRDS(here::here(paste0("results/", "local_mult", i, ".rds")))

contrast <- lmtp_contrast(mult_all, ref = obs_all)

print(contrast)
}

contrasts_df_final <- tibble(
  shift = contrast$vals$shift,
  ref = contrast$vals$ref,
  estimate = contrast$vals$estimate,
  std.error = contrast$vals$std.error,
  conf.low = contrast$vals$conf.low,
  conf.high = contrast$vals$conf.high,
  p.value = contrast$vals$p.value,
)

write.csv(contrasts_df_final, here::here(paste0("results_csv/contrasts_local.csv")))

