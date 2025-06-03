library(tidyverse)
library(fastDummies)
library(purrr)
library(zoo)

data <- read_csv("data/rudolph_01b.csv") |>
  # removing any variables from the 14 year visit
  select(-matches("_14_y")) |>
    filter(cancer_bl == 0 | is.na(cancer_bl)) |> # only keep no cancer or missing (n = 2 removed)
    mutate(first_obs = case_when(years_2y_10_5y >= 1 ~ "10_5y", # getting period of "first" observation (where years >= 1), this is time t = 0/1
                                 years_2y_12y >= 1 ~ "12y",
                                 #years_2y_14y >= 1 ~ "14y",
                                 years_2y_16y >= 1 ~ "16y",
                                 years_2y_18y >= 1 ~ "18y",
                                 years_2y_mc1 >= 1 ~ "mc1",
                                 TRUE ~ as.character(NA)
                                 )) |>
  filter(is.na(first_obs) == FALSE) |> # removing those that are never observed for >= 1 year (n = 109 removed)
  # censoring when outcome is missing
  mutate(censored_period = case_when(first_obs == "10_5y" & (is.na(mhtn_10_5y)) ~ 1,
                                     first_obs == "10_5y" & (is.na(mhtn_12y)) ~ 2,
                                     #first_obs == "10_5y" & (is.na(mhtn_14y)) ~ 2,
                                     first_obs == "10_5y" & (is.na(mhtn_16y)) ~ 3,
                                     first_obs == "10_5y" & (is.na(mhtn_18y)) ~ 4,
                                     first_obs == "10_5y" & (is.na(mhtn_mc1)) ~ 5,
                                     first_obs == "12y" & (is.na(mhtn_12y)) ~ 1,
                                     #first_obs == "12y" & (is.na(mhtn_14y)) ~ 1,
                                     first_obs == "12y" & (is.na(mhtn_16y)) ~ 2,
                                     first_obs == "12y" & (is.na(mhtn_18y)) ~ 3,
                                     first_obs == "12y" & (is.na(mhtn_mc1)) ~ 4,
                                     first_obs == "12y" ~ 5,
                                     # first_obs == "14y" & (is.na(mhtn_14y)) ~ 1,
                                     # first_obs == "14y" & (is.na(mhtn_16y)) ~ 2,
                                     # first_obs == "14y" & (is.na(mhtn_18y)) ~ 3,
                                     # first_obs == "14y" & (is.na(mhtn_mc1)) ~ 4,
                                     first_obs == "16y" & (is.na(mhtn_16y)) ~ 1,
                                     first_obs == "16y" & (is.na(mhtn_18y)) ~ 2,
                                     first_obs == "16y" & (is.na(mhtn_mc1)) ~ 3,
                                     first_obs == "16y" ~ 4,
                                     first_obs == "18y" & (is.na(mhtn_18y)) ~ 1,
                                     first_obs == "18y" & (is.na(mhtn_mc1)) ~ 2,
                                     first_obs == "18y" ~ 3,
                                     first_obs == "mc1" & (is.na(mhtn_mc1)) ~ 1,
                                     first_obs == "mc1" ~ 2,
                                     TRUE ~ 0 # never censored
  ))
  # censoring when one of the following occurs: outcome is missing, number of years observed falls below 1 or is missing
  # mutate(censored_period = case_when(first_obs == "10_5y" & (is.na(mhtn_10_5y)) ~ 0,
  #                                    first_obs == "10_5y" & (is.na(years_2y_12y) | years_2y_12y < 1 | is.na(mhtn_12y)) ~ 1,
  #                                    #first_obs == "10_5y" & (is.na(years_2y_14y) | years_2y_14y < 1 | is.na(mhtn_14y)) ~ 2,
  #                                    first_obs == "10_5y" & (is.na(years_2y_16y) | years_2y_16y < 1 | is.na(mhtn_16y)) ~ 2,
  #                                    first_obs == "10_5y" & (is.na(years_2y_18y) | years_2y_18y < 1 | is.na(mhtn_18y)) ~ 3,
  #                                    first_obs == "10_5y" & (is.na(years_2y_mc1) | years_2y_mc1 < 1 | is.na(mhtn_mc1)) ~ 4,
  #                                    first_obs == "12y" & (is.na(mhtn_12y)) ~ 0,
  #                                    #first_obs == "12y" & (is.na(years_2y_14y) | years_2y_14y < 1 | is.na(mhtn_14y)) ~ 1,
  #                                    first_obs == "12y" & (is.na(years_2y_16y) | years_2y_16y < 1 | is.na(mhtn_16y)) ~ 1,
  #                                    first_obs == "12y" & (is.na(years_2y_18y) | years_2y_18y < 1 | is.na(mhtn_18y)) ~ 2,
  #                                    first_obs == "12y" & (is.na(years_2y_mc1) | years_2y_mc1 < 1 | is.na(mhtn_mc1)) ~ 3,
  #                                    first_obs == "12y" ~ 4,
  #                                    # first_obs == "14y" & (is.na(mhtn_14y)) ~ 0,
  #                                    # first_obs == "14y" & (is.na(years_2y_16y) | years_2y_16y < 1 | is.na(mhtn_16y)) ~ 1,
  #                                    # first_obs == "14y" & (is.na(years_2y_18y) | years_2y_18y < 1 | is.na(mhtn_18y)) ~ 2,
  #                                    # first_obs == "14y" & (is.na(years_2y_mc1) | years_2y_mc1 < 1 | is.na(mhtn_mc1)) ~ 3,
  #                                    first_obs == "16y" & (is.na(mhtn_16y)) ~ 0,
  #                                    first_obs == "16y" & (is.na(years_2y_18y) | years_2y_18y < 1 | is.na(mhtn_18y)) ~ 1,
  #                                    first_obs == "16y" & (is.na(years_2y_mc1) | years_2y_mc1 < 1 | is.na(mhtn_mc1)) ~ 2,
  #                                    first_obs == "16y" ~ 3,
  #                                    first_obs == "18y" & (is.na(mhtn_18y)) ~ 0,
  #                                    first_obs == "18y" & (is.na(years_2y_mc1) | years_2y_mc1 < 1 | is.na(mhtn_mc1)) ~ 1,
  #                                    first_obs == "18y" ~ 2,
  #                                    first_obs == "mc1" & (is.na(mhtn_mc1)) ~ 0,
  #                                    first_obs == "mc1" ~ 1,
  #                                    TRUE ~ as.numeric(NA) # never censored
  #                                    ))

# fixing outcome for survival (carrying forward hypertension)
data <- data |>
  mutate(mhtn_12y = ifelse(mhtn_10_5y == 1, 1, mhtn_12y),
         #mhtn_14y = ifelse(mhtn_12y == 1, 1, mhtn_14y),
         mhtn_16y = ifelse(mhtn_12y == 1, 1, mhtn_16y), #do not carry forward 14 year
         mhtn_18y = ifelse(mhtn_16y == 1, 1, mhtn_18y),
         mhtn_mc1 = ifelse(mhtn_18y == 1, 1, mhtn_mc1)
         )

# setting missing exposures to missing when years_PERIOD or years_PERIOD < 1 
vars_10_5y <- c("op_kg_2_year_10_5y",
                "pyr_kg_2_year_10_5y",
                "carb_kg_2_year_10_5y",
                "neo_kg_2_year_10_5y",
                "mn_kg_2_year_10_5y",
                "gly_kg_2_year_10_5y",
                "paraq_kg_2_year_10_5y"
                )

vars_12y <- c("op_kg_2_year_12y",
                "pyr_kg_2_year_12y",
                "carb_kg_2_year_12y",
                "neo_kg_2_year_12y",
                "mn_kg_2_year_12y",
                "gly_kg_2_year_12y",
                "paraq_kg_2_year_12y"
)

# vars_14y <- c("op_kg_2_year_14y",
#               "pyr_kg_2_year_14y",
#               "carb_kg_2_year_14y",
#               "neo_kg_2_year_14y",
#               "mn_kg_2_year_14y",
#               "gly_kg_2_year_14y",
#               "paraq_kg_2_year_14y"
# )

vars_16y <- c("op_kg_2_year_16y",
              "pyr_kg_2_year_16y",
              "carb_kg_2_year_16y",
              "neo_kg_2_year_16y",
              "mn_kg_2_year_16y",
              "gly_kg_2_year_16y",
              "paraq_kg_2_year_16y"
)

vars_18y <- c("op_kg_2_year_18y",
              "pyr_kg_2_year_18y",
              "carb_kg_2_year_18y",
              "neo_kg_2_year_18y",
              "mn_kg_2_year_18y",
              "gly_kg_2_year_18y",
              "paraq_kg_2_year_18y"
)

vars_mc1 <- c("op_kg_2_year_mc1",
              "pyr_kg_2_year_mc1",
              "carb_kg_2_year_mc1",
              "neo_kg_2_year_mc1",
              "mn_kg_2_year_mc1",
              "gly_kg_2_year_mc1",
              "paraq_kg_2_year_mc1"
)

data <- data |>
  mutate(across(all_of(vars_10_5y), ~ if_else(is.na(years_2y_10_5y) | years_2y_10_5y < 1, NA, .)),
         across(all_of(vars_12y), ~ if_else(is.na(years_2y_12y) | years_2y_12y < 1, NA, .)),
         #across(all_of(vars_14y), ~ if_else(is.na(years_2y_14y) | years_2y_14y < 1, NA, .)),
         across(all_of(vars_16y), ~ if_else(is.na(years_2y_16y) | years_2y_16y < 1, NA, .)),
         across(all_of(vars_18y), ~ if_else(is.na(years_2y_18y) | years_2y_18y < 1, NA, .)),
         across(all_of(vars_mc1), ~ if_else(is.na(years_2y_mc1) | years_2y_mc1 < 1, NA, .)),
         )

# identifying individuals who experienced outcome before first observation (n = 11 removed)
data <- data |>
  mutate(outcome_before_first_flag = case_when(mhtn_10_5y == 1 & first_obs == "12y" ~ 1,
                                               #mhtn_10_5y == 1 & first_obs == "14y" ~ 1,
                                               mhtn_10_5y == 1 & first_obs == "16y" ~ 1,
                                               mhtn_10_5y == 1 & first_obs == "18y" ~ 1,
                                               mhtn_10_5y == 1 & first_obs == "mc1" ~ 1,
                                               #mhtn_12y == 1 & first_obs == "14y" ~ 1,
                                               mhtn_12y == 1 & first_obs == "16y" ~ 1,
                                               mhtn_12y == 1 & first_obs == "18y" ~ 1,
                                               mhtn_12y == 1 & first_obs == "mc1" ~ 1,
                                               # mhtn_14y == 1 & first_obs == "16y" ~ 1,
                                               # mhtn_14y == 1 & first_obs == "18y" ~ 1,
                                               # mhtn_14y == 1 & first_obs == "mc1" ~ 1,
                                               mhtn_16y == 1 & first_obs == "18y" ~ 1,
                                               mhtn_16y == 1 & first_obs == "mc1" ~ 1,
                                               mhtn_18y == 1 & first_obs == "mc1" ~ 1,
                                               TRUE ~ 0)) |>
  filter(outcome_before_first_flag == 0)


# TESTING (complete set)
# data <- data |>
#   filter(first_obs == "10_5y",
#          is.na(censored_period))
  

  Mode <- function(x) {
    x <- na.omit(x)
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  median_narm <- function(x) {
    y <- median(x, na.rm = TRUE)
    y
  }
  
  covars_outcome <- data |>
    mutate(born_in_usa = case_when(ageusa_bl == -9 ~ 1,
                                   TRUE ~ 0)) |> # 2 missing cases in original data
    select(newid, 
           first_obs,
           censored_period,
           # exposure variables
           starts_with("op_kg_2_year_"),
           starts_with("pyr_kg_2_year_"),
           starts_with("carb_kg_2_year_"),
           starts_with("neo_kg_2_year_"),
           starts_with("mn_kg_2_year_"),
           starts_with("gly_kg_2_year_"),
           starts_with("paraq_kg_2_year_"),
           # conditional grouping variables
           starts_with("d_op_kg_2_year_"),
           starts_with("d_pyr_kg_2_year_"),
           starts_with("d_carb_kg_2_year_"),
           starts_with("d_neo_kg_2_year_"),
           starts_with("d_mn_kg_2_year_"),
           starts_with("d_gly_kg_2_year_"),
           starts_with("d_paraq_kg_2_year_"),
           # baseline variables
           cham, 
           momdl_age2, 
           educat_bl, 
           hbp_bl,
           #hbpage_bl,
           diab_bl,
           diabage_bl,
           born_in_usa,
           # cancer variable
           cancer_bl,
           # time-varying covariates
           starts_with("age_"),
           "bodyage_mc1",
           starts_with("marstat_"),
           starts_with("marcat_"),
           starts_with("ipovcat_"),
           starts_with("povcat"),
           starts_with("hhagwork_"),
           starts_with("work_"),
           # outcome
           starts_with("mhtn_")
    )
  
  summary(covars_outcome)
  
# for time-varying use LOCF unless baseline, then use modes/median
  covars_outcome <- covars_outcome |>
    mutate(cham = cham - 1, # making into 0-1
           age_9y = ifelse(is.na(age_9y), median_narm(age_9y), age_9y),
           age_10_5y = ifelse(is.na(age_10_5y), age_9y + 1.5, age_10_5y),
           age_12y = ifelse(is.na(age_12y), age_10_5y + 1.5, age_12y),
           #age_14y = ifelse(is.na(age_14y), age_12y + 2, age_14y),
           age_16y = ifelse(is.na(age_16y), age_12y + 4, age_16y),
           age_18y = ifelse(is.na(age_18y), age_16y + 2, age_18y),
           age_mc1 = bodyage_mc1,
           age_mc1 = ifelse(is.na(age_mc1), age_18y + 2, age_mc1),
           #ageusa_bl = ifelse(ageusa_bl == -9, 0, ageusa_bl), # if born in USA 0
           #ageusa_bl_missing = ifelse(is.na(ageusa_bl), 1, 0),
           #ageusa_bl = ifelse(is.na(ageusa_bl), median_narm(ageusa_bl), ageusa_bl),
           hbp_bl_missing = ifelse(is.na(hbp_bl) | hbp_bl == 9, 1, 0),
           hbp_bl = ifelse(is.na(hbp_bl) | hbp_bl == 9, Mode(hbp_bl), hbp_bl),
           #hbpage_bl_missing = ifelse(is.na(hbpage_bl), 1, 0),
           #hbpage_bl = ifelse(is.na(hbpage_bl), median_narm(hbpage_bl), hbpage_bl),
           diab_bl_missing = ifelse(is.na(diab_bl) | diab_bl == 9, 1, 0),
           diab_bl = ifelse(is.na(diab_bl) | diab_bl == 9, Mode(diab_bl), diab_bl),
           #diabage_bl_missing = ifelse(is.na(diabage_bl) | diabage_bl == 99, 1, 0),
           #diabage_bl = ifelse(is.na(diabage_bl) | diabage_bl == 99, median_narm(diabage_bl), diabage_bl),
           marstat_9y_missing = ifelse(is.na(marstat_9y), 1, 0),
           marstat_9y = ifelse(is.na(marstat_9y), Mode(marstat_9y), marstat_9y),
           marstat_10_5y_missing = ifelse(is.na(marstat_10_5y), 1, 0),
           marstat_10_5y = ifelse(is.na(marstat_10_5y), marstat_9y, marstat_10_5y),
           marstat_12y_missing = ifelse(is.na(marstat_12y), 1, 0),
           marstat_12y = ifelse(is.na(marstat_12y), marstat_10_5y, marstat_12y),
           # marstat_14y_missing = ifelse(is.na(marstat_14y), 1, 0),
           # marstat_14y = ifelse(is.na(marstat_14y), marstat_12y, marstat_14y),
           marstat_16y_missing = ifelse(is.na(marstat_16y), 1, 0),
           marstat_16y = ifelse(is.na(marstat_16y),marstat_12y, marstat_16y),
           marstat_18y_missing = ifelse(is.na(marstat_18y), 1, 0),
           marstat_18y = ifelse(is.na(marstat_18y), marstat_16y, marstat_18y),
           marstat_mc1_missing = ifelse(is.na(marstat_mc1), 1, 0),
           marstat_mc1 = ifelse(is.na(marstat_mc1), marstat_18y, marstat_mc1),
           marcat_9y_missing = ifelse(is.na(marcat_9y), 1, 0),
           marcat_9y = ifelse(is.na(marcat_9y), Mode(marcat_9y), marcat_9y),
           marcat_10_5y_missing = ifelse(is.na(marcat_10_5y), 1, 0),
           marcat_10_5y = ifelse(is.na(marcat_10_5y), marcat_9y, marcat_10_5y),
           marcat_12y_missing = ifelse(is.na(marcat_12y), 1, 0),
           marcat_12y = ifelse(is.na(marcat_12y), marcat_10_5y, marcat_12y),
           # marcat_14y_missing = ifelse(is.na(marcat_14y), 1, 0),
           # marcat_14y = ifelse(is.na(marcat_14y), marcat_12y, marcat_14y),
           marcat_16y_missing = ifelse(is.na(marcat_16y), 1, 0),
           marcat_16y = ifelse(is.na(marcat_16y), marcat_12y, marcat_16y),
           marcat_18y_missing = ifelse(is.na(marcat_18y), 1, 0),
           marcat_18y = ifelse(is.na(marcat_18y), marcat_16y, marcat_18y),
           marcat_mc1_missing = ifelse(is.na(marcat_mc1), 1, 0),
           marcat_mc1 = ifelse(is.na(marcat_mc1), marcat_18y, marcat_mc1),
           ipovcat_9y_missing = ifelse(is.na(ipovcat_9y), 1, 0),
           ipovcat_9y = ifelse(is.na(ipovcat_9y), Mode(ipovcat_9y), ipovcat_9y),
           ipovcat_10_5y_missing = ifelse(is.na(ipovcat_10_5y), 1, 0),
           ipovcat_10_5y = ifelse(is.na(ipovcat_10_5y), ipovcat_9y, ipovcat_10_5y),
           ipovcat_12y_missing = ifelse(is.na(ipovcat_12y), 1, 0),
           ipovcat_12y = ifelse(is.na(ipovcat_12y), ipovcat_10_5y, ipovcat_12y),
           # ipovcat_14y_missing = ifelse(is.na(ipovcat_14y), 1, 0),
           # ipovcat_14y = ifelse(is.na(ipovcat_14y), ipovcat_12y, ipovcat_14y),
           ipovcat_16y_missing = ifelse(is.na(ipovcat_16y), 1, 0),
           ipovcat_16y = ifelse(is.na(ipovcat_16y), ipovcat_12y, ipovcat_16y),
           ipovcat_18y_missing = ifelse(is.na(povcat_18y), 1, 0),
           # poverty categories at times 18y and mc1 recorded slightly differently than previous periods (not imputed) -- fixing this now to use LOCF
           ipovcat_18y = ifelse(is.na(povcat_18y), ipovcat_16y, povcat_18y),
           ipovcat_mc1_missing = ifelse(is.na(povcat_mc1), 1, 0),
           ipovcat_mc1 = ifelse(is.na(povcat_mc1), ipovcat_18y, povcat_mc1),
           hhagwork_9y_missing = ifelse(is.na(hhagwork_9y), 1, 0),
           hhagwork_9y = ifelse(is.na(hhagwork_9y), Mode(hhagwork_9y), hhagwork_9y),
           hhagwork_10_5y_missing = ifelse(is.na(hhagwork_10_5y), 1, 0),
           hhagwork_10_5y = ifelse(is.na(hhagwork_10_5y), Mode(hhagwork_10_5y), hhagwork_10_5y),
           hhagwork_12y_missing = ifelse(is.na(hhagwork_12y), 1, 0),
           hhagwork_12y = ifelse(is.na(hhagwork_12y), Mode(hhagwork_12y), hhagwork_12y),
           # hhagwork_14y_missing = ifelse(is.na(hhagwork_14y), 1, 0),
           # hhagwork_14y = ifelse(is.na(hhagwork_14y), Mode(hhagwork_14y), hhagwork_14y),
           hhagwork_16y_missing = ifelse(is.na(hhagwork_16y), 1, 0),
           hhagwork_16y = ifelse(is.na(hhagwork_16y), Mode(hhagwork_16y), hhagwork_16y),
           hhagwork_18y_missing = ifelse(is.na(hhagwork_18y), 1, 0),
           hhagwork_18y = ifelse(is.na(hhagwork_18y), Mode(hhagwork_18y), hhagwork_18y),
           hhagwork_mc1_missing = ifelse(is.na(hhagwork_mc1), 1, 0),
           hhagwork_mc1 = ifelse(is.na(hhagwork_mc1), Mode(hhagwork_mc1), hhagwork_mc1),
           # work category at 12 years only binary -- making it binary at all other times
           work_cat_9y_missing = ifelse(is.na(work_cat_9y), 1, 0),
           work_cat_9y = ifelse(is.na(work_cat_9y), Mode(work_cat_9y), work_cat_9y),
           work_cat_9y = case_when(work_cat_9y <= 2 ~ 1, # employed
                                   work_cat_9y == 3 ~ 0), # not employed
           work_cat_10_5y_missing = ifelse(is.na(work_cat_10_5y), 1, 0),
           work_cat_10_5y = ifelse(is.na(work_cat_10_5y), work_cat_9y, work_cat_10_5y),
           work_cat_10_5y = case_when(work_cat_10_5y <= 2 ~ 1, # employed
                                      work_cat_10_5y == 3 ~ 0), # not employed
           work_cat_12y_missing = ifelse(is.na(work_12y), 1, 0),
           work_cat_12y = ifelse(is.na(work_12y), work_cat_10_5y, work_12y),
           work_cat_12y = case_when(work_cat_12y == 1 ~ 1, # employed
                                    work_cat_12y == 0 ~ 0), # not employed
           # work_cat_14y_missing = ifelse(is.na(work_cat_14y), 1, 0),
           # work_cat_14y = ifelse(is.na(work_cat_14y), work_cat_12y, work_cat_14y),
           # work_cat_14y = case_when(work_cat_14y <= 2 ~ 1, # employed
           #                         work_cat_14y == 3 ~ 0), # not employed
           work_cat_16y_missing = ifelse(is.na(work_cat_16y), 1, 0),
           work_cat_16y = ifelse(is.na(work_cat_16y), work_cat_12y, work_cat_16y),
           work_cat_16y = case_when(work_cat_16y <= 2 ~ 1, # employed
                                   work_cat_16y == 3 ~ 0), # not employed
           work_cat_18y_missing = ifelse(is.na(work_cat_18y), 1, 0),
           work_cat_18y = ifelse(is.na(work_cat_18y), work_cat_16y, work_cat_18y),
           work_cat_18y = case_when(work_cat_18y <= 2 ~ 1, # employed
                                   work_cat_18y == 3 ~ 0), # not employed
           work_cat_mc1_missing = ifelse(is.na(work_cat_mc1), 1, 0),
           work_cat_mc1 = ifelse(is.na(work_cat_mc1), work_cat_18y, work_cat_mc1),
           work_cat_mc1 = case_when(work_cat_mc1 <= 2 ~ 1, # employed
                                    work_cat_mc1 == 3 ~ 0) # not employed
    ) |>
    dummy_cols(c("educat_bl", 
                 "marstat_9y", "marstat_10_5y", "marstat_12y", "marstat_14y", "marstat_16y", "marstat_18y", "marstat_mc1", 
                 "ipovcat_9y", "ipovcat_10_5y", "ipovcat_12y", "ipovcat_14y", "ipovcat_16y", "ipovcat_18y", "ipovcat_mc1"), 
               remove_first_dummy = TRUE,
               remove_selected_columns = FALSE) |>
    select(-c(starts_with("povcat"), "work_12y", starts_with("bodyage")))

summary(covars_outcome)
  
saveRDS(covars_outcome, paste0("data/longitudinal_data_pre_aligned.rds"))

# aligning cohort based on "first" observed
# first observed time is treated as time 1

containing <- c("10_5y", "12y", "16y", "18y", "mc1")

process_row <- function(row, containing, colnames) {
  first_val <- row[["first_obs"]]
  first_idx <- which(containing == first_val)
  if (length(first_idx) == 0) return(as.data.frame(row)) 
  
  new_row <- as.list(row)
  
  for (i in seq_along(containing[first_idx:length(containing)])) {
    suf <- containing[first_idx + i - 1]
    
    matched_cols <- colnames[str_detect(colnames, fixed(suf))]
    
    for (col in matched_cols) {
      base_name <- str_replace(col, fixed(suf), "")
      
      base_name <- str_replace_all(base_name, "_+", "_")     
      
      base_name <- str_replace(base_name, "^_|_$", "")          
      
      new_col_name <- paste0(base_name, "_time_", i)
      new_row[[new_col_name]] <- row[[col]]
    }
  }
  
  as.data.frame(new_row)
}

transform_df <- function(df, containing) {
  colnames <- names(df)
  df |>
    pmap_dfr(~process_row(list(...), containing, colnames))
}

covars_outcome <- transform_df(covars_outcome, containing)

summary(covars_outcome)

# cleaning up censoring (carry-forward censoring)
covars_outcome <- covars_outcome |>
  mutate(mhtn_time_2 = ifelse(mhtn_time_1 == 1, 1, mhtn_time_2),
         mhtn_time_3 = ifelse(mhtn_time_2 == 1, 1, mhtn_time_3),
         mhtn_time_4 = ifelse(mhtn_time_3 == 1, 1, mhtn_time_4), 
         mhtn_time_5 = ifelse(mhtn_time_4 == 1, 1, mhtn_time_5)
  ) |>
  mutate(censor_time_1 = ifelse(is.na(mhtn_time_1) | censored_period == 1, 0, 1),
       censor_time_2 = ifelse(is.na(mhtn_time_2) | censored_period == 2, 0, 1),
       censor_time_3 = ifelse(is.na(mhtn_time_3) | censored_period == 3, 0, 1),
       censor_time_4 = ifelse(is.na(mhtn_time_4) | censored_period == 4, 0, 1),
       censor_time_5 = ifelse(is.na(mhtn_time_5) | censored_period == 5, 0, 1)) |>
  mutate(censor_time_2 = case_when(censor_time_1 == 0 ~ as.numeric(NA), 
                                   mhtn_time_1 == 1 ~ as.numeric(NA),
                                   TRUE ~ censor_time_2),
         censor_time_3 = case_when(censor_time_2 == 0 ~ as.numeric(NA), 
                                   is.na(censor_time_2) ~ as.numeric(NA), 
                                   mhtn_time_1 == 1 ~ as.numeric(NA),
                                   mhtn_time_2 == 1 ~ as.numeric(NA),
                                   TRUE ~ censor_time_3),
         censor_time_4 = case_when(censor_time_3 == 0 ~ as.numeric(NA), 
                                   is.na(censor_time_2) ~ as.numeric(NA), 
                                   is.na(censor_time_3) ~ as.numeric(NA), 
                                   mhtn_time_1 == 1 ~ as.numeric(NA),
                                   mhtn_time_2 == 1 ~ as.numeric(NA),
                                   mhtn_time_3 == 1 ~ as.numeric(NA),
                                   TRUE ~ censor_time_4),
         censor_time_5 = case_when(censor_time_4 == 0 ~ as.numeric(NA), 
                                   is.na(censor_time_2) ~ as.numeric(NA), 
                                   is.na(censor_time_3) ~ as.numeric(NA), 
                                   is.na(censor_time_4) ~ as.numeric(NA), 
                                   mhtn_time_1 == 1 ~ as.numeric(NA),
                                   mhtn_time_2 == 1 ~ as.numeric(NA),
                                   mhtn_time_3 == 1 ~ as.numeric(NA),
                                   mhtn_time_4 == 1 ~ as.numeric(NA),
                                   TRUE ~ censor_time_5)
  )

# LOCF for missing covariates/exposures (this occurs if someone has outcome measured but Yt= 0 at time t but is then censored at time t+1 but doesn't have exposures/covariates measured)

locf_func <- function(data, prefix, years = 0) {
  cols <- data |> dplyr::select(dplyr::starts_with(prefix))
  
  if (years > 0)
  {
  cols_filled <- t(apply(cols, 1, function(row) {
    filled <- zoo::na.locf(row, na.rm = FALSE)
    filled[is.na(row) & !is.na(filled)] <- filled[is.na(row) & !is.na(filled)] + years
    filled
  }))
  } else
  {
    cols_filled <- t(apply(cols, 1, function(row) {
    filled <- zoo::na.locf(row, na.rm = FALSE)
    filled[is.na(row) & !is.na(filled)] <- filled[is.na(row) & !is.na(filled)] 
    filled
  }))
    
  }
  
  colnames(cols_filled) <- colnames(cols)
  tibble::as_tibble(cols_filled)
}

covars_outcome <- covars_outcome |>
  select(-c(starts_with("op_kg_2_year_time_"),
            starts_with("pyr_kg_2_year_time_"),
            starts_with("carb_kg_2_year_time_"),
            starts_with("neo_kg_2_year_time_"),
            starts_with("mn_kg_2_year_time_"),
            starts_with("gly_kg_2_year_time_"),
            starts_with("paraq_kg_2_year_time_"),
            starts_with("marstat_2_time_"),
            starts_with("marstat_3_time_"),
            starts_with("marstat_4_time_"),
            starts_with("marstat_5_time_"),
            starts_with("marstat_6_time_"),
            starts_with("marcat_time_"),
            starts_with("ipovcat_2_time_"),
            starts_with("ipovcat_3_time_"),
            starts_with("hhagwork_time_"),
            starts_with("work_cat_time_"),
            starts_with("work_cat_time_"),
            starts_with("age_time_"))) |>
  bind_cols(locf_func(covars_outcome, "op_kg_2_year_time_")) |>
  bind_cols(locf_func(covars_outcome, "pyr_kg_2_year_time_")) |>
  bind_cols(locf_func(covars_outcome, "carb_kg_2_year_time_")) |>
  bind_cols(locf_func(covars_outcome, "neo_kg_2_year_time_")) |>
  bind_cols(locf_func(covars_outcome, "mn_kg_2_year_time_")) |>
  bind_cols(locf_func(covars_outcome, "gly_kg_2_year_time_")) |>
  bind_cols(locf_func(covars_outcome, "paraq_kg_2_year_time_")) |>
  bind_cols(locf_func(covars_outcome, "marstat_2_time_")) |>
  bind_cols(locf_func(covars_outcome, "marstat_3_time_")) |>
  bind_cols(locf_func(covars_outcome, "marstat_4_time_")) |>
  bind_cols(locf_func(covars_outcome, "marstat_5_time_")) |>
  bind_cols(locf_func(covars_outcome, "marstat_6_time_")) |>
  bind_cols(locf_func(covars_outcome, "marcat_time_")) |>
  bind_cols(locf_func(covars_outcome, "ipovcat_2_time_")) |>
  bind_cols(locf_func(covars_outcome, "ipovcat_3_time_")) |>
  bind_cols(locf_func(covars_outcome, "hhagwork_time_")) |>
  bind_cols(locf_func(covars_outcome, "work_cat_time_")) |>
  bind_cols(locf_func_age(covars_outcome, "age_time_"), years = 2)


# once censored or experienced outcome, make any future variables NA
censored_future_vars_NA <- function(df) {
  all_suffixes <- str_extract(names(df), "_\\d+$")
  all_times <- unique(as.integer(str_remove(all_suffixes[!is.na(all_suffixes)], "_")))
  all_times <- sort(all_times)
  
  for (i in seq_len(nrow(df))) {
    for (t in all_times) {
      censor_col <- paste0("censor_time_", t)
      outcome_col <- paste0("mhtn_time_", t)
      
      censor_val <- if (censor_col %in% names(df)) df[[censor_col]][i] else NA
      outcome <- if (outcome_col %in% names(df)) df[[outcome_col]][i] else NA
      
      if ((!is.na(censor_val) && censor_val == 0) || (!is.na(outcome) && outcome == 1)) {
        future_ts <- all_times[all_times > t]
        
        for (ft in future_ts) {
          cols_to_na <- names(df)[str_detect(names(df), paste0("time_", ft, "$"))]
          if (length(cols_to_na) > 0) {
            df[i, cols_to_na] <- NA
          }
        }
        
        break
      }
    }
  }
  
  return(df)
}

covars_outcome <- censored_future_vars_NA(covars_outcome) |>
  mutate(mhtn_time_2 = ifelse(mhtn_time_1 == 1, 1, mhtn_time_2),
         mhtn_time_3 = ifelse(mhtn_time_2 == 1, 1, mhtn_time_3),
         mhtn_time_4 = ifelse(mhtn_time_3 == 1, 1, mhtn_time_4), 
         mhtn_time_5 = ifelse(mhtn_time_4 == 1, 1, mhtn_time_5)
  ) |>
  mutate(censor_time_2 = ifelse(censor_time_1 == 0, 0, censor_time_2),
         censor_time_3 = ifelse(censor_time_2 == 0, 0, censor_time_3),
         censor_time_4 = ifelse(censor_time_3 == 0, 0, censor_time_4), 
         censor_time_5 = ifelse(censor_time_4 == 0, 0, censor_time_5)
  )

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

# truncate exposures at 95th percentile

covars_outcome <- covars_outcome |>
  mutate(across(
    all_of(unlist(A)),
    ~ pmin(., quantile(., 0.95, na.rm = TRUE))
  ))

saveRDS(covars_outcome, paste0("data/longitudinal_data_aligned.rds"))


