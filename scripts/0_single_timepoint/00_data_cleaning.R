library(tidyverse)
library(fastDummies)

for (years in c("1", "1_5", "2"))
{
data <- read_csv("data/rudolph_01b.csv") |>
  filter(cancer_bl == 0 | is.na(cancer_bl)) |> # only keep no cancer or missing
  filter(!is.na(years_2y_10_5y)) |> # 445 observations non-missing
  filter(years_2y_10_5y >= case_when(years == "1" ~ 1,
                                     years == "1_5" ~ 1.5,
                                     years == "2" ~ 2)) |> # try 1, 1.5, 2
   filter(d_op_kg_2_year_10_5y == 1,
          d_pyr_kg_2_year_10_5y == 1,
          d_carb_kg_2_year_10_5y == 1,    
          d_neo_kg_2_year_10_5y == 1,
          d_mn_kg_2_year_10_5y == 1,
          d_gly_kg_2_year_10_5y == 1,
          d_paraq_kg_2_year_10_5y == 1)

classes <- data |>
  select("newid",
         starts_with("years_"),
         starts_with("d_"),
         contains("_kg_2_year_")) |>
  select("newid",
         starts_with("years_"),
         starts_with("op_") |
           starts_with("pyr_") |
           starts_with("carb_") |
           starts_with("neo_") |
           starts_with("mn_") |
           starts_with("gly_") |
           starts_with("paraq_") |
           starts_with("d_op_") |
           starts_with("d_pyr_") |
           starts_with("d_carb_") |
           starts_with("d_neo_") |
           starts_with("d_mn_") |
           starts_with("d_gly_") |
           starts_with("d_paraq_")) |>
  select(newid, ends_with("_10_5y"))

saveRDS(classes, paste0("data/exposures_7_classes_time_1_years_obs_", years, ".rds"))

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
         starts_with("marstat_"),
         starts_with("marcat_"),
         starts_with("ipovcat_"),
         starts_with("hhagwork_"),
         starts_with("work_cat_"),
         # outcome
         starts_with("mhtn_"),
         # self report
         starts_with("mhbp"),
         # systolic
         starts_with("mbpsys"),
         # diabolic
         starts_with("mbpdia"),
         # pulse pressure
         starts_with("mbppul"),
         # summary blood pressure
         starts_with("mbpmap"),
         # taking bp medication (10.5 years)
         starts_with("mbpmed")
         )

summary(covars_outcome)

covars_outcome <- covars_outcome |>
  mutate(cham = cham - 1, # making into 0-1
         #ageusa_bl = ifelse(ageusa_bl == -9, 0, ageusa_bl), # if born in USA 0
         censor_10_5y = ifelse(is.na(mhtn_10_5y), 0, 1),
         censor_12y = ifelse(is.na(mhtn_12y), 0, 1),
         censor_14y = ifelse(is.na(mhtn_14y), 0, 1),
         censor_16y = ifelse(is.na(mhtn_16y), 0, 1),
         censor_18y = ifelse(is.na(mhtn_18y), 0, 1),
         censor_mc1 = ifelse(is.na(mhtn_mc1), 0, 1),
         censor_med_10_5y = ifelse(is.na(mbpmed_10_5y), 0, 1),
         censor_med_12y = ifelse(is.na(mbpmed_12y), 0, 1),
         censor_med_16y = ifelse(is.na(mbpmed_16y), 0, 1),
         censor_med_18y = ifelse(is.na(mbpmed_18y), 0, 1),
         censor_med_mc1 = ifelse(is.na(mbpmed_mc1), 0, 1),
         censor_self_report_12y = ifelse(is.na(mhbp_12y), 0, 1),
         censor_self_report_16y = ifelse(is.na(mhbp_16y), 0, 1),
         censor_self_report_18y = ifelse(is.na(mhbp_18y), 0, 1),
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
         age_9y_missing = ifelse(is.na(age_9y), 1, 0),
         age_9y = ifelse(is.na(age_9y), median_narm(age_9y), age_9y),
         age_10_5y_missing = ifelse(is.na(age_10_5y), 1, 0),
         age_10_5y = ifelse(is.na(age_10_5y), median_narm(age_10_5y), age_10_5y),
         marstat_9y_missing = ifelse(is.na(marstat_9y), 1, 0),
         marstat_9y = ifelse(is.na(marstat_9y), Mode(marstat_9y), marstat_9y),
         marstat_10_5y_missing = ifelse(is.na(marstat_10_5y), 1, 0),
         marstat_10_5y = ifelse(is.na(marstat_10_5y), Mode(marstat_10_5y), marstat_10_5y),
         marcat_9y_missing = ifelse(is.na(marcat_9y), 1, 0),
         marcat_9y = ifelse(is.na(marcat_9y), Mode(marcat_9y), marcat_9y),
         marcat_10_5y_missing = ifelse(is.na(marcat_10_5y), 1, 0),
         marcat_10_5y = ifelse(is.na(marcat_10_5y), Mode(marcat_10_5y), marcat_10_5y),
         ipovcat_9y_missing = ifelse(is.na(ipovcat_9y), 1, 0),
         ipovcat_9y = ifelse(is.na(ipovcat_9y), Mode(ipovcat_9y), ipovcat_9y),
         ipovcat_10_5y_missing = ifelse(is.na(ipovcat_10_5y), 1, 0),
         ipovcat_10_5y = ifelse(is.na(ipovcat_10_5y), Mode(ipovcat_10_5y), ipovcat_10_5y),
         hhagwork_9y_missing = ifelse(is.na(hhagwork_9y), 1, 0),
         hhagwork_9y = ifelse(is.na(hhagwork_9y), Mode(hhagwork_9y), hhagwork_9y),
         hhagwork_10_5y_missing = ifelse(is.na(hhagwork_10_5y), 1, 0),
         hhagwork_10_5y = ifelse(is.na(hhagwork_10_5y), Mode(hhagwork_10_5y), hhagwork_10_5y),
         work_cat_9y_missing = ifelse(is.na(work_cat_9y), 1, 0),
         work_cat_9y = ifelse(is.na(work_cat_9y), Mode(work_cat_9y), work_cat_9y),
         work_cat_10_5y_missing = ifelse(is.na(work_cat_10_5y), 1, 0),
         work_cat_10_5y = ifelse(is.na(work_cat_10_5y), Mode(work_cat_10_5y), work_cat_10_5y),
         work_in_field_agriculture_10_5_y = case_when(work_cat_10_5y <= 1 ~ 1,
                                                      TRUE ~ 0) # some missing -- treated as mode category
  ) |>
  dummy_cols(c("educat_bl", "work_cat_9y", "work_cat_10_5y", "marstat_9y", "marstat_10_5y", "ipovcat_9y", "ipovcat_10_5y"), 
             remove_first_dummy = TRUE,
             remove_selected_columns = FALSE)

summary(covars_outcome)
         
saveRDS(covars_outcome, paste0("data/covars_outcome_time_1_years_obs_", years, ".rds"))
}
  
  
  
  