CHAMACOS Pesticide Use ReadME
================

Exposures (7 classes): - Organophophates - Pyrethroids - Carbamates -
Neonicotinoids - Manganese fungicides - Glyphosate herbicides - Paraquat
herbicides

Outcome: - Maternal hypertension

- File `00_data_cleaning.R` contains basic data cleaning on the original
  data (n = 621), such as:
  - Completely ignoring the `14 year` visit due to differences in
    outcome measurement at this time point
    - NOTE: This means any exposures, time-varying covariates, and
      outcomes are ignored in both the analysis and when doing LOCF (or
      should they be included when doing LOCF imputation?)
  - Removing individuals with cancer at baseline (n = 11 removed)
    - NOTE: 2 individuals are missing baseline cancer/high blood
      pressure status but they are kept in the data for now
  - Identifying the first period of observation (we define this as the
    first period during which years_PERIOD \>= 1 year), individuals with
    no sufficient first period of observation removed (n = 109 removed)
    - NOTE: See image below for further details
    - NOTE: the 1 year cutoff was chosen in discussion with Lucia
  - For any subsequent time points, if an individual has missing
    years_PERIOD or years_PERIOD \< 1 year, their exposures will be set
    to `NA` for that period and the exposure values from the previous
    time period will be carried forward. This is done because we assume
    that not enough exposure data has been observed at that time point
    - NOTE: Alternatively, we could consider censoring individuals when
      the number of years observed is insufficient
  - Creating censoring variables where censoring occurs when outcome is
    missing; converting censoring into a survival-type censoring,
    meaning that once an individual experiences censoring, they will
    have experienced it for all remaining time points
  - Converting maternal hypertension outcome to a survival-type outcome,
    meaning that once an individual experiences the outcome, they will
    have experienced it for all remaining time points
  - Removing individuals who experienced outcome or were reported to
    have high blood pressure prior to time period 1 (n = 142 removed)
  - Adding missing indicators for missing values in baseline variables,
    impute baseline missing values with mode/median, any subsequent
    missing values imputed using LOCF (except for age which we impute
    using an approximation; for example, if an individual is missing
    `age_10_5y` but not `age_9y`, we can impute the approximation
    `age_9y` + 1.5 years)
  - Converting any variables occurring after outcome has occurred or
    censoring has occurred to `NA`
  - Truncating all exposure variables at their respective 95th
    percentile values due to presence of outliers

How many people are censored because they are actually missing the
outcome or because “joined late”? - Looks like more people are censored
because they are missing the oucome so we will use LOCF for missing
exposures and time-varying covariates for those that “joined late”

|        | Missing outcome | Joined Late |
|--------|-----------------|-------------|
| Time 1 | 13              | –           |
| Time 2 | 28              | 0           |
| Time 3 | 32              | 7           |
| Time 4 | 9               | 10          |
| Time 5 | 0               | 8           |

n = 359 individuals remaining

![](plots/CHAMACOS_Outcome.png)

- File `1_convex_hull.R` contains code to normalize the treatment
  variables and apply the convex hull at each time period independently
  - We apply a 20% reduction shift on Glyphosate herbicides and Paraquat
    herbicides and keep other exposures at their observed values
  - When applying the shift, 64.6%, 63.5%, 52.6%, 49.3%, and 55.7% of
    individuals stay within the bounds of the convex hull for the 20%
    reductions of the two herbicides at time points 1, 2, 3, 4, and 5,
    respectively.
    - NOTE: the percent of individuals staying within the bounds of the
      convex hull changes depending on the rounding done (more rounding
      means more people stay in the bounds of the convex hull) – we
      could consider rounding to 2 digits (85.4%, 83.7%, 80.1%, 73.7%,
      78.1%.
  - For those that fall outside of the convex hull, we keep their
    exposures at the observed values
- File `2a_analysis_local_convex_hull_multiplicative.R` contains code to
  conduct a longitudinal analysis on the convex hull shifted exposures
  - This is a local analysis in which we assess the effects among those
    with use of Glyphosate herbicides or Paraquat herbicides in the past
    2yr within 1km at the first time point.
- File `3_analysis_local_longitudinal.R` contains code to conduct a
  longitudinal analysis on 20% reduction shifts on the Glyphosate
  herbicides and Paraquat herbicides (while keeping the remaining
  exposures at their observed values)
  - This is a local analysis in which we assess the effects among those
    with use of Glyphosate herbicides or Paraquat herbicides in the past
    2yr within 1km at the first time point.
