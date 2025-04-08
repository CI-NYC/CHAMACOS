README
================
2025-02-06

## CHAMACOS Pesticide Use ReadME

- file `00_data_cleaning.R` contains basic data cleaning BEFORE applying
  the convex hull shifts, such as:
  - removing individuals with cancer at baseline
  - removing individuals with missing values in the years observed
    variable at time t = 0 (`years_2y_10_5y`) – this likely means they
    were not present at this timepoint
  - keeping only individuals with number of years observed greater than
    or equal to 1 year, 1.5 years, and 2 years (this is flexible and
    results may change depending on the filtering done here)
    - 1 year (n = 317), 1.5 years (n = 310), 2 years (n = 250)
  - keeping only individuals with use of each pesticide within 1 km
    (mentioned by Lucia)
  - add missing indicators for missing values in baseline variables,
    impute missing values with mode/median
- file `01_getting_shifted_convexhull.R` contains code to apply
  normalization between 0 and 1 and then apply convex hull shifts (1. a
  multiplicative 20% decrease shift and 2. an additive shift by 10
  scaled units)
  - apply truncation to all 7 exposure variables (truncating at the 95th
    percentile due to heavy right skew)
  - normalize each exposure variable between 0 and 1
  - apply convex hull shifts (1. a multiplicative 20% decrease shift
    and 2. an additive shift by 10 scaled units)
  - summary statistics about shifts
  - unnormalize each exposure variable (don’t need to do this but may be
    better for interpretation?)
- file `02_lmtp.R` contains code to apply lmtp analysis to exposure data
  using baseline covariates, timevarying covariates at t = 0, and
  exposures at t = 0. Loop through all maternal hypertension outcome
  timepoints (10.5 years, 12 years, 14 years, 16 years, 18 years, and
  MC1)
  - NOTE: this does not use the longitudinal structure of the data
    (i.e. only exposures and timevarying covariates at t = 0 are used,
    also not using previous outcomes)
  - results are (mostly) in opposite direction of expected (e.g. comparing
    shift - decreasing shifts yield INCREASED incidence of maternal
    hypertension compared to the observed treatment values)

### Preliminary Results

- `>=`1 year observed for first three timepoints
  - ![](figures/1_year.png)
- `>=`1.5 years observed for first three timepoints
  - ![](figures/1_5_years.png)
- `>=`2 years observed for first three timepoints
  - ![](figures/2_years.png)
