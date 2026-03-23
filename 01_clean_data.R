# =============================================================================
# 01_clean_data.R
# Who Gets Access? — College Enrollment Gaps by Income & Geography in Texas
#
# Purpose: Load raw ACS CSV exports, reshape them from wide (county-as-column)
#          to long (county-as-row), extract target variables, merge, and save
#          a clean analysis-ready dataset.
#
# Input files:
#   data/ACSST5Y2024.S1401-2026-03-23T082616.csv  (School Enrollment)
#   data/ACSST5Y2024.S1901-2026-03-23T082509.csv  (Income)
#
# Output:
#   output/clean_enrollment_income.csv
#
# ---- ACS table format --------------------------------------------------------
# These exports are TRANSPOSED relative to a typical tidy layout:
#   - Each ROW is a demographic category (e.g. "Population 18 to 24 years")
#   - Each COLUMN is a geography + measure, named like:
#       "Anderson County, Texas!!Total!!Estimate"
#       "Anderson County, Texas!!Total!!Margin of Error"
# There is a single header row (row 1). No rows need to be skipped.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Setup
# -----------------------------------------------------------------------------

setwd("/Users/jonathanpro/Documents/DATA PROJECTS/Who Gets Access")

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidycensus)  # for fips_codes lookup table; install with:
                     #   install.packages("tidycensus")
# install.packages(c("readr","dplyr","tidyr","stringr","tidycensus"))

# ACS table exports are very wide (one column per county × measure).
# Increase vroom's read buffer to handle lines longer than the 128 KB default.
Sys.setenv(VROOM_CONNECTION_SIZE = 10 * 1024 * 1024)  # 10 MB

s1401_path <- "data/ACSST5Y2024.S1401-2026-03-23T082616.csv"
s1901_path <- "data/ACSST5Y2024.S1901-2026-03-23T082509.csv"


# =============================================================================
# PART 1 — School Enrollment (S1401)
# =============================================================================

# -----------------------------------------------------------------------------
# 1. Read S1401
# -----------------------------------------------------------------------------

s1401_raw <- read_csv(s1401_path, col_types = cols(.default = "c"))

# The first column is named "Label (Grouping)" and contains the row labels.
# All other columns are named like "Anderson County, Texas!!Total!!Estimate".
#
# NOTE: ACS table exports use non-breaking spaces (U+00A0) for indentation in
# the Label column, not regular spaces. Normalize them so string matching works.
s1401_raw <- s1401_raw |>
  mutate(`Label (Grouping)` = str_replace_all(`Label (Grouping)`, "\u00a0", " "))


# -----------------------------------------------------------------------------
# 2. Extract the two target rows from S1401
#
# Row: "Population 18 to 24 years"
#   → gives total 18-24 population for each county
#
# Row: "    Enrolled in college or graduate school"  (4-space indent)
#   → gives the count of 18-24 year-olds enrolled in college or grad school
#   → NOTE: indentation matters. The file also contains 8-space-indented rows
#     for males and females separately. The 4-space row is the combined total
#     and appears exactly once in the file.
# -----------------------------------------------------------------------------

pop_18_24_row <- s1401_raw |>
  filter(`Label (Grouping)` == "Population 18 to 24 years")

enrolled_row <- s1401_raw |>
  filter(`Label (Grouping)` == "    Enrolled in college or graduate school")

# Sanity check: each should be exactly one row
stopifnot(nrow(pop_18_24_row) == 1, nrow(enrolled_row) == 1)


# -----------------------------------------------------------------------------
# 3. Helper: pivot one S1401 row from wide to long
#
# For each target row, we:
#   (a) Select only the "!!Total!!Estimate" columns (raw count estimates)
#   (b) Pivot to long so each county becomes its own row
#   (c) Drop the statewide Texas column (keeps only county-level rows)
#   (d) Parse the county name from the column name
#   (e) Convert the value from a comma-formatted string to a number
#       (parse_number() also returns NA for suppressed values like "N", "-")
# -----------------------------------------------------------------------------

pivot_s1401_row <- function(row_df, value_col_name) {
  row_df |>
    select(matches("!!Total!!Estimate$")) |>
    pivot_longer(
      cols      = everything(),
      names_to  = "col_name",
      values_to = value_col_name
    ) |>
    filter(!str_starts(col_name, "Texas!!")) |>          # drop TX statewide total
    mutate(
      # Column name pattern: "Anderson County, Texas!!Total!!Estimate"
      # str_extract up to (but not including) the first comma gives "Anderson County"
      county_name = str_extract(col_name, "^[^,]+"),
      across(all_of(value_col_name), parse_number)       # "4,945" → 4945; "N" → NA
    ) |>
    select(county_name, all_of(value_col_name))
}


# -----------------------------------------------------------------------------
# 4. Apply the pivot to both target rows
# -----------------------------------------------------------------------------

pop_18_24  <- pivot_s1401_row(pop_18_24_row, "pop_18_24")
enrolled   <- pivot_s1401_row(enrolled_row,  "enrolled_college")


# -----------------------------------------------------------------------------
# 5. Join and calculate enrollment rate
# -----------------------------------------------------------------------------

s1401_clean <- inner_join(pop_18_24, enrolled, by = "county_name") |>
  mutate(
    # Enrollment rate: share of 18-24 year-olds enrolled in college or grad school
    # Will be NA if either pop or enrolled is NA (suppressed by Census for small pops)
    enrollment_rate = enrolled_college / pop_18_24
  )


# =============================================================================
# PART 2 — Household Income (S1901)
# =============================================================================

# -----------------------------------------------------------------------------
# 6. Read S1901 and extract the median household income row
#
# Target row: "Median income (dollars)"
# Target columns: "!!Households!!Estimate" (median income for all households)
# -----------------------------------------------------------------------------

s1901_raw <- read_csv(s1901_path, col_types = cols(.default = "c"))

# Same non-breaking space normalization as S1401
s1901_raw <- s1901_raw |>
  mutate(`Label (Grouping)` = str_replace_all(`Label (Grouping)`, "\u00a0", " "))

median_income_row <- s1901_raw |>
  filter(`Label (Grouping)` == "Median income (dollars)")

stopifnot(nrow(median_income_row) == 1)

s1901_clean <- median_income_row |>
  select(matches("!!Households!!Estimate$")) |>
  pivot_longer(
    cols      = everything(),
    names_to  = "col_name",
    values_to = "median_household_income"
  ) |>
  filter(!str_starts(col_name, "Texas!!")) |>
  mutate(
    county_name             = str_extract(col_name, "^[^,]+"),
    median_household_income = parse_number(median_household_income)
  ) |>
  select(county_name, median_household_income)


# =============================================================================
# PART 3 — Merge and enrich
# =============================================================================

# -----------------------------------------------------------------------------
# 7. Merge S1401 and S1901 by county name
#
# Both tables cover the same 254 Texas counties using identical county name
# strings, so a direct inner join is safe. An anti-join check confirms no
# counties are dropped due to naming mismatches.
# -----------------------------------------------------------------------------

# Check for unexpected mismatches before committing to inner join
unmatched <- anti_join(s1401_clean, s1901_clean, by = "county_name")
if (nrow(unmatched) > 0) {
  warning("Counties in S1401 with no S1901 match: ",
          paste(unmatched$county_name, collapse = ", "))
}

merged <- inner_join(s1401_clean, s1901_clean, by = "county_name")


# -----------------------------------------------------------------------------
# 8. Add county FIPS codes
#
# Uses the fips_codes dataset from the tidycensus package.
# Texas state FIPS = 48; full 5-digit FIPS = state_code + county_code
# e.g. Anderson County → 48001
# -----------------------------------------------------------------------------

tx_fips <- fips_codes |>
  filter(state == "TX") |>
  transmute(
    county_name = county,                              # "Anderson County" etc.
    fips        = paste0(state_code, county_code)      # "48001" etc.
  )

merged <- left_join(merged, tx_fips, by = "county_name")


# -----------------------------------------------------------------------------
# 9. Flag small-sample counties
#
# ACS 5-year estimates for counties with fewer than 200 people aged 18–24 are
# unreliable: the margin of error is typically larger than the estimate itself.
# We retain these counties in the dataset but mark them so they can be excluded
# from the correlation analysis without silently distorting results.
# -----------------------------------------------------------------------------

merged <- merged |>
  mutate(small_sample = pop_18_24 < 200)

n_small <- sum(merged$small_sample, na.rm = TRUE)
message(n_small, " counties flagged as small_sample (pop_18_24 < 200)")


# -----------------------------------------------------------------------------
# 10. Recode suppressed zeros in enrolled_college
#
# In the ACS export, a cell value of 0 for enrolled_college in a county with a
# non-zero 18–24 population almost certainly reflects Census suppression, not a
# true count of zero enrollees. The ACS uses "-" or "N" for explicit suppression
# (which parse_number() already converts to NA), but rounds of small counts can
# also produce a reported 0 that is not analytically meaningful.
#
# Treating these as true zeros would artificially pull the enrollment rate to 0%
# and bias the income–enrollment correlation downward. We recode them as NA.
#
# Note: all affected counties also meet the small_sample threshold (< 200), so
# this recode is consistent with that flag.
# -----------------------------------------------------------------------------

n_zero_enrolled <- sum(merged$enrolled_college == 0, na.rm = TRUE)
message(n_zero_enrolled, " counties had enrolled_college == 0 — recoding to NA")

merged <- merged |>
  mutate(
    enrolled_college = if_else(enrolled_college == 0, NA_real_, enrolled_college),
    # Recalculate enrollment_rate now that zeros are NA
    enrollment_rate  = enrolled_college / pop_18_24
  )


# -----------------------------------------------------------------------------
# 11. Missing value report (post-corrections)
# -----------------------------------------------------------------------------

missing_report <- merged |>
  summarise(
    total_counties          = n(),
    small_sample_flagged    = sum(small_sample, na.rm = TRUE),
    missing_enrolled        = sum(is.na(enrolled_college)),
    missing_enrollment_rate = sum(is.na(enrollment_rate)),
    missing_median_income   = sum(is.na(median_household_income)),
    missing_fips            = sum(is.na(fips))
  )

message("\n--- Missing value report (after corrections) ---")
print(missing_report)

# All 254 counties are retained. NAs are preserved for transparency.
# Downstream scripts should filter on !small_sample and !is.na(enrollment_rate)
# before running correlation or regression analyses.
analysis_data <- merged


# -----------------------------------------------------------------------------
# 12. Final column order and save
# -----------------------------------------------------------------------------

analysis_data <- analysis_data |>
  select(
    fips,
    county_name,
    pop_18_24,
    enrolled_college,
    enrollment_rate,
    median_household_income,
    small_sample
  ) |>
  arrange(fips)

glimpse(analysis_data)

write_csv(analysis_data, "output/clean_enrollment_income.csv")
message("Saved: output/clean_enrollment_income.csv")
