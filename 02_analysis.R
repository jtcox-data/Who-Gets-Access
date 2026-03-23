# =============================================================================
# 02_analysis.R
# Who Gets Access? — College Enrollment Gaps by Income & Geography in Texas
#
# Purpose: Analyze the relationship between county-level household income and
#          college enrollment rates across Texas counties.
#
# Input:   output/clean_enrollment_income.csv  (produced by 01_clean_data.R)
# Output:  output/quartile_summary.csv
#          output/lowest_enrollment_counties.csv
#          Console output with all key results labeled
#
# Key decisions:
#   - All analyses use only counties where small_sample == FALSE (pop_18_24 >= 200).
#     The remaining 33 counties have ACS estimates too unreliable to include.
#     See 01_clean_data.R for full documentation of this exclusion.
#   - Enrollment rate = enrolled_college / pop_18_24 (ages 18–24).
#   - Income quartiles are computed within the analysis subset only.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Setup
# -----------------------------------------------------------------------------

setwd("/Users/jonathanpro/Documents/DATA PROJECTS/Who Gets Access")

library(readr)
library(dplyr)

# A small helper to print a section header to the console cleanly
section <- function(title) {
  cat("\n", strrep("=", 60), "\n", title, "\n", strrep("=", 60), "\n", sep = "")
}


# -----------------------------------------------------------------------------
# 1. Load data and apply analysis filter
# -----------------------------------------------------------------------------

all_counties <- read_csv("output/clean_enrollment_income.csv", show_col_types = FALSE)

# Restrict to counties with a reliable 18–24 population estimate.
# Counties with pop_18_24 < 200 were flagged as small_sample in 01_clean_data.R.
# This single filter also removes all counties with NA enrollment rates, since
# every NA enrollment was caused by small population suppression.
counties <- all_counties |>
  filter(!small_sample, !is.na(enrollment_rate))

n_total    <- nrow(all_counties)
n_excluded <- n_total - nrow(counties)
n_analysis <- nrow(counties)

cat("\nData loaded.\n")
cat("  Total Texas counties in dataset: ", n_total, "\n")
cat("  Excluded (small_sample or NA):   ", n_excluded, "\n")
cat("  Counties used in analysis:        ", n_analysis, "\n")


# =============================================================================
# 2. Descriptive statistics
# =============================================================================

section("DESCRIPTIVE STATISTICS")

# --- Enrollment rate ----------------------------------------------------------
enroll_mean   <- mean(counties$enrollment_rate)
enroll_median <- median(counties$enrollment_rate)
enroll_sd     <- sd(counties$enrollment_rate)
enroll_min    <- min(counties$enrollment_rate)
enroll_max    <- max(counties$enrollment_rate)

cat("\nCollege enrollment rate (share of 18–24 year-olds enrolled):\n")
cat(sprintf("  Mean:    %.1f%%\n", enroll_mean   * 100))
cat(sprintf("  Median:  %.1f%%\n", enroll_median * 100))
cat(sprintf("  SD:      %.1f%%\n", enroll_sd     * 100))
cat(sprintf("  Min:     %.1f%%  (%s)\n",
            enroll_min * 100,
            counties$county_name[which.min(counties$enrollment_rate)]))
cat(sprintf("  Max:     %.1f%%  (%s)\n",
            enroll_max * 100,
            counties$county_name[which.max(counties$enrollment_rate)]))

# --- Median household income --------------------------------------------------
income_mean   <- mean(counties$median_household_income)
income_median <- median(counties$median_household_income)
income_sd     <- sd(counties$median_household_income)
income_min    <- min(counties$median_household_income)
income_max    <- max(counties$median_household_income)

cat("\nMedian household income:\n")
cat(sprintf("  Mean:    $%s\n", format(round(income_mean),   big.mark = ",")))
cat(sprintf("  Median:  $%s\n", format(round(income_median), big.mark = ",")))
cat(sprintf("  SD:      $%s\n", format(round(income_sd),     big.mark = ",")))
cat(sprintf("  Min:     $%s  (%s)\n",
            format(income_min, big.mark = ","),
            counties$county_name[which.min(counties$median_household_income)]))
cat(sprintf("  Max:     $%s  (%s)\n",
            format(income_max, big.mark = ","),
            counties$county_name[which.max(counties$median_household_income)]))


# =============================================================================
# 3. Enrollment rate by income quartile
# =============================================================================
#
# Divide the 221 analysis counties into four equal groups (quartiles) ranked
# by median household income. Then compare average enrollment within each group.
# A monotonic rise in enrollment from Q1 to Q4 would indicate a clear
# income–access gradient.

section("ENROLLMENT RATE BY INCOME QUARTILE")

counties <- counties |>
  mutate(
    income_quartile = ntile(median_household_income, 4),
    income_quartile_label = factor(
      income_quartile,
      levels = 1:4,
      labels = c("Q1 — Lowest income", "Q2", "Q3", "Q4 — Highest income")
    )
  )

quartile_summary <- counties |>
  group_by(income_quartile_label) |>
  summarise(
    n_counties          = n(),
    income_range_low    = min(median_household_income),
    income_range_high   = max(median_household_income),
    mean_enrollment     = mean(enrollment_rate),
    median_enrollment   = median(enrollment_rate),
    sd_enrollment       = sd(enrollment_rate),
    .groups = "drop"
  )

# Print a readable table
cat("\n")
for (i in seq_len(nrow(quartile_summary))) {
  q <- quartile_summary[i, ]
  cat(sprintf(
    "%-25s  n=%d  income $%s–$%s  |  mean enroll: %4.1f%%  (median %4.1f%%, SD %.1f%%)\n",
    as.character(q$income_quartile_label),
    q$n_counties,
    format(q$income_range_low,  big.mark = ","),
    format(q$income_range_high, big.mark = ","),
    q$mean_enrollment   * 100,
    q$median_enrollment * 100,
    q$sd_enrollment     * 100
  ))
}

# Enrollment gap: difference between highest and lowest quartile means
gap <- quartile_summary$mean_enrollment[4] - quartile_summary$mean_enrollment[1]
cat(sprintf(
  "\nEnrollment gap (Q4 mean minus Q1 mean): %.1f percentage points\n", gap * 100
))

write_csv(quartile_summary, "output/quartile_summary.csv")
cat("Saved: output/quartile_summary.csv\n")


# =============================================================================
# 4. Correlation — enrollment rate vs. median household income
# =============================================================================
#
# Pearson correlation tests the linear association between income and enrollment.
# Because both variables are continuous and roughly unimodal, Pearson is
# appropriate here. The p-value tests H0: true correlation = 0.

section("CORRELATION: ENROLLMENT RATE vs. MEDIAN HOUSEHOLD INCOME")

cor_result <- cor.test(
  counties$median_household_income,
  counties$enrollment_rate,
  method = "pearson"
)

cat(sprintf("\n  Pearson r:     %.3f\n",  cor_result$estimate))
cat(sprintf("  p-value:       %s\n",
            ifelse(cor_result$p.value < 0.001, "< 0.001",
                   sprintf("%.4f", cor_result$p.value))))
cat(sprintf("  95%% CI:        [%.3f, %.3f]\n",
            cor_result$conf.int[1], cor_result$conf.int[2]))
cat(sprintf("  n (counties):  %d\n", cor_result$parameter + 2))

# Interpret the strength of the association
pearson_r <- as.numeric(cor_result$estimate)
strength <- dplyr::case_when(
  abs(pearson_r) >= 0.7 ~ "strong",
  abs(pearson_r) >= 0.4 ~ "moderate",
  abs(pearson_r) >= 0.2 ~ "weak",
  TRUE                  ~ "negligible"
)
direction <- ifelse(pearson_r > 0, "positive", "negative")
cat(sprintf("\n  Interpretation: %s %s correlation.\n", strength, direction))
cat("  Counties with higher median household income tend to have",
    ifelse(pearson_r > 0, "higher", "lower"), "college enrollment rates.\n")


# =============================================================================
# 5. Ten counties with the lowest enrollment rates
# =============================================================================
#
# These counties represent the most acute access gaps in the dataset.
# Showing income alongside enrollment highlights whether low enrollment
# co-occurs with low income (as the correlation would predict) or whether
# some low-enrollment counties have relatively high incomes — which would
# suggest factors beyond income are driving the gap.

section("10 COUNTIES WITH LOWEST ENROLLMENT RATES")

lowest_10 <- counties |>
  slice_min(enrollment_rate, n = 10) |>
  select(county_name, pop_18_24, enrolled_college,
         enrollment_rate, median_household_income, income_quartile_label) |>
  arrange(enrollment_rate)

cat("\n")
cat(sprintf("  %-24s  %9s  %8s  %6s  %10s  %s\n",
            "County", "Pop 18-24", "Enrolled", "Rate", "Med Income", "Income Quartile"))
cat("  ", strrep("-", 80), "\n", sep = "")

for (i in seq_len(nrow(lowest_10))) {
  row <- lowest_10[i, ]
  cat(sprintf("  %-24s  %9s  %8s  %5.1f%%  %10s  %s\n",
              row$county_name,
              format(row$pop_18_24,           big.mark = ","),
              format(row$enrolled_college,    big.mark = ","),
              row$enrollment_rate * 100,
              paste0("$", format(row$median_household_income, big.mark = ",")),
              row$income_quartile_label))
}

write_csv(lowest_10, "output/lowest_enrollment_counties.csv")
cat("\nSaved: output/lowest_enrollment_counties.csv\n")


# =============================================================================
# 6. Summary
# =============================================================================

section("SUMMARY")

cat(sprintf(
  "\nAcross %d Texas counties (excluding %d with unreliable small-sample estimates):\n",
  n_analysis, n_excluded
))
cat(sprintf(
  "  - The average college enrollment rate for 18-24 year-olds is %.1f%%.\n",
  enroll_mean * 100
))
cat(sprintf(
  "  - Counties in the highest income quartile enroll %.1f%% on average,\n",
  quartile_summary$mean_enrollment[4] * 100
))
cat(sprintf(
  "    compared to %.1f%% in the lowest income quartile — a %.1f-point gap.\n",
  quartile_summary$mean_enrollment[1] * 100, gap * 100
))
cat(sprintf(
  "  - The Pearson correlation between income and enrollment is r = %.3f (p %s),\n",
  pearson_r,
  ifelse(cor_result$p.value < 0.001, "< 0.001",
         sprintf("= %.4f", cor_result$p.value))
))
cat(sprintf("    indicating a %s %s association.\n", strength, direction))
