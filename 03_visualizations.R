# =============================================================================
# 03_visualizations.R
# Who Gets Access? — College Enrollment Gaps by Income & Geography in Texas
#
# Purpose: Produce four publication-quality charts for the project.
#
# Input:
#   output/clean_enrollment_income.csv  (from 01_clean_data.R)
#   output/quartile_summary.csv         (from 02_analysis.R)
#
# Output:
#   output/01_scatter_income_vs_enrollment.png
#   output/02_bar_enrollment_by_quartile.png
#   output/03_bar_lowest_enrollment_counties.png
#   output/04_histogram_enrollment_distribution.png
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Setup
# -----------------------------------------------------------------------------

setwd("/Users/jonathanpro/Documents/DATA PROJECTS/Who Gets Access")

library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)   # non-overlapping county labels on the scatter plot
library(scales)    # axis label formatting (dollars, percents)

# --- Load data ----------------------------------------------------------------

all_counties <- read_csv("output/clean_enrollment_income.csv", show_col_types = FALSE)
quartile_summary <- read_csv("output/quartile_summary.csv",    show_col_types = FALSE)

# Analysis subset: exclude small-sample counties (pop_18_24 < 200)
# This is the same filter applied in 02_analysis.R
counties <- all_counties |>
  filter(!small_sample, !is.na(enrollment_rate))


# --- Shared style elements ----------------------------------------------------

# Income quartile color palette: warm-to-cool sequential, policy-brief friendly
# Q1 (lowest) → muted coral; Q4 (highest) → deep policy blue
QUARTILE_COLORS <- c(
  "Q1 — Lowest income" = "#c85250",
  "Q2"                 = "#e8a338",
  "Q3"                 = "#6baed6",
  "Q4 — Highest income"= "#1d6fa4"
)

# Single accent color for charts that don't need quartile coloring
ACCENT_BLUE <- "#1d6fa4"

# Caption used on every chart
ACS_CAPTION <- "Source: U.S. Census Bureau, ACS 5-Year Estimates 2024 (Tables S1401 & S1901)\nAnalysis excludes 33 Texas counties with fewer than 200 residents aged 18–24 (unreliable estimates)."

# Base theme: clean and minimal, sized for 300 dpi PNG output
theme_brief <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14, margin = margin(b = 4)),
    plot.subtitle    = element_text(color = "#555555", size = 11, margin = margin(b = 10)),
    plot.caption     = element_text(color = "#888888", size = 8, lineheight = 1.3,
                                    margin = margin(t = 10)),
    plot.margin      = margin(16, 16, 16, 16),
    axis.title       = element_text(size = 10, color = "#333333"),
    axis.text        = element_text(size = 9,  color = "#555555"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#eeeeee"),
    legend.title     = element_text(size = 9, face = "bold"),
    legend.text      = element_text(size = 9)
  )


# =============================================================================
# CHART 1 — Scatter plot: median income vs. enrollment rate
#
# This is the central chart of the project. Each point is a Texas county.
# The trend line shows the income–enrollment relationship described by r = 0.199.
# Labeled outliers tell the story the correlation alone cannot: university towns
# (Brazos, Erath, Nacogdoches) dominate the top of the chart regardless of
# income, while some middle-income rural counties sit at the very bottom.
# =============================================================================

# Identify counties to label: highest/lowest absolute residual from a simple
# linear model, plus the extremes of each axis.
scatter_model <- lm(enrollment_rate ~ median_household_income, data = counties)
counties_scatter <- counties |>
  mutate(
    income_quartile_label = case_when(
      ntile(median_household_income, 4) == 1 ~ "Q1 — Lowest income",
      ntile(median_household_income, 4) == 2 ~ "Q2",
      ntile(median_household_income, 4) == 3 ~ "Q3",
      ntile(median_household_income, 4) == 4 ~ "Q4 — Highest income"
    ),
    resid = residuals(scatter_model)
  )

# Select ~12 counties to label: strongest residuals (both directions) + axis extremes
label_counties <- counties_scatter |>
  mutate(
    is_axis_extreme = county_name %in% c(
      counties_scatter$county_name[which.max(counties_scatter$enrollment_rate)],
      counties_scatter$county_name[which.min(counties_scatter$enrollment_rate)],
      counties_scatter$county_name[which.max(counties_scatter$median_household_income)],
      counties_scatter$county_name[which.min(counties_scatter$median_household_income)]
    )
  ) |>
  filter(
    rank(-abs(resid)) <= 10 | is_axis_extreme
  ) |>
  distinct(county_name, .keep_all = TRUE)

# Strip "County" from labels to reduce clutter
label_counties <- label_counties |>
  mutate(label = sub(" County$", "", county_name))

p_scatter <- ggplot(
  counties_scatter,
  aes(x = median_household_income, y = enrollment_rate)
) +
  # All county points, colored by quartile at low alpha so the trend reads clearly
  geom_point(
    aes(color = income_quartile_label),
    size  = 2, alpha = 0.65
  ) +
  # Linear trend line with 95% confidence interval shading
  geom_smooth(
    method  = "lm",
    formula = y ~ x,
    se      = TRUE,
    color   = "#333333",
    fill    = "#cccccc",
    linewidth = 0.8,
    alpha   = 0.25
  ) +
  # Non-overlapping labels for notable counties
  geom_text_repel(
    data          = label_counties,
    aes(label     = label),
    size          = 2.8,
    color         = "#222222",
    box.padding   = 0.4,
    point.padding = 0.3,
    segment.color = "#aaaaaa",
    segment.size  = 0.35,
    max.overlaps  = 20,
    seed          = 42
  ) +
  scale_color_manual(values = QUARTILE_COLORS, name = "Income Quartile") +
  scale_x_continuous(
    labels = function(x) paste0("$", round(x / 1000), "K"),
    breaks = seq(40000, 130000, by = 20000)
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    breaks = seq(0, 0.80, by = 0.20)
  ) +
  labs(
    title    = "College Enrollment vs. Household Income Across Texas Counties",
    subtitle = "Each point is a county. Trend line shows a weak but significant positive relationship (r = 0.20, p = 0.003).",
    x        = "Median Household Income",
    y        = "College Enrollment Rate (Ages 18–24)",
    caption  = ACS_CAPTION
  ) +
  theme_brief +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.5, "lines"))

ggsave("output/01_scatter_income_vs_enrollment.png",
       p_scatter, width = 10, height = 7, dpi = 300, bg = "white")
message("Saved: output/01_scatter_income_vs_enrollment.png")


# =============================================================================
# CHART 2 — Bar chart: mean enrollment rate by income quartile
#
# This chart directly answers the headline question: does enrollment differ
# by income group? The Q4–Q1 gap (5.1 percentage points) is real but modest,
# and the non-monotonic Q2 > Q3 pattern hints that factors beyond income —
# particularly university presence — generate considerable variation within
# each quartile. Error bars (±1 SD) make the within-quartile spread visible.
# =============================================================================

# Re-level the quartile factor so bars appear in income order
quartile_summary <- quartile_summary |>
  mutate(
    income_quartile_label = factor(
      income_quartile_label,
      levels = c("Q1 — Lowest income", "Q2", "Q3", "Q4 — Highest income")
    ),
    # Formatted income range for x-axis subtitle
    income_range = paste0(
      "$", round(income_range_low / 1000), "K",
      " – ",
      "$", round(income_range_high / 1000), "K"
    )
  )

p_quartile <- ggplot(
  quartile_summary,
  aes(x = income_quartile_label, y = mean_enrollment, fill = income_quartile_label)
) +
  geom_col(width = 0.62, show.legend = FALSE) +
  # ±1 SD error bars show how much counties within each quartile vary
  geom_errorbar(
    aes(
      ymin = mean_enrollment - sd_enrollment,
      ymax = mean_enrollment + sd_enrollment
    ),
    width     = 0.18,
    linewidth = 0.55,
    color     = "#444444"
  ) +
  # Percentage labels above each bar
  geom_text(
    aes(label = paste0(round(mean_enrollment * 100, 1), "%")),
    vjust = -0.7,
    size  = 3.8,
    fontface = "bold",
    color = "#333333"
  ) +
  # Income range as a secondary x-axis label
  geom_text(
    aes(y = 0, label = income_range),
    vjust  = 1.8,
    size   = 2.8,
    color  = "#777777"
  ) +
  scale_fill_manual(values = QUARTILE_COLORS) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    limits = c(-0.02, 0.48),
    breaks = seq(0, 0.40, by = 0.10)
  ) +
  labs(
    title    = "College Enrollment Rate by County Income Quartile",
    subtitle = "Mean % of 18–24 year-olds enrolled in college. Error bars show ±1 standard deviation across counties.",
    x        = "Income Quartile (54–56 counties each)",
    y        = "Mean Enrollment Rate",
    caption  = ACS_CAPTION
  ) +
  theme_brief +
  theme(
    axis.text.x  = element_text(size = 9.5),
    panel.grid.major.x = element_blank()
  )

ggsave("output/02_bar_enrollment_by_quartile.png",
       p_quartile, width = 9, height = 6.5, dpi = 300, bg = "white")
message("Saved: output/02_bar_enrollment_by_quartile.png")


# =============================================================================
# CHART 3 — Horizontal bar chart: 15 counties with lowest enrollment rates
#
# Puts faces on the access gap. Coloring bars by income quartile tests whether
# low enrollment simply tracks low income — it mostly does, but the presence
# of Q3 counties (Mills, Lipscomb, Karnes) in the bottom 15 reveals that
# geography and institutional access matter independently of income.
# =============================================================================

# Reconstruct income quartile within the analysis subset (same as 02_analysis.R)
bottom_15 <- counties |>
  mutate(
    income_quartile_label = factor(
      case_when(
        ntile(median_household_income, 4) == 1 ~ "Q1 — Lowest income",
        ntile(median_household_income, 4) == 2 ~ "Q2",
        ntile(median_household_income, 4) == 3 ~ "Q3",
        ntile(median_household_income, 4) == 4 ~ "Q4 — Highest income"
      ),
      levels = c("Q1 — Lowest income", "Q2", "Q3", "Q4 — Highest income")
    )
  ) |>
  slice_min(enrollment_rate, n = 15) |>
  # Reorder so lowest rate appears at the bottom of the horizontal chart
  mutate(county_name = reorder(county_name, enrollment_rate))

# Strip "County" suffix to save horizontal space
levels(bottom_15$county_name) <- sub(" County$", "", levels(bottom_15$county_name))

p_bottom_15 <- ggplot(
  bottom_15,
  aes(x = enrollment_rate, y = county_name, fill = income_quartile_label)
) +
  geom_col(width = 0.7) +
  # Enrollment rate label at the end of each bar
  geom_text(
    aes(label = paste0(round(enrollment_rate * 100, 1), "%")),
    hjust  = -0.15,
    size   = 3.2,
    color  = "#333333"
  ) +
  scale_fill_manual(values = QUARTILE_COLORS, name = "Income Quartile") +
  scale_x_continuous(
    labels = label_percent(accuracy = 1),
    limits = c(0, 0.22),
    breaks = seq(0, 0.20, by = 0.05),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title    = "15 Texas Counties with the Lowest College Enrollment Rates",
    subtitle = "Bars colored by county income quartile. Most low-enrollment counties are in Q1 or Q2,\nbut several middle-income counties (Q3) appear — suggesting geography matters beyond income.",
    x        = "College Enrollment Rate (Ages 18–24)",
    y        = NULL,
    caption  = ACS_CAPTION
  ) +
  theme_brief +
  theme(
    legend.position    = "bottom",
    legend.key.size    = unit(0.5, "lines"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#eeeeee"),
    axis.text.y        = element_text(size = 9.5)
  )

ggsave("output/03_bar_lowest_enrollment_counties.png",
       p_bottom_15, width = 9, height = 7, dpi = 300, bg = "white")
message("Saved: output/03_bar_lowest_enrollment_counties.png")


# =============================================================================
# CHART 4 — Histogram: distribution of enrollment rates
#
# Shows the full shape of the enrollment rate distribution across all 221
# counties. The right skew — with a long tail of high-enrollment counties —
# reflects university towns pulling the distribution upward. The vertical
# line marks the mean, and the annotation box summarizes key statistics so
# the chart is self-contained as a standalone figure.
# =============================================================================

enroll_mean   <- mean(counties$enrollment_rate)
enroll_median <- median(counties$enrollment_rate)

# A readable annotation string for key stats
stats_label <- paste0(
  "Mean:    ", round(enroll_mean   * 100, 1), "%\n",
  "Median:  ", round(enroll_median * 100, 1), "%\n",
  "n = ", nrow(counties), " counties"
)

p_hist <- ggplot(counties, aes(x = enrollment_rate)) +
  geom_histogram(
    binwidth = 0.05,
    fill     = ACCENT_BLUE,
    color    = "white",
    linewidth = 0.3,
    alpha    = 0.85
  ) +
  # Mean reference line
  geom_vline(
    xintercept = enroll_mean,
    color      = "#c85250",
    linewidth  = 0.9,
    linetype   = "dashed"
  ) +
  # Mean label
  annotate(
    "text",
    x     = enroll_mean + 0.01,
    y     = Inf,
    label = paste0("Mean\n", round(enroll_mean * 100, 1), "%"),
    hjust = 0,
    vjust = 1.4,
    size  = 3.2,
    color = "#c85250"
  ) +
  # Stats box
  annotate(
    "label",
    x     = 0.72,
    y     = Inf,
    label = stats_label,
    hjust = 1,
    vjust = 1.3,
    size  = 3.2,
    color = "#333333",
    fill      = "#f8f8f8",
    linewidth = 0.25
  ) +
  scale_x_continuous(
    labels = label_percent(accuracy = 1),
    breaks = seq(0, 0.80, by = 0.10)
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks()
  ) +
  labs(
    title    = "Distribution of College Enrollment Rates Across Texas Counties",
    subtitle = "Each bar represents a 5-percentage-point range. The long right tail reflects university host counties.",
    x        = "College Enrollment Rate (Ages 18–24)",
    y        = "Number of Counties",
    caption  = ACS_CAPTION
  ) +
  theme_brief

ggsave("output/04_histogram_enrollment_distribution.png",
       p_hist, width = 9, height = 6, dpi = 300, bg = "white")
message("Saved: output/04_histogram_enrollment_distribution.png")


message("\nAll four charts saved to /output.")
