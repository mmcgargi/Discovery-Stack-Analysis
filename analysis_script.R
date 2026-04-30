############################################################
# Discovery Stack Analysis
# Author: Maureen McGargill
# Date: 4/30/2026
#
# Description:
# This script analyzes variability in Quality and Impact scores,
# performs subsampling-based bootstrap analysis, and exports
# results and histogram bins for Prism.
#
# Required folder structure:
#
# DiscoveryStack_Analysis/
# ├── analysis_script.R
# ├── data/
# │   └── Composite_Score_Clean.xlsx
# └── output/
#
# To run:
# 1. Open this folder as an RStudio Project
# 2. Ensure the data file is in /data
# 3. Run the script from top to bottom
############################################################

# ---- Load libraries ----
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(ggplot2)

# ---- Set output directory ----
output_dir <- "output"

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# ---- Load data ----
data <- read_excel(file.path("data", "Composite_Score_Clean.xlsx")) %>%
  mutate(
    Manuscript_ID = as.character(Manuscript_ID),
    Score_Type = as.character(Score_Type),
    Score = as.numeric(Score)
  )


# ---- Check data structure ----
glimpse(data)

data %>%
  count(Manuscript_ID, Score_Type)

# ---- Calculate SD, range, and IQR by manuscript and score type ----
dispersion_by_type <- data %>%
  filter(!is.na(Score)) %>%
  group_by(Manuscript_ID, Score_Type) %>%
  summarise(
    n_reviews = n(),
    mean_score = mean(Score),
    SD = sd(Score),
    Range = max(Score) - min(Score),
    IQR = as.numeric(
      quantile(Score, 0.75, type = 7) -
        quantile(Score, 0.25, type = 7)
    ),
    .groups = "drop"
  )

# ---- Pivot Quality and Impact side by side ----
dispersion_wide <- dispersion_by_type %>%
  pivot_wider(
    names_from = Score_Type,
    values_from = c(n_reviews, mean_score, SD, Range, IQR)
  ) %>%
  mutate(
    SD_Diff = SD_Impact - SD_Quality,
    Range_Diff = Range_Impact - Range_Quality,
    IQR_Diff = IQR_Impact - IQR_Quality
  )

# ---- Paired Wilcoxon tests ----
sd_test <- wilcox.test(
  dispersion_wide$SD_Impact,
  dispersion_wide$SD_Quality,
  paired = TRUE,
  alternative = "greater"
)

range_test <- wilcox.test(
  dispersion_wide$Range_Impact,
  dispersion_wide$Range_Quality,
  paired = TRUE,
  alternative = "greater"
)

iqr_test <- wilcox.test(
  dispersion_wide$IQR_Impact,
  dispersion_wide$IQR_Quality,
  paired = TRUE,
  alternative = "greater"
)

# ---- Summary of paired tests ----
test_summary <- tibble(
  Metric = c("SD", "Range", "IQR"),
  Mean_Quality = c(
    mean(dispersion_wide$SD_Quality, na.rm = TRUE),
    mean(dispersion_wide$Range_Quality, na.rm = TRUE),
    mean(dispersion_wide$IQR_Quality, na.rm = TRUE)
  ),
  Mean_Impact = c(
    mean(dispersion_wide$SD_Impact, na.rm = TRUE),
    mean(dispersion_wide$Range_Impact, na.rm = TRUE),
    mean(dispersion_wide$IQR_Impact, na.rm = TRUE)
  ),
  Mean_Difference_Impact_minus_Quality = c(
    mean(dispersion_wide$SD_Diff, na.rm = TRUE),
    mean(dispersion_wide$Range_Diff, na.rm = TRUE),
    mean(dispersion_wide$IQR_Diff, na.rm = TRUE)
  ),
  Wilcoxon_P_Value = c(
    sd_test$p.value,
    range_test$p.value,
    iqr_test$p.value
  )
)

print(test_summary)

# ---- Export initial dispersion results ----
write_xlsx(
  list(
    Dispersion_By_Type = dispersion_by_type,
    Quality_vs_Impact = dispersion_wide,
    Test_Summary = test_summary
  ),
  path = file.path(output_dir, "Quality_vs_Impact_Dispersion_Results.xlsx")
)

# ---- Bootstrap/subsampling analysis ----
set.seed(123)

n_boot <- 5000

bootstrap_unmatched_once <- function(data, metric = "IQR") {
  
  data %>%
    filter(!is.na(Score)) %>%
    group_by(Manuscript_ID) %>%
    summarise(
      Q_vals = list(Score[Score_Type == "Quality"]),
      I_vals = list(Score[Score_Type == "Impact"]),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      n_q = length(unlist(Q_vals)),
      n_i = length(unlist(I_vals)),
      
      I_sample = list(sample(
        unlist(I_vals),
        size = n_q,
        replace = FALSE
      )),
      
      Q_metric = case_when(
        metric == "SD" ~ sd(unlist(Q_vals)),
        metric == "Range" ~ max(unlist(Q_vals)) - min(unlist(Q_vals)),
        metric == "IQR" ~ as.numeric(
          quantile(unlist(Q_vals), 0.75, type = 7) -
            quantile(unlist(Q_vals), 0.25, type = 7)
        )
      ),
      
      I_metric = case_when(
        metric == "SD" ~ sd(unlist(I_sample)),
        metric == "Range" ~ max(unlist(I_sample)) - min(unlist(I_sample)),
        metric == "IQR" ~ as.numeric(
          quantile(unlist(I_sample), 0.75, type = 7) -
            quantile(unlist(I_sample), 0.25, type = 7)
        )
      ),
      
      diff = I_metric - Q_metric
    ) %>%
    ungroup() %>%
    summarise(
      mean_diff = mean(diff, na.rm = TRUE)
    ) %>%
    pull(mean_diff)
}

# ---- Run bootstrap ----
bootstrap_results <- tibble(
  Iteration = 1:n_boot,
  SD = replicate(n_boot, bootstrap_unmatched_once(data, metric = "SD")),
  Range = replicate(n_boot, bootstrap_unmatched_once(data, metric = "Range")),
  IQR = replicate(n_boot, bootstrap_unmatched_once(data, metric = "IQR"))
)

# ---- Check bootstrap distributions ----
summary(bootstrap_results$SD)
summary(bootstrap_results$Range)
summary(bootstrap_results$IQR)

# ---- Summarize bootstrap results ----
bootstrap_summary <- bootstrap_results %>%
  pivot_longer(
    cols = c(SD, Range, IQR),
    names_to = "Metric",
    values_to = "Bootstrap_Mean_Diff"
  ) %>%
  group_by(Metric) %>%
  summarise(
    Mean_Diff = mean(Bootstrap_Mean_Diff, na.rm = TRUE),
    CI_Lower = quantile(Bootstrap_Mean_Diff, 0.025, na.rm = TRUE),
    CI_Upper = quantile(Bootstrap_Mean_Diff, 0.975, na.rm = TRUE),
    P_Value = mean(Bootstrap_Mean_Diff <= 0, na.rm = TRUE),
    .groups = "drop"
  )

print(bootstrap_summary)

# ---- Export bootstrap results ----
write_xlsx(
  list(
    Dispersion_By_Type = dispersion_by_type,
    Quality_vs_Impact = dispersion_wide,
    Paired_Test_Summary = test_summary,
    Bootstrap_Iterations = bootstrap_results,
    Bootstrap_Summary = bootstrap_summary
  ),
  path = file.path(output_dir, "Quality_vs_Impact_Dispersion_Bootstrap_Results.xlsx")
)

# ---- Prepare long-format bootstrap data for plotting/export ----
bootstrap_long <- bootstrap_results %>%
  pivot_longer(
    cols = c(SD, Range, IQR),
    names_to = "Metric",
    values_to = "Bootstrap_Mean_Diff"
  ) %>%
  mutate(
    Metric = factor(Metric, levels = c("SD", "Range", "IQR"))
  )

bootstrap_summary_plot <- bootstrap_summary %>%
  mutate(
    Metric = factor(Metric, levels = c("SD", "Range", "IQR"))
  )

# ---- Define shared bin breaks for all metrics ----
n_bins <- 60

all_values <- bootstrap_long$Bootstrap_Mean_Diff

x_min <- min(all_values, na.rm = TRUE)
x_max <- max(all_values, na.rm = TRUE)

breaks_shared <- seq(
  from = x_min,
  to = x_max,
  length.out = n_bins + 1
)

# ---- Plot bootstrap histograms using shared bin breaks ----
bootstrap_histogram_plot <- ggplot(bootstrap_long, aes(x = Bootstrap_Mean_Diff)) +
  geom_histogram(
    breaks = breaks_shared,
    fill = "gray75",
    color = "black",
    linewidth = 0.2
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4) +
  geom_segment(
    data = bootstrap_summary_plot,
    aes(
      x = CI_Lower,
      xend = CI_Upper,
      y = -Inf,
      yend = -Inf
    ),
    inherit.aes = FALSE,
    linewidth = 1.2
  ) +
  geom_point(
    data = bootstrap_summary_plot,
    aes(
      x = Mean_Diff,
      y = -Inf
    ),
    inherit.aes = FALSE,
    size = 1.5
  ) +
  facet_wrap(~ Metric, ncol = 1, scales = "free_y") +
  labs(
    x = "Mean difference in dispersion (Impact − Quality)",
    y = "Count"
  ) +
  theme_classic(base_size = 9) +
  theme(
    strip.text = element_text(size = 9, face = "bold"),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 9),
    panel.spacing = unit(0.4, "lines")
  )

print(bootstrap_histogram_plot)

# ---- Save R-generated histogram plot ----
ggsave(
  filename = file.path(output_dir, "Bootstrap_Dispersion_Histograms.pdf"),
  plot = bootstrap_histogram_plot,
  width = 3.5,
  height = 3.2
)

ggsave(
  filename = file.path(output_dir, "Bootstrap_Dispersion_Histograms.png"),
  plot = bootstrap_histogram_plot,
  width = 3.5,
  height = 3.2,
  dpi = 600
)

# ---- Generate histogram bin counts for Prism ----
histogram_bins <- bootstrap_long %>%
  group_by(Metric) %>%
  group_modify(~ {
    h <- hist(
      .x$Bootstrap_Mean_Diff,
      breaks = breaks_shared,
      plot = FALSE
    )
    
    tibble(
      Bin_Left = h$breaks[-length(h$breaks)],
      Bin_Right = h$breaks[-1],
      Bin_Midpoint = h$mids,
      Count = h$counts
    )
  }) %>%
  ungroup()

# ---- Confirm each metric sums to number of bootstrap iterations ----
histogram_bin_check <- histogram_bins %>%
  group_by(Metric) %>%
  summarise(
    Total_Count = sum(Count),
    .groups = "drop"
  )

print(histogram_bin_check)

# ---- Export histogram bin values for Prism ----
write_xlsx(
  list(
    Histogram_Bins = histogram_bins,
    Bin_Check = histogram_bin_check,
    Bootstrap_Summary = bootstrap_summary
  ),
  path = file.path(output_dir, "Bootstrap_Histogram_Bins_for_Prism.xlsx")
)
