# =============================================================================
# DSP Survey: NEW vs DSP-enrolled sensitivity analysis
# -----------------------------------------------------------------------------
# Compares survey responses between participants who were new to the study
# (NEW) and those who were already enrolled to participate as authors or
# reviewers prior to recruitment (DSP-enrolled). For each survey item,
# this script performs:
# - Mann-Whitney U (Wilcoxon rank-sum) test per question, with tie-corrected
#   normal approximation and continuity correction (R's default).
# - Pearson chi-square with Monte Carlo p-value (10,000 simulations) as
#   a small-cell-safe sensitivity check.
# - Benjamini-Hochberg (BH) FDR correction across all questions.
# - Forest plot of mean Likert-score differences with 95% bootstrap CIs.
# - Diverging stacked Likert plot of response distributions.
#
# Both plots are ordered by Figure, then Figure_Position.
#
# Required packages: readxl, dplyr, tidyr, forcats, ggplot2, stringr, purrr,
#                    svglite (used by ggsave to write .svg files)
# The block below auto-installs any that are missing, then loads them all.
# =============================================================================

required_pkgs <- c("readxl", "dplyr", "tidyr", "forcats",
                   "ggplot2", "stringr", "purrr", "svglite")
missing_pkgs <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(forcats)
  library(ggplot2)
  library(stringr)
  library(purrr)
  library(svglite)
})

# -- Configuration ------------------------------------------------------------
INPUT_FILE  <- "Data/DSP_Survey_Counts_New_Vs_old.xlsx"   # adjust if needed
OUTPUT_DIR  <- "output"                                    # where to save outputs
SET_SEED    <- 20260503                               # for reproducibility
N_BOOT      <- 5000                                   # bootstrap replicates for CIs
N_MC        <- 10000                                  # Monte Carlo replicates for chi-square
ALPHA       <- 0.05                                   # significance threshold

set.seed(SET_SEED)

# -- 1. Load and clean data ---------------------------------------------------
raw <- read_excel(INPUT_FILE)
names(raw) <- trimws(names(raw))             # strip leading/trailing spaces from column names
raw <- raw |>
  mutate(
    # Replace non-breaking spaces (U+00A0) sometimes present in Excel text with normal spaces
    Response = trimws(gsub(" ", " ", Response)),
    Figure   = trimws(Figure)
  )

# -- 2. Define response orderings (most -> least favorable, scored 1..5) ------
RESP_ORDER <- list(
  agreement  = c("Strongly Agree", "Agree", "Neither", "Disagree", "Strongly Disagree"),
  comparison = c("Much Better",   "Slightly better", "About the Same",
                 "Slightly Worse", "Much Worse"),
  intent     = c("Yes, definitely", "Yes, probably", "Undecided",
                 "Probably not", "No")
)

detect_scale <- function(responses) {
  for (nm in names(RESP_ORDER)) {
    if (setequal(unique(responses), RESP_ORDER[[nm]])) return(nm)
  }
  stop("Unrecognized response set: ",
       paste(unique(responses), collapse = " | "))
}

# -- 3. Per-question analysis -------------------------------------------------
analyze_question <- function(df) {
  scale_name <- detect_scale(df$Response)
  ord        <- RESP_ORDER[[scale_name]]
  df         <- df[match(ord, df$Response), ]   # ensure row order = scale order

  new_counts <- df$New_Count
  dsp_counts <- df$`DSP-enrolled_Count`
  K          <- length(ord)

  # Expand counts into per-respondent ordinal scores
  new_scores <- rep(seq_len(K), times = new_counts)
  dsp_scores <- rep(seq_len(K), times = dsp_counts)

  # --- Mann-Whitney U (Wilcoxon rank-sum) ---
  # exact = FALSE forces normal approximation with tie correction;
  # correct = TRUE applies continuity correction.
  mw <- suppressWarnings(
    wilcox.test(new_scores, dsp_scores, exact = FALSE, correct = TRUE)
  )
  # --- Z score and effect size r for Mann-Whitney U ---
  n1 <- length(new_scores)
  n2 <- length(dsp_scores)
  N  <- n1 + n2
  
  U <- unname(mw$statistic)
  
  # Expected value of U under the null
  mu_U <- n1 * n2 / 2
  
  # Tie-corrected variance of U
  all_scores <- c(new_scores, dsp_scores)
  tie_counts <- table(all_scores)
  
  tie_correction <- sum(tie_counts^3 - tie_counts) / (N * (N - 1))
  
  sigma_U <- sqrt((n1 * n2 / 12) * ((N + 1) - tie_correction))
  
  # Continuity correction, matching correct = TRUE above
  cc <- 0.5 * sign(U - mu_U)
  
  Z <- (U - mu_U - cc) / sigma_U
  
  # Effect size r
  r_effect <- Z / sqrt(N)
  
  # --- Pearson chi-square with Monte Carlo p-value ---
  # Drop response categories where both groups have 0 counts (chisq.test
  # returns NaN otherwise). MW handles zero categories fine, so this only
  # affects the chi-square sensitivity check.
  keep <- (new_counts + dsp_counts) > 0
  tbl <- rbind(NEW = new_counts[keep], DSP = dsp_counts[keep])
  chi <- suppressWarnings(
    chisq.test(tbl, simulate.p.value = TRUE, B = N_MC)
  )

  # --- Bootstrap 95% CI for mean(NEW) - mean(DSP) ---
  diffs <- replicate(N_BOOT, {
    a <- sample(new_scores, replace = TRUE)
    b <- sample(dsp_scores, replace = TRUE)
    mean(a) - mean(b)
  })
  ci <- quantile(diffs, c(0.025, 0.975), names = FALSE)

  tibble::tibble(
    scale     = scale_name,
    n_NEW     = sum(new_counts),
    n_DSP     = sum(dsp_counts),
    mean_NEW  = mean(new_scores),
    mean_DSP  = mean(dsp_scores),
    diff      = mean(new_scores) - mean(dsp_scores),
    median_NEW = median(new_scores),
    median_DSP = median(dsp_scores),
    ci_lo     = ci[1],
    ci_hi     = ci[2],
    U         = U,
    Z_MW      = Z,
    r_effect  = r_effect,
    p_MW      = mw$p.value,
    chi_sq    = unname(chi$statistic),
    p_chi     = chi$p.value
  )
}

results <- raw |>
  group_by(Figure, Figure_Position, Question) |>
  group_modify(~ analyze_question(.x)) |>
  ungroup() |>
  mutate(
    p_MW_BH  = p.adjust(p_MW,  method = "BH"),
    p_chi_BH = p.adjust(p_chi, method = "BH")
  )

# -- 4. Order questions by Figure, then Figure_Position -----------------------
fig_levels <- raw |>
  distinct(Figure) |>
  mutate(
    fig_num = as.integer(str_extract(Figure, "(?<=\\.\\s)\\d+")),
    fig_let = str_extract(Figure, "[A-Za-z]+$")
  ) |>
  arrange(fig_num, fig_let) |>
  pull(Figure)

results <- results |>
  mutate(Figure = factor(Figure, levels = fig_levels)) |>
  arrange(Figure, Figure_Position) |>
  mutate(plot_label = paste0(Figure, ": ", Question),
         plot_label = factor(plot_label, levels = plot_label))

# Save the results table
write.csv(results,
          file.path(OUTPUT_DIR, "results_NEW_vs_DSP.csv"),
          row.names = FALSE)

cat("\n--- Results summary (sorted by figure) ---\n")
print(results |>
        select(Figure, Question, n_NEW, n_DSP, median_NEW, median_DSP, mean_NEW, mean_DSP,
               U, Z_MW, r_effect, p_MW, p_MW_BH, p_chi, p_chi_BH),
      n = Inf)

cat("\nItems significant after BH correction (Mann-Whitney):\n")
print(filter(results, p_MW_BH < ALPHA) |>
        select(Figure, Question, p_MW, p_MW_BH))

# =============================================================================
# 5. Forest plot
# =============================================================================
forest_data <- results |>
  mutate(sig_BH = p_MW_BH < ALPHA)

forest <- ggplot(forest_data,
                 aes(x = diff, y = fct_rev(plot_label), color = sig_BH)) +
  geom_vline(xintercept = 0, linewidth = 0.5) +
  geom_errorbar(aes(xmin = ci_lo, xmax = ci_hi),
                orientation = "y",
                width = 0.30, linewidth = 0.6) +
  geom_point(size = 2.4) +
  scale_color_manual(values = c(`FALSE` = "#1f5582",
                                `TRUE`  = "#c1422a"),
                     guide = "none") +
  labs(
    x = expression(atop("Mean Likert score difference (NEW − DSP-enrolled)",
                        "(← NEW more favorable     |     NEW less favorable →)")),
    y = NULL,
    title    = "Difference in mean response: NEW vs DSP-enrolled",
    subtitle = "Lower scores = more favorable; horizontal lines = 95% bootstrap CI"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor    = element_blank(),
    panel.grid.major.y  = element_blank(),
    axis.text.y         = element_text(size = 9),
    plot.title          = element_text(face = "bold")
  )

ggsave(file.path(OUTPUT_DIR, "ForestPlot_NEW_vs_DSP.png"),
       forest, width = 8, height = 9, dpi = 300)
ggsave(file.path(OUTPUT_DIR, "ForestPlot_NEW_vs_DSP.svg"),
       forest, width = 8, height = 9)

# =============================================================================
# 6. Diverging stacked Likert plot
# =============================================================================
PALETTE_5 <- c("#1f5582",  # most favorable (e.g., Strongly Agree / Much Better)
               "#5fa8d3",  # favorable
               "#d6d6d6",  # neutral
               "#f4a261",  # unfavorable
               "#c1422a")  # most unfavorable

# Reshape to long: one row per (question, group, response category)
likert_long <- raw |>
  group_by(Question) |>
  mutate(scale = detect_scale(Response)) |>
  ungroup() |>
  mutate(resp_idx = map2_int(Response, scale,
                             ~ match(.x, RESP_ORDER[[.y]]))) |>
  pivot_longer(cols = c(New_Count, `DSP-enrolled_Count`),
               names_to = "Group", values_to = "Count") |>
  mutate(Group = recode(Group,
                        "New_Count"            = "NEW",
                        "DSP-enrolled_Count"   = "DSP-enrolled")) |>
  group_by(Question, Group) |>
  mutate(pct = Count / sum(Count) * 100) |>
  ungroup()

# Compute centered x-positions (neutral category straddles zero)
likert_pos <- likert_long |>
  arrange(Question, Group, resp_idx) |>
  group_by(Question, Group) |>
  mutate(
    cum         = cumsum(pct),
    cum_prev    = lag(cum, default = 0),
    neutral_mid = cum[resp_idx == 3] - pct[resp_idx == 3] / 2,
    x_left      = cum_prev - neutral_mid,
    x_right     = cum      - neutral_mid
  ) |>
  ungroup() |>
  mutate(plot_label = factor(paste0(Figure, ": ", Question),
                             levels = levels(results$plot_label)))

# y-position: each question gets two stacked bars.
# Y_SPACING controls vertical space between question pairs (> 1 = more space).
Y_SPACING <- 1.3

likert_pos <- likert_pos |>
  mutate(
    y_offset = ifelse(Group == "NEW", 0.25, -0.25),  # larger -> more space within pair
    y        = as.numeric(fct_rev(plot_label)) * Y_SPACING + y_offset
  )

likert <- ggplot(likert_pos) +
  geom_rect(aes(xmin = x_left, xmax = x_right,
                ymin = y - 0.18, ymax = y + 0.18,
                fill = factor(resp_idx)),
            color = "black", linewidth = 0.3) +
  geom_vline(xintercept = 0, linewidth = 0.5) +
  geom_vline(xintercept = -100, color = "black", linewidth = 0.3) +  # left axis line at -100%
  geom_text(data = distinct(likert_pos, plot_label, Group, y),
            aes(x = -108, y = y, label = Group),
            hjust = 1, size = 2.6, color = "#444") +
  scale_fill_manual(
    values = PALETTE_5,
    labels = c("Most favorable\n(Strongly Agree / Much Better / Yes definitely)",
               "Favorable",
               "Neutral",
               "Unfavorable",
               "Most unfavorable\n(Strongly Disagree / Much Worse / No)"),
    name   = "Response (most-to-least favorable)"
  ) +
  scale_x_continuous(
    breaks = c(-100, -75, -50, -25, 0, 25, 50, 75, 100),
    labels = c("100%", "75%", "50%", "25%", "0", "25%", "50%", "75%", "100%"),
    limits = c(-130, 110),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq_along(levels(likert_pos$plot_label)) * Y_SPACING,
    labels = rev(levels(likert_pos$plot_label)),
    expand = expansion(add = 0.6)
  ) +
  labs(
    x = "← More favorable     Percent of group     Less favorable →",
    y = NULL,
    title = "Response distributions: NEW (top) vs DSP-enrolled (bottom)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),                                # cleaner with axis line
    axis.line.x        = element_line(color = "black", linewidth = 0.3),  # bottom axis line
    axis.ticks.x       = element_line(color = "black", linewidth = 0.3),
    legend.position    = "bottom",
    legend.key.size    = unit(0.5, "lines"),
    legend.text        = element_text(size = 8),
    plot.title         = element_text(face = "bold")
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

ggsave(file.path(OUTPUT_DIR, "LikertPlot_NEW_vs_DSP.png"),
       likert, width = 11, height = 10, dpi = 300)
ggsave(file.path(OUTPUT_DIR, "LikertPlot_NEW_vs_DSP.svg"),
       likert, width = 11, height = 10)

cat("\nDone.  Outputs in:", normalizePath(OUTPUT_DIR), "\n")
cat("  - results_NEW_vs_DSP.csv\n")
cat("  - ForestPlot_NEW_vs_DSP.png / .svg\n")
cat("  - LikertPlot_NEW_vs_DSP.png  / .svg\n")
