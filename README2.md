# Discovery Stack Pilot Analysis

This repository contains code and data for two analyses from: 
Discovery Stack Pilot: Feasibility and Outcomes of a Scientist-Designed Peer Review Model Separating Quality and Impact
Maureen A. McGargill, Beiyun C. Liu, Michael S. Kuhns, Daniel Mucida, Isabella Rauch, Lauren B. Rodda, Meghan A. Koch, Hugo Gonzalez, Ken Cadwell, Tanya S. Freedman, Tiffany C. Scharschmidt, Richard Sever, Jose Ordovas-Montanes, Sara Suliman, Andrew Oberst, Brooke Runnette, Matthew F. Krummel
bioRxiv 2025.10.31.685758; doi: https://doi.org/10.1101/2025.10.31.685758

1. **Quality vs. Impact dispersion analysis** — comparison of variability in Quality and Impact scores within manuscripts.
2. **NEW vs DSP-enrolled sensitivity analysis** — post-hoc comparison of survey responses between participants new to the study and those already enrolled as authors or reviewers

## Repository structure---
├── data/
│   ├── Composite_Score_Clean.xlsx
│   └── DSP_Survey_Counts_New_Vs_old.xlsx
├── analysis_script.R - variance_analysis
│   
├── DSP_Survey_new_vs_enrolled.R - sensitivity_analysis
│   
├── output/                            (gitignored; created by scripts)
└── README.md

## Analysis 1: Quality vs. Impact dispersion

### Analysis overview
The script performs the following analyses:

- Calculates standard deviation, range, and interquartile range (IQR) for Quality and Impact scores within each manuscript.
- Compares Quality and Impact variability using paired Wilcoxon signed-rank tests.
- Performs subsampling-based bootstrap analysis in which Impact scores are randomly subsampled without replacement to match the number of Quality scores for each manuscript.
- Generates bootstrap distributions of mean differences in dispersion metrics.
- Exports summary tables and histogram bin values for plotting in GraphPad Prism.

### Input data
The input file should be located at: `data/Composite_Score_Clean.xlsx`

The dataset should include the following columns:

- `Manuscript_ID`
- `Reviewer`
- `Score`
- `Score_Type`

`Score_Type` should indicate whether each score is a Quality or Impact score.

### How to run
1. Open the repository folder as an RStudio Project.
2. Confirm that `Composite_Score_Clean.xlsx` is in the `data/` folder.
3. Open `variance_analysis/analysis_script.R`.
4. Run the script from top to bottom.
5. The script will create output files in the `output/` folder.

### Output files
The script generates:

- `output/Quality_vs_Impact_Dispersion_Results.xlsx`
- `output/Quality_vs_Impact_Dispersion_Bootstrap_Results.xlsx`
- `output/Bootstrap_Histogram_Bins_for_Prism.xlsx`
- `output/Bootstrap_Dispersion_Histograms.pdf`
- `output/Bootstrap_Dispersion_Histograms.png`

### Statistical approach
For each manuscript, dispersion was calculated separately for Quality and Impact scores using standard deviation, range, and IQR. IQR was calculated as the difference between the 75th and 25th percentiles using the inclusive quantile definition.

To account for differences in reviewer numbers, Impact scores were randomly subsampled without replacement to match the number of Quality scores for each manuscript. This procedure was repeated for 5,000 iterations. For each iteration, the difference in dispersion was calculated as Impact − Quality and averaged across manuscripts.

Mean differences, 95% percentile confidence intervals, and one-sided p-values were estimated from the resulting empirical distributions. One-sided p-values were defined as the proportion of iterations in which the mean difference was less than or equal to zero.

---

## Analysis 2: NEW vs DSP-enrolled sensitivity analysis

### Analysis overview
The script performs the following analyses:

- Compares survey responses between participants who were new to the study (NEW) and those who were already enrolled to participate as authors or reviewers (DSP-enrolled).
- Performs a Mann-Whitney U (Wilcoxon rank-sum) test per survey question, with tie-corrected normal approximation and continuity correction.
- Performs a Pearson chi-square test with Monte Carlo p-value (10,000 simulations) as a small-cell-safe sensitivity check.
- Applies Benjamini-Hochberg false-discovery-rate (FDR) correction across all questions.
- Generates a forest plot of mean Likert-score differences with 95% bootstrap confidence intervals.
- Generates a diverging stacked Likert plot of response distributions per question.

### Input data
The input file should be located at: `data/DSP_Survey_Counts_New_Vs_old.xlsx`

The dataset should include the following columns:

- `Figure`
- `Figure_Position`
- `Question`
- `Role`
- `Response`
- `ALL_Count`
- `New_Count`
- `DSP-enrolled_Count`

The script automatically handles the three Likert scales used in the survey (agreement, comparison, intent) and uses the `Total` row when multiple roles answered a question (or the single role's row when only one role answered).

### How to run
1. Open the repository folder as an RStudio Project.
2. Confirm that `DSP_Survey_Counts_New_Vs_old.xlsx` is in the `data/` folder.
3. Open `sensitivity_analysis/DSP_Survey_new_vs_enrolled.R`.
4. Run the script from top to bottom.
5. The script will auto-install any missing packages and create output files in the `output/` folder.

### Output files
The script generates:

- `output/results_NEW_vs_DSP.csv`
- `output/ForestPlot_NEW_vs_DSP.png`
- `output/ForestPlot_NEW_vs_DSP.svg`
- `output/LikertPlot_NEW_vs_DSP.png`
- `output/LikertPlot_NEW_vs_DSP.svg`

### Statistical approach
Survey responses were treated as ordinal Likert data, scored from 1 (most favorable) to 5 (least favorable). For each survey item, responses from NEW and DSP-enrolled participants were compared using two complementary tests.

The primary test was the Mann-Whitney U test (Wilcoxon rank-sum), which ranks all responses jointly and compares the sum of ranks between groups. The test was performed using R's `wilcox.test()` with the normal approximation, tie correction, and continuity correction.

As a sensitivity check, a Pearson chi-square test was performed on the 2 × K contingency table of response counts. Because expected cell counts were below 5 for many questions, p-values were estimated by Monte Carlo simulation with 10,000 permutations of the group labels (R's `chisq.test()` with `simulate.p.value = TRUE`). Response categories with zero counts in both groups were dropped from the chi-square calculation only; this does not change the test result but avoids division-by-zero in R.

Across all questions, p-values were adjusted using the Benjamini-Hochberg FDR procedure to account for multiple comparisons. Both raw and adjusted p-values are reported.

Confidence intervals shown on the forest plot are 95% percentile bootstrap intervals computed from 5,000 resamples of the per-respondent ordinal scores within each group.

---

## Software
Analyses were performed in R using:

- `dplyr`
- `tidyr`
- `readxl`
- `writexl`
- `ggplot2`
- `forcats` (sensitivity analysis)
- `stringr` (sensitivity analysis)
- `purrr` (sensitivity analysis)
- `svglite` (sensitivity analysis, for SVG figure export)

The sensitivity analysis script auto-installs any missing packages on first run.