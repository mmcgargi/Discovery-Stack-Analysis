Analysis overview
The script performs the following analyses:
Calculates standard deviation, range, and interquartile range (IQR) for Quality and Impact scores within each manuscript.
Compares Quality and Impact variability using paired Wilcoxon signed-rank tests.
Performs subsampling-based bootstrap analysis in which Impact scores are randomly subsampled without replacement to match the number of Quality scores for each manuscript.
Generates bootstrap distributions of mean differences in dispersion metrics.
Exports summary tables and histogram bin values for plotting in GraphPad Prism.
Input data
The input file should be located at: data/Composite_Score_Clean.xlsx
The dataset should include the following columns:
Manuscript_ID
Reviewer
Score
Score_Type
Score_Type should indicate whether each score is a Quality or Impact score.
How to run
Open the repository folder as an RStudio Project.
Confirm that Composite_Score_Clean.xlsx is in the data/ folder.
Open analysis_script.R.
Run the script from top to bottom.
The script will create output files in the output/ folder.
Output files
The script generates:
output/Quality_vs_Impact_Dispersion_Results.xlsx
output/Quality_vs_Impact_Dispersion_Bootstrap_Results.xlsx
output/Bootstrap_Histogram_Bins_for_Prism.xlsx
output/Bootstrap_Dispersion_Histograms.pdf
output/Bootstrap_Dispersion_Histograms.png
Statistical approach
For each manuscript, dispersion was calculated separately for Quality and Impact scores using standard deviation, range, and IQR. IQR was calculated as the difference between the 75th and 25th percentiles using the inclusive quantile definition.
To account for differences in reviewer numbers, Impact scores were randomly subsampled without replacement to match the number of Quality scores for each manuscript. This procedure was repeated for 5,000 iterations. For each iteration, the difference in dispersion was calculated as Impact − Quality and averaged across manuscripts.
Mean differences, 95% percentile confidence intervals, and one-sided p-values were estimated from the resulting empirical distributions. One-sided p-values were defined as the proportion of iterations in which the mean difference was less than or equal to zero.
Software
Analyses were performed in R using:
dplyr
tidyr
readxl
writexl
ggplot2
