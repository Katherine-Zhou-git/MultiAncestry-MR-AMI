# Multi-Ancestry Mendelian Randomization: COVID-19 and Acute Myocardial Infarction (AMI)

This repository contains the R code for a **multi-ancestry two-sample Mendelian Randomization (MR)** study examining the causal relationship between COVID-19 phenotypes and acute myocardial infarction (AMI) across European, East Asian, South Asian, and African ancestries.

## Outputs
For each dataset, the pipeline creates:

MR estimates
Odds ratio table
Single-SNP results
Heterogeneity test results
Horizontal pleiotropy test results
MR-PRESSO summary
Scatter, forest, leave-one-out, and funnel plots

## Repository Structure

```bash
COVID19-AMI-MultiAncestry-MR/
├── README.md
├── requirements.R
├── run_all.R
├── scripts/
│   ├── EUR_A2.R
│   ├── EUR_B2.R
│   ├── EUR_C2.R
│   ├── EAS_A2.R
│   ├── EAS_B2.R
│   ├── EAS_C2.R
│   ├── SAS_A2.R
│   ├── SAS_B2.R
│   ├── SAS_C2.R
│   ├── AFR_A2.R
│   ├── AFR_B2.R
│   └── AFR_C2.R
├── summary_forest_plots.R
├── results/
│   ├── tables/          # MR results (CSV)
│   └── figures/         # Scatter, forest, funnel, leave-one-out plots
├── data/                # GWAS summary statistics (not included)
└── .gitignore
How to Run
Bash# 1. Install required packages
Rscript requirements.R

# 2. Run all MR analyses
Rscript run_all.R

# 3. Generate summary forest plots
Rscript summary_forest_plots.R
Requirements

R version ≥ 4.2.0
PLINK (for LD clumping)
Key R packages:
TwoSampleMR
MendelianRandomization
MRPRESSO (for outlier detection)
data.table, tidyverse, ggplot2, patchwork, kableExtra


See requirements.R for automatic installation.
Data Sources

COVID-19 GWAS: COVID-19 Host Genetics Initiative (HGI)
AMI GWAS: UK Biobank & Biobank Japan

Analysis Details

Exposure: Three COVID-19 phenotypes (A2: Very Severe, B2: Hospitalized, C2: Infected)
Outcome: Acute Myocardial Infarction (AMI)
Methods: IVW (fixed/random), MR-Egger, Weighted Median, Weighted Mode, MR-PRESSO
Sensitivity analyses: Heterogeneity, pleiotropy, leave-one-out, funnel plots

Citation
If you use this code in your research, please cite the original manuscript (once published).
License
MIT License
