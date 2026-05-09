# MultiAncestry-MR-AMIMendelian Randomization Analysis Pipeline
Project: COVID-19 and Acute Myocardial Infarction (AMI)


This repository contains a reproducible R pipeline for two-sample Mendelian randomization analyses of COVID-19 exposures and acute myocardial infarction outcomes across ancestry-specific datasets.

Repository Structure
.
├── config/
│   └── analysis_plan.csv
├── data/
│   └── README.md
├── results/
│   └── .gitkeep
├── scripts/
│   ├── install_packages.R
│   └── run_mr_analysis.R
├── .gitignore
└── README.md
Input Data
Place harmonized TwoSampleMR-ready CSV files in the data/ folder. The default analysis plan expects these files:

eur-A2-GCST90473536.csv
eur-B2-GCST90473536.csv
eur-C2-GCST90473536.csv
sas-A2-GCST90473537.csv
sas-B2-GCST90473537.csv
sas-C2-GCST90473537.csv
eas_A2_AMI_GCST90018657.csv
eas_B2_AMI_GCST90018657.csv
eas_C2_AMI_GCST90018657.csv
afr_A2_COIVD-19_AMIGCST90473535.csv
afr_B2_COIVD-19_AMIGCST90473535.csv
afr_C2_COIVD-19_AMIGCST90473535.csv
The files are not committed by default because GWAS summary or harmonized data files may be large or access-controlled.

Install Dependencies
From the project root:

source("scripts/install_packages.R")
Some MR packages may require Bioconductor or GitHub installation depending on your R setup. The install script handles common CRAN, Bioconductor, and GitHub sources.

Run the Analysis
source("scripts/run_mr_analysis.R")
By default, the script reads config/analysis_plan.csv, loads each listed CSV from data/, filters invalid instruments, runs MR-PRESSO, performs MR analyses, and writes tables and plots to results/.

Outputs
For each dataset listed in the analysis plan, the pipeline creates:

MR estimates
Odds ratio table
Single-SNP results
Heterogeneity test results
Horizontal pleiotropy test results
MR-PRESSO summary
Scatter, forest, leave-one-out, and funnel plots
Notes
The original local setwd() calls were removed so the project can run on any computer.
The original script repeatedly reassigned mr_dat; this pipeline runs each CSV as a separate analysis.
Edit config/analysis_plan.csv to add, remove, or rename analyses.
