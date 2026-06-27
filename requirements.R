# ================================================
# Requirements for Multi-Ancestry COVID-19 vs AMI MR Project
# ================================================

cat("Installing required packages...\n\n")

# Install devtools first (needed for GitHub installs)
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools", repos = "https://cran.rstudio.com/", quiet = TRUE)
}

# CRAN packages
cran_packages <- c(
  "data.table", "tidyverse", "dplyr", "ggplot2", "patchwork",
  "kableExtra", "gt", "R.utils", "readr", "ieugwasr", "LDlinkR"
)
install.packages(cran_packages, repos = "https://cran.rstudio.com/", quiet = TRUE)

# Bioconductor
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager", quiet = TRUE)
}
BiocManager::install("MendelianRandomization", update = FALSE, ask = FALSE, quiet = TRUE)

# GitHub packages
if (!requireNamespace("TwoSampleMR", quietly = TRUE)) {
  devtools::install_github("MRCIEU/TwoSampleMR")
}
if (!requireNamespace("genetics.binaRies", quietly = TRUE)) {
  devtools::install_github("MRCIEU/genetics.binaRies")
}
if (!requireNamespace("MRPRESSO", quietly = TRUE)) {
  devtools::install_github("rondolab/MR-PRESSO")
}

cat("\n All packages installed successfully!\n")
cat("Make sure PLINK is installed and accessible for LD clumping.\n")
