# ================================================
# Requirements for Multi-Ancestry COVID-19 vs AMI MR Project
# ================================================

cat("Installing required packages...\n\n")

# CRAN packages
cran_packages <- c(
  "data.table", "tidyverse", "dplyr", "ggplot2", "patchwork",
  "kableExtra", "gt", "R.utils", "readr"
)

install.packages(cran_packages, repos = "https://cran.rstudio.com/", quiet = TRUE)

# Bioconductor / MR-specific packages
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager", quiet = TRUE)
}

BiocManager::install(c("TwoSampleMR", "MendelianRandomization"), 
                     update = FALSE, ask = FALSE, quiet = TRUE)

# Optional: MR-PRESSO (if not already installed)
# Uncomment the line below if you need MR-PRESSO
# devtools::install_github("rondolab/MR-PRESSO")

cat("\n✅ All core packages installed successfully!\n")
cat("You may need to manually install 'genetics.binaRies' and 'MRPracticals' if not available.\n")
cat("Make sure PLINK is installed and accessible for LD clumping.\n")
