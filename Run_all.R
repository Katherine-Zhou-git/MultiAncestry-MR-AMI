# ================================================
# Run All Analyses - Multi-Ancestry COVID-19 vs AMI Mendelian Randomization
# ================================================

dir.create("results/tables",  showWarnings = FALSE, recursive = TRUE)
dir.create("results/figures", showWarnings = FALSE, recursive = TRUE)

cat("Starting Multi-Ancestry COVID-19 vs AMI MR Analysis...\n")
cat("==================================================\n\n")

# ==================== EUROPEAN ANCESTRY ====================
cat("Running European Ancestry Analyses...\n")
source("scripts/EUR_A2.R")
source("scripts/EUR_B2.R")
source("scripts/EUR_C2.R")
cat("European Ancestry Completed\n\n")

# ==================== EAST ASIAN ANCESTRY ====================
cat("Running East Asian Ancestry Analyses...\n")
source("scripts/EAS_A2.R")
source("scripts/EAS_B2.R")
source("scripts/EAS_C2.R")
cat("East Asian Ancestry Completed\n\n")

# ==================== SOUTH ASIAN ANCESTRY ====================
cat("Running South Asian Ancestry Analyses...\n")
source("scripts/SAS_A2.R")
source("scripts/SAS_B2.R")
source("scripts/SAS_C2.R")
cat("South Asian Ancestry Completed\n\n")

# ==================== AFRICAN ANCESTRY ====================
cat("Running African Ancestry Analyses...\n")
source("scripts/AFR_A2.R")
source("scripts/AFR_B2.R")
source("scripts/AFR_C2.R")
cat("African Ancestry Completed\n\n")

# ==================== SUMMARY FOREST PLOTS ====================
cat("Generating Summary Forest Plots...\n")
source("scripts/summary_forest_plots.R")
cat("Summary Forest Plots Generated\n\n")

cat("==================================================\n")
cat("ALL ANALYSES COMPLETED SUCCESSFULLY!\n")
cat("Results are saved in 'results/tables/' and 'results/figures/'\n")
