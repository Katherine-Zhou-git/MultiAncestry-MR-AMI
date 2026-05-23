# ================================================
# South Asian - COVID-19 Infection (C2) vs AMI
# ================================================

library(data.table)
library(MendelianRandomization)
library(MRPracticals)
library(TwoSampleMR)
library(LDlinkR)
library(dplyr)
library(tidyverse)
library(readr)
library(ieugwasr)
library(genetics.binaRies)
library(kableExtra)
library(gt)
library(phenoscanner)
library(R.utils)

rm(list = ls(all=TRUE))
set.seed(123456)
setwd("./COVID_GWAS")

# Exposure: Infection (C2) - South Asian
exposure_raw <- fread("./COVID-19_AMI_All/COVID19_HGI_C2_ALL_sas_leave23andme_20220403.tsv.gz")
exposure_c1 <- exposure_raw[, c(1,2,3,4,5,6,7,8,9,14,15)]
colnames(exposure_c1) <- c("CHR","POS","other_allele.exposure","effect_allele.exposure",
                          "SNP","N","beta.exposure","se.exposure","pval","eaf.exposure","rsid")

exposure_c1$exposure <- "Infected_COVID19_C2_SAS"
exposure_c1$id.exposure <- "covid_c2_sas"
exposure_c1$Z.exposure <- exposure_c1$beta.exposure / exposure_c1$se.exposure
exposure_c1 <- filter(exposure_c1, pval < 5e-6)
exposure_c1$F.exposure <- (exposure_c1$Z.exposure)^2

exposure_snps <- ld_clump(dat = exposure_c1, clump_kb = 10000, clump_r2 = 0.001,
                          plink_bin = genetics.binaRies::get_plink_binary(), bfile = "./SAS")

# Outcome
outcome_raw <- fread("./COVID-19_AMI_All/GCST90473537.h.tsv.gz")
outcome_ss1 <- outcome_raw[, c(1,2,3,4,5,6,7,8,9)]
colnames(outcome_ss1) <- c("CHR","POS","effect_allele.outcome","other_allele.outcome",
                          "beta.outcome","se.outcome","eaf.outcome","pval","rsid")
outcome_ss1$outcome <- "AMI"
outcome_ss1$id.outcome <- "ami_sas"
outcome_ss1$Z.outcome <- outcome_ss1$beta.outcome / outcome_ss1$se.outcome
outcome_clumped <- outcome_ss1

mr_dat <- harmonise_data(
  dplyr::tibble(SNP = exposure_snps$rsid, beta.exposure = exposure_snps$beta.exposure,
                se.exposure = exposure_snps$se.exposure, effect_allele.exposure = exposure_snps$effect_allele.exposure,
                other_allele.exposure = exposure_snps$other_allele.exposure, eaf.exposure = NA,
                id.exposure = exposure_snps$id.exposure, exposure = exposure_snps$exposure),
  dplyr::tibble(SNP = outcome_clumped$rsid, beta.outcome = outcome_clumped$beta.outcome,
                se.outcome = outcome_clumped$se.outcome, effect_allele.outcome = outcome_clumped$effect_allele.outcome,
                other_allele.outcome = outcome_clumped$other_allele.outcome, eaf.outcome = NA,
                outcome = outcome_clumped$outcome, id.outcome = outcome_clumped$id.outcome),
  action = 1
)

mr_dat$outcome[mr_dat$outcome == "ami"] <- "AMI"
mr_dat <- mr_dat[mr_dat$mr_keep == TRUE & mr_dat$palindromic != TRUE, ]


# ==================== MR-PRESSO ====================
results <- mr_presso(BetaOutcome = "beta.outcome", BetaExposure = "beta.exposure",
                     SdOutcome = "se.outcome", SdExposure = "se.exposure",
                     OUTLIERtest = TRUE, DISTORTIONtest = TRUE, data = mr_dat,
                     NbDistribution = length(mr_dat$SNP)/0.05, SignifThreshold = 0.05)

cat("Global test p =", results$`MR-PRESSO results`$`Global Test`$Pvalue, "\n")

# ==================== Main MR Analysis ====================
res <- mr(mr_dat, method_list = c(
  "mr_egger_regression",
  "mr_weighted_median",
  "mr_ivw_fe",
  "mr_ivw_mre",
  "mr_weighted_mode"
))

# Save results
odd <- generate_odds_ratios(res)
write.csv(odd, "results/tables/SAS_A2_results.csv", row.names = FALSE)

# ==================== Plots ====================
ggsave("results/figures/SAS_A2_scatter.png", mr_scatter_plot(res, mr_dat)[[1]], width = 8, height = 6)
ggsave("results/figures/SAS_A2_forest.png", mr_forest_plot(mr_singlesnp(mr_dat))[[1]], width = 10, height = 8)
ggsave("results/figures/SAS_A2_leaveoneout.png", mr_leaveoneout_plot(mr_leaveoneout(mr_dat))[[1]], width = 10, height = 8)
ggsave("results/figures/SAS_A2_funnel.png", mr_funnel_plot(mr_singlesnp(mr_dat))[[1]], width = 8, height = 6)

# ==================== Sensitivity Tests ====================
mr_heterogeneity(mr_dat)
mr_pleiotropy_test(mr_dat)


cat("SAS A2 Analysis Completed Successfully\n")
