# ================================================
# Summary Forest Plots - Multi-Ancestry COVID-19 and AMI
# ================================================

library(ggplot2)
library(dplyr)
library(patchwork)

# Shared theme
shared_theme <- theme_minimal() +
  theme(text = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank(),
        strip.text = element_blank(),
        plot.margin = margin(10, 5, 10, 5))

# ================================================
# 1. EUROPEAN ANCESTRY
# ================================================
data_euro <- data.frame(
  Phenotype = factor(rep(c("COVID-19 susceptibility", "Hospitalized COVID-19", "Very Severe COVID-19"), each = 5),
                     levels = c("COVID-19 susceptibility", "Hospitalized COVID-19", "Very Severe COVID-19")),
  Method = factor(rep(c("MR Egger", "Weighted Median", "IVW (Fixed)", "IVW (Random)", "Weighted Mode"), 3),
                  levels = rev(c("IVW (Fixed)", "IVW (Random)", "Weighted Median", "Weighted Mode", "MR Egger"))),
  OR = c(0.98,0.95,0.92,0.92,0.94, 0.99,0.97,0.94,0.94,0.97, 1.01,0.98,0.95,0.95,0.98),
  CI_lower = c(0.79,0.81,0.82,0.84,0.80, 0.89,0.91,0.90,0.89,0.92, 0.94,0.94,0.92,0.91,0.95),
  CI_upper = c(1.20,1.11,1.04,1.01,1.11, 1.09,1.03,0.98,0.99,1.03, 1.08,1.02,0.98,0.99,1.03),
  p_val = c(0.813,0.474,0.182,0.086,0.474, 0.786,0.302,0.005,0.029,0.370, 0.872,0.333,0.00047,0.008,0.474),
  SNPs = c(rep(13,5), rep(28,5), rep(24,5))
)

table_data <- data_euro %>%
  mutate(OR_CI = sprintf("%.2f (%.2f-%.2f)", OR, CI_lower, CI_upper),
         P = sprintf("%.3f", p_val))

pheno_labels <- table_data %>% group_by(Phenotype) %>% summarize(MidPoint = levels(Method)[3])

p_pheno  <- ggplot(table_data, aes(y = Method)) + 
  geom_text(data = pheno_labels, aes(y = MidPoint, x = 0.25, label = Phenotype), hjust = 0) +
  facet_grid(Phenotype ~ ., scales = "free_y", space = "free_y") +
  scale_x_continuous(limits = c(0,1), breaks = 0.25, labels = "Phenotype", position = "top") +
  labs(x = NULL, y = NULL) + shared_theme

p_method <- ggplot(table_data, aes(y = Method)) + 
  geom_text(aes(x = 0, label = as.character(Method)), hjust = 0) +
  facet_grid(Phenotype ~ ., scales = "free_y", space = "free_y") +
  scale_x_continuous(limits = c(0,1), breaks = 0, labels = "Method", position = "top") +
  labs(x = NULL, y = NULL) + shared_theme

p_forest <- ggplot(table_data, aes(x = OR, y = Method)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper, color = Phenotype), height = 0.2, linewidth = 0.8) +
  geom_point(aes(color = Phenotype), size = 2.8) +
  facet_grid(Phenotype ~ ., scales = "free_y", space = "free_y") +
  scale_color_manual(values = c("COVID-19 susceptibility" = "#1f77b4",
                                "Hospitalized COVID-19" = "#ff7f0e",
                                "Very Severe COVID-19" = "#d62728")) +
  scale_x_continuous(breaks = seq(0.8, 1.2, by = 0.1)) +
  labs(x = "Odds Ratio (95% CI)", y = NULL) + shared_theme +
  theme(axis.text.y = element_blank(), legend.position = "none")

p_table <- ggplot(table_data, aes(y = Method)) +
  geom_text(aes(x = 0, label = OR_CI), hjust = 0.5) +
  geom_text(aes(x = 1.3, label = P), hjust = 0.5) +
  geom_text(aes(x = 2.1, label = SNPs), hjust = 0.5) +
  facet_grid(Phenotype ~ ., scales = "free_y", space = "free_y") +
  scale_x_continuous(breaks = c(0, 1.3, 2.1),
                     labels = c("OR (95% CI)", "P-value", "SNPs"),
                     limits = c(-0.6, 2.5), position = "top") +
  labs(x = NULL, y = NULL) + shared_theme +
  theme(axis.text.y = element_blank())

final_euro <- p_pheno + p_method + p_forest + p_table +
  plot_layout(widths = c(1.4, 0.8, 1.2, 1.9)) +
  plot_annotation(title = "Mendelian Randomization: European Ancestry",
                  theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)))

ggsave("results/figures/Forest_European.png", final_euro, width = 15, height = 10, dpi = 300)
print(final_euro)

# ================================================
# 2. EAST ASIAN ANCESTRY
# ================================================
data_ea <- data.frame(
  Phenotype = factor(rep(c("COVID-19 susceptibility", "Hospitalized COVID-19", "Very Severe COVID-19"), each = 5),
                     levels = c("COVID-19 susceptibility", "Hospitalized COVID-19", "Very Severe COVID-19")),
  Method = factor(rep(c("MR Egger", "Weighted Median", "IVW Fixed Effects", "IVW Random Effects", "Weighted Mode"), 3),
                  levels = rev(c("IVW Fixed Effects", "IVW Random Effects", "Weighted Median", "Weighted Mode", "MR Egger"))),
  OR = c(0.85,0.97,0.96,0.96,1.02, 1.09,0.99,0.99,0.99,1.02, 0.97,0.99,0.98,0.98,0.99),
  CI_lower = c(0.70,0.89,0.91,0.90,0.89, 0.97,0.93,0.94,0.94,0.91, 0.92,0.95,0.95,0.96,0.94),
  CI_upper = c(1.06,1.08,1.04,1.04,1.18, 1.22,1.07,1.04,1.05,1.15, 1.04,1.03,1.01,1.00,1.04),
  p_val = c(0.21,0.63,0.35,0.38,0.77, 0.18,0.93,0.71,0.74,0.69, 0.47,0.54,0.18,0.06,0.73),
  SNPs = c(rep(7,5), rep(9,5), rep(9,5))
)

table_data <- data_ea %>%
  mutate(OR_CI = sprintf("%.2f (%.2f-%.2f)", OR, CI_lower, CI_upper),
         P = sprintf("%.3f", p_val))

pheno_labels <- table_data %>% group_by(Phenotype) %>% summarize(MidPoint = levels(Method)[3])

p_pheno  <- ggplot(table_data, aes(y = Method)) + 
  geom_text(data = pheno_labels, aes(y = MidPoint, x = 0.25, label = Phenotype), hjust = 0) +
  facet_grid(Phenotype ~ ., scales = "free_y", space = "free_y") +
  scale_x_continuous(limits = c(0,1), breaks = 0.25, labels = "Phenotype", position = "top") +
  labs(x = NULL, y = NULL) + shared_theme

p_method <- ggplot(table_data, aes(y = Method)) + 
  geom_text(aes(x = 0, label = as.character(Method)), hjust = 0) +
  facet_grid(Phenotype ~ ., scales = "free_y", space = "free_y") +
  scale_x_continuous(limits = c(0,1), breaks = 0, labels = "Method", position = "top") +
  labs(x = NULL, y = NULL) + shared_theme

p_forest <- ggplot(table_data, aes(x = OR, y = Method)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper, color = Phenotype), height = 0.2, linewidth = 0.8) +
  geom_point(aes(color = Phenotype), size = 2.8) +
  facet_grid(Phenotype ~ ., scales = "free_y", space = "free_y") +
  scale_color_manual(values = c("COVID-19 susceptibility" = "#2ca02c",
                                "Hospitalized COVID-19" = "#9467bd",
                                "Very Severe COVID-19" = "#e377c2")) +
  scale_x_continuous(breaks = seq(0.5, 2.0, by = 0.5)) +
  labs(x = "Odds Ratio (95% CI)", y = NULL) + shared_theme +
  theme(axis.text.y = element_blank(), legend.position = "none")

p_table <- ggplot(table_data, aes(y = Method)) +
  geom_text(aes(x = 0, label = OR_CI), hjust = 0.5) +
  geom_text(aes(x = 1.3, label = P), hjust = 0.5) +
  geom_text(aes(x = 2.1, label = SNPs), hjust = 0.5) +
  facet_grid(Phenotype ~ ., scales = "free_y", space = "free_y") +
  scale_x_continuous(breaks = c(0, 1.3, 2.1),
                     labels = c("OR (95% CI)", "P-value", "SNPs"),
                     limits = c(-0.6, 2.5), position = "top") +
  labs(x = NULL, y = NULL) + shared_theme +
  theme(axis.text.y = element_blank())

final_ea <- p_pheno + p_method + p_forest + p_table +
  plot_layout(widths = c(1.4, 0.8, 1.2, 1.9)) +
  plot_annotation(title = "Mendelian Randomization: East Asian Ancestry",
                  theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)))

ggsave("results/figures/Forest_EastAsian.png", final_ea, width = 15, height = 10, dpi = 300)
print(final_ea)

# ================================================
# 3. SOUTH ASIAN ANCESTRY
# ================================================
data_sa <- data.frame(
  Phenotype = factor(rep(c("COVID-19 susceptibility", "Hospitalized COVID-19", "Very Severe COVID-19"), each = 5),
                     levels = c("COVID-19 susceptibility", "Hospitalized COVID-19", "Very Severe COVID-19")),
  Method = factor(rep(c("MR Egger", "Weighted Median", "IVW Fixed Effects", "IVW Random Effects", "Weighted Mode"), 3),
                  levels = rev(c("IVW Fixed Effects", "IVW Random Effects", "Weighted Median", "Weighted Mode", "MR Egger"))),
  OR = c(0.94,0.93,0.93,0.93,0.94, 0.93,0.92,0.92,0.92,1.02, 1.12,1.03,1.02,1.02,1.04),
  CI_lower = c(0.45,0.62,0.68,0.75,0.50, 0.69,0.79,0.82,0.85,0.80, 0.91,0.91,0.94,0.97,0.88),
  CI_upper = c(1.99,1.39,1.27,1.15,1.78, 1.24,1.06,1.02,0.99,1.31, 1.38,1.17,1.12,1.08,1.22),
  p_val = c(0.88,0.72,0.64,0.50,0.86, 0.62,0.26,0.13,0.02,0.85, 0.29,0.64,0.59,0.39,0.66),
  SNPs = c(rep(15,5), rep(18,5), rep(19,5))
)

table_data <- data_sa %>%
  mutate(OR_CI = sprintf("%.2f (%.2f-%.2f)", OR, CI_lower, CI_upper),
         P = sprintf("%.3f", p_val))

pheno_labels <- table_data %>% group_by(Phenotype) %>% summarize(MidPoint = levels(Method)[3])

p_pheno  <- ggplot(table_data, aes(y = Method)) + 
  geom_text(data = pheno_labels, aes(y = MidPoint, x = 0.25, label = Phenotype), hjust = 0) +
  facet_grid(Phenotype ~ ., scales = "free_y", space = "free_y") +
  scale_x_continuous(limits = c(0,1), breaks = 0.25, labels = "Phenotype", position = "top") +
  labs(x = NULL, y = NULL) + shared_theme

p_method <- ggplot(table_data, aes(y = Method)) + 
  geom_text(aes(x = 0, label = as.character(Method)), hjust = 0) +
  facet_grid(Phenotype ~ ., scales = "free_y", space = "free_y") +
  scale_x_continuous(limits = c(0,1), breaks = 0, labels = "Method", position = "top") +
  labs(x = NULL, y = NULL) + shared_theme

p_forest <- ggplot(table_data, aes(x = OR, y = Method)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper, color = Phenotype), height = 0.2, linewidth = 0.8) +
  geom_point(aes(color = Phenotype), size = 2.8) +
  facet_grid(Phenotype ~ ., scales = "free_y", space = "free_y") +
  scale_color_manual(values = c("COVID-19 susceptibility" = "#8c564b",
                                "Hospitalized COVID-19" = "#e377c2",
                                "Very Severe COVID-19" = "#7f7f7f")) +
  scale_x_continuous(breaks = seq(0.5, 2.0, by = 0.5)) +
  labs(x = "Odds Ratio (95% CI)", y = NULL) + shared_theme +
  theme(axis.text.y = element_blank(), legend.position = "none")

p_table <- ggplot(table_data, aes(y = Method)) +
  geom_text(aes(x = 0, label = OR_CI), hjust = 0.5) +
  geom_text(aes(x = 1.3, label = P), hjust = 0.5) +
  geom_text(aes(x = 2.1, label = SNPs), hjust = 0.5) +
  facet_grid(Phenotype ~ ., scales = "free_y", space = "free_y") +
  scale_x_continuous(breaks = c(0, 1.3, 2.1),
                     labels = c("OR (95% CI)", "P-value", "SNPs"),
                     limits = c(-0.6, 2.5), position = "top") +
  labs(x = NULL, y = NULL) + shared_theme +
  theme(axis.text.y = element_blank())

final_sa <- p_pheno + p_method + p_forest + p_table +
  plot_layout(widths = c(1.4, 0.8, 1.2, 1.9)) +
  plot_annotation(title = "Mendelian Randomization: South Asian Ancestry",
                  theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)))

ggsave("results/figures/Forest_SouthAsian.png", final_sa, width = 15, height = 10, dpi = 300)
print(final_sa)

# ================================================
# 4. AFRICAN ANCESTRY
# ================================================
data_af <- data.frame(
  Phenotype = factor(rep(c("COVID-19 susceptibility", "Hospitalized COVID-19", "Very Severe COVID-19"), each = 5),
                     levels = c("COVID-19 susceptibility", "Hospitalized COVID-19", "Very Severe COVID-19")),
  Method = factor(rep(c("MR Egger", "Weighted Median", "IVW Fixed Effects", "IVW Random Effects", "Weighted Mode"), 3),
                  levels = rev(c("IVW Fixed Effects", "IVW Random Effects", "Weighted Median", "Weighted Mode", "MR Egger"))),
  OR = c(0.51,0.86,1.09,1.09,0.48, 1.28,0.78,0.84,0.84,0.76, 1.30,0.86,1.01,1.01,0.82),
  CI_lower = c(0.22,0.44,0.71,0.65,0.10, 0.79,0.55,0.66,0.66,0.38, 0.79,0.65,0.83,0.81,0.49),
  CI_upper = c(1.18,1.67,1.66,1.81,2.24, 2.08,1.10,1.07,1.06,1.49, 2.16,1.13,1.22,1.26,1.36),
  p_val = c(0.13,0.65,0.70,0.75,0.36, 0.33,0.16,0.15,0.15,0.43, 0.33,0.28,0.93,0.94,0.46),
  SNPs = c(rep(27,5), rep(27,5), rep(10,5))
)

table_data <- data_af %>%
  mutate(OR_CI = sprintf("%.2f (%.2f-%.2f)", OR, CI_lower, CI_upper),
         P = sprintf("%.3f", p_val))

pheno_labels <- table_data %>% group_by(Phenotype) %>% summarize(MidPoint = levels(Method)[3])

p_pheno  <- ggplot(table_data, aes(y = Method)) + 
  geom_text(data = pheno_labels, aes(y = MidPoint, x = 0.25, label = Phenotype), hjust = 0) +
  facet_grid(Phenotype ~ ., scales = "free_y", space = "free_y") +
  scale_x_continuous(limits = c(0,1), breaks = 0.25, labels = "Phenotype", position = "top") +
  labs(x = NULL, y = NULL) + shared_theme

p_method <- ggplot(table_data, aes(y = Method)) + 
  geom_text(aes(x = 0, label = as.character(Method)), hjust = 0) +
  facet_grid(Phenotype ~ ., scales = "free_y", space = "free_y") +
  scale_x_continuous(limits = c(0,1), breaks = 0, labels = "Method", position = "top") +
  labs(x = NULL, y = NULL) + shared_theme

p_forest <- ggplot(table_data, aes(x = OR, y = Method)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper, color = Phenotype), height = 0.2, linewidth = 0.8) +
  geom_point(aes(color = Phenotype), size = 2.8) +
  facet_grid(Phenotype ~ ., scales = "free_y", space = "free_y") +
  scale_color_manual(values = c("COVID-19 susceptibility" = "#17becf",
                                "Hospitalized COVID-19" = "#bcbd22",
                                "Very Severe COVID-19" = "#e377c2")) +
  scale_x_continuous(breaks = seq(0.5, 2.5, by = 0.5)) +
  labs(x = "Odds Ratio (95% CI)", y = NULL) + shared_theme +
  theme(axis.text.y = element_blank(), legend.position = "none")

p_table <- ggplot(table_data, aes(y = Method)) +
  geom_text(aes(x = 0, label = OR_CI), hjust = 0.5) +
  geom_text(aes(x = 1.3, label = P), hjust = 0.5) +
  geom_text(aes(x = 2.1, label = SNPs), hjust = 0.5) +
  facet_grid(Phenotype ~ ., scales = "free_y", space = "free_y") +
  scale_x_continuous(breaks = c(0, 1.3, 2.1),
                     labels = c("OR (95% CI)", "P-value", "SNPs"),
                     limits = c(-0.6, 2.5), position = "top") +
  labs(x = NULL, y = NULL) + shared_theme +
  theme(axis.text.y = element_blank())

final_af <- p_pheno + p_method + p_forest + p_table +
  plot_layout(widths = c(1.4, 0.8, 1.2, 1.9)) +
  plot_annotation(title = "Mendelian Randomization: African Ancestry",
                  theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)))

ggsave("results/figures/Forest_African.png", final_af, width = 15, height = 10, dpi = 300)
print(final_af)

cat("All forest plots generated and saved successfully!\n")
