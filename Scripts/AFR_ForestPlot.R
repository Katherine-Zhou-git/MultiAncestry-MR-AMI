library(ggplot2)
library(dplyr)
library(patchwork)

# 1. Prepare African Data with Updated Values
data_af <- data.frame(
  Phenotype = factor(rep(c("COVID-19 Infection", "Hospitalized COVID-19", "Very Severe COVID-19"), each = 5),
                     levels = c("COVID-19 Infection", "Hospitalized COVID-19", "Very Severe COVID-19")),
  
  Method = factor(rep(c("MR Egger", "Weighted Median", "IVW Fixed Effects", "IVW Random Effects", "Weighted Mode"), 3),
                  levels = rev(c("IVW Fixed Effects", "IVW Random Effects", "Weighted Median", "Weighted Mode", "MR Egger"))),
  
  OR = c(
    # COVID-19 Infection (C2)
    0.51, 0.86, 1.09, 1.09, 0.48,
    # Hospitalized COVID-19 (B2)
    1.28, 0.78, 0.84, 0.84, 0.76,
    # Very Severe COVID-19 (A2)
    1.30, 0.86, 1.01, 1.01, 0.82
  ),
  
  CI_lower = c(
    # COVID-19 Infection (C2)
    0.22, 0.45, 0.71, 0.65, 0.09,
    # Hospitalized COVID-19 (B2)
    0.79, 0.55, 0.66, 0.66, 0.38,
    # Very Severe COVID-19 (A2)
    0.79, 0.66, 0.83, 0.81, 0.33
  ),
  
  CI_upper = c(
    # COVID-19 Infection (C2)
    1.18, 1.62, 1.66, 1.81, 2.55,
    # Hospitalized COVID-19 (B2)
    2.08, 1.11, 1.07, 1.06, 1.52,
    # Very Severe COVID-19 (A2)
    2.16, 1.13, 1.22, 1.26, 2.00
  ),
  
  p_val = c(
    # COVID-19 Infection (C2)
    0.128, 0.634, 0.696, 0.745, 0.394,
    # Hospitalized COVID-19 (B2)
    0.334, 0.163, 0.155, 0.148, 0.441,
    # Very Severe COVID-19 (A2)
    0.331, 0.270, 0.928, 0.939, 0.667
  ),
  
  SNPs = c(
    rep(27, 5),  # COVID-19 Infection (C2)
    rep(27, 5),  # Hospitalized COVID-19 (B2)
    rep(10, 5)   # Very Severe COVID-19 (A2)
  )
)

table_data <- data_af %>%
  mutate(
    OR_CI = sprintf("%.2f (%.2f-%.2f)", OR, CI_lower, CI_upper),
    P = sprintf("%.3f", p_val)
  )

# Calculate middle position for centered Phenotype labels
pheno_labels <- table_data %>%
  group_by(Phenotype) %>%
  summarize(MidPoint = levels(Method)[3])

shared_theme <- theme_minimal() +
  theme(
    text = element_text(color = "black", face = "plain"),
    axis.text = element_text(color = "black", face = "plain"),
    panel.grid = element_blank(),
    strip.text = element_blank(),
    plot.margin = margin(t = 10, r = 2, b = 10, l = 2)
  )

# 2. Column 1: Phenotype
p_pheno <- ggplot(table_data, aes(y = Method)) +
  geom_text(data = pheno_labels, aes(y = MidPoint, x = 0.25, label = Phenotype), hjust = 0) +
  facet_grid(Phenotype ~ ., scales = "free_y", space = "free_y") +
  scale_x_continuous(limits = c(0, 1), breaks = 0.25, labels = "Phenotype", position = "top") +
  labs(x = NULL, y = NULL) +
  shared_theme +
  theme(axis.text.x = element_text(face = "bold", size = 11),
        axis.text.y = element_blank(),
        plot.margin = margin(t = 10, r = 0, b = 10, l = 0))

# 3. Column 2: Method
p_method <- ggplot(table_data, aes(y = Method)) +
  geom_text(aes(x = 0, label = as.character(Method)), hjust = 0) +
  facet_grid(Phenotype ~ ., scales = "free_y", space = "free_y") +
  scale_x_continuous(limits = c(0, 1), breaks = 0, labels = "Method", position = "top") +
  labs(x = NULL, y = NULL) +
  shared_theme +
  theme(axis.text.x = element_text(face = "bold", size = 11),
        axis.text.y = element_blank(),
        plot.margin = margin(t = 10, r = 5, b = 10, l = 0))

# 4. Column 3: Forest Plot
p_forest <- ggplot(table_data, aes(x = OR, y = Method)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper, color = Phenotype), height = 0.2, linewidth = 0.8) +
  geom_point(aes(color = Phenotype), size = 2.5) +
  facet_grid(Phenotype ~ ., scales = "free_y", space = "free_y") +
  scale_color_manual(values = c("COVID-19 Infection" = "#4DAF4A",
                                "Hospitalized COVID-19" = "#4DAF4A",
                                "Very Severe COVID-19" = "#4DAF4A")) +
  scale_x_continuous(position = "bottom", breaks = seq(0.5, 2.5, by = 0.5)) +
  labs(x = "Odds Ratio (95% CI)", y = NULL) +
  shared_theme +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.ticks.x = element_line(color = "black"),
    axis.line.x = element_line(color = "black"),
    axis.title.x = element_text(face = "bold", size = 11, margin = margin(t = 10)),
    legend.position = "none"
  )

# 5. Column 4: Stats Table
p_table <- ggplot(table_data, aes(y = Method)) +
  geom_text(aes(x = 0, label = OR_CI), hjust = 0.5) +
  geom_text(aes(x = 1.3, label = P), hjust = 0.5) +
  geom_text(aes(x = 2.1, label = SNPs), hjust = 0.5) +
  facet_grid(Phenotype ~ ., scales = "free_y", space = "free_y") +
  scale_x_continuous(breaks = c(0, 1.3, 2.1),
                     labels = c("OR (95% CI)", "P-value", "SNPs"),
                     limits = c(-0.6, 2.5), position = "top") +
  labs(x = NULL, y = NULL) +
  shared_theme +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold", size = 11))

# 6. Final Assembly
final_figure <- p_pheno + p_method + p_forest + p_table +
  plot_layout(widths = c(1.4, 0.8, 1.2, 1.9)) +
  plot_annotation(
    title = "Mendelian Randomization: African Ancestry",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  )

print(final_figure)
