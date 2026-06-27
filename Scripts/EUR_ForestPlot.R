# Forest plot for the Figures


library(ggplot2)
library(dplyr)
library(patchwork)

# 1. Prepare European Data with Refined Names and Ordering
data_euro <- data.frame(
  Phenotype = factor(rep(c("COVID-19 susceptibility", "Hospitalized COVID-19", "Very Severe COVID-19"), each = 5),
                     levels = c("COVID-19 susceptibility", "Hospitalized COVID-19", "Very Severe COVID-19")),
  
  Method = factor(rep(c("MR Egger", "Weighted Median", "IVW (Fixed)", "IVW (Random)", "Weighted Mode"), 3),
                  levels = rev(c("IVW (Fixed)", "IVW (Random)", "Weighted Median", "Weighted Mode", "MR Egger"))),
  
  OR = c(
    # Infection
    0.98, 0.95, 0.92, 0.92, 0.94,
    # Hospitalized
    0.99, 0.97, 0.94, 0.94, 0.97,
    # Very Severe
    1.01, 0.98, 0.95, 0.95, 0.98
  ),
  
  CI_lower = c(
    # Infection
    0.79, 0.81, 0.82, 0.84, 0.80,
    # Hospitalized
    0.89, 0.91, 0.90, 0.89, 0.92,
    # Very Severe
    0.94, 0.94, 0.92, 0.91, 0.95
  ),
  
  CI_upper = c(
    # Infection
    1.20, 1.11, 1.04, 1.01, 1.11,
    # Hospitalized
    1.09, 1.03, 0.98, 0.99, 1.03,
    # Very Severe
    1.08, 1.02, 0.98, 0.99, 1.03
  ),
  
  p_val = c(
    # Infection
    0.813, 0.474, 0.182, 0.086, 0.474,
    # Hospitalized
    0.786, 0.302, 0.005, 0.029, 0.370,
    # Very Severe
    0.872, 0.333, 0.00047, 0.008, 0.474
  ),
  
  SNPs = c(rep(13, 5), rep(28, 5), rep(24, 5))
)

table_data <- data_euro %>%
  mutate(
    OR_CI = sprintf("%.2f (%.2f-%.2f)", OR, CI_lower, CI_upper),
    P = sprintf("%.3f", p_val)
  )

# Calculate midpoint for centered Phenotype labels
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

# 2. Column 1: Phenotype (Moved x to 0.25, Left Aligned)
p_pheno <- ggplot(table_data, aes(y = Method)) +
  geom_text(data = pheno_labels, aes(y = MidPoint, x = 0.25, label = Phenotype), hjust = 0) +
  facet_grid(Phenotype ~ ., scales = "free_y", space = "free_y") +
  scale_x_continuous(limits = c(0, 1), breaks = 0.25, labels = "Phenotype", position = "top") +
  labs(x = NULL, y = NULL) +
  shared_theme +
  theme(axis.text.x = element_text(face = "bold", size = 11), 
        axis.text.y = element_blank(),
        plot.margin = margin(t = 10, r = 0, b = 10, l = 0))

# 3. Column 2: Method (Left Aligned)
p_method <- ggplot(table_data, aes(y = Method)) +
  geom_text(aes(x = 0, label = as.character(Method)), hjust = 0) +
  facet_grid(Phenotype ~ ., scales = "free_y", space = "free_y") +
  scale_x_continuous(limits = c(0, 1), breaks = 0, labels = "Method", position = "top") +
  labs(x = NULL, y = NULL) +
  shared_theme +
  theme(axis.text.x = element_text(face = "bold", size = 11), 
        axis.text.y = element_blank(),
        plot.margin = margin(t = 10, r = 5, b = 10, l = 0))

# 4. Column 3: Forest Plot (Scale enabled with ticks)
p_forest <- ggplot(table_data, aes(x = OR, y = Method)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "#FE9929") +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper, color = Phenotype), height = 0.2, linewidth = 0.8) +
  geom_point(aes(color = Phenotype), size = 2.5) +
  facet_grid(Phenotype ~ ., scales = "free_y", space = "free_y") +
  # scale_color_manual(values = c("COVID-19 Infection" = "#4DAF4A", 
  #                               "Hospitalized COVID-19" = "#377EB8", 
  #                               "Very Severe COVID-19" = "#E41A1C")) +
  scale_shape_manual(values = c(
    "COVID-19 susceptibility"     = 16,  # ● Filled circle
    "Hospitalized COVID-19" = 15,  # ■ Filled square
    "Very Severe COVID-19"  = 18   # ♦ Filled diamond
  ))+
  scale_color_manual(values = c("COVID-19 susceptibility" = "#FE9929", 
                                "Hospitalized COVID-19" = "#FE9929", 
                                "Very Severe COVID-19" = "#FE9929")) +
  
  
  scale_x_continuous(position = "bottom", breaks = seq(0.8, 1.2, by = 0.1)) + 
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

# 5. Column 4: Stats Table (Header size 11, increased spacing for OR-P)
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
    title = "Mendelian Randomization: European Ancestry",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  )

print(final_figure)
