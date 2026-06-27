library(ggplot2)
library(dplyr)
library(patchwork)

data_ea <- data.frame(
  Phenotype = factor(rep(c("COVID-19 Infection", "Hospitalized COVID-19", "Very Severe COVID-19"), each = 5),
                     levels = c("COVID-19 Infection", "Hospitalized COVID-19", "Very Severe COVID-19")),
  
  Method = factor(rep(c("MR Egger", "Weighted Median", "IVW Fixed Effects", "IVW Random Effects", "Weighted Mode"), 3),
                  levels = rev(c("IVW Fixed Effects", "IVW Random Effects", "Weighted Median", "Weighted Mode", "MR Egger"))),
  
  OR = c(
    # COVID-19 Infection (C2) — 7 SNPs
    0.8569, 0.9766, 0.9689, 0.9689, 1.0221,
    # Hospitalized COVID-19 (B2) — 9 SNPs
    1.0911, 0.9967, 0.9906, 0.9906, 1.0229,
    # Very Severe COVID-19 (A2) — 9 SNPs
    0.9758, 0.9885, 0.9804, 0.9804, 0.9905
  ),
  
  CI_lower = c(
    # COVID-19 Infection (C2)
    0.6958, 0.8932, 0.9070, 0.9029, 0.8784,
    # Hospitalized COVID-19 (B2)
    0.9727, 0.9277, 0.9426, 0.9368, 0.9011,
    # Very Severe COVID-19 (A2)
    0.9159, 0.9525, 0.9524, 0.9601, 0.9331
  ),
  
  CI_upper = c(
    # COVID-19 Infection (C2)
    1.0551, 1.0678, 1.0350, 1.0398, 1.1893,
    # Hospitalized COVID-19 (B2)
    1.2239, 1.0709, 1.0411, 1.0476, 1.1610,
    # Very Severe COVID-19 (A2)
    1.0398, 1.0258, 1.0093, 1.0012, 1.0513
  ),
  
  p_val = c(
    # COVID-19 Infection (C2)
    0.2055, 0.6039, 0.3486, 0.3805, 0.7867,
    # Hospitalized COVID-19 (B2)
    0.1805, 0.9288, 0.7108, 0.7416, 0.7358,
    # Very Severe COVID-19 (A2)
    0.4747, 0.5395, 0.1823, 0.0647, 0.7609
  ),
  
  SNPs = c(
    rep(7, 5),   # COVID-19 Infection
    rep(9, 5),   # Hospitalized COVID-19
    rep(9, 5)    # Very Severe COVID-19
  )
)

table_data <- data_ea %>%
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

# Column 1: Phenotype
p_pheno <- ggplot(table_data, aes(y = Method)) +
  geom_text(data = pheno_labels, aes(y = MidPoint, x = 0.25, label = Phenotype), hjust = 0) +
  facet_grid(Phenotype ~ ., scales = "free_y", space = "free_y") +
  scale_x_continuous(limits = c(0, 1), breaks = 0.25, labels = "Phenotype", position = "top") +
  labs(x = NULL, y = NULL) +
  shared_theme +
  theme(axis.text.x = element_text(face = "bold", size = 11),
        axis.text.y = element_blank(),
        plot.margin = margin(t = 10, r = 0, b = 10, l = 0))

# Column 2: Method
p_method <- ggplot(table_data, aes(y = Method)) +
  geom_text(aes(x = 0, label = as.character(Method)), hjust = 0) +
  facet_grid(Phenotype ~ ., scales = "free_y", space = "free_y") +
  scale_x_continuous(limits = c(0, 1), breaks = 0, labels = "Method", position = "top") +
  labs(x = NULL, y = NULL) +
  shared_theme +
  theme(axis.text.x = element_text(face = "bold", size = 11),
        axis.text.y = element_blank(),
        plot.margin = margin(t = 10, r = 5, b = 10, l = 0))

# Column 3: Forest Plot
p_forest <- ggplot(table_data, aes(x = OR, y = Method)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper, color = Phenotype), height = 0.2, linewidth = 0.8) +
  geom_point(aes(color = Phenotype), size = 2.5) +
  facet_grid(Phenotype ~ ., scales = "free_y", space = "free_y") +
  scale_color_manual(values = c("COVID-19 Infection" = "blue",
                                "Hospitalized COVID-19" = "blue",
                                "Very Severe COVID-19" = "blue")) +
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

# Column 4: Stats Table
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

# Final Assembly
final_figure <- p_pheno + p_method + p_forest + p_table +
  plot_layout(widths = c(1.3, 0.8, 1.2, 1.8)) +
  plot_annotation(
    title = "Mendelian Randomization: East Asian Ancestry",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  )

print(final_figure)

ggsave("./EAS_ForestPlot_Figure3.png", final_figure, width = 14, height = 8, dpi = 300)
