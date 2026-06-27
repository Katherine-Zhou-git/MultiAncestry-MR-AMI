library(ggplot2)
library(dplyr)
library(patchwork)

data_sa <- data.frame(
  Phenotype = factor(rep(c("COVID-19 Infection", "Hospitalized COVID-19", "Very Severe COVID-19"), each = 5),
                     levels = c("COVID-19 Infection", "Hospitalized COVID-19", "Very Severe COVID-19")),
  
  Method = factor(rep(c("MR Egger", "Weighted Median", "IVW Fixed Effects", "IVW Random Effects", "Weighted Mode"), 3),
                  levels = rev(c("IVW Fixed Effects", "IVW Random Effects", "Weighted Median", "Weighted Mode", "MR Egger"))),
  
  OR = c(
    # COVID-19 Infection (C2)
    0.9447, 0.9270, 0.9278, 0.9278, 0.9450,
    # Hospitalized COVID-19 (B2)
    0.9274, 0.9176, 0.9170, 0.9170, 1.0241,
    # Very Severe COVID-19 (A2)
    1.1214, 1.0301, 1.0248, 1.0248, 1.0370
  ),
  
  CI_lower = c(
    # COVID-19 Infection (C2)
    0.4481, 0.6201, 0.6804, 0.7461, 0.3985,
    # Hospitalized COVID-19 (B2)
    0.6941, 0.7916, 0.8207, 0.8510, 0.7665,
    # Very Severe COVID-19 (A2)
    0.9127, 0.9096, 0.9382, 0.9696, 0.7761
  ),
  
  CI_upper = c(
    # COVID-19 Infection (C2)
    1.9918, 1.3858, 1.2652, 1.1539, 2.2408,
    # Hospitalized COVID-19 (B2)
    1.2390, 1.0637, 1.0245, 0.9881, 1.3683,
    # Very Severe COVID-19 (A2)
    1.3779, 1.1666, 1.1193, 1.0831, 1.3857
  ),
  
  p_val = c(
    # COVID-19 Infection (C2)
    0.883, 0.712, 0.636, 0.501, 0.900,
    # Hospitalized COVID-19 (B2)
    0.617, 0.254, 0.126, 0.023, 0.874,
    # Very Severe COVID-19 (A2)
    0.291, 0.640, 0.587, 0.386, 0.809
  ),
  
  SNPs = c(
    rep(15, 5),  # COVID-19 Infection
    rep(18, 5),  # Hospitalized COVID-19
    rep(19, 5)   # Very Severe COVID-19
  )
)

table_data <- data_sa %>%
  mutate(
    OR_CI = sprintf("%.2f (%.2f-%.2f)", OR, CI_lower, CI_upper),
    P = sprintf("%.3f", p_val)
  )

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
  scale_color_manual(values = c("COVID-19 Infection" = 'darkorchid1',
                                "Hospitalized COVID-19" = 'darkorchid1',
                                "Very Severe COVID-19" = "darkorchid1")) +
  scale_x_continuous(position = "bottom", breaks = seq(0.5, 2.0, by = 0.5)) +
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
  plot_layout(widths = c(1.4, 0.8, 1.2, 1.9)) +
  plot_annotation(
    title = "Mendelian Randomization: South Asian Ancestry",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  )

print(final_figure)
