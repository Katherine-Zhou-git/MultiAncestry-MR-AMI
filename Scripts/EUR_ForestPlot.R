library(ggplot2)
library(dplyr)
library(patchwork)

data_euro <- data.frame(
  Phenotype = factor(rep(c("COVID-19 Infection", "Hospitalized COVID-19", "Very Severe COVID-19"), each = 5),
                     levels = c("COVID-19 Infection", "Hospitalized COVID-19", "Very Severe COVID-19")),
  
  Method = factor(rep(c("MR Egger", "Weighted Median", "IVW (Fixed)", "IVW (Random)", "Weighted Mode"), 3),
                  levels = rev(c("IVW (Fixed)", "IVW (Random)", "Weighted Median", "Weighted Mode", "MR Egger"))),
  
  OR = c(
    # C2 - COVID-19 Infection
    0.9750433594, 0.946078866, 0.9217779187, 0.9217779187, 0.940398275,
    # B2 - Hospitalized
    0.9861703958, 0.9681659895, 0.9417101058, 0.9417101058, 0.972723204,
    # A2 - Very Severe
    1.005610474, 0.9792187756, 0.9490921311, 0.9490921311, 0.9848935296
  ),
  
  CI_lower = c(
    # C2
    0.7946704998, 0.8086267355, 0.8179150207, 0.8399098997, 0.6091717621,
    # B2
    0.8927291178, 0.9063092309, 0.9027239012, 0.8924227957, 0.6829382892,
    # A2
    0.940366061, 0.9384658423, 0.9216872312, 0.9130532192, 0.7477392398
  ),
  
  CI_upper = c(
    # C2
    1.196356922, 1.106895409, 1.038829842, 1.011625809, 1.451723423,
    # B2
    1.089392101, 1.034244551, 0.9823800191, 0.9937194878, 1.385469883,
    # A2
    1.07538167, 1.021741407, 0.977311872, 0.9865535266, 1.29726409
  ),
  
  p_val = c(
    # C2
    0.8131085418, 0.4889154626, 0.1817391725, 0.08608489018, 0.786198225,
    # B2
    0.7861000248, 0.3368469779, 0.005367849609, 0.02854528564, 0.8793375863,
    # A2
    0.8716397588, 0.3329030803, 0.0004737258267, 0.008158891714, 0.9146964133
  ),
  
  SNPs = c(rep(13, 5), rep(28, 5), rep(24, 5))
)

table_data <- data_euro %>%
  mutate(
    OR_CI = sprintf("%.2f (%.2f-%.2f)", OR, CI_lower, CI_upper),
    P = ifelse(p_val < 0.001,
               sprintf("%.2e", p_val),
               sprintf("%.3f", p_val))
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
  geom_vline(xintercept = 1, linetype = "dashed", color = "#FE9929") +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper, color = Phenotype), height = 0.2, linewidth = 0.8) +
  geom_point(aes(color = Phenotype), size = 2.5) +
  facet_grid(Phenotype ~ ., scales = "free_y", space = "free_y") +
  scale_shape_manual(values = c(
    "COVID-19 Infection" = 16,
    "Hospitalized COVID-19"   = 15,
    "Very Severe COVID-19"    = 18
  )) +
  scale_color_manual(values = c(
    "COVID-19 Infection" = "#FE9929",
    "Hospitalized COVID-19"   = "#FE9929",
    "Very Severe COVID-19"    = "#FE9929"
  )) +
  scale_x_continuous(position = "bottom", breaks = seq(0.6, 1.5, by = 0.1)) +
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
    title = "Mendelian Randomization: European Ancestry",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  )

print(final_figure)
