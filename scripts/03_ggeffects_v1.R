#------------------------------
# Biol 406 Project
# By: Elias, Rebecca, Sara, Sahar
# 2024-03-13
# Description: Making ggeffects figures
#------------------------------
# Plotting Light Availability Metrics ####
# With outlier
summary(leaf.by.light.lm) # lmer(avg.leaf.area ~ SRI.mediated.normalized + (1|plot), data = plant.data.no)
lm.light.prediction <-
  as.data.frame(ggpredict(leaf.by.light.lm, terms = "SRI.mediated.normalized"))

leaf.light.plot <-
  ggplot(lm.light.prediction) +
  geom_point(
    data = plant.data.no,
    aes(x = SRI.mediated.normalized, y = avg.leaf.area),
    size = 2.3,
    alpha = 0.8
  ) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(aes(x = x, y = predicted), color = "red") +
  labs(x = "Light Availability (Unitless, Normalized 0-1)",
       y = "Leaf Area (cm^2)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_line(),
    panel.grid.minor = element_line(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )
leaf.light.plot

# Plotting Relationship w/ Outlier ####
plant.data$annotation_text <- ifelse(plant.data$plant.id == "12-1", "1", 
                                     ifelse(plant.data$plant.id == "12-2", "2", NA))
leaf.light.plot.outlier <-
  ggplot(plant.data,
         aes(x = SRI.mediated.normalized, y = avg.leaf.area, color = outlier_points)) +
  geom_point(size = 2, alpha = 0.6) +
  scale_color_manual(values = c("black", "red")) +
  geom_text(
    data = subset(plant.data, outlier_points),
    aes(label = annotation_text),
    hjust = -0.5,
    vjust = -0.5,
    color = "red"
  ) +
  labs(x = "Light Availability (Unitless, Normalized 0-1)", 
       y = "Leaf Area (cm^2)") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_line(),
    panel.grid.minor = element_line(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )
leaf.light.plot.outlier

## Plotting leaf area as a function of height
height.lm <- lm(avg.leaf.area ~ height, data = plant.data)
summary(height.lm)
lm.h.prediction <-
  as.data.frame(ggpredict(height.lm, terms = "height"))

height.plot <-
  ggplot(lm.h.prediction) +
  geom_point(
    data = plant.data,
    aes(x = height, y = avg.leaf.area),
    size = 2,
    alpha = 0.6
  ) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(aes(x = x, y = predicted), color = "red") +
  labs(x = "Plant Height (cm)",
       y = "Leaf Area (cm^2)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_line(),
    panel.grid.minor = element_line(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )
height.plot

## Plotting Leaf Area based on Light - with plot grouping
plot.id.plot <-
  ggplot(aes(x = SRI.mediated.normalized, y = avg.leaf.area, color = plot_id), 
       data = plant.data.no) +
  geom_point(
    size = 2.5,
    alpha = 1
  ) +
  labs(x = "Light Availability (Unitless, Normalized 0-1)",
       y = "Leaf Area (cm^2)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_line(),
    panel.grid.minor = element_line(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

# Saving plots ####
ggsave(
  "AvailableLight.scatterplot.png",
  leaf.light.plot,
  path = "figures/",
  width = 2000,
  height = 1000,
  units = "px"
)
ggsave(
  "WithOutlier.scatterplot.png",
  leaf.light.plot.outlier,
  path = "figures/",
  width = 2000,
  height = 1000,
  units = "px"
)
ggsave(
  "height.scatterplot.png",
  height.plot,
  path = "figures/",
  width = 2000,
  height = 1000,
  units = "px"
)
ggsave(
  "plot.scatterplot.png",
  plot.id.plot,
  path = "figures/",
  width = 2000,
  height = 1000,
  units = "px"
)