#------------------------------
# Biol 406 Project
# By: Elias, Rebecca, Sara, Sahar 
# 2024-03-13
# Description: Making ggeffects figures
#------------------------------
# Plotting Light Availability Metrics ####
# With outlier
summary(leaf.by.light.lm) # lmer(avg.leaf.area ~ SRI.mediated.normalized + (1|plot), data = plant.data.no)
lm.light.prediction <- as.data.frame(ggpredict(leaf.by.light.lm, terms = "SRI.mediated.normalized"))

leaf.light.plot <-
  ggplot(lm.light.prediction) +
  geom_point(data = plant.data.no, aes(x = SRI.mediated.normalized, y = avg.leaf.area, color = plot_id), size = 2.3, alpha = 0.8) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(aes(x = x, y = predicted), color = "red") +
  labs(x = "Light Availability (Unitless, Normalized 0-1)",
       y = "Leaf Area (cm^2)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_line(),
        panel.grid.minor = element_line(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))
leaf.light.plot

# Plotting Linear Relationship w/ outlier ####
area.cover.lm
summary(area.cover.lm)
lm.prediction <- as.data.frame(ggpredict(area.cover.lm, terms = "avg.canopy.cover"))

area.cover.plot.outlier <-
  ggplot(lm.prediction) +
  geom_point(data = plant.data, aes(x = avg.canopy.cover, y = avg.leaf.area, color = plot_id), size = 2, alpha = 0.6) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(aes(x = x, y = predicted), color = "red") +
  labs(
       #title = "Scatterplot Comparing Leaf Area to \nCanopy Cover with a Simple Linear Trendline", 
       x = "Canopy Cover (%)",
       y = "Leaf Area (cm^2)") +
  theme_minimal() +
  scale_x_continuous(limits=c(45, 90)) +
  scale_y_continuous(limits=c(15, 85)) +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_line(),
        panel.grid.minor = element_line(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# Plotting Linear Relationship w/o outlier ####
area.cover.lm.no
summary(area.cover.lm.no)
lm.no.prediction <- as.data.frame(ggpredict(area.cover.lm.no, terms = "avg.canopy.cover"))

area.cover.plot <-
  ggplot(lm.no.prediction) +
  geom_point(data = plant.data.no, aes(x = avg.canopy.cover, y = avg.leaf.area, color = plot_id), size = 2, alpha = 0.6) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(aes(x = x, y = predicted), color = "red") +
  labs(x = "Canopy Cover (%)",
       y = "Leaf Area (cm^2)") +
  theme_minimal() +
  scale_x_continuous(limits=c(45, 85)) +
  scale_y_continuous(limits=c(20, 90)) +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_line(),
        panel.grid.minor = element_line(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

## Plotting leaf area as a function of height
height.lm <- lm(avg.leaf.area ~ height, data = plant.data)
summary(height.lm)
lm.h.prediction <- as.data.frame(ggpredict(height.lm, terms = "height"))

height.plot <-
  ggplot(lm.h.prediction) +
  geom_point(data = plant.data, aes(x = height, y = avg.leaf.area), size = 2, alpha = 0.6) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(aes(x = x, y = predicted), color = "red") +
  labs(x = "Plant Height (cm)",
       y = "Leaf Area (cm^2)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_line(),
        panel.grid.minor = element_line(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

## Plotting leaf area as a function of light availability
leaf.by.light.lm <- lm(avg.leaf.area ~ SRI.mediated.normalized + plot, data = plant.data.no)
summary(leaf.by.light.lm)

lm.light.prediction <- as.data.frame(ggpredict(leaf.by.light.lm, terms = "SRI.mediated.normalized"))

new.light.plot <-
  ggplot(lm.light.prediction) +
  geom_point(
    data = plant.data.no,
    aes(x = SRI.mediated.normalized, y = avg.leaf.area),
    size = 2,
    alpha = 0.6
  ) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(aes(x = x, y = predicted), color = "red") +
  labs(x = "Light Availability (No units, out of 1)",
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

new.light.plot









# Saving plots
ggsave("CanopyArea.scatterplot.outlier.png", area.cover.plot.outlier, path = "figures/", width = 2000, height = 1000, units = "px")
ggsave("CanopyArea.scatterplot.1.png", area.cover.plot, path = "figures/", width = 2000, height = 1000, units = "px")
ggsave("height.scatterplot.png", height.plot, path = "figures/", width = 2000, height = 1000, units = "px") # This plot currently includes the outlier
ggsave("light.scatterplot.png", light.plot, path = "figures/", width = 2000, height = 1000, units = "px") # This plot currently includes the outlier, and also is very close to significant 
