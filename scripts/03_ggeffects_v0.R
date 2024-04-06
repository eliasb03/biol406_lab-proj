#------------------------------
# Biol 406 Project
# By: Elias, Rebecca, Sara, Sahar 
# 2024-03-13
# Description: Making ggeffects figures
#------------------------------
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
  labs(
       #title = "Scatterplot Comparing Leaf Area to \nCanopy Cover with a Simple Linear Trendline\nExcluding outlier point", 
       x = "Canopy Cover (%)",
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

# Saving plots
ggsave("CanopyArea.scatterplot.outlier.png", area.cover.plot.outlier, path = "figures/", width = 2000, height = 1000, units = "px")
ggsave("CanopyArea.scatterplot.png", area.cover.plot, path = "figures/", width = 2300, height = 1500, units = "px")
