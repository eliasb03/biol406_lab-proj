#------------------------------
# Biol 406 Project
# By: Elias, Rebecca, Sara, Sahar 
# 2024-03-13
# Description: Making figures
#------------------------------

# Histogram of Canopy Cover
canopy.hist <- ggplot(plant.data, aes(x = avg.canopy.cover)) +
  geom_histogram(binwidth = 2.5, fill = "darkgreen", color = "black", alpha = 0.6) +
  labs(title = "Histogram of Canopy Cover Samples",
       x = "Canopy Cover (%)",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        legend.position = "none")

# Histogram of Leaf Area
leaf.hist <- ggplot(plant.data, aes(x = avg.leaf.area)) +
  geom_histogram(binwidth = 5, fill = "darkgreen", color = "black", alpha = .6) +
  labs(title = "Histogram of Leaf Area Samples",
       x = "Leaf Area (cm^2)",y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        legend.position = "none")

# Histogram of Plant Height
height.hist <- ggplot(plant.data, aes(x = height)) +
  geom_histogram(binwidth = 7, fill = "darkgreen", color = "black", alpha = 0.6) +
  labs(title = "Histogram of Plant Height Samples",
       x = "Plant Height (cm)",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        legend.position = "none")

# Linear Model Plot
############ Trying to work with GGeffect
# 
# 
# # Fit the regression model
# model <- lm(avg.leaf.area ~ avg.canopy.cover + height + plot, data = plant.data)
# 
# # Calculate marginal effects
# marginal <- ggpredict(model, c("avg.canopy.cover", "height"), by = "plot")
# 
# # Plot the marginal effects
# plot(marginal)
# 
# # Calculate predicted values using ggeffect()
# eff <- ggpredict(area.cover.lm2, c("avg.canopy.cover", "height"))
# 
# # Extract predicted values and predictor values
# predicted <- as.data.frame(eff$predicted)
# predictors <- as.data.frame(eff$predicted)
# 
# # Create scatterplot
# scatterplot <- ggplot(plant.data, aes(x = avg.canopy.cover, y = avg.leaf.area)) +
#   geom_point() +
#   theme_minimal()
# 
# # Add custom trendline
# custom_trendline <- scatterplot +
#   geom_line(data = predicted, aes(x = x, y = predicted)) +
#   geom_line(data = predictors, aes(x = x, y = predicted), color = "red")
# 
# # Show the plot
# custom_trendline

##### 
# Extract coefficients and R-squared from simple model
coef <- round(coef(area.cover.lm2), 4)
rsq <- round(summary(area.cover.lm2)$r.squared, 3)

# Create plot
scatterplot <- ggplot(plant.data, aes(x = avg.canopy.cover, y = avg.leaf.area)) +
  geom_point(color = "darkgreen", size = 3, alpha = 0.6) +
#  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") + ### NEED TO ADD OTHER PREDICTORS (FIXED EFFECTS)
  labs(title = "Scatterplot Comparing Leaf Area to \nCanopy Cover with a Simple Linear Trendline",
       x = "Canopy Cover (%)",
       y = "Leaf Area (cm^2)") +
  annotate("text", x = 80, y = 40, label = paste("y =", coef[2], "x +", coef[1])) +
  annotate("text", x = 80, y = 37, label = paste("R² =", rsq)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_line(),
        panel.grid.minor = element_line(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

canopy.leaf.scatterplot <- scatterplot + geom_abline(intercept = coef(area.cover.lm2)[1], slope = coef(area.cover.lm2)[2], color = "red")

canopy.leaf.scatterplot

ggsave("canopy.hist.png", canopy.hist, path = "figures/", width = 2000, height = 1000, units = "px")
ggsave("height.hist.png", height.hist, path = "figures/", width = 2000, height = 1000, units = "px")
ggsave("leaf.hist.png", leaf.hist, path = "figures/", width = 2000, height = 1000, units = "px")
ggsave("canopy.leaf.scatterplot.png", canopy.leaf.scatterplot, path = "figures/", width = 2000, height = 1000, units = "px")


