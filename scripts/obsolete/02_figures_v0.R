#------------------------------
# Biol 406 Project
# By: Elias, Rebecca, Sara, Sahar 
# 2024-03-13
# Description: Making figures
#------------------------------
# Function to get p-value
get_p <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

# Histogram of Canopy Cover ####
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

# Histogram of Leaf Area ####
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

# Histogram of Plant Height ####
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

# Histograms of Slope and Aspect
plant.data$aspect <- as.numeric(plant.data$aspect)
aspect.hist <- ggplot(plant.data, aes(x = aspect)) +
  geom_histogram(binwidth = 15, fill = "darkgreen", color = "black", alpha = 0.6) +
  labs(title = "Histogram of Aspects of \nPlant Samples",
       x = "Aspect (Degrees)",
       y = "Frequency") +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_line(color = "grey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        legend.position = "none") + 
  scale_x_continuous(breaks = c(-180, -90, 0, 90, 180)) + 
  coord_polar(start = 0 - pi/24)

ggsave("aspect.hist.png", aspect.hist, path = "figures/", width = 1500, height = 1500, units = "px")

slope.hist <- ggplot(plant.data, aes(x = slope)) +
  geom_histogram(binwidth = 2, fill = "darkgreen", color = "black", alpha = 0.6) +
  labs(title = "Histogram of Slopes of Plant Samples",
       x = "Slope (Degrees)",
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

ggsave("slope.hist.png", slope.hist, path = "figures/", width = 2000, height = 1000, units = "px")

# Linear Model Plot ####
############ Trying to work with GGeffect
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

# Create plot with outlier ####
# Extract coefficients and R-squared from simple model
coef <- round(coef(area.cover.lm2), 4)
rsq <- round(summary(area.cover.lm2)$r.squared, 3)
p <- round(get_p(area.cover.lm2), 3)

confidence_interval <- predict(area.cover.lm2, interval = "confidence", level = 0.95)

# Extract upper and lower confidence bounds
upper_bound <- confidence_interval[, "upr"]
lower_bound <- confidence_interval[, "lwr"]

# Create a data frame for the confidence intervals
confidence_df <- data.frame(plant.data, upper_bound, lower_bound)

# creatingplot
scatterplot <- ggplot(plant.data, aes(x = avg.canopy.cover, y = avg.leaf.area)) +
  geom_point(color = "darkgreen", size = 3, alpha = 0.6) +
  labs(title = "Scatterplot Comparing Leaf Area to \nCanopy Cover with a Simple Linear Trendline",
       x = "Canopy Cover (%)",
       y = "Leaf Area (cm^2)") +
  annotate("text", x = 80, y = 38, label = paste("y =", coef[2], "x +", coef[1])) +
  annotate("text", x = 80, y = 34, label = paste("R² =", rsq)) +
  annotate("text", x = 80, y = 30, label = paste("p-value =", p)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_line(),
        panel.grid.minor = element_line(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

canopy.leaf.scatterplot <- scatterplot + 
  geom_abline(intercept = coef(area.cover.lm2)[1], slope = coef(area.cover.lm2)[2], color = "red") +

canopy.leaf.scatterplot


# Create plot without outlier ####
coef1 <- round(coef(area.cover.lm3), 4)
rsq1 <- round(summary(area.cover.lm3)$r.squared, 3)
p1 <- round(get_p(area.cover.lm3), 3)

coef2 <- round(coef(area.cover.lm4), 4)
rsq2 <- round(summary(area.cover.lm4)$r.squared, 3)
p2 <- round(get_p(area.cover.lm4), 3)


scatterplot2 <- ggplot(plant.data.no, aes(x = avg.canopy.cover, y = avg.leaf.area)) +
  geom_point(color = "darkgreen", size = 3, alpha = 0.6) +
  #  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") + ### NEED TO ADD OTHER PREDICTORS (FIXED EFFECTS)
  labs(title = "Scatterplot Comparing Leaf Area to \nCanopy Cover with a Simple Linear Trendline\nExcluding outlier point", 
       x = "Canopy Cover (%)",
       y = "Leaf Area (cm^2)") +
  annotate("text", color = "red", x = 70, y = 26, label = "avg.leaf.area ~ avg.canopy.cover + height + plot") +
  annotate("text", color = "red", x = 70, y = 22, label = paste("y =", coef1[2], "x +", coef1[1])) +
  annotate("text", color = "red", x = 70, y = 18, label = paste("R² =", rsq1)) +
  annotate("text", color = "red", x = 70, y = 14, label = paste("p-value =", p1)) +
  ## Above mixed model, below simple
  annotate("text", color = "blue", x = 38, y = 64, label = "avg.leaf.area ~ avg.canopy.cover") +
  annotate("text", color = "blue", x = 38, y = 60, label = paste("y =", coef2[2], "x +", coef2[1])) +
  annotate("text", color = "blue", x = 38, y = 56, label = paste("R² =", rsq2)) +
  annotate("text", color = "blue", x = 38, y = 52, label = paste("p-value =", p2)) +
  theme_minimal() +
  scale_x_continuous(limits=c(25, 90)) +
  scale_y_continuous(limits=c(0, 80)) +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_line(),
        panel.grid.minor = element_line(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

canopy.leaf.scatterplot2 <- scatterplot2 +
  geom_abline(intercept = coef(area.cover.lm3)[1], slope = coef(area.cover.lm3)[2], color = "red") + 
  geom_abline(intercept = coef(area.cover.lm4)[1], slope = coef(area.cover.lm4)[2], color = "blue")

# Creating a scatterplot with no line but grouped by plot

plot.group.scatter <- ggplot(plant.data, aes(x = avg.canopy.cover, y = avg.leaf.area, color = plot_id)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(title = "Scatterplot Comparing Leaf Area to \nCanopy Cover with Grouping by Plot",
       x = "Canopy Cover (%)",
       y = "Leaf Area (cm^2)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_line(),
        panel.grid.minor = element_line(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")) +
  scale_color_discrete(name = "Plot ID", labels = c("Plot 1", "Plot 2", "Plot 3", "Plot 4", "Plot 5", "Plot 6", "Plot 7"))


############## Thinking about making a grouped scatterplot based on whether there is some deciduous or not 
# plot.group.scatter <- ggplot(plant.data, aes(x = avg.canopy.cover, y = avg.leaf.area, color = decid.y_n)) +
#   geom_point(size = 3, alpha = 0.8) +
#   labs(title = "Scatterplot Comparing Leaf Area to \nCanopy Cover with Grouping by Plot",
#        x = "Canopy Cover (%)",
#        y = "Leaf Area (cm^2)") +
#   theme_minimal() +
#   theme(plot.title = element_text(size = 20, face = "bold"),
#         axis.title = element_text(size = 14),
#         axis.text = element_text(size = 12),
#         axis.line = element_line(color = "black"),
#         panel.grid.major = element_line(),
#         panel.grid.minor = element_line(),
#         panel.background = element_rect(fill = "white"),
#         plot.background = element_rect(fill = "white")) +
#   scale_color_discrete(name = "Plot ID", labels = c("Plot 1", "Plot 2", "Plot 3", "Plot 4", "Plot 5", "Plot 6", "Plot 7"))

# Saving charts ####
ggsave("canopy.hist.png", canopy.hist, path = "figures/", width = 2000, height = 1000, units = "px")
ggsave("height.hist.png", height.hist, path = "figures/", width = 2000, height = 1000, units = "px")
ggsave("leaf.hist.png", leaf.hist, path = "figures/", width = 2000, height = 1000, units = "px")
ggsave("canopy.leaf.scatterplot.png", canopy.leaf.scatterplot, path = "figures/", width = 2000, height = 1000, units = "px")
ggsave("canopy.leaf.scatterplot.no.png", canopy.leaf.scatterplot2, path = "figures/", width = 2300, height = 1500, units = "px")
ggsave("plot.group.scatter.png", plot.group.scatter, path = "figures/", width = 2300, height = 1500, units = "px")


