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
canopy.hist <- 
  ggplot(plant.data.no, aes(x = avg.canopy.cover)) +
  geom_histogram(binwidth = 2.5, fill = "darkgreen", color = "black", alpha = 0.6) +
  labs(#title = "Histogram of Canopy Cover Samples",
       x = "Canopy Cover (%)",
       y = "Frequency") +
  geom_vline(aes(xintercept = mean(avg.canopy.cover), color = "Mean"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(avg.canopy.cover), color = "Median"), linetype = "dotted", size = 1) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        legend.position = c(0.9, 0.9),
        legend.justification = c("right", "top")) +
  scale_color_manual(values = c("Mean" = "red", "Median" = "blue"),
                     labels = c(paste0("Mean (", round(mean(plant.data.no$avg.canopy.cover), 2), ")"), 
                                paste0("Median (", round(median(plant.data.no$avg.canopy.cover), 2), ")")))


# Histogram of Leaf Area ####
leaf.hist <- 
  ggplot(plant.data.no, aes(x = avg.leaf.area)) +
  geom_histogram(binwidth = 4, fill = "darkgreen", color = "black", alpha = .6) +
  labs(#title = "Histogram of Leaf Area Samples",
       x = "Leaf Area (cm^2)",y = "Frequency") +
  geom_vline(aes(xintercept = mean(avg.leaf.area), color = "Mean"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(avg.leaf.area), color = "Median"), linetype = "dotted", size = 1) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        legend.position = c(0.9, 0.9),
        legend.justification = c("right", "top")) +
  scale_color_manual(values = c("Mean" = "red", "Median" = "blue"),
                     labels = c(paste0("Mean (", round(mean(plant.data.no$avg.leaf.area), 2), ")"), 
                                paste0("Median (", round(median(plant.data.no$avg.leaf.area), 2), ")")))

# Histogram of Plant Height ####
height.hist <- 
  ggplot(plant.data.no, aes(x = height)) +
  geom_histogram(binwidth = 6, fill = "darkgreen", color = "black", alpha = 0.6) +
  labs(#title = "Histogram of Plant Height Samples",
       x = "Plant Height (cm)",
       y = "Frequency") +
  geom_vline(aes(xintercept = mean(height), color = "Mean"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(height), color = "Median"), linetype = "dotted", size = 1) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        legend.position = c(0.98, 0.95),
        legend.justification = c("right", "top")) +
  scale_color_manual(values = c("Mean" = "red", "Median" = "blue"),
                     labels = c(paste0("Mean (", round(mean(plant.data.no$height), 2), ")"), 
                                paste0("Median (", round(median(plant.data.no$height), 2), ")")))

# Histograms of Slope and Aspect
aspect.hist <- 
  ggplot(plant.data.no, aes(x = aspect)) +
  geom_histogram(binwidth = 15, fill = "darkgreen", color = "black", alpha = 0.6) +
  labs(#title = "Histogram of Aspects of \nPlant Samples",
       x = "Aspect (Degrees)",
       y = "Frequency") +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_line(color = "grey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")) +
  scale_x_continuous(breaks = c(-180, -90, 0, 90, 180)) + 
  coord_polar(start = 0 - pi/24)


slope.hist <- 
  ggplot(plant.data.no, aes(x = slope)) +
  geom_histogram(binwidth = 2, fill = "darkgreen", color = "black", alpha = 0.6) +
  labs(#title = "Histogram of Slopes of Plant Samples",
       x = "Slope (Degrees)",
       y = "Frequency") +
  geom_vline(aes(xintercept = mean(slope), color = "Mean"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(slope), color = "Median"), linetype = "dotted", size = 1) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        legend.position = c(0.90, 0.90),
        legend.justification = c("right", "top")) +
  scale_color_manual(values = c("Mean" = "red", "Median" = "blue"),
                     labels = c(paste0("Mean (", round(mean(plant.data.no$slope), 2), ")"), 
                                paste0("Median (", round(median(plant.data.no$slope), 2), ")")))

# Histograms of Light Availablity
SRI.normal.hist <- 
  ggplot(plant.data.no, aes(x = SRI.normalized)) +
  geom_histogram(binwidth = .05, fill = "darkgreen", color = "black", alpha = 0.6) +
  labs(#title = "Histogram of Solar Radiation Index of Plant Samples",
       x = "Solar Radiation Index \n(unitless, normalized between 0-1)",
       y = "Frequency") +
  geom_vline(aes(xintercept = mean(SRI.normalized), color = "Mean"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(SRI.normalized), color = "Median"), linetype = "dotted", size = 1) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        legend.position = c(0.25, 0.95),
        legend.justification = c("right", "top")) +
  scale_color_manual(values = c("Mean" = "red", "Median" = "blue"),
                     labels = c(paste0("Mean (", round(mean(plant.data.no$SRI.normalized), 2), ")"), 
                                paste0("Median (", round(median(plant.data.no$SRI.normalized), 2), ")")))

SRI.mediated.normal.hist <- 
  ggplot(plant.data.no, aes(x = SRI.mediated.normalized)) +
  geom_histogram(bins = 22, fill = "darkgreen", color = "black", alpha = 0.6) +
  labs(#title = "Histogram of Canopy Mediated Solar Radiation Index of Plant Samples",
       x = "Canopy Mediated Solar Radiation Index \n(unitless, normalized between 0-1)",
       y = "Frequency") +
  geom_vline(aes(xintercept = mean(SRI.mediated.normalized), color = "Mean"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(SRI.mediated.normalized), color = "Median"), linetype = "dotted", size = 1) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        legend.position = c(0.25, 0.95),
        legend.justification = c("right", "top")) +
  scale_color_manual(values = c("Mean" = "red", "Median" = "blue"),
                     labels = c(paste0("Mean (", round(mean(plant.data.no$SRI.mediated.normalized), 2), ")"), 
                                paste0("Median (", round(median(plant.data.no$SRI.mediated.normalized), 2), ")")))


# Saving charts ####
ggsave("aspect.hist.png", aspect.hist, path = "figures/", width = 1500, height = 1500, units = "px")
ggsave("slope.hist.png", slope.hist, path = "figures/", width = 2000, height = 1000, units = "px")
ggsave("canopy.hist.png", canopy.hist, path = "figures/", width = 2000, height = 1000, units = "px")
ggsave("height.hist.png", height.hist, path = "figures/", width = 2000, height = 1000, units = "px")
ggsave("leaf.hist.png", leaf.hist, path = "figures/", width = 2000, height = 1000, units = "px")
ggsave("SRI.normal.hist.png", SRI.normal.hist, path = "figures/", width = 2000, height = 1000, units = "px")
ggsave("SRI.m.normal.hist.png", SRI.mediated.normal.hist, path = "figures/", width = 2000, height = 1000, units = "px")


