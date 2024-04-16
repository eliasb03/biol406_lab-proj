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

SRI.hist <- 
  ggplot(plant.data, aes(x = SRI)) +
  geom_histogram(fill = "darkgreen", color = "black", alpha = 0.6) +
  labs(title = "Histogram of SRI of Plant Samples",
       x = "SRI (unit-less)",
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

SRI.normal.hist <- 
  ggplot(plant.data, aes(x = SRIm.normalized)) +
  geom_histogram(fill = "darkgreen", color = "black", alpha = 0.6) +
  labs(title = "Histogram of SRI of Plant Samples",
       x = "SRI (unit-less)",
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

#plot(plant.data$aspect, plant.data$SRIm.normalized) # plotting to see if theres a clear relationship or not
  
  
# Saving charts ####
ggsave("slope.hist.png", slope.hist, path = "figures/", width = 2000, height = 1000, units = "px")
ggsave("canopy.hist.png", canopy.hist, path = "figures/", width = 2000, height = 1000, units = "px")
ggsave("height.hist.png", height.hist, path = "figures/", width = 2000, height = 1000, units = "px")
ggsave("leaf.hist.png", leaf.hist, path = "figures/", width = 2000, height = 1000, units = "px")


