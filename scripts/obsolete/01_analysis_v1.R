#------------------------------
# Biol 406 Project
# By: Elias, Rebecca, Sara, Sahar 
# 2024-03-13
# Description: This R script should complete the analysis and figure generation of BIOL 406 Lab project data.
# This will involve visualizing the data as a scatterplot, creating a simple linear model, and completing the regression
# it may also develop a mixed model
#------------------------------
# Creating summary statistics ####
## Relevant Functions
# Create subset table of numeric variables for summary stats
summary.input.df <- plant.data %>%
  mutate(aspect = as.numeric(aspect)) %>%
  select(!c("plant.id", "plot","plot_id"))

# Summary statistics function
summary_stats <- function(x) {
  mean_val <- round(mean(x, na.rm = TRUE), 2)
  median_val <- round(median(x, na.rm = TRUE), 2)
  iqr_val <- round(IQR(x, na.rm = TRUE), 2)
  sd_val <- round(sd(x, na.rm = TRUE), 2)
  max <- round(max(x, na.rm = TRUE), 2)
  min <- round(min(x, na.rm = TRUE), 2)
  num <- round(sum(!is.na(x)), 0)
  
  result <- c(mean_val, median_val, iqr_val, sd_val, max, min, num)
  names(result) <- c("Mean", "Median", "Inter Quartile Range", "Standard Deviation", "Minimum", "Maximum", "Number of Samples")
  
  return(result)
}

# Apply summary statistics function to each numeric variable
summary_results <- sapply(summary.input.df, function(x) {
  if(is.numeric(x)) {
    summary_stats(x)
  } else {
    NA
  }
})

# Print the summary results
custom_col_names <- c("Average Leaf Area (cm^2)", "Average Canopy Area (%)", "Plant Height (cm)", "Plot Aspect (degrees from N)", "Plot Slope (degrees)", "Percent Coniferous Canopy (%)", "Percent Deciduous Canopy (%)")

# kable_table <- kable(summary_results, format = "html", align = "c", col.names = custom_col_names) %>%
#   kable_styling(full_width = FALSE)
# 
# kable_table

# Linear Models ####
## Checking assumptions ####
### Linearity
plot(avg.leaf.area ~ avg.canopy.cover, data = plant.data) # Hard to say
### Normality
hist(plant.data$avg.leaf.area) # Normal!
hist(plant.data$avg.canopy.cover) # Normal-ish?
### Consistent Variance of Error
### Sample Size (should be >20 cases per independent variable)
## Creating Models #####
# Including Outlier
area.cover.lm <- lm(avg.leaf.area ~ avg.canopy.cover + height + plot, data = plant.data)
summary(area.cover.lm)
# Excluding Plant 12-2 and 12-1 outlier
plant.data.no <- plant.data %>% 
  filter(plant.id != "12-2") %>% # filtering outlier
  filter(plant.id != "12-1") # filter decidous outlier
area.cover.lm.no <- lm(avg.leaf.area ~ avg.canopy.cover + height + plot, data = plant.data.no)
summary(area.cover.lm.no)

# Comparing Leaf Area to Plant Height
height.lm <- lm(avg.leaf.area ~ height, data = plant.data)
summary(height.lm)

## Creating Light availability - BASED ON CHATGPT FOR PRELIM WORK####
# # Convert aspect from degrees to radians
# plant.data$aspect_rad <- plant.data$aspect * (pi / 180)
# 
# # Calculate Solar Incidence Angle (SIA) using slope and aspect
# plant.data$SIA <- acos(cos(plant.data$slope * (pi / 180)) * cos((pi / 2) - plant.data$aspect_rad))
# 
# # Calculate Cosine of Solar Incidence Angle
# plant.data$light_availability <- cos(plant.data$SIA)
# 
# # Optional: You can rescale the light_availability values between 0 and 1 for better interpretation
# plant.data$light_availability <-
#   (plant.data$light_availability - min(plant.data$light_availability, na.rm = TRUE)) / (max(plant.data$light_availability, na.rm = TRUE) - min(plant.data$light_availability, na.rm = TRUE))
# 

## Solar Radiation Index as per (Keating, et al. 2010) ####
latitude <-
  49.256 # Latitude estimate for all samples in Pacific Spirit

plant.data.no <- plant.data.no %>%
  filter(!is.na(slope)) %>%
  mutate(
    plot_id = as.factor(plot_id),
    aspect.num = as.numeric(aspect),
    aspect_180 = (180 - aspect.num), # Keating et al. reports to convert aspect by 180 degrees
    SRI = ((cos(latitude) * cos(slope)) + (sin(latitude) * sin(slope) * # formula outlined in Keating
                                            cos(aspect_180))),
    sky.cover = (100 - (avg.canopy.cover)), # Calculating how much sky is visible, inverse of Canopy Cover
  ) 

plant.data.no <- plant.data.no %>% 
  mutate(
    SRI.normalized = (SRI - min(plant.data.no$SRI, na.rm = TRUE)) / 
      (max(plant.data.no$SRI, na.rm = TRUE) - min(plant.data.no$SRI, na.rm = TRUE)), # Normalizing SRI.mediated between 0-1
    SRI.mediated = (SRI * sky.cover), # Integrating canopy cover measurment by multiplication - using sky cover because higher values here correspond to greater light
    SRI.mediated.normalized = (SRI.mediated - min(plant.data.no$SRI.mediated)) / 
      (max(plant.data.no$SRI.mediated) - min(plant.data.no$SRI.mediated))
     )



## Plotting with Light Availability Metrics ####
leaf.by.SRI.lm <- lm(avg.leaf.area ~ SRI + sky.cover + plot, data = plant.data.no)
summary(leaf.by.SRI.lm)

leaf.by.light.lm <- lm(avg.leaf.area ~ SRI.mediated.normalized + plot, data = plant.data.no)
summary(leaf.by.light.lm)

lm.light.prediction <- as.data.frame(ggpredict(leaf.by.light.lm, terms = "SRI.mediated.normalized"))

#new.light.plot <-
  ggplot(lm.light.prediction) +
  geom_point(data = plant.data.no, aes(x = SRI.mediated.normalized, y = avg.leaf.area), size = 2, alpha = 0.6) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(aes(x = x, y = predicted), color = "red") +
  labs(x = "Light Availability (No units, out of 1)",
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

# ## Attempting the PCA approach ####
# plant.data.no.complete <- na.omit(plant.data.no[, c("slope", "aspect.num", "avg.canopy.cover", "avg.leaf.area")])
# pca_result <- prcomp(plant.data.no.complete[, c("slope", "aspect.num", "avg.canopy.cover")], scale. = TRUE)
#                                             # Creating a pca of light availability metrics
# 
# loadings <- pca_result$rotation
# 
# pca_data <- data.frame(PC1 = pca_result$x[,1], 
#                        PC2 = pca_result$x[,2],
#                        leaf_area = plant.data.no.complete$avg.leaf.area)
# 
# # Mostly as a test, but I've chosen PC3 to represent light availability because it has a negative loading for avg.canopy.cover (-0.640), which I'm interpreting to mean that it variations in avg.canopy.cover contribute significantly to PC3 in the negative direction. Aspect (0.283) seems to have a slight positive relationship - in reality aspect should differneitally increase light acess based on when its closest to 180 (south). Finally, slope has a large positive relationship to the variation in PC3 (0.714) - which may make sense given a slope to the south would increase light availability, however more data faces northeast in my dataset so I should check my understanding and whether this logic actually makes any sense.
# # For this test PC3 will be considered a proxy for light availability in the dataset
# # Extract PC3 scores
# PC3_scores <- pca_result$x[, 3]
# 
# # Create a data frame for plotting
# PCA_plot_data <- data.frame(PC3 = PC3_scores, leaf_area = plant.data.no.complete$avg.leaf.area)
# 
# # Create a quick lm, to use with GGEffects to show a trendline
# PCA.lm <- lm(leaf_area ~ PC3_scores, data = PCA_plot_data)
# summary(PCA.lm)
# lm.s.prediction <- as.data.frame(ggpredict(PCA.lm, terms = "PC3_scores"))
# 
# #pca.light.plot <-
#   ggplot(lm.s.prediction) +
#   geom_point(data = PCA_plot_data, aes(x = PC3_scores, y = leaf_area), size = 2, alpha = 0.6) +
#   geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line(aes(x = x, y = predicted), color = "red") +
#   labs(x = "PCA3 (unitless proxy for Light Availability)",
#        y = "Leaf Area (cm^2)") +
#   theme_minimal() +
#   theme(plot.title = element_text(size = 20, face = "bold"),
#         axis.title = element_text(size = 14),
#         axis.text = element_text(size = 12),
#         axis.line = element_line(color = "black"),
#         panel.grid.major = element_line(),
#         panel.grid.minor = element_line(),
#         panel.background = element_rect(fill = "white"),
#         plot.background = element_rect(fill = "white"))
# 

