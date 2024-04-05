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

kable_table <- kable(summary_results, format = "html", align = "c", col.names = custom_col_names) %>%
  kable_styling(full_width = FALSE)

kable_table

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

# Excluding Plant 12-2 outlier
plant.data.no <- plant.data %>% filter(plant.id != "12-2") # filtering outlier
area.cover.lm.no <- lm(avg.leaf.area ~ avg.canopy.cover + height + plot, data = plant.data.no)

