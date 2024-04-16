#------------------------------
# Biol 406 Project
# By: Elias, Rebecca, Sara, Sahar 
# 2024-03-13
# Description: This R script should complete the analysis and figure generation of BIOL 406 Lab project data.
# This will involve visualizing the data as a scatterplot, creating a simple linear model, and completing the regression
# it may also develop a mixed model
#------------------------------
# Relevant Functions ####
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

# Creating summary statistics table ####
# Print the summary results
custom_col_names <- c("Average Leaf Area (cm^2)", "Average Canopy Area (%)", "Plant Height (cm)", "Plot Aspect (degrees from N)", "Plot Slope (degrees)", "Percent Coniferous Canopy (%)", "Percent Deciduous Canopy (%)")

# kable_table <- kable(summary_results, format = "html", align = "c", col.names = custom_col_names) %>%
#   kable_styling(full_width = FALSE)
# 
# kable_table

# Producing Linear Models ####
## Checking assumptions ####
### Linearity
  # w/ outlier
plot(avg.leaf.area ~ SRI, data = plant.data) # Hard to say
  # w/o outlier
plot(avg.leaf.area ~ SRI, data = plant.data.no) # Hard to say
plot(avg.leaf.area ~ SRI.mediated, data = plant.data.no) # Ish

### Normality
hist(plant.data$avg.leaf.area) # Normal!
hist(plant.data$SRI) # Not very Normal
hist(plant.data$SRI.normalized) # Not much more Normal
hist(plant.data$SRI.mediated) # Normal ish
hist(plant.data$SRI.mediated.normalized) # I'd say this is pretty normal!

hist(plant.data$height)

### Consistent Variance of Error
### Sample Size (should be >20 cases per independent variable)
skimr::skim(plant.data) # 31 observations w/ outliers
skimr::skim(plant.data.no) # 20 observations w/ outliers

### Checking for Colinearity
columns_of_interest <- c("SRI.mediated.normalized", "height")

# Subset the data frame to include only the columns of interest
subset_plant.data <- plant.data[, columns_of_interest]
subset_plant.data.no <- plant.data.no[, columns_of_interest]

# Calculate the correlation matrix for the subset
correlation_matrix <- cor(subset_plant.data)
correlation_matrix.no <- cor(subset_plant.data.no)

# Print the correlation matrix
print(correlation_matrix) # weak positive colinearity between height and SRI.mediated.normalized = 0.149
print(correlation_matrix.no) # weak positive = 0.126
# Co linearity decreases in my outlier removed model

## Creating Models #####
# Excludes outlier and height
  # Including the outlier leads to overfitting of the model
leaf.by.light.lm <- lmer(avg.leaf.area ~ SRI.mediated.normalized + (1|plot), data = plant.data.no)

# Variation including height - worry about the complexity of this model
# leaf.by.light.height.lm <- lmer(avg.leaf.area ~ SRI.mediated.normalized + height + (1|plot), data = plant.data.no)
# summary(leaf.by.light.height.lm)

# Comparing Leaf Area to Plant Height
height.lm <- lm(avg.leaf.area ~ height, data = plant.data.no)
summary(height.lm)

# Creating Anova Summary Tables for models ####
summary(leaf.by.light.lm)
light_summary_table <- tab_model(leaf.by.light.lm, show.icc = FALSE, show.re.var = FALSE)
light_summary_table

summary(height.lm)
height_summary_table <- tab_model(height.lm, show.icc = FALSE, show.re.var = FALSE)
height_summary_table

