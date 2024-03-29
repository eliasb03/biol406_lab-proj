#------------------------------
# Biol 406 Project
# By: Elias, Rebecca, Sara, Sahar 
# 2024-03-13
# Description: This R script should complete the analysis and figure generation of BIOL 406 Lab project data.
# This will involve visualizing the data as a scatterplot, creating a simple linear model, and completing the regression
# it may also develop a mixed model
#------------------------------

# Creating a Linear Model Obselete ####
## Checking assumptions 
## Normality - Creating histograms
# hist(plant.data$avg.leaf.area) # Normal!
# hist(plant.data$avg.canopy.cover) # Normal?
# 
# ## Linearity - Plotting the Scatterplot 
# plot(avg.leaf.area ~ avg.canopy.cover, data = plant.data) # Hard to say
# 
# ## Creating simple Linear Model
# area.cover.lm <- lm(avg.leaf.area ~ avg.canopy.cover, data = plant.data)
# 
# # Other plots to check assumptions
# plot(area.cover.lm)
# 
# ## Viewing the Linear model
# summary(area.cover.lm)



# Creating a Linear Model with all Fixed Effects ####
area.cover.lm2 <- lm(avg.leaf.area ~ avg.canopy.cover +  height + plot, data = plant.data)
  # plot is the grouping variable, including height here as a grouping also

## Checking Assumptions
  # Linearity
  # Constant Variance in Errors
  # Independence of Errors
  # Normal Distribution of Errors

summary(area.cover.lm2)
