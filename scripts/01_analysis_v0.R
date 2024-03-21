#------------------------------
# Biol 406 Project
# By: Elias, Rebecca, Sara, Sahar 
# 2024-03-13
# Description: This R script should complete the analysis and figure generation of BIOL 406 Lab project data.
# This will involve visualizing the data as a scatterplot, creating a simple linear model, and completing the regression
# it may also develop a mixed model
#------------------------------

# Creating a Linear Model
## Checking assumption of normality
hist(plant.data$avg.leaf.area)
hist(plant.data$avg.canopy.cover)
## Plotting the scatterplot
plot(avg.leaf.area ~ avg.canopy.cover, data = plant.data)
## Creating the Linear Model
area.cover.lm <- lm(avg.leaf.area ~ avg.canopy.cover, data = plant.data)
## Viewing th Linear model
summary(area.cover.lm)
# plot(area.cover.lm)


# Creating a Mixed Linear Model
area.cover.mlm <- lmer(avg.leaf.area ~ avg.canopy.cover + (1|plot) + (1|height), data = plant.data)
# plot is the grouping variable, including height here as a grouping also
summary(area.cover.mlm)
# the below treats height as another fixed effect
area.cover.mlm2 <- lmer(avg.leaf.area ~ avg.canopy.cover + height + (1|plot), data = plant.data)
summary(area.cover.mlm2)
