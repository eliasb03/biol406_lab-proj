#------------------------------
# Biol 406 Project
# By: Elias, Rebecca, Sara, Sahar 
# 2024-03-13
# Description: Initial R script for the setup of the BIOL 406 Lab project
# This script should import packages, prepare functions, and import data 
#------------------------------

# Import Packages ####
library(dplyr)
library(tidyverse)
library(skimr)
library(rmarkdown)
library(lme4)
library(lmerTest)
library(ggeffects)


# Importing Data #### 
raw_data_path <- file.path("data", "raw") # Creating File Path to the raw folder within the data structure
raw_file_name <- "raw_datasheet_03_20.csv" # Name of the file to be imported below
  # The above works fine, but will need to be updated for different datasheet titles, will need to rework this to a more reproducible format later

raw.data <- read.csv(file.path(raw_data_path, raw_file_name), header = TRUE, skip = 3) # Importing datasheet from file path
  # header = TRUE specifies to use the column titles from the first row
  # skip = 3 is specific to this data set, and uses the 3rd row as the header, removes some of the metadata regarding collection on the sheet

# Tidying Data ####
# initial summary of what needs to be done:
  # table in wide format, canopy cover and leaf area should be pivoted longer
  # aspect can lose the characters ex: "120SE" --> "120"
  # Conif and Decid should be changed to perc_conif and perc_decid
  # Create a plant.id value that is the plot and salal number merged
  # Should give an initial thought about handling NAs
  # Reorder columns

## Pivoting the data to long format
raw.data.long <- raw.data %>%
  pivot_longer(cols = starts_with("leaf_area"),
               names_to = "leaf_area_index",
               names_prefix = "leaf_area",
               values_to = "leaf_area") %>%
  pivot_longer(cols = starts_with("canopy_cover"),
               names_to = "canopy_cover_index",
               names_prefix = "canopy_cover",
               values_to = "canopy_cover")

## Cleaning aspect values
  # The following code makes two columns, one with the aspect as a number, the second with the plot direction as a generalized direction string
  # the code makes use of a seperator that finds an occurance of anumber next to a letter
raw.data.long <- raw.data.long %>%
  separate(aspect, into = c("aspect", "plot.dir"), sep = "(?<=\\d)(?=[A-Za-z])", remove = FALSE)

## Adding a plant.id column
raw.data.long$plant.id <- paste(raw.data.long$plot, raw.data.long$salal_number, sep = "-")


## Renaming Columns in the Dataset
new_column_names <- c("salal.num", "perc.conif", "perc.decid", "leaf.area.count", "leaf.area", "canopy.cover.count", "canopy.cover")
columns_to_rename <- c(2, 7, 8, 10, 11, 12, 13)
colnames(raw.data.long)[columns_to_rename] <- new_column_names

## Reorder Columns
desired_order <- c("plant.id", "plot", "salal.num", "leaf.area.count", "leaf.area", "canopy.cover.count", "canopy.cover", "height", "aspect", "plot.dir", "slope", "perc.conif", "perc.decid", "notes") 
clean.data <- raw.data.long[, desired_order]

# Saving Tidy Data ####
## close old raw dataframes to declutter
rm("raw.data")
rm("raw.data.long")
## save clean data to computer
clean_data_path <- file.path("data", "clean") # Creating File Path to the clean  folder within the data structure
clean_file_name <- "forest_understory_plant_data.csv" # titling the clean dataframe export
write.csv(clean.data, file = file.path(clean_data_path, clean_file_name), row.names = FALSE)

# Creating a plant level average plot ####
## Creating a new dataframe that averages the data to the individual plant level, such that there is a single occurance of avg.leaf.area and avg.canopy.cover for each plant, the level of replication
plant.data <- clean.data %>%
  group_by(plant.id) %>%
  summarize(
    avg.leaf.area = mean(leaf.area), # finding leaf.area average
    avg.canopy.cover = mean(canopy.cover), # finding canopy.cover average
    plot = first(plot),
    height = first(height),
    aspect = first(aspect),
    slope = first(slope),
    perc.conif = first(perc.conif),
    perc.decid = first(perc.decid)
  )

plant.data$plot <- as.factor(plant.data$plot)

plant.data <- plant.data %>%
  mutate(plot_id = paste0("plot", match(plot, unique(plot)))) 


# Saving this dataset to ~/data/clean also
plant_level_file_name <- "plant_level_data.csv" # titling the clean dataframe export
write.csv(plant.data, file = file.path(clean_data_path, plant_level_file_name), row.names = FALSE) # Writing to .csv

