# biol406\_lab-proj

# Metadata File for Forest Structure and Understory Plant Traits Study

## Project Information:

-   Project Title: Investigating the Influence of Forest Structure on
    Salal Traits
-   Research Question: How does canopy cover influence the physiology of
    light-capturing traits of Gaultheria shallon?
-   Completed for lab project for BIOL 406 at UBC
-   Dataset collected and project conducted by: Elias, Rebecca, Sahar,
    Sara
-   Metadata written: 2024-03-13
-   GitHub Repository for a BIOL 406 Lab Group project, started
    2024-02-08

## Dataset Description:

-   Dataset Name: forest\_understory\_plant\_data
-   Description: This dataset contains measurements of salal plant
    traits along with canopy cover data collected from various sites
    within Pacific Spirit Park
-   Date of Collection: This data was collected on March 7 and 8, 2024.
    Data will continue to be collected on March 14 and 15, 2024.
-   Location: Pacific Spirit Park, Vancouver, BC, Canada

## Variables:

1.  **plant.id:**
    -   Description: Unique identifier for each plant, format
        “plot”.”salal.num”
    -   Type: Integer
    -   Example: 3, 14, 11, 1
2.  **plot:**
    -   Description: Unique identifier for each plot where data was
        collected.
    -   Type: Integer
    -   Example: 3, 14, 11, 1
3.  **salal.num:**
    -   Description: Identifier for individual salal plants within each
        plot, counting up within range 5. 5 observations not always met
    -   Type: Integer
    -   Example: 1, 2, 3, 4
4.  **leaf.area.count:**
    -   Description: counts of leaf area collected from each salal
        plant, up to 5 leaves
    -   Type: Integer
    -   Example: 1, 3, 5
5.  **leaf.area:**
    -   Description: Measurements of specific leaf area (cm^3) for three
        leaves per plant. These measurements were taken using Leaf Area
        App.
    -   Type: Float
    -   Example: 46.17, 54.13, 46.47
6.  **canopy.cover.count:**
    -   Description: counts of canopy cover measurements collected from
        each salal plant, 4 values were collected from each plant at
        each cardinal direction
    -   Type: Float
    -   Example: 46.17, 54.13, 46.47
7.  **canopy.cover:**
    -   Description: Canopy cover (%) measured from four cardinal
        directions at each site plant.
    -   Type: Float
    -   Example: 75.757, 77.097, 73.724, 79.323
8.  **Height:**
    -   Description: Height of the salal plant in centimeters.
    -   Type: Float
    -   Example: 100, 105, 90, 70
9.  **Aspect:**
    -   Description: Compass direction of the slope at the plot
        location.
    -   Type: Integer
    -   Example: 120, 140, 314, 309
10. **plot.dir:**

-   Description: Cardinal directions of the slope at the plot location.
-   Type: String
-   Example: NE, SE, S, W

11.  **Slope:**

-   Description: Slope angle of the terrain at the plot location.
-   Type: Integer
-   Example: 7, 9, 2, 5

12.  **perc.conif**

-   Description: Percentage of surrounding canopy that is coniferous,
    compares to perc.decid which expresses amount of canopy that is
    deciduous
-   Type: Integer
-   Example: 100, 88, 75

13.  **perc.decid**

-   Description: Percentage of surrounding canopy that is deciduous,
    compares to perc.conif which expresses amount of canopy that is
    coniferous
-   Type: Integer
-   Example: 100, 88, 75

14.  **Notes:**

-   Description: Additional notes or observations recorded during data
    collection.
-   Type: String
-   Example: Day 1 start; 1/2 m wide, on log, Day 2 end

## Data Collection Methodology:

-   Sampling Design: Random points distributed within Pacific Spirit
    Park, specific plants sampled at each site.
-   Measurement Tools: Easy Leaf Area app for leaf area measurement,
    CanopyApp for canopy cover measurement.
-   Fieldwork Procedures: Will detail methodology for site selection, plant
    sampling, leaf selection, and data recording.

## Timeline:

-   Date of Data Collection: March 8 - March 15
-   Data Analysis: March 20 - March 28
-   Presentation: April 11
-   Written Report Due: April 21

## Roles and Responsibilities:

-   Rebecca: Access leaf app, help with data analysis (R) and writing
    reports.
-   Elias: Distribute Random Points in GIS, help with Data Analysis in
    R.
-   Sahar: Writing the report, editing, and presentation.
-   Sara: Help with writing the report, editing, creating the
    presentation.