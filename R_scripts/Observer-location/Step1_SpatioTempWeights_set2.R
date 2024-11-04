#' [Observer-location BRT Model - set 2]
#' 
#' Goal: Build species distribution models for all species of birds from Oregon using BRT
#' Step 1:  Calculate sampling effort weight for occurrence data
#' [NOTE] "spatiotemp_weights" is called once for all species
#' Load packages
## -------------------------------------------------------------------------------------------------------------

#if (!require('dynamicSDM')) install.packages('dynamicSDM'); library(dynamicSDM)
#if (!require('dismo')) install.packages('dismo'); library(dismo)
#if (!require('raster')) install.packages('raster'); library(raster)
#if (!require('terra')) install.packages('terra'); library(terra)

library(dynamicSDM)
library(dismo)
library(raster)
library(terra)

# Define the base directory path
base_dir <- "/nfs/stak/users/shenf/hpc-share/dynamicSDM"

#' 
#' Load the data to calculate sampling weights
## -------------------------------------------------------------------------------------------------------------
oregon <- read.csv("/nfs/stak/users/shenf/hpc-share/oregon_all-zerofilled.csv")

#' 
#' Load environmental values from observer-location
## -------------------------------------------------------------------------------------------------------------
bird.env.noRadius <- read.csv("/nfs/stak/users/shenf/hpc-share/Gini_index/Median_withSpecialYear_surveyCentered_noRadius.csv")
bird.env.fixRadius <- read.csv("/nfs/stak/users/shenf/hpc-share/Gini_index/Median_withSpecialYear_surveyCentered_100mfixedRadius.csv")
bird.env.esw <- read.csv("/nfs/stak/users/shenf/hpc-share/Gini_index/Median_withSpecialYear_surveyCentered_ESW.csv")

#' 
#' Rename columns for future input in calculating sampling effort weight weight in dynamicSDM
## -------------------------------------------------------------------------------------------------------------
checklistid.split <- strsplit(as.character(oregon$Unique_Checklist_ID), "_")
date <- sapply(checklistid.split, function(x) strsplit(x[3], "-"))

oregon$year <- as.numeric(oregon$Year)
oregon['month'] <- sapply(date, function(x) as.numeric(x[2]))
oregon['day'] <- sapply(date, function(x) as.numeric(x[3]))
oregon['year'] <- oregon['Year'] 
oregon['x'] <- oregon['Longitude'] 
oregon['y'] <- oregon['Latitude'] 
oregon <- subset(oregon, select = -c(Year, Longitude, Latitude))
                        
#' 
#' Calculate sampling effort weight for occurrence data - Will weight
#' records by sampling effort when fitting SDMs.
## -------------------------------------------------------------------------------------------------------------
# Create sampling event file
selected_cols <- c("x","y","year","month","day")
sample_events_data <- oregon[selected_cols]

# Unique sampling event - regardless species occurrences
print(nrow(sample_events_data))
sample_events_data <- unique(sample_events_data)
print(nrow(sample_events_data))
                        
# Split Oregon dataset into two sets
  # === Set 1 === #
#oregon_set1 <- oregon[1:1514135,]
  # === Set 2 ====#
oregon_set2 <- oregon[1514136:nrow(oregon),]

 # === Perform spatial-weight analyses ===#
all_birds.weight <- spatiotemp_weights(occ.data = oregon_set2,
                                            samp.events = sample_events_data,
                                            spatial.dist = 104200,
                                            temporal.dist = 76)
write.csv(all_birds.weight, 
          file = sprintf("%s/SurveyCentered_weights_set2.csv", base_dir),
          row.names = FALSE)
                      
#' 
## -------------------------------------------------------------------------------------------------------------
# Merge back the explanatory data with weight
all_birds.weight.select <- unique(all_birds.weight[,c("Unique_Checklist_ID", "Common_Name", "SAMP_EFFORT","REL_SAMP_EFFORT")])

# For fixed radius
all_birds.weight.env.fixed <- merge(all_birds.weight.select, bird.env.fixRadius, 
                                   by = c("Unique_Checklist_ID", "Common_Name"), all.y = TRUE)

# For No radius
all_birds.weight.env.noRadius <- merge(all_birds.weight.select, bird.env.noRadius, 
                                      by = c("Unique_Checklist_ID", "Common_Name"), all.y = TRUE)

# For effective strip width
all_birds.weight.env.ESW <- merge(all_birds.weight.select, bird.env.esw, 
                                 by = c("Unique_Checklist_ID", "Common_Name"), all.y = TRUE)

# Remove NA rows

columns_to_check <- c("SAMP_EFFORT", "REL_SAMP_EFFORT")
all_birds.weight.env.fixed <- all_birds.weight.env.fixed[!rowSums(is.na(all_birds.weight.env.fixed[, columns_to_check])), ]
all_birds.weight.env.noRadius <- all_birds.weight.env.noRadius[!rowSums(is.na(all_birds.weight.env.noRadius[, columns_to_check])), ]
all_birds.weight.env.ESW <- all_birds.weight.env.ESW[!rowSums(is.na(all_birds.weight.env.ESW[, columns_to_check])), ]


# Save the spatio_temp weights for each Strategy
write.csv(all_birds.weight.env.fixed, 
          file = sprintf("%s/Median_surveyCentered_fixedRadius_weights_set2.csv", base_dir),
          row.names = FALSE)
    
write.csv(all_birds.weight.env.noRadius, 
          file = sprintf("%s/Median_surveyCentered_noRadius_weights_set2.csv", base_dir),
          row.names = FALSE)

write.csv(all_birds.weight.env.ESW,
          file = sprintf("%s/Median_surveyCentered_ESW_weights_set2.csv", base_dir),
          row.names = FALSE)

