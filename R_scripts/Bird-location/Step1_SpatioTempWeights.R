#' [Species Centered BRT Model]
#' 
#' Goal: Build species distribution models for all species of birds from Oregon using BRT
#' Step 1:  Calculate sampling effort weight for occurrence data
#' 
#' Load packages
## -------------------------------------------------------------------------------------------------------------

library(dynamicSDM)
library(raster)
library(terra)
library(foreach)
library(doParallel)

# Define the base directory path
base_dir <- "/nfs/stak/users/stanleyf/hpc-share/Ecology/Data"

#' 
#' Load the data to calculate sampling weights
## -------------------------------------------------------------------------------------------------------------
oregon <- read.csv(sprintf("%s/Oregon2020/oregon2020-zerofilled_birdloc.csv", base_dir))

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

clean_common_names <- function(x) {
  x$Common_Name <- gsub("/", " ", x$Common_Name)
  return(x)
}
                        
oregon <- clean_common_names(oregon)
individual_species_oregon <- split(oregon, oregon$Common_Name)
                        
# Set the number of cores to use
num_cores <- detectCores()
cat("\n", num_cores, " num_cores")
                       
# Initialize parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)
                        
# Parallelized loop
all_birds.weight <- foreach(i = unique(oregon$Common_Name), .combine = rbind, .multicombine = TRUE, .packages = c("dynamicSDM")) %dopar% {
  
      #cat(paste0(i, "\n"), file="/nfs/stak/users/stanleyf/hpc-share/Ecology/Scripts/species_st.txt",append=TRUE)
    
      per_bird <- individual_species_oregon[[i]]
      per_bird$index <- 1:nrow(per_bird)
      
      per_bird.weight <- spatiotemp_weights(occ.data = per_bird,
                                            samp.events = sample_events_data,
                                            spatial.dist = 104200,
                                            temporal.dist = 76)
      
      # Merge back the explanatory data with weight
      #per_bird.weight.select <- unique(per_bird.weight[,c("Unique_Checklist_ID", "SAMP_EFFORT", "REL_SAMP_EFFORT")])      
      return(per_bird.weight)
}

                        
all_birds <- do.call(rbind, all_birds.weight)             
print(nrow(all_birds))

# Stop cluster
stopCluster(cl)
                        
write.csv(all_birds, 
          file = sprintf("%s/SpatioTempWeights/SpeciesCentered_weights.csv", base_dir),
          row.names = FALSE)
