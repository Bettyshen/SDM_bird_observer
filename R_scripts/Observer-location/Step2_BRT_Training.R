#' [Observer-location BRT Model] 
#' 
#' Goal: Build species distribution models for all species of birds from Oregon using BRT
#' 
#' Load packages
## -----------------------------------------------------------------------------
library(dynamicSDM)
library(dismo)
library(raster)
library(terra)
library(foreach)
library(doParallel)
library(data.table)
library(gbm)
library(stringr)
library(dplyr)


# Define the base directory path
base_dir <- "/Volumes/T7/eBird_Oregon2020/distance/Distance-Sampling/BRT_train"

# See whether the directory exist 
directory_path <- paste0(base_dir, "/TestData/")

if (dir.exists(directory_path)) {
  print(paste("Directory exists:", directory_path))
} else {
  print(paste("Directory does not exist:", directory_path))
}

#' Load environmental values from observer-location
#' We need to load two sets of file
## -------------------------------------------------------------------------------------------------------------
 # No radius
bird.env.noRadius.1 <- fread("/Volumes/T7/eBird_Oregon2020/distance/Distance-Sampling/env_extraction/Median_surveyCentered_noRadius_weights_set1.csv")
bird.env.noRadius.2 <- fread("/Volumes/T7/eBird_Oregon2020/distance/Distance-Sampling/env_extraction/Median_surveyCentered_noRadius_weights_set2.csv")
 # Fixed radius
bird.env.fixRadius.1 <- fread("/Volumes/T7/eBird_Oregon2020/distance/Distance-Sampling/env_extraction/Median_surveyCentered_fixedRadius_weights_set1.csv")
bird.env.fixRadius.2 <- fread("/Volumes/T7/eBird_Oregon2020/distance/Distance-Sampling/env_extraction/Median_surveyCentered_fixedRadius_weights_set2.csv")
 # Effective strip width
bird.env.esw.1 <- fread("/Volumes/T7/eBird_Oregon2020/distance/Distance-Sampling/env_extraction/Median_surveyCentered_ESW_weights_set1.csv")
bird.env.esw.2 <- fread("/Volumes/T7/eBird_Oregon2020/distance/Distance-Sampling/env_extraction/Median_surveyCentered_ESW_weights_set2.csv")

#' Merge two sets of data into one
## -----------------------------------------------------------------------------------------------------------------

  # No radius
bird.env.noRadius.all <- rbind(bird.env.noRadius.1, bird.env.noRadius.2)

  # Fixed radius
bird.env.fixRadius.all <- rbind(bird.env.fixRadius.1, bird.env.fixRadius.2)

  # ESW
bird.env.esw.all <- rbind(bird.env.esw.1, bird.env.esw.2)

#' Rename columns for future input in assigning blocks
##------------------------------------------------------------------------------------------------------------
  # No radius
bird.env.noRadius.all <- cbind(bird.env.noRadius.all, str_split(bird.env.noRadius.all$Unique_Checklist_ID, pattern = "_", simplify = TRUE))
    # Rename new columns
bird.env.noRadius.all <- bird.env.noRadius.all %>%
  rename(
    x = V2,
    y = V1,
    YMD = V3,
    month = Month,
    day = Day
  )
  # Make latitude & longitude numeric
bird.env.noRadius.all$x <- as.numeric(bird.env.noRadius.all$x)
bird.env.noRadius.all$y <- as.numeric(bird.env.noRadius.all$y)
str(bird.env.noRadius.all)

  # Fixed radius
bird.env.fixRadius.all <- cbind(bird.env.fixRadius.all, str_split(bird.env.fixRadius.all$Unique_Checklist_ID, pattern = "_", simplify = TRUE))
    # Rename new columns
bird.env.fixRadius.all <- bird.env.fixRadius.all %>%
  rename(
    x = V2,
    y = V1,
    YMD = V3,
    month = Month,
    day = Day
  )
  # Make latitude & longitude numeric
  bird.env.fixRadius.all$x <- as.numeric(bird.env.fixRadius.all$x)
  bird.env.fixRadius.all$y <- as.numeric(bird.env.fixRadius.all$y)
  str(bird.env.fixRadius.all)
  
  # Effective Strip Width
bird.env.esw.all <- cbind(bird.env.esw.all, str_split(bird.env.esw.all$Unique_Checklist_ID, pattern = "_", simplify = TRUE))
    # Rename new columns
bird.env.esw.all <- bird.env.esw.all %>%
  rename(
    x = V2,
    y = V1,
    YMD = V3,
    month = Month,
    day = Day
  )

# Make latitude & longitude numeric
bird.env.esw.all$x <- as.numeric(bird.env.esw.all$x)
bird.env.esw.all$y <- as.numeric(bird.env.esw.all$y)
str(bird.env.esw.all)

#' Building Boosted Regression Tree (BRT) models
#' Let's divide dataset into `training` (80%) and `testing` (20%). The selected rows will be applied to all methods (fixed, pixel, ESW)
## -----------------------------------------------------------------------------
# Initialize parallel backend
num_cores <- detectCores()
cat("\n", num_cores, " num_cores")
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Parallelized loop
all_birds_results <- foreach(i = unique(bird.env.fixRadius.all$Common_Name), .packages = c("dynamicSDM", "terra", "gbm", "dismo", "data.table")) %dopar% {
    
    cat(paste0(i, "\n"), file="/Volumes/T7/eBird_Oregon2020/distance/Distance-Sampling/BRT_train/species_brt.txt",append=TRUE)
                               
    # Set seed to make our partition reproducible
    set.seed(123)
                               
    varNames = c("SR_B1_median", "SR_B2_median", "SR_B3_median", "SR_B4_median", "SR_B5_median", "SR_B7_median", "QA_PIXEL_median")

    oregon.extent <- data.frame(lat = c(41.99305, 46.23474),
                                lon = c(-124.5276, -116.6899))
    oregon.raster <- rast(oregon.extent, type = "")
    
    per_bird.weight.env.fixed <- bird.env.fixRadius.all[bird.env.fixRadius.all$Common_Name == i, ]
    per_bird.weight.env.noRadius <- bird.env.noRadius.all[bird.env.noRadius.all$Common_Name == i, ]
    per_bird.weight.env.ESW <- bird.env.esw.all[bird.env.esw.all$Common_Name == i, ]

    #=== Split into Train-Test ====#
    # 80 % of the sample size
    smp_size <- floor(0.8*nrow(per_bird.weight.env.fixed))

    # Select the no. of rows to be training
    train_select <- sample(seq_len(nrow(per_bird.weight.env.fixed)), size = smp_size)
    
    # For fixed radius
    fixed.train <- per_bird.weight.env.fixed[train_select,]
    fixed.test <- per_bird.weight.env.fixed[-train_select,]
    
    # For no radius
    noRadius.train <- per_bird.weight.env.noRadius[train_select,]
    noRadius.test <- per_bird.weight.env.noRadius[-train_select,]
    
    # For ESW
    esw.train <- per_bird.weight.env.ESW[train_select,]
    esw.test <- per_bird.weight.env.ESW[-train_select,]
    
    #=== Get Block Indices ====#
    # For fixed radius
    block.fixed <- spatiotemp_block(occ.data = fixed.train,
                          vars.to.block.by = varNames,
                          temporal.block = "day",
                          spatial.layer = oregon.raster,
                          spatial.split.degrees = 1,
                          n.blocks = 10,
                          iterations = 5000)

    # For no radius
    block.noRadius <- spatiotemp_block(occ.data = noRadius.train,
                          vars.to.block.by = varNames,
                          temporal.block = "day",
                          spatial.layer = oregon.raster,
                          spatial.split.degrees = 1,
                          n.blocks = 10,
                          iterations = 5000)

    # For effective strip width
    block.ESW <- spatiotemp_block(occ.data = esw.train,
                          vars.to.block.by = varNames,
                          temporal.block = "day",
                          spatial.layer = oregon.raster,
                          spatial.split.degrees = 1,
                          n.blocks = 10,
                          iterations = 5000)
    
    #=== Train BRT Model ====#
    #===For fixed radius====#
    
    weights.fixed <- (1 - block.fixed$REL_SAMP_EFFORT) # Need to pull out weights independently
    block.row.fixed <- as.vector(block.fixed$BLOCK.CATS) # Need to pull out assigned blocks 
    
    fixed.brt <- gbm.step(block.fixed,
                      gbm.x = varNames,
                      gbm.y = "Occur",
                      family = "poisson",
                      site.weights = weights.fixed,
                      fold.vector = block.row.fixed,
                      tree.complexity = 5,
                      learning.rate = 0.001)
        
    #====For no radius=====#
    
    weights.noRadius <- (1 - block.noRadius$REL_SAMP_EFFORT) # Need to pull out weights independently
    block.row.noRadius <- as.vector(block.noRadius$BLOCK.CATS) # Need to pull out assigned blocks

    noRadius.brt <- gbm.step(block.noRadius,
                              gbm.x = varNames,
                              gbm.y = "Occur",
                              family = "poisson",
                              site.weights = weights.noRadius,
                              fold.vector = block.row.noRadius,
                              tree.complexity = 5,
                              learning.rate = 0.001)
    
    #====For ESW =====#
    
    weights.ESW <- (1 - block.ESW$REL_SAMP_EFFORT) # Need to pull out weights independently
    block.row.ESW <- as.vector(block.ESW$BLOCK.CATS) # Need to pull out assigned blocks

    ESW.brt <- gbm.step(block.ESW,
                        gbm.x = varNames,
                        gbm.y = "Occur",
                        family = "poisson",
                        site.weights = weights.ESW,
                        fold.vector = block.row.ESW,
                        tree.complexity = 5,
                        learning.rate = 0.001)

    
    #=== Save Models and Test Data  ====#
    
    i <- gsub("/", " ", i)
    
    # Save RDS files with dynamic paths
      # Fixed radius
    tryCatch({
      saveRDS(fixed.brt, file = paste0(base_dir, "/TrainedModel/SurveyCentered_FixedRadius/", i, ".rds"))
    }, error = function(e) {
      print(paste("Failed to write fixed.brt to file:", paste0(base_dir, "/TrainedModel/SurveyCentered_FixedRadius/", i, ".rds"), "Error:", e$message))
    })
      # No radius
    tryCatch({
      saveRDS(noRadius.brt, file = paste0(base_dir, "/TrainedModel/SurveyCentered_NoRadius/", i, ".rds"))
    }, error = function(e) {
      print(paste("Failed to write noRadius.brt to file:", paste0(base_dir, "/TrainedModel/SurveyCentered_NoRadius/", i, ".rds"), "Error:", e$message))
    })
      # ESW
    tryCatch({
      saveRDS(ESW.brt, file = paste0(base_dir, "/TrainedModel/SurveyCentered_ESW/", i, ".rds"))
    }, error = function(e) {
      print(paste("Failed to write ESW.brt to file:", paste0(base_dir, "/TrainedModel/SurveyCentered_ESW/", i, ".rds"), "Error:", e$message))
    })
    
    
    # Specify paths to save test data
    # Fixed radius
    tryCatch({
    fname.fixed <- paste0(base_dir, "/TestData/SurveyCentered_FixedRadius.csv")
    write.table(fixed.test, file = fname.fixed, sep = ",",
                append = TRUE, col.names = !file.exists(fname.fixed), row.names = FALSE)
    }, error = function(e) {
      print(paste("Failed to write to fixed radius test data:", fname.fixed, "Error:", e$message))
    })
    
    # No radius
    tryCatch({
    fname.noRadius <- paste0(base_dir, "/TestData/SurveyCentered_noRadius.csv")
    write.table(noRadius.test, file = fname.noRadius, sep = ",",
                append = TRUE, col.names = !file.exists(fname.noRadius), row.names = FALSE)
    }, error = function(e) {
      print(paste("Failed to write to no radius test data:", fname.noRadius, "Error:", e$message))
    })
    
    # Effective strip width
    tryCatch({
    fname.esw <- paste0(base_dir, "/TestData/SurveyCentered_ESW.csv")
    write.table(esw.test, file = fname.esw, sep = ",",
                append = TRUE, col.names = !file.exists(fname.esw), row.names = FALSE)
    }, error = function(e) {
      print(paste("Failed to write to ESW test data:", fname.esw, "Error:", e$message))
    })
    

}

# Stop cluster
stopCluster(cl)
#' 
