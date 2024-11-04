#' [Species Centered BRT Model]
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
base_dir <- "/nfs/stak/users/stanleyf/hpc-share/Ecology/Data"

#' Load environmental values from bird-centered
## -------------------------------------------------------------------------------------------------------------
bird.env.noRadius <- read.csv(sprintf("%s/Oregon2020/Median_speciesCentered_noRadius.csv", base_dir))
bird.env.fixRadius <- read.csv(sprintf("%s/Oregon2020/Median_speciesCentered_fixedRadius.csv", base_dir))
bird.env.esw <- read.csv(sprintf("%s/Oregon2020/Median_speciesCentered_ESW.csv", base_dir))

#' Load Spatio temporal values for bird-centered
## -------------------------------------------------------------------------------------------------------------
all_birds.weight <- read.csv(sprintf("%s/SpatioTempWeights/SpeciesCentered_weights.csv", base_dir))

## -------------------------------------------------------------------------------------------------------------
# Merge back the explanatory data with weight
all_birds.weight.select <- unique(all_birds.weight[,c("Unique_Checklist_ID", "Common_Name", "SAMP_EFFORT","REL_SAMP_EFFORT", "x", "y", "year", "month", "day")])

# For fixed radius
all_birds.weight.env.fixed <- merge(all_birds.weight.select, bird.env.fixRadius, 
                                   by = c("Unique_Checklist_ID", "Common_Name"), all.y = TRUE)

# For No radius
all_birds.weight.env.noRadius <- merge(all_birds.weight.select, bird.env.noRadius, 
                                      by = c("Unique_Checklist_ID", "Common_Name"), all.y = TRUE)

# For effective strip width
all_birds.weight.env.ESW <- merge(all_birds.weight.select, bird.env.esw, 
                                 by = c("Unique_Checklist_ID", "Common_Name"), all.y = TRUE)

columns_to_check <- c("SAMP_EFFORT", "REL_SAMP_EFFORT", "Occur")

print(nrow(all_birds.weight.env.fixed))
all_birds.weight.env.fixed <- all_birds.weight.env.fixed[!rowSums(is.na(all_birds.weight.env.fixed[, columns_to_check])), ]
print(nrow(all_birds.weight.env.fixed))
print(names(all_birds.weight.env.fixed))

print(nrow(all_birds.weight.env.noRadius))
all_birds.weight.env.noRadius <- all_birds.weight.env.noRadius[!rowSums(is.na(all_birds.weight.env.noRadius[, columns_to_check])), ]
print(nrow(all_birds.weight.env.noRadius))

print(nrow(all_birds.weight.env.ESW))
all_birds.weight.env.ESW <- all_birds.weight.env.ESW[!rowSums(is.na(all_birds.weight.env.ESW[, columns_to_check])), ]
print(nrow(all_birds.weight.env.ESW))

#' Rename columns for future input in assigning blocks
##-----------------------------------------------------

  # No radius
all_birds.weight.env.noRadius <- cbind(all_birds.weight.env.noRadius, str_split(all_birds.weight.env.noRadius$Unique_Checklist_ID, pattern = "_", simplify = TRUE))

  # Fixed radius
all_birds.weight.env.fixed <- cbind(all_birds.weight.env.fixed, str_split(all_birds.weight.env.fixed$Unique_Checklist_ID, pattern = "_", simplify = TRUE))

  # Effective Strip Width
all_birds.weight.env.ESW <- cbind(all_birds.weight.env.ESW, str_split(all_birds.weight.env.ESW$Unique_Checklist_ID, pattern = "_", simplify = TRUE))

columns_to_use <- c("Common_Name","REL_SAMP_EFFORT","x","y","month","day",
                    "EVI_median","LST_Day_median","Occur","QA_PIXEL_median","SR_B1_median","SR_B2_median",
                    "SR_B3_median","SR_B4_median","SR_B5_median","SR_B7_median",
                    "ppt_median","tmean_median")
                    
all_birds.weight.env.noRadius.selectedcol <- subset(all_birds.weight.env.noRadius, select = columns_to_use)
all_birds.weight.env.fixed.selectedcol <- subset(all_birds.weight.env.fixed, select = columns_to_use)
all_birds.weight.env.ESW.selectedcol <- subset(all_birds.weight.env.ESW, select = columns_to_use)
#' 
#' Building BRT models
#' Let's divide dataset into `training` (80%) and `testing` (20%). The selected rows will be applied to all methods (fixed, no-radius, ESW)
## -----------------------------------------------------------------------------
# Initialize parallel backend
num_cores <- detectCores()
cat("\n", num_cores, " num_cores")
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Parallelized loop
all_birds_results <- foreach(i = unique(all_birds.weight.env.noRadius.selectedcol$Common_Name), .packages = c("dynamicSDM", "terra", "dismo", "gbm", "data.table")) %dopar% {
    
        cat(paste0(i, "\n"), file="/nfs/stak/users/stanleyf/hpc-share/Ecology/Scripts/species_brt.txt",append=TRUE)

        # Set seed to make our partition reproducible
        set.seed(123)

        varNames = c("SR_B1_median", "SR_B2_median", "SR_B3_median", "SR_B4_median", "SR_B5_median", "SR_B7_median", "QA_PIXEL_median")

        oregon.extent <- data.frame(lat = c(41.99305, 46.23474),
                                    lon = c(-124.5276, -116.6899))
        oregon.raster <- rast(oregon.extent, type = "")

        per_bird.weight.env.fixed <- all_birds.weight.env.fixed.selectedcol[all_birds.weight.env.fixed.selectedcol$Common_Name == i, ]
        per_bird.weight.env.noRadius <- all_birds.weight.env.noRadius.selectedcol[all_birds.weight.env.noRadius.selectedcol$Common_Name == i, ]
        per_bird.weight.env.ESW <- all_birds.weight.env.ESW.selectedcol[all_birds.weight.env.ESW.selectedcol$Common_Name == i, ]

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

        block.fixed <- na.omit(block.fixed)
        block.noRadius <- na.omit(block.noRadius)
        block.ESW <- na.omit(block.ESW)
        
        #=== Train BRT Model ====#
        #===For fixed radius====#

        # Write.csv for debugging purposes

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
        saveRDS(fixed.brt, file = paste0(base_dir, "/TrainedModel/SpeciesCentered_FixedRadius/", i, ".rds"))
        saveRDS(noRadius.brt, file = paste0(base_dir, "/TrainedModel/SpeciesCentered_NoRadius/", i, ".rds"))
        saveRDS(ESW.brt, file = paste0(base_dir, "/TrainedModel/SpeciesCentered_ESW/", i, ".rds"))

        # Specify paths to save test data /nfs/stak/users/stanleyf/hpc-share/Ecology/Data/TestData
        fname.fixed <- paste0(base_dir, "/TestData/SpeciesCentered_FixedRadius.csv")
        write.table(fixed.test, file = fname.fixed, sep = ",",
                    append = TRUE, col.names = !file.exists(fname.fixed), row.names = FALSE)

        fname.noRadius <- paste0(base_dir, "/TestData/SpeciesCentered_noRadius.csv")
        write.table(noRadius.test, file = fname.noRadius, sep = ",",
                    append = TRUE, col.names = !file.exists(fname.noRadius), row.names = FALSE)

        fname.esw <- paste0(base_dir, "/TestData/SpeciesCentered_ESW.csv")
        write.table(esw.test, file = fname.esw, sep = ",",
                    append = TRUE, col.names = !file.exists(fname.esw), row.names = FALSE)

}

# Close progress bar and Stop cluster
stopCluster(cl)
#'
