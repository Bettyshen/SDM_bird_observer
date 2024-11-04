
#The goal of this script is to build detection function for all species

#===Set working directory and load libraries===#
setwd("/nfs/stak/users/shenf/hpc-share") #set working directory
library(readr)
library(stringr)
install.packages(c("mrds","Distance","dplyr","coda","knitr","futile.logger","stringi"),repos = "http://cran.us.r-project.org")
library(stringi)
library(futile.logger)
library(mrds)
library(Distance)
library(dplyr)
library(coda)
library(parallel)
library(knitr)
flog.threshold(INFO)  # Set the logging threshold to INFO level

#****Load Oregon2020 Data***====#

oregon2020 <- read.csv(file="/nfs/stak/users/shenf/hpc-share/oregon2020_full_prepped_090120.csv", header=TRUE,sep=",",dec=".", fill=TRUE, fileEncoding = "Windows-1252")

zero <- read.csv(file = "/nfs/stak/users/shenf/hpc-share/oregon2020_counts_zerofilled_090120.csv", header=TRUE,sep=",",dec=".", fill=TRUE, fileEncoding = "Windows-1252")
#====Load Bird Species Selection====#
bird_select <- read.csv(file = "/nfs/stak/users/shenf/hpc-share/Oregon2020_bird_freq.csv",
                        header = TRUE, sep = ",", dec = ".", fill = TRUE, fileEncoding = "Windows-1252")
bird_select <- as.data.frame(unique(bird_select[complete.cases(bird_select),]))


#***List all species***=====#

sp.Ore <- as.data.frame(unique(oregon2020$Common_Name))
sp.Ore <- as.data.frame(sp.Ore[complete.cases(sp.Ore),])
colnames(sp.Ore)[1] <- "Common_Name"

#***Change Bird Name for Oregon 2020**=#

# Change Pacific-slope Flycatcher + Pacific-slope/Cordilleran Flycather + Cordilleran Flycatcher to Western Flycatcher
oregon2020$Common_Name <-stri_replace_all_regex(oregon2020$Common_Name, 
                                                pattern = c("Pacific-slope Flycatcher", "Pacific-slope/Cordilleran Flycatcher","Cordilleran Flycatcher"), 
                                                replacement = "Western Flycatcher",
                                                vectorize = FALSE)

# Change Yellow-rumped Warbler to Yellow-rumped Warbler (Audubon's)
oregon2020$Common_Name[oregon2020$Common_Name == "Yellow-rumped Warbler"] <- "Yellow-rumped Warbler (Audubon's)"

sp.Ore.new <- as.data.frame(unique(oregon2020$Common_Name))


#**** Change Bird Name for zero-filled Oregon 2020 ***#

# Change Pacific-slope Flycatcher + Pacific-slope/Cordilleran Flycather + Cordilleran Flycatcher to Western Flycatcher
zero$Common_Name <- stri_replace_all_regex(zero$Common_Name,
                                           pattern = c("Pacific-slope Flycatcher", "Pacific-slope/Cordilleran Flycatcher","Cordilleran Flycatcher"), 
                                           replacement = "Western Flycatcher",
                                           vectorize = FALSE)

# Change Yellow-rumped Warbler to Yellow-rumped Warbler (Audubon's)
zero$Common_Name[zero$Common_Name == "Yellow-rumped Warbler"] <- "Yellow-rumped Warbler (Audubon's)"


#**Only Select the bird species we want to work on**#
bird.sp.select <- bird_select[bird_select$Include== "Y",]

#=== For Oregon 2020 Data ===#

oregon2020 <- oregon2020[oregon2020$Common_Name %in% bird.sp.select$Common_Name,]

#=== For zero-filled data ====#

zero <- zero[zero$Common_Name %in% bird.sp.select$Common_Name,]


#****Transform to data format that includes distance, zero counts***#

  #===Keep the columns we want from Oregon 2020 before merging with zero-filled data===#
oregon2020.bird <- oregon2020[,c("Distance","Location_Name","Common_Name","Unique_Checklist_ID")]

  #=== Split into two sub dataset to add distance information ===#
  #****===For occurrence = 0 ===***#
oregon2020.zero <- zero[zero$Occur == 0,]
  #=== Add column with distance 0 for on each row ===#
distance.zero <- as.data.frame(matrix(rep(NA), nrow = nrow(oregon2020.zero), ncol = 1))
colnames(distance.zero)[1] <- "Distance"

oregon2020.zero <- cbind(oregon2020.zero, distance.zero)
  #== Add another column for location name ==#
oregon2020.zero$Location_Name <- oregon2020.zero$Unique_Checklist_ID

  #***===For occurrence = 1 ===***#
oregon2020.one <- zero[zero$Occur == 1,]

  #=== Create another unique checklist ID adding bird name ===#
    # For Occurrence = 1
oregon2020.one$birdID <- paste(oregon2020.one$Unique_Checklist_ID, oregon2020.one$Common_Name, sep = "_")

    # For the original bird data
oregon2020.bird$birdID <- paste(oregon2020.bird$Unique_Checklist_ID, oregon2020.bird$Common_Name, sep = "_")

  #=== Merge distance information from the lark data ===#
oregon2020.one.merge <- merge(oregon2020.one, oregon2020.bird, by = "birdID", all.x = TRUE)
colnames(oregon2020.one.merge)[5] <- "Common_Name"
colnames(oregon2020.one.merge)[2] <- "Unique_Checklist_ID"
oregon2020.one.merge.uni <- unique(oregon2020.one.merge)


  #====Merge occur = 1 & occur = 0 data together===#

oregon2020.all <- rbind(oregon2020.zero[,c("Unique_Checklist_ID","Count","Common_Name","Distance","Location_Name")],oregon2020.one.merge.uni[,c("Unique_Checklist_ID","Count","Common_Name","Distance","Location_Name")])

  #====See occurrence====#
table(oregon2020.one$Common_Name)
birdoccur <- as.data.frame(table(oregon2020.one$Common_Name))
bird_freq <- birdoccur[order(birdoccur$Freq, decreasing = TRUE),]


#***Format bird data into Distance format***#
#The area size of Oregon was retrieved from https://www.census.gov/quickfacts/OR 
  #== Create Region Label===#
region <- as.data.frame(matrix("oregon", nrow = nrow(oregon2020.all), ncol = 1))
colnames(region)[1] <- "Region.Label"

  #==Create Effort ====#
  #== Since observers only visit each point once, so the effort will be "1" for each record ==#
effort <- as.data.frame(matrix(seq(1), nrow = nrow(oregon2020.all), ncol = 1))
colnames(effort)[1] <- "Effort"

  #==Create Area (squre miles) (https://www.census.gov/quickfacts/OR) ==#
area <- as.data.frame(matrix(248628.4, nrow = nrow(oregon2020.all), ncol = 1))
colnames(area)[1] <- "Area"

  #==Combine the columns===#
oregon.all <- cbind(region, area, oregon2020.all["Location_Name"], effort, oregon2020.all["Distance"], oregon2020.all["Common_Name"])
colnames(oregon.all)[5] <- "distance"
colnames(oregon.all)[3] <- 'Sample.Label'
colnames(oregon.all)[6] <- 'species'

  #== Create object column for abundance estimation==#
  # Assign sequence numbers to rows with numerical distance
oregon.all<- oregon.all %>%
  mutate(object = row_number())
  # Re-order the rownames
  #rownames(oregon.all) <- c(1: nrow(oregon.all))

oregon.all.new <- oregon.all[!is.na(oregon.all$Sample.Label),]

  #The `sample_table` data frame needs to know the "names" of each stratum (here - oregon) and the names of the transects (location name) and the stratum to which each transect belongs.
oregonRegion <- data.frame(Region.Label= "oregon", Area= 248628.4)
oregonRegion[2] <- lapply(oregonRegion[2], as.numeric)

oregonSample <- unique(oregon.all.new[c(1,3,4)])

#***Run Distance function***#
  #*#First ensure the correct unit to what we want
  #The order for input: distance_units; effort_units; area_units
conversion <- convert_units("meter", NULL , "square kilometer")

  #====Create a function to run different detection functions ===#
fit.hn.uni.haz <- function(data, trunc, print=TRUE) {
  #  Purpose:  fit three key functions to transect data, 
  #            perform model selection and
  #            print model selection table
  #  Input: data to analyse, truncation distance, print flag
  #  Output: fitted model object (class `dsmodel`)
  #  Rexstad August 2018
  hn.herm <- ds(data, trun=trunc, key="hn", adj="herm", transect="point",
                convert_units = conversion, region_table = oregonRegion, sample_table = oregonSample)
  uni.cos <- ds(data, trun=trunc, key="unif", adj="cos", transect="point",
                convert_units = conversion, region_table = oregonRegion, sample_table = oregonSample)
  haz.simp <- ds(data, trun=trunc, key="hr", adj="poly", transect="point",
                 convert_units = conversion, region_table = oregonRegion, sample_table = oregonSample)
  mods <- summarize_ds_models(hn.herm, uni.cos, haz.simp, output="plain")
  if(print) print(knitr::kable(mods))
  names(mods) <- c("mod","key","form","fit","pa","sepa","daic")
  if(mods[1,1]=="hn.herm") {
    result <- hn.herm
  } else {
    if(mods[1,1]=="uni.cos") {
      result <- uni.cos
    } else {
      result <- haz.simp
    }
  }  
  return(result)
}

 #**Run birds in detection function**#
 
species_list <- bird_freq$Var1
species_list <- head(species_list, 228) # Extract species names as a vector

#species_list <- head(bird_freq$Var1,5) # Extract species names as a vector

print(species_list)


num_cores <- detectCores()
cl <- makeCluster(num_cores)
# Export the fit.hn.uni.haz function and other required libraries
clusterExport(cl, c("fit.hn.uni.haz", "oregon.all.new", "species_list", "oregonRegion", "oregonSample", "conversion"))

# Define a function for parallel execution
fit_and_plot_species <- function(species_name) {
  # Load necessary libraries and data within the parallel worker
  library(parallel)
  library(mrds)
  library(Distance)
  library(knitr)
  library(futile.logger)  # Load the futile.logger library within the worker
  
  # Log that you're processing a specific species
  flog.info(paste("Processing species:", species_name))
  
  tryCatch({
    # Run the function as before
    best.model <- fit.hn.uni.haz(oregon.all.new[oregon.all.new$species == species_name, ],
                                 trunc = "10%", print = FALSE)
    # Calculating the effective strip width, m =  w (truncation distance) * P (detection probability), (Buckland et al. 2015:10)
    dis_sp <- oregon.all.new[oregon.all.new$species == species_name, ]
    detection.pro <- best.model$ddf$fitted[[1]]
    max_distance <- max(dis_sp$distance, na.rm = TRUE) # Calculate max distance
    
    if (!is.na(max_distance)) {
      w <- 0.90 * max_distance  # Right truncation unit (m)
      effectiveRadius <- detection.pro * w
      effectiveRadius <- as.numeric(round(effectiveRadius, 4))
      
      # Add effectiveRadius to the best model
      best.model$effectiveRadius <- effectiveRadius
      
      # Calculating confidence interval for effective strip width. 
      #== Uncertainty Estiamte ==#
      uncer <- dht2(best.model, flatfile = oregon.all.new, convert_units = conversion, strat_formula = ~Region.Label)
      #== Calculate standard deviation (SD) ==#
      #== Get sample size (n) ==#
      n <- uncer$n
      #=== Transfer SE to SD ====#
      #== Get SE of perceptibility ==#
      bird.summary <- summary(best.model$ddf) 
      se <- as.numeric(bird.summary$average.p.se)
      #== Calculate SD ==#
      sd <- se*sqrt(n)
      #== Calculate margin of error, which depends on the degree of confidence by user, vary between 90%-99.9% ==#
      margin <- qt(0.975, df=n-1)*sd/sqrt(n)  
      
      lowerinterval <- detection.pro - margin
      upperinterval <- detection.pro + margin
      
      #== Calculate Confidence Interval of Effective Strip Width (esw)
      u.esw <- w*upperinterval
      l.esw <- w*lowerinterval
      
      # Add upper and lower effective strip width confidence interval to the best model
      best.model$u.esw <- u.esw
      best.model$l.esw <- l.esw
      
      # Print out effective strip width information
      cat(paste(species_name, max_distance, detection.pro, effectiveRadius, u.esw, l.esw, sep = ","), file = "effectiveSW_info.csv", "\n", sep = ",", append = TRUE)
      
      #===Plot====#
      plot(best.model,
           main = paste("Oregon 2020, species ", species_name,
                        "\nD-hat=", round(best.model$dht$individuals$D$Estimate, 4),
                        "SE=", round(best.model$dht$individuals$D$se, 4),
                        "\nAverage p =", round(detection.pro, 4),
                        "\n", best.model$ddf$name.message,
                        "\nEffective Strip Width:", best.model$effectiveRadius))
      
      # Log the "Finished processing species" message
      flog.info(paste("Finished processing species:", species_name, sep = ","))
      
      # Write the species name to a log file for successful species
      cat(paste("Finished processing species:", species_name, sep = ","), file = "species_log.csv", "\n", sep = ",", append = TRUE)
      
      # Return the best.model or any other relevant results
      return(best.model)
    } else {
      # Log an error if there are NA values in distance
      flog.error("Error: NA values found in distance column.")
      
      # Write an error message to the log file for failed species
      cat(paste("Error processing species:", species_name, ": NA values found in distance column."), file = "species_dist.csv", "\n", sep = ",", append = TRUE)
    }
  }, error = function(err) {
    # Log the error message using futile.logger
    flog.error(paste("Error processing species:", species_name, ":", sep = ",", conditionMessage(err)))
    
    # Write the error message and species name to a log file for failed species
    cat(paste("Error processing species:", species_name, ":", sep = ",", conditionMessage(err)), file = "species_log.csv", "\n", sep = ",", append = TRUE)
  })
}
# Run the function in parallel for all species
results <- parLapply(cl, species_list, fit_and_plot_species)

#=====Processing species results======#

sp_status <- read.csv("/Volumes/T7/eBird_Oregon2020/distance/Distance-Sampling/species_log.csv", header = FALSE, sep = ",")

# Successful Species
sp_success <- sp_status[sp_status$V1 == "Finished processing species:", ]
sp_success_list <- as.factor(sp_success$V2)
# Failed Species
sp_failed <- sp_status[sp_status$V1 == "Error processing species:", ]
sp_failed_list <-as.factor(sp_failed$V2)




#***Plot detection function plot for each bird***#
#*# Locate where to save plots
png_dir <- "/nfs/stak/users/shenf/hpc-share/DetectionFunctionPlot"

# Create a directory for PNG files if it doesn't exist
if (!dir.exists(png_dir)) {
  dir.create(png_dir)
}

# Split the results into groups of 10 species each
num_species <- length(species_list) # Species count
num_plots_per_file <- 10 # Constrain the number of species per PNG file
num_files <- ceiling(num_species / num_plots_per_file) # Count the total PNG files we will produce

# Create and export PNG files
# Iterates through the number of files needed to export the plots
for (i in 1:num_files) {
  # Calculate the starting and ending indices for the species to be included in the current file. The purpose is to ensure that each file contains a subset of the total species we have to plot.
  start_idx <- (i - 1) * num_plots_per_file + 1 # First species to include in the current file
  end_idx <- min(i * num_plots_per_file, num_species) # The last species to include in the current file.
  
  if (start_idx <= end_idx) {
    
    print(paste("Plotting species from", start_idx, "to", end_idx))
    png_file <- file.path(png_dir, paste0("bird_plots_", i, ".png"))
    png(png_file, width = 300, height = 500, units = "mm", res = 300)
    par(mfrow = c(5, 2))  # Visualize # of graphs at once
    
    # Iterates through the species within the current file. It starts from start_idx and goes up to end_idx, which ensures that only the species assigned to the current file are processed.
    # Iterate through the species within the current file.
    for (j in start_idx:end_idx) {
      if (j <= length(results)) {
        # Get the current species name [j] from species_list
        species_name <- species_list[j]
        
        # Check if the species is in the list of failed species
        if (!(species_name %in% sp_failed_list)) {
          print(paste("Processing species", species_name))
          
          # Capture the results for the current species [j]
          current_result <- results[[j]]
          
          # Plot the results and add a legend with the species name
          plot(current_result,
               main = paste("Oregon 2020, species ", species_name,
                            "\nD-hat=", round(current_result$dht$individuals$D$Estimate, 4),
                            "SE=", round(current_result$dht$individuals$D$se, 4),
                            "\nAverage p =", round(current_result$ddf$fitted[[1]], 4),
                            "\n", current_result$ddf$name.message,
                            "\nEffective Strip Width", current_result$effectiveRadius),
               cex.axis = 2, # Adjust the font size as needed
               cex.lab = 1.7)  # Adjust the font size as needed
          
          # Add a legend with the species name
          legend("topright", legend = species_name, bty = "n", cex = 2)
        } else {
          print(paste("Skipping plotting for failed species", species_name))
        }
      }
    }
    
    dev.off()  # Close the PNG device
  }
}

# Make sure to stop the cluster at the end
stopCluster(cl)
