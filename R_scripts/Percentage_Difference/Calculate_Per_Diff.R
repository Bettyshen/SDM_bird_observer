---
title: "evaMetric_compare"
output: html_document
date: "2024-08-28"
---

[Goal] - plot the evaluation metric differences (%) from observer-location and species-location results from all species + generalists vs. specialists

Load library
```{r}
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)
```

Read both observer-location & species-location evaluation metrics outcome
```{r}
species.result <- read.csv("/Volumes/T7/eBird_Oregon2020/distance/Distance-Sampling/Evaluation/Evaluation_Metric/evalMetric-SpeciesLocation_Deviance.csv")

observer.result <- read.csv("/Volumes/T7/eBird_Oregon2020/distance/Distance-Sampling/Evaluation/Evaluation_Metric/evalMetric-ObserverLocationDeviance.csv")
```

Import habitat generalists vs. specialists list
```{r}
generSpecia <- read.csv("/Volumes/T7/eBird_Oregon2020/distance/Distance-Sampling/Oregon2020_bird_specialization.csv", fileEncoding = "Windows-1252")
colnames(generSpecia)[1] <- "Species"
generSpecia <- generSpecia[complete.cases(generSpecia), ]

# Select species to include
generSpecia <- generSpecia[generSpecia$Include == "Y",]

```


Merge results together
```{r}
com.result <- rbind(species.result, observer.result)

 # Merge with habitat specialists and generalists information
all.result <- merge(generSpecia, com.result, by = "Species", all.x = TRUE)
all.result.na <- all.result[complete.cases(all.result$Extraction_Approach),]
all.result.final <- all.result.na[complete.cases(all.result.na),]
print(all.result.final)
```

Shorten the approach names
```{r}
head(all.result.final)

# Remove " Approach" from Extraction_Approach
all.result.final <- all.result.final %>%
  mutate(Extraction_Approach = sub(" Approach", "", Extraction_Approach))

```

Organize reporting order
```{r}
# Assuming your data is named `df`
all.results.organi <- all.result.final %>%
  mutate(
    Metric_Order = case_when(
      Metric == "deviance" & Extraction_Approach == "Species-location" ~ 1,
      Metric == "deviance" & Extraction_Approach == "Observer-location" ~ 2,
      Metric == "Kendall" & Extraction_Approach == "Species-location" ~ 3,
      Metric == "Kendall" & Extraction_Approach == "Observer-location" ~ 4,
      Metric == "RMSE" & Extraction_Approach == "Species-location" ~ 5,
      Metric == "RMSE" & Extraction_Approach == "Observer-location" ~ 6,
      TRUE ~ 999 # Default value for unlisted combinations
    )
  ) %>%
  arrange(Species, Metric_Order) %>%
  select(-Metric_Order) # Remove helper column if not needed

# Round digits
all.results.organi <- all.results.organi %>%
  mutate(
    Pixel_radius = round(Pixel_radius, 3),
    Fixed_Radius = round(Fixed_Radius, 3),
    Effective_Radius = round(Effective_Radius, 3)
  )

# View the result
print(head(all.results.organi))

# Delete the specialists & include columns
all.results.organi <- all.results.organi[,-c(2,3)]

```

Export final results
```{r}
write.table(all.results.organi, file = "/Volumes/T7/eBird_Oregon2020/distance/Distance-Sampling/Evaluation/Evaluation_Metric/evaluation_metric_all.csv", sep = ",", row.names = FALSE)

```

Filter out species that have both bird-location and observer-location evaluation metrics across all radii
```{r}
all.result.final <- all.result.final[all.result.final$Species %in% species_list$Species, ]

```


Summarize median of bird-location and observer-location by habitat characterizations
```{r}
# Filter for relevant metrics 
metrics_data <- all.result.final %>% 
  filter(Metric %in% c("deviance", "Kendall", "RMSE"))

# Gather all radii types into a single column
metrics_long <- metrics_data %>%
  pivot_longer(cols = c(Pixel_radius, Fixed_Radius, Effective_Radius),
               names_to = "Radius_Type", values_to = "Radius_Value")

# Group by Assessment_Approach, Metric, and calculate the median for the unified radius values
median_metrics_combined <- metrics_long %>%
  group_by(Extraction_Approach, Metric) %>%
  summarize(Median_Radius = round(median(Radius_Value, na.rm = TRUE),3))

# View the summarized dataframe
print(median_metrics_combined)

```

Summarize median of bird-location and observer-location by habitat characterizations and radius
```{r}

# Group by Assessment_Approach, Metric, and calculate the median for the unified radius values
median_metrics_combined_radii <- metrics_long %>%
  group_by(Extraction_Approach, Metric, Radius_Type) %>%
  summarize(Median_Radius = round(median(Radius_Value, na.rm = TRUE),3))

# View the summarized dataframe
print(median_metrics_combined_radii)

```

Summarize range of evaluation metrics
```{r}
head(metrics_long)
range_metrics <- metrics_long %>%
  group_by(Extraction_Approach, Metric) %>%
  summarise(
    Min_Value = round(min(Radius_Value),3),
    Max_Value = round(max(Radius_Value),3),
    Range_Value = round(Max_Value - Min_Value,3),
    median_Value = round(median(Radius_Value, na.rm = TRUE),3), 
    .groups = "drop"
  )

print(range_metrics)

```

Summarize median of habitat generalists and specialists
```{r}
# Gather all radii types from habitat generalists and specialists
metrics_SpGen <- metrics_long %>%
  group_by(Specialist, Metric, Extraction_Approach) %>%
  summarize(Median_Radius = round(median(Radius_Value, na.rm = TRUE),3))

```



Reformat dataframe
```{r}
 # Extract individual metric
    #===== deviance ======#
deviance <- all.result.final[all.result.final$Metric == "deviance",]

# Pivot the dataframe to longer format
deviance_long <- deviance %>%
  pivot_longer(cols = c(Pixel_radius, Fixed_Radius, Effective_Radius),
               names_to = "Radii",
               values_to = "deviance") %>%
  arrange(Species, Extraction_Approach, Specialist)

# View the final dataframe
print(deviance_long)

   #======= Kendall ========#
Kendall <-  all.result.final[all.result.final$Metric == "Kendall",]

# Pivot the dataframe to longer format
Kendall_long <- Kendall %>%
  pivot_longer(cols = c(Pixel_radius, Fixed_Radius, Effective_Radius),
               names_to = "Radii",
               values_to = "Kendall") %>%
  arrange(Species, Extraction_Approach, Specialist)

# View the final dataframe
print(Kendall_long)

   #======= Root mean square error (RMSE) ==========#
RMSE <- all.result.final[all.result.final$Metric == "RMSE",]

# Pivot the dataframe to longer format
RMSE_long <- RMSE %>%
  pivot_longer(cols = c(Pixel_radius, Fixed_Radius, Effective_Radius),
               names_to = "Radii",
               values_to = "RMSE") %>%
  arrange(Species, Extraction_Approach, Specialist)

# View the final dataframe
print(RMSE_long)
```


Statistics of evaluation metric - all species
```{r}
# Function to summarize evaluation metrics for each approach and radii
metricSummaryAll <- function(data, metric_col_name) {
  
  # Unique assessment approaches and radii
  approaches <- unique(data$Extraction_Approach)
  
  # Empty list to store summary results
  summary_list <- list()
  
  # Loop through each approach
  for (approach in approaches) {
    
    # Filter the data for the current approach
    data_filtered <- data %>%
      filter(Extraction_Approach == approach)
    
    # Group by Radii and calculate median and mean for each using dynamic column
    summary_table <- data_filtered %>%
      group_by(Radii) %>%
      summarize(
        Median = round(median(.data[[metric_col_name]], na.rm = TRUE),3),  # Dynamic metric column
        Mean = mean(.data[[metric_col_name]], na.rm = TRUE)       # Dynamic metric column
      )
    
    # Add additional columns to identify the approach and metric
    summary_table <- summary_table %>%
      mutate(Approach = approach, Metric = metric_col_name)
    
    # Store the result
    summary_list[[paste(approach, metric_col_name)]] <- summary_table
  }
  
  # Combine all the summary results into one data frame
  final_summary <- bind_rows(summary_list)
  
  # Reorder the columns for a nicer output
  final_summary <- final_summary %>%
    select(Approach, Metric, Radii, Median, Mean)
  
  return(final_summary)
}

# Call the function for each dataframe with its respective metric column name
summary_deviance <- metricSummaryAll(deviance_long, "deviance")
summary_Kenall <- metricSummaryAll(Kendall_long, "Kendall")
summary_RMSE <- metricSummaryAll(RMSE_long, "RMSE")

# Combine all summaries into a single table
final_summary <- bind_rows(summary_deviance, summary_Kenall, summary_RMSE)

# View the result
print(final_summary)


```

Create a function to perform percentage difference of evaluation metric (calculate average across 6 models)
```{r}
calculate_mean_diff <- function(data, metric_col) {
  # Calculate the mean of the metric for each species
  metric_mean <- data %>%
    group_by(Species) %>%
    summarise(meanMetric = mean(.data[[metric_col]], na.rm = TRUE))
  
  # Merge back to the original dataset
  data_merge <- merge(metric_mean, data, by = "Species", all.x = TRUE)
  
  # Subtract the metric value from the mean and calculate the percentage difference
  data_merge <- data_merge %>%
    mutate(
      diffMetric = .data[[metric_col]] - meanMetric,
      meanMetricDiff = (diffMetric / meanMetric) * 100
    )
  
  # Return the updated dataset
  return(data_merge)
}

```

Apply function to Deviance, Kendall, RMSE
```{r}
Deviance_long_merge <- calculate_mean_diff(deviance_long, metric_col = "deviance")
Kendall_long_merge <- calculate_mean_diff(Kendall_long, metric_col = "Kendall")
RMSE_long_merge <- calculate_mean_diff(RMSE_long, metric_col = "RMSE")

```

Create a function to calculate percentage differences of evaluation metric pariwise (observer-location radius vs. bird-location radius)
```{r}
# Calculate percentage difference
calculate_diff_radii <- function(data, metric_col){
percentage_diff <- data %>%
  select(Species, Radii, Extraction_Approach, metric_col) %>%
  pivot_wider(names_from = Extraction_Approach, values_from = metric_col) %>%
  mutate(Percentage_Difference = ((`Species-location` - `Observer-location`) / `Observer-location`) * 100)
}

```

Apply function to Deviance, Kendall, RMSE
```{r}
Deviance_radii_diff <- calculate_diff_radii(deviance_long, metric_col = "deviance")
Kendall_radii_diff <- calculate_diff_radii(Kendall_long, metric_col = "Kendall")
RMSE_radii_diff <- calculate_diff_radii(RMSE_long, metric_col = "RMSE")

# Reorder the levels of Radii
Deviance_radii_diff <- Deviance_radii_diff %>%
  mutate(Radii = factor(Radii, levels = c("Fixed_Radius", "Pixel_radius", "Effective_Radius")))

Kendall_radii_diff <- Kendall_radii_diff %>%
  mutate(Radii = factor(Radii, levels = c("Fixed_Radius", "Pixel_radius", "Effective_Radius")))

RMSE_radii_diff <- RMSE_radii_diff %>%
  mutate(Radii = factor(Radii, levels = c("Fixed_Radius", "Pixel_radius", "Effective_Radius")))

# Summary of the metric
summary(Deviance_radii_diff$Percentage_Difference)
summary(Kendall_radii_diff$Percentage_Difference)
summary(RMSE_radii_diff$Percentage_Difference)

```

Look up which species has NA in either observer- or species-location model evaluation
& Choose species that have both evaluation metrics
```{r}
species.works <- Deviance_radii_diff[complete.cases(Deviance_radii_diff),]
species_list <- data.frame(unique(species.works$Species))
colnames(species_list)[1] <- "Species"
write.csv(species_list, "/Volumes/T7/eBird_Oregon2020/distance/Distance-Sampling/Evaluation/Evaluation_Metric/species_list.csv", row.names = FALSE, fileEncoding = "UTF-8" )
```

Summarize median percentage difference of bird-location and observer-location by habitat characterizations and radius
```{r}

# Group by Assessment_Approach, Metric, and calculate the median for the unified radius values
median_metrics_perce_radii <- function (data) {
  data %>%
  group_by(Radii) %>%
  summarize(Median_Percent = round(median(Percentage_Difference, na.rm = TRUE),3))
}

# View the summarized dataframe
  # Deviance
median_metrics_perce_radii(Deviance_radii_diff)

  # Kendall
median_metrics_perce_radii(Kendall_radii_diff)

  # Root mean square error
median_metrics_perce_radii(RMSE_radii_diff)

```

Summarize % of species in species-location models outperformed observer-location model
```{r}

# Create function to calculate for Deviance & RMSE
cal_per_bird_obs <- function (data, radius) {
   # Ensure column types are consistent
  data$Radii <- as.character(data$Radii)
  data.radius <- data[data$Radii == radius, ]
  # Remove rows with NA in Percentage_Difference
  data.radius <- data.radius[!is.na(data.radius$Percentage_Difference), ]
  greaterPer <- data.radius[data.radius$Percentage_Difference < 0,]
  return(greaterPer)
}

# Deviance
  # Fixed radius (63 species)
devFixedOutPerBird <- cal_per_bird_obs(Deviance_radii_diff, "Fixed_Radius")
nrow(devFixedOutPerBird)
  # Pixel radius (54 species)
devPixelOutPerBird <- cal_per_bird_obs(Deviance_radii_diff, "Pixel_radius")
nrow(devPixelOutPerBird)

# RMSE 
  # Fixed Radius (54 species)
RMSEFixedOutPerBird <- cal_per_bird_obs(RMSE_radii_diff, "Fixed_Radius")
nrow(RMSEFixedOutPerBird)

  # Pixel radius (51 species)
RMSEPixelOutPerBird <- cal_per_bird_obs(RMSE_radii_diff, "Pixel_radius")
nrow(RMSEPixelOutPerBird)

  # Effective radius (47 species)
RMSEEffectiveOutBird <- cal_per_bird_obs(RMSE_radii_diff, "Effective_Radius")
nrow(RMSEEffectiveOutBird)


# Create function to calculate for Kendall's
cal_per_bird_obs_Kendall <- function (data, radius) {
   # Ensure column types are consistent
  data$Radii <- as.character(data$Radii)
  data.radius <- data[data$Radii == radius, ]
  # Remove rows with NA in Percentage_Difference
  data.radius <- data.radius[!is.na(data.radius$Percentage_Difference), ]
  greaterPer <- data.radius[data.radius$Percentage_Difference > 0,]
  return(greaterPer)
}

# Kendall's
  # Fixed Radius (101) = 70%
nrow(cal_per_bird_obs_Kendall(Kendall_radii_diff, "Fixed_Radius"))
  # Pixel radius (102) = 70%
nrow(cal_per_bird_obs_Kendall(Kendall_radii_diff, "Pixel_radius"))
  # Effective radius (101) = 70%
nrow(cal_per_bird_obs_Kendall(Kendall_radii_diff, "Effective_Radius"))

head(RMSE_radii_diff_special)
```


Plot box plot of all species (raw evaluation metric)
```{r}
png(filename = "/Volumes/T7/eBird_Oregon2020/distance/Distance-Sampling/Evaluation/Figures/evaMetic-all_deviance.png", width = 2500, height = 4500, res = 300)
  
# Deviance
Deviance_plot <- ggplot(deviance_long, aes(x = Radii, y = deviance, fill = Extraction_Approach)) +
  geom_boxplot() +
  theme_bw() +
  scale_x_discrete(labels = c("Effective Radius", "Fixed Radius", "Pixel Radius")) +
  labs(fill = "Assessment Approach") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15, vjust = 0.2),
        axis.title.y = element_text(size = 15),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.margin = margin(10, 10, 15, 10))

# Kendall
Kendall_plot <- ggplot(Kendall_long, aes(x = Radii, y = Kendall, fill = Extraction_Approach)) +
  geom_boxplot() +
  theme_bw() +
  scale_x_discrete(labels = c("Effective Radius", "Fixed Radius", "Pixel Radius")) +
  labs(fill = "Assessment Approach") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15, vjust = 0.2),
        axis.title.y = element_text(size = 15),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.margin = margin(5, 10, 15, 10))

# RMSE
RMSE_plot <- ggplot(RMSE_long, aes(x = Radii, y = RMSE, fill = Extraction_Approach)) +
  geom_boxplot() +
  theme_bw() +
  scale_x_discrete(labels = c("Effective Radius", "Fixed Radius", "Pixel Radius")) +
  labs(fill = "Assessment Approach") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15, vjust = 0.2),
        axis.title.y = element_text(size = 15),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.margin = margin(5, 10, 10, 10))

#plot.margin = margin(top, right, bottom, left, unit)
all_plots <- ggarrange(Deviance_plot, 
                       Kendall_plot, 
                       RMSE_plot,
          labels = c("A","B","C"),
          ncol = 1,
          nrow = 3)

annotate_figure(all_plots, top = text_grob("All species evaluation metrics comparison of\n observer-location vs. bird-location", 
              hjust = 0.5, vjust = 0.2, color = "black", face = "bold", size = 20)) +
  theme(plot.margin = margin(1,1,1,1, "cm"))

dev.off()

```


Plot box plot of all species (percent difference from the species mean evaluation metric across all radii)
```{r}

# Use a colorblind-friendly palette from palette.colors()
cb_palette <- palette.colors(palette = "Okabe-Ito")[c(3, 7)]  # "Okabe-Ito" 

png(filename = "/Volumes/T7/eBird_Oregon2020/distance/Distance-Sampling/Evaluation/Figures/evaMetic-all_perDiff_deviance.png", width = 2000, height = 4500, res = 300)
  
# Deviance
Deviance_plot_diff <- ggplot(Deviance_long_merge, aes(x = Radii, y = meanMetricDiff, fill = Extraction_Approach)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual(values = cb_palette) + # Apply the custom palette
  labs(fill = "Assessment Approach") +
  ylab("Percent Difference in deviance") +
  theme(
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(face="bold", size = 15),
        #legend.position = "none",
        legend.position = "top",
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12))
        #plot.margin = margin(5, 10, 1, 10)) # margin(top, right, bottom, left, unit)

# Kendall
Kendall_plot_diff <- ggplot(Kendall_long_merge, aes(x = Radii, y = meanMetricDiff, fill = Extraction_Approach)) +
  geom_boxplot() +
  theme_bw() +
  #scale_x_discrete(labels = c("Effective Radius", "Fixed Radius", "Pixel Radius")) +
  scale_fill_manual(values = cb_palette) + # Apply the custom palette
  labs(fill = "Assessment Approach") +
  ylab("Percent Difference in Kendall") +
  theme(
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(face="bold", size = 15),
        legend.position = "none")
        #legend.title = element_text(size = 12),
        #legend.text = element_text(size = 12),
        #plot.margin = margin(1, 10, 1, 10)) # margin(top, right, bottom, left, unit)

# RMSE
RMSE_plot_diff <- ggplot(RMSE_long_merge, aes(x = Radii, y = meanMetricDiff, fill = Extraction_Approach)) +
  geom_boxplot() +
  theme_bw() +
  scale_x_discrete(labels = c("Effective Radius", "Fixed Radius", "Pixel Radius")) +
  scale_fill_manual(values = cb_palette) + # Apply the custom palette
  labs(fill = "Assessment Approach") +
  ylab("Percent Difference in RMSE") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(face="bold", size = 15, vjust = 0.2),
        axis.title.y = element_text(face="bold", size = 15),
        #legend.title = element_text(size = 12),
        #legend.text = element_text(size = 12),
        legend.position = "none")
        #legend.position = "bottom",
        #plot.margin = margin(1, 10, 5, 10)) # margin(top, right, bottom, left, unit)

#plot.margin = margin(top, right, bottom, left, unit)
all_plots <- ggarrange(Deviance_plot_diff, 
                       Kendall_plot_diff, 
                       RMSE_plot_diff,
          #labels = c("a","b","c"),
          align = "h",
          heights = c(1, 1, 1),
          font.label = list(size = 20, color = "black", face = "bold"),
          ncol = 1,
          nrow = 3)
# Annotate the combined plot
annotate_figure(all_plots, top = text_grob("All species percentage differences of\n evaluation metrics comparison between\n observer-location vs. bird-location", 
              hjust = 0.5, vjust = 0.2, color = "black", face = "bold", size = 15)) +
  theme(plot.margin = margin(1,1,1,1, "cm"))

dev.off()

```


Plot box plot of all species (percent difference from radii-pairwise evaluation metric)
Using base R
```{r}
summary(Deviance_radii_diff$Percentage_Difference)

cb_palette <- palette.colors(palette = "Okabe-Ito")[c(2, 3, 7)]  # "Okabe-Ito" 
# Set up the layout and margins
png(filename = "/Volumes/T7/eBird_Oregon2020/distance/Distance-Sampling/Evaluation/Figures/evaMetric-all_perDiff_radii_deviance.png", 
    width = 1800, height = 3500, res = 300)

par(mfrow = c(3, 1),        # 1 row, 3 columns
    mar = c(4 , 7, 2.5, 5),    # Margins for individual plots (bottom, left, top, right)
    oma = c(4, 6, 4, 5))    # Outer margins for the whole plot (bottom, left, top, right)

# Plot Deviance
boxplot(Deviance_radii_diff$Percentage_Difference ~ Deviance_radii_diff$Radii, 
        col = cb_palette, 
        ylab = "Percent Difference\nin Deviance", 
        cex.axis = 1.2,
        cex.lab = 2,
        xlab = "", 
        xaxt = "n",
        ylim = c(-60, 60),
        yaxt = "n",
        whisklty = 1,
        xpd = TRUE)
abline(h = 0, lty = 2, lwd = 2, col = "blue") # lwd = thickness; lty = type
axis(side = 1, at = seq_along(Deviance_radii_diff$Radii), labels = FALSE, tick = TRUE) # customize x axis
axis(side = 2, at = seq(-60, 60, by = 10), labels = seq(-60, 60, by = 10), cex.axis = 1.2) # customize y axis

# Add panel label for Deviance 
text(x = par("usr")[1] - 0.4, 
     y = par("usr")[4], 
     labels = "(a)", 
     xpd = TRUE, 
     cex = 2, 
     font = 2, 
     adj = c(1, 1))

# Add a shared legend at the top
legend("top",                            # Position the legend at the top
       legend = c("Fixed Radius", "Pixel Radius", "Effective Radius"),  # Legend labels
       fill = cb_palette,               # Use the color palette
       horiz = TRUE,                    # Arrange legend items horizontally
       inset = -0.15 ,                    # Move the legend upwards (adjust as needed)
       cex = 1.5,                       # Scale the legend size
       bty = "n",
       xpd = TRUE,
       x.intersp = 0.5,
       text.width = 1 )                       

# Plot Kendall
boxplot(Kendall_radii_diff$Percentage_Difference ~ Kendall_radii_diff$Radii, 
        col = cb_palette, 
        ylab = "Percent Difference\nin Kendall's",
        cex.axis = 1.2,
        cex.lab = 2,
        xlab = "", 
        xaxt = "n",
        whisklty = 1,
        ylim = c(-50, 130))

# Add panel label for Kendall
text(x = par("usr")[1] - 0.4, 
     y = par("usr")[4], 
     labels = "(b)", 
     xpd = TRUE, 
     cex = 2, 
     font = 2, 
     adj = c(1, 1))

abline(h = 0, lty = 2, lwd = 2, col = "blue") # lwd = thickness; lty = type
axis(side = 1, at = seq_along(Kendall_radii_diff$Radii), labels = FALSE, tick = TRUE)
#axis(side = 2, at = seq(-50, 130, by = 10), labels = seq(-50, 130, by = 10), cex.axis = 1.2) # customize x axis

# Plot RMSE

boxplot(RMSE_radii_diff$Percentage_Difference ~ RMSE_radii_diff$Radii, 
        col = cb_palette, 
        ylab = "Percent Difference\nin RMSE", 
        xlab = "Radii", 
        cex.lab = 2,
        ylim = c(-40, 60),
        names = FALSE,
        xpd = TRUE,
        xaxt = "n",
        yaxt = "n",
        whisklty = 1)

# Add panel label for RMSE
text(x = par("usr")[1] - 0.4, 
     y = par("usr")[4], 
     labels = "(c)", 
     xpd = TRUE, 
     cex = 2, 
     font = 2, 
     adj = c(1, 1))

abline(h = 0, lty = 2, lwd = 2, col = "blue") # lwd = thickness; lty = type

axis(side = 1, at = seq_along(RMSE_radii_diff$Radii), labels = FALSE, tick = TRUE)
axis(side = 2, at = seq(-40, 60, by = 20 ), labels = seq(-40, 60, by = 20), cex.axis = 1.2) # customize y axis

# Define label positions and text
label_positions <- seq_along(unique(RMSE_radii_diff$Radii))  
label = c("Fixed", "Pixel", "Effective")

# Add inclined x-axis labels
text(x = label_positions, 
     y = par("usr")[3] - 6  ,  # Position below x-axis
     labels = label, 
     adj = 0.5,                # Right-aligned text
     xpd = TRUE,             # Allow drawing outside plot area
     cex = 1.7 )              # Adjust text size


dev.off()

```


***Habitat generalists***
- Raw evaluation metrics
```{r}
# Extract habitat generalists
Deviance_gener <- deviance_long[deviance_long$Specialist == 0,]
Kendall_gener <- Kendall_long[Kendall_long$Specialist == 0, ]
RMSE_gener <- RMSE_long[RMSE_long$Specialist == 0,]

# Count number of habitat generalist species #78 species
generalist_count <- unique(Deviance_gener$Species)
print(generalist_count)
length(generalist_count)
```

Statistics of evaluation metric - generalists
```{r}

# Call the function for each dataframe with its respective metric column name
summary_Deviance_gener <- metricSummaryAll(Deviance_gener, "deviance")
summary_Kendall_gener <- metricSummaryAll(Kendall_gener, "Kendall")
summary_RMSE_gener <- metricSummaryAll(RMSE_gener, "RMSE")

# Combine all summaries into a single table
final_summary_gener <- bind_rows(summary_Deviance_gener, summary_Kendall_gener, summary_RMSE_gener)

# View the result
print(final_summary_gener)

# Range of bird-location metric
assRange_gene_bird <- function(data, metric){
  data_bird_loc <- data[data$Extraction_Approach == "Species-location",]
  range_metric <- range(data_bird_loc[[metric]], na.rm = TRUE)
  return(range_metric)
}

  # Deviance
assRange_gene_bird(Deviance_gener, "deviance")
  # Kendall
assRange_gene_bird(Kendall_gener, "Kendall")
  # RMSE
assRange_gene_bird(RMSE_gener, "RMSE")

# Range of observer-location metric
assRange_gene_obs <- function(data, metric){
  data_bird_loc <- data[data$Extraction_Approach == "Observer-location",]
  range_metric <- range(data_bird_loc[[metric]], na.rm = TRUE)
  return(range_metric)
}
  # Deviance
assRange_gene_obs(Deviance_gener, "deviance")
  # Kendall
assRange_gene_obs(Kendall_gener, "Kendall")
  # RMSE
assRange_gene_obs(RMSE_gener, "RMSE")
```


Calculate median value of each metric
```{r}
# View the summarized dataframe
 # Deivance
Deviance.medi.gener <- median_metrics_SpGene(Deviance_gener, "deviance")
print(Deviance.medi.gener)
 # Kendall
Kendall.medi.gener <- median_metrics_SpGene(Kendall_gener, "Kendall")
print(Kendall.medi.gener)
 # RMSE
RMSE.medi.gener <- median_metrics_SpGene(RMSE_gener, "RMSE")
print(RMSE.medi.gener)
```


Calculate number of specialists showed better model performance in species-location
```{r}

# Deviance & RMSE
 # Fixed radius  = 44 
nrow(cal_per_bird_obs(Deviance_radii_diff_special[Deviance_radii_diff_special$Specialist ==0,], "Fixed_Radius"))
 # Pixel radius = 44 
nrow(cal_per_bird_obs(Deviance_radii_diff_special[Deviance_radii_diff_special$Specialist ==0,], "Pixel_radius"))


# Kendall's 
  # Fixed radius = 69 
nrow(cal_per_bird_obs_Kendall(Kendall_radii_diff_special[Kendall_radii_diff_special$Specialist ==0,], "Fixed_Radius"))
  # Pixel radius = 72 
nrow(cal_per_bird_obs_Kendall(Kendall_radii_diff_special[Kendall_radii_diff_special$Specialist ==0,], "Pixel_radius"))
```


Plot box plot of habitat generalists (raw evaluation metric)
```{r}
png(filename = "/Volumes/T7/eBird_Oregon2020/distance/Distance-Sampling/Evaluation/Figures/evaMetic-all_deviance_generalists.png", width = 2500, height = 4500, res = 300)
  
# Deviance
Deviance_plot_gener <- ggplot(Deviance_gener, aes(x = Radii, y = deviance, fill = Extraction_Approach)) +
  geom_boxplot() +
  theme_bw() +
  scale_x_discrete(labels = c("Effective Radius", "Fixed Radius", "Pixel Radius")) +
  labs(fill = "Assessment Approach") +
  ylab("Deviance") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15, vjust = 0.2),
        axis.title.y = element_text(size = 15),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.margin = margin(10, 10, 15, 10))

# Kendall
Kendall_plot_gener <- ggplot(Kendall_gener, aes(x = Radii, y = Kendall, fill = Extraction_Approach)) +
  geom_boxplot() +
  theme_bw() +
  ylab("Kendall's") +
  scale_x_discrete(labels = c("Effective Radius", "Fixed Radius", "Pixel Radius")) +
  labs(fill = "Assessment Approach") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15, vjust = 0.2),
        axis.title.y = element_text(size = 15),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.margin = margin(5, 10, 15, 10))

# RMSE
RMSE_plot_gener <- ggplot(RMSE_gener, aes(x = Radii, y = RMSE, fill = Extraction_Approach)) +
  geom_boxplot() +
  theme_bw() +
  scale_x_discrete(labels = c("Effective Radius", "Fixed Radius", "Pixel Radius")) +
  labs(fill = "Assessment Approach") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15, vjust = 0.2),
        axis.title.y = element_text(size = 15),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.margin = margin(5, 10, 10, 10))

#plot.margin = margin(top, right, bottom, left, unit)
all_plots_gener <- ggarrange(Deviance_plot_gener, 
                       Kendall_plot_gener, 
                       RMSE_plot_gener,
          labels = c("A","B","C"),
          ncol = 1,
          nrow = 3) 

annotate_figure(all_plots_gener, top = text_grob("Habitat generalists evaluation metrics comparison of\n observer-location vs. bird-location", 
              hjust = 0.5, vjust = 0.2, color = "black", face = "bold", size = 20)) +
  theme(plot.margin = margin(1,1,1,1, "cm"))

dev.off()

```


***Habitat specialists***
```{r}
# Extract habitat specialists
Deviance_specia <- deviance_long[deviance_long$Specialist == 1,]
Kendall_specia <- Kendall_long[Kendall_long$Specialist == 1, ]
RMSE_specia <- RMSE_long[RMSE_long$Specialist == 1,]

# Count number of habitat specialists species #33 species
specialist_count <- unique(Deviance_specia$Species)
print(specialist_count)

```

Statistics of evaluation metric - specialists
```{r}
# Call the function for each dataframe with its respective metric column name
summary_Deviance_spec <- metricSummaryAll(Deviance_specia, "deviance")
summary_Kendall_spec <- metricSummaryAll(Kendall_specia, "Kendall")
summary_RMSE_spec <- metricSummaryAll(RMSE_specia, "RMSE")

# Combine all summaries into a single table
final_summary_spec <- bind_rows(summary_Deviance_spec, summary_Kendall_spec, summary_RMSE_spec)

# View the result
print(final_summary_spec)

# Range of bird-location metric
assRange <- function(data, metric){
  data_bird_loc <- data[data$Extraction_Approach == "Species-location",]
  range_metric <- range(data_bird_loc[[metric]], na.rm = TRUE)
  return(range_metric)
}

  # Deviance
assRange(Deviance_specia, "deviance")
  # Kendall
assRange(Kendall_specia, "Kendall")
  # RMSE
assRange(RMSE_specia, "RMSE")

# Range of observer-location metric
assRange_obs <- function(data, metric){
  data_bird_loc <- data[data$Extraction_Approach == "Observer-location",]
  range_metric <- range(data_bird_loc[[metric]], na.rm = TRUE)
  return(range_metric)
}
  # Deviance
assRange_obs(Deviance_specia, "deviance")
  # Kendall
assRange_obs(Kendall_specia, "Kendall")
  # RMSE
assRange_obs(RMSE_specia, "RMSE")
```

Calculate median value of each metric
```{r}

# Group by Assessment_Approach, Metric, and calculate the median for the unified radius values
median_metrics_SpGene <- function (data, metric_col) {
  data %>%
  group_by(Extraction_Approach, Metric) %>%
  summarize(Median_Radius = round(median(.data[[metric_col]], na.rm = TRUE),3))
}

# View the summarized dataframe
 # Deivance
Deviance.medi.Specia <- median_metrics_SpGene(Deviance_specia, "deviance")
print(Deviance.medi.Specia)
 # Kendall
Kendall.medi.Specia <- median_metrics_SpGene(Kendall_specia, "Kendall")
print(Kendall.medi.Specia)
 # RMSE
RMSE.medi.Specia <- median_metrics_SpGene(RMSE_specia, "RMSE")
print(RMSE.medi.Specia)

```

Calculate number of specialists showed better model performance in species-location
```{r}

# Deviance & RMSE
 # Fixed radius  = 19 species 
nrow(cal_per_bird_obs(Deviance_radii_diff_special[Deviance_radii_diff_special$Specialist ==1,], "Fixed_Radius"))
 # Pixel radius = 10 species 
nrow(cal_per_bird_obs(Deviance_radii_diff_special[Deviance_radii_diff_special$Specialist ==1,], "Pixel_radius"))


# Kendall's 
  # Fixed radius = 32 species 
nrow(cal_per_bird_obs_Kendall(Kendall_radii_diff_special[Kendall_radii_diff_special$Specialist ==1,], "Fixed_Radius"))
  # Pixel radius  = 30 species 
nrow(cal_per_bird_obs_Kendall(Kendall_radii_diff_special[Kendall_radii_diff_special$Specialist ==1,], "Pixel_radius"))
```

Plot percentage difference for both habitat specialists and generalist at one figure

- Merge species specialization information to percentage difference table
```{r}
Deviance_radii_diff_special <- merge(Deviance_radii_diff, generSpecia, by = "Species", all.x = TRUE)
Kendall_radii_diff_special <- merge(Kendall_radii_diff, generSpecia, by = "Species", all.x = TRUE)
RMSE_radii_diff_special <- merge(RMSE_radii_diff, generSpecia, by = "Species", all.x = TRUE)
```

Calculate percentage difference on evaluation metric across radii (bird-location vs. observer-location)
- specialists
```{r}
# Create function
medianEvalu <- function(data){ 
  data %>%
  group_by(Radii) %>%
  summarize(Median_Diff = round(median(Percentage_Difference, na.rm = TRUE), 3))
}

# Deviance
Deviance_allmedianDiff <- medianEvalu(Deviance_radii_diff_special)
Deviance_allmedianDiff
# Kendall
Kendall_allmedianDiff <- medianEvalu(Kendall_radii_diff_special)
Kendall_allmedianDiff
# RMSE
RMSE_allmedianDiff <- medianEvalu(RMSE_radii_diff_special)
RMSE_allmedianDiff
```


Create a function to plot all panels together
```{r}
plot_habitat_metrics <- function(data_list, metric_names, y_labels,  y_limits, y_seq, cb_palette, alphabet, output_file) {
  # Open a plotting device
  png(filename = output_file, width = 3060, height = 3800, res = 300)
  
  # Set up layout: 3 rows, 2 columns
  par(mfrow = c(3, 2),        # 3 rows, 2 columns
      mar = c(2, 5, 2, 2),    # Margins for individual plots (bottom, left, top, right)
      oma = c(10 , 5, 4, 2))    # Outer margins for the whole plot (bottom, left, top, right)
  
  # Initialize a panel order (counter)
  panel_index <- 1 

  # Loop through metrics
  for (i in seq_along(metric_names)) {
    for (j in c(1, 0)) { #  1 = Specialists; 0 = Generalists,
      # Subset data for specialists or generalists
      subset_data <- data_list[[i]][data_list[[i]]$Specialist == j, ]
      
      # Plot boxplot
      boxplot(subset_data$Percentage_Difference ~ subset_data$Radii, 
              col = cb_palette,
              ylab = ifelse(j == 0, "", y_labels[i]),  # Add y-axis label only for the left column
              xlab = "",     # Add x-axis label only for the bottom row
              cex.axis = 1.5,
              cex.lab = 2,
              xaxt = "n",
              whisklty = 1,
              yaxt = ifelse(j == 1, "s", "n"),        # Only left column gets y-axis
              ylim = y_limits[[i]])                   # Adjust ylim as per your data
      abline(h = 0, lty = 2, lwd = 2, col = "blue") # lwd = thickness; lty = type
      
      # Add x-axis with custom labels (show ticks)
      axis(side = 1, at = seq_along(subset_data$Radii), labels = FALSE, tick = TRUE)
      
      # Add panel label
      text(x = par("usr")[1] - 0.3, 
           y = par("usr")[4], 
           labels = alphabet[panel_index], 
           xpd = TRUE, 
           cex = 2, 
           font = 2, 
           adj = c(1, 1))
       # Increment panel_index
      panel_index <- panel_index + 1
      
      if (j == 0) { # Right column
        axis(side = 2, at = seq(y_limits[[i]][1], y_limits[[i]][2], by = y_seq[i]), labels = FALSE, tick = TRUE)
      }
      
      
      # Add x-axis with custom labels
      if (i == 3) { # Bottom row
        # Define label positions and text
        label_positions <- seq_along(unique(RMSE_radii_diff$Radii))  
        label = c("Fixed", "Pixel", "Effective")

        # Add inclined x-axis labels
        text(x = label_positions, 
             y = par("usr")[3] - 6  ,  # Position below x-axis
            labels = label, 
            adj = 0.5,                # Right-aligned text
            xpd = TRUE,             # Allow drawing outside plot area
            cex = 1.8 )              # Adjust text size
      }

      # Add title for each plot
      #title(main =  paste(metric_names[i], ifelse(j == 0, "Generalists", "Specialists")), cex.main = 1.5)
    }
  }
  
   # Add shared titles at the top of the panels
  mtext("Habitat Specialists", side = 3, outer = TRUE, line = -1, cex = 1.5, adj = 0.20, font = 2)  # First column title
  mtext("Habitat Generalists", side = 3, outer = TRUE, line = -1, cex = 1.5, adj = 0.85, font = 2) # Second column title
  
  # Add shared y-axis and x-axis titles
  mtext("Percentage Difference", side = 2, outer = TRUE, line = 2, cex = 1.5, font = 2)
  mtext("Radii", side = 1, outer = TRUE, line = 1, cex = 1.5, font = 2)
  
  # Add a shared legend at the bottom
  legend(
       x = grconvertX(-0.95, from = "npc", to = "user"),  # Center horizontally across the figure
       y = grconvertY(-0.2, from = "npc", to = "user"), # Place below the plot area (adjust as needed)
       legend = c("Fixed Radius", "Pixel Radius", "Effective Radius") ,  # Legend labels
       fill = cb_palette,               # Use the color palette
       horiz = TRUE,                    # Arrange legend items horizontally 
       cex = 2,                       # Scale the legend size
       bty = "n",
       xpd = NA, # Allow placement outside plot area
       x.intersp = 0.7,
       text.width = 1.2)         

  # Close plotting device
  dev.off()
}

# Plot figure
plot_habitat_metrics(
  data_list = list(Deviance_radii_diff_special, Kendall_radii_diff_special, RMSE_radii_diff_special),
  metric_names = c("Deviance", "Kendall", "RMSE"),
  y_labels = c("Deviance", "Kendall's", "Root mean square error "),
  y_limits = list(c(-60, 60), c(-50, 130), c(-60, 60)),  # Custom y-axis ranges
  y_seq = c(20 , 50, 20),
  cb_palette = cb_palette,
  alphabet = c("(a)","(b)","(c)","(d)","(e)","(f)"),
  output_file = "/Volumes/T7/eBird_Oregon2020/distance/Distance-Sampling/Evaluation/Figures/Eva_diff_SpGner_deviance.png"
)

```

Plot box plot of habitat specialist (raw evaluation metric)
```{r}
png(filename = "/Volumes/T7/eBird_Oregon2020/distance/Distance-Sampling/Evaluation/Figures/evaMetic-all_deviance_specialist.png", width = 2500, height = 4500, res = 300)
  
# Deviance
Deviance_plot_specia <- ggplot(Deviance_specia, aes(x = Radii, y = deviance, fill = Extraction_Approach)) +
  geom_boxplot() +
  theme_bw() +
  scale_x_discrete(labels = c("Effective Radius", "Fixed Radius", "Pixel Radius")) +
  labs(fill = "Assessment Approach") +
  ylab("Deviance") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15, vjust = 0.2),
        axis.title.y = element_text(size = 15),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.margin = margin(10, 10, 15, 10))

# Kendall
Kendall_plot_specia <- ggplot(Kendall_specia, aes(x = Radii, y = Kendall, fill = Extraction_Approach)) +
  geom_boxplot() +
  theme_bw() +
  ylab("Kendall's") +
  scale_x_discrete(labels = c("Effective Radius", "Fixed Radius", "Pixel Radius")) +
  labs(fill = "Assessment Approach") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15, vjust = 0.2),
        axis.title.y = element_text(size = 15),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.margin = margin(5, 10, 15, 10))

# RMSE
RMSE_plot_specia <- ggplot(RMSE_specia, aes(x = Radii, y = RMSE, fill = Extraction_Approach)) +
  geom_boxplot() +
  theme_bw() +
  scale_x_discrete(labels = c("Effective Radius", "Fixed Radius", "Pixel Radius")) +
  labs(fill = "Assessment Approach") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15, vjust = 0.2),
        axis.title.y = element_text(size = 15),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.margin = margin(5, 10, 10, 10))

#plot.margin = margin(top, right, bottom, left, unit)
all_plots_specia <- ggarrange(Deviance_plot_specia, 
                       Kendall_plot_specia, 
                       RMSE_plot_specia,
          labels = c("A","B","C"),
          ncol = 1,
          nrow = 3) 

annotate_figure(all_plots_specia, top = text_grob("Habitat specialists evaluation metrics comparison of\n observer-location vs. bird-location", 
              hjust = 0.5, vjust = 0.2, color = "black", face = "bold", size = 20)) +
  theme(plot.margin = margin(1,1,1,1, "cm"))

dev.off()

```

Calculate median percentage difference for habitat specialists and generalists
Apply function to Deviance, Kendall, RMSE
```{r}

# Create function
medianSpecGener <- function(data){ 
  data %>%
  group_by(Specialist, Radii) %>%
  summarize(Median_Diff = round(median(Percentage_Difference, na.rm = TRUE),3))
}

# Deviance
Deviancemedian_SpGen <- medianSpecGener(Deviance_radii_diff_special)
Deviancemedian_SpGen
# Kendall
Kendallmedian_SpGen <- medianSpecGener(Kendall_radii_diff_special)
Kendallmedian_SpGen
# RMSE
RMSEmedian_SpGen <- medianSpecGener(RMSE_radii_diff_special)
RMSEmedian_SpGen
```


Create a function to calculate percentage of species that benefit from species-location approach
```{r}
# Generalized function for calculating percentages
calculate_percentage <- function(data, metric, condition, radius_types = c("Fixed_Radius", "Pixel_radius", "Effective_Radius")) {
  results <- data.frame()
  
  for (radius in radius_types) {
    # Filter data for the specific radius
    data_radius <- subset(data, Radii == radius & !is.na(Percentage_Difference))
    
    # Apply the condition to find better-performing models
    better_models <- if (condition == "lower") {
      subset(data_radius, Percentage_Difference < 0)  # Deviance and RMSE
    } else if (condition == "higher") {
      subset(data_radius, Percentage_Difference > 0)  # Kendall's
    }
    
    # Calculate percentage
    percentage <- nrow(better_models) / nrow(data_radius) * 100
    #results <- rbind(results, data.frame(Metric = metric, Radius = radius, Percentage = percentage))
    results <- rbind(results, data.frame(Metric = metric, Radius = radius, Percentage = percentage, Total_Species = nrow(data_radius)))

  }
  
  return(results)
}

# Example dataframes: Replace `Deviance_radii_diff`, `RMSE_radii_diff`, etc. with your actual dataframes
metrics <- list(
  list(name = "Deviance", data = Deviance_radii_diff_special, condition = "lower"),
  list(name = "RMSE", data = RMSE_radii_diff_special, condition = "lower"),
  list(name = "Kendall's", data = Kendall_radii_diff_special, condition = "higher")
)

# Loop over metrics and calculate percentages
all_results <- data.frame()

for (metric in metrics) {
  results <- calculate_percentage(
    data = metric$data, 
    metric = metric$name, 
    condition = metric$condition
  )
  all_results <- rbind(all_results, results)
}


# Print or save the summarized results
print(all_results)
```

Run all species, specialists, generalists to calculate percentage of species that had better performance in species-location models
```{r}
calculate_percentage_all_groups <- function(data, metric, condition, 
                                            radius_types = c("Fixed_Radius", "Pixel_radius", "Effective_Radius")) {
  results <- data.frame()
  
  # Loop through "All species" and the two groups (specialists and generalists)
  for (group in c("All species", unique(data$Specialist))) {
    # Filter for group
    if (group == "All species") {
      group_data <- data
      group_name <- "All species"
    } else {
      group_data <- subset(data, Specialist == group)
      group_name <- ifelse(group == 1, "Specialists", "Generalists")
    }
    
    # Loop through radii types
    for (radius in radius_types) {
      # Filter data for the specific radius and exclude NA values
      data_radius <- subset(group_data, Radii == radius & !is.na(Percentage_Difference))
      
      # Apply the condition to find better-performing models
      better_models <- if (condition == "lower") {
        subset(data_radius, Percentage_Difference < 0)  # Deviance and RMSE
      } else if (condition == "higher") {
        subset(data_radius, Percentage_Difference > 0)  # Kendall's
      }
      
      # Calculate percentage and include total species in the analysis
      percentage <- if (nrow(data_radius) > 0) {
        round(nrow(better_models) / nrow(data_radius) * 100, 0)
      } else {
        NA  # Handle cases where no species are valid for the radius
      }
      results <- rbind(results, data.frame(
        Group = group_name,
        Metric = metric,
        Radius = radius,
        Percentage = percentage,
        Total_Species = nrow(data_radius)  # Number of valid species
      ))
    }
  }
  
  return(results)
}

# Example dataframes: Replace `Deviance_radii_diff`, `RMSE_radii_diff`, etc. with your actual dataframes
metrics <- list(
  list(name = "Deviance", data = Deviance_radii_diff_special, condition = "lower"),
  list(name = "RMSE", data = RMSE_radii_diff_special, condition = "lower"),
  list(name = "Kendall's", data = Kendall_radii_diff_special, condition = "higher")
)

# Loop over metrics and calculate percentages
all_SpGenResults <- data.frame()

for (metric in metrics) {
  results <- calculate_percentage_all_groups(
    data = metric$data, 
    metric = metric$name, 
    condition = metric$condition
  )
  all_SpGenResults <- rbind(all_SpGenResults, results)
}

write.table(all_SpGenResults, file = "/Volumes/T7/eBird_Oregon2020/distance/Distance-Sampling/Evaluation/Evaluation_Metric/Per_AllSpeGene.csv",
            sep = ",",
            row.names = FALSE,
            fileEncoding = "UTF-8")
```
