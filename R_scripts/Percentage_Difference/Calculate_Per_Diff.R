---
title: "evaMetric_compare_Percentage_difference"
output: html_document
date: "2025-02-26"
---

[Goal] - Summarize Kendall's results & plot the evaluation metric differences (%) from observer-location and species-location results from all species + generalists vs. specialists

Load library
```{r}
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)
```

Read both observer-location & species-location evaluation metrics outcome
```{r}
result <- read.csv("/Volumes/T7/eBird_Oregon2020/distance/Distance-Sampling/Evaluation/Evaluation_Metric/Oregon2020/evalu_MetricSpeGen.csv")
```


Summarize median of bird-location and observer-location by habitat characterizations
```{r}
# Filter for relevant metrics 
metrics_data <- result %>% 
  filter(Metric %in% c("Kendall"))

# Gather all radii types into a single column
metrics_long <- metrics_data %>%
  pivot_longer(cols = c(Pixel_radius, Fixed_Radius, Effective_Radius),
               names_to = "Radius_Type", values_to = "Radius_Value")

# Group by Assessment_Approach, Metric, and calculate the median for the each radius values
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
Summarize median value of habitat specialist vs. generalist
```{r}
# Group by Assessment Approach, specialization, and calculate the median for the unified radius values
median_metrics_speciaGener <- metrics_long %>%
  group_by(Extraction_Approach, Specialist) %>%
  summarize(Median_Radius = round(median(Radius_Value, na.rm = TRUE),3))

# View the summarized dataframe
print(median_metrics_speciaGener)

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

   #======= Kendall ========#
Kendall <-  result[result$Metric == "Kendall",]

# Pivot the dataframe to longer format
Kendall_long <- Kendall %>%
  pivot_longer(cols = c(Pixel_radius, Fixed_Radius, Effective_Radius),
               names_to = "Radii",
               values_to = "Kendall") %>%
  arrange(Species, Extraction_Approach, Specialist)

# View the final dataframe
print(Kendall_long)

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
summary_Kenall <- metricSummaryAll(Kendall_long, "Kendall")


# View the result
print(summary_Kenall)


```


Create a function to calculate percentage differences of evaluation metric pariwise (observer-location radius vs. bird-location radius)
```{r}
# Calculate percentage difference
calculate_diff_radii <- function(data, metric_col){
percentage_diff <- data %>%
  select(Species, Radii, Extraction_Approach, metric_col) %>%
  pivot_wider(names_from = Extraction_Approach, values_from = metric_col) %>%
  mutate(Percentage_Difference = ((`Bird-location` - `Observer-location`) / `Observer-location`) * 100)
}

```

Apply function to Kendall
```{r}
Kendall_radii_diff <- calculate_diff_radii(Kendall_long, metric_col = "Kendall")

# Reorder the levels of Radii
Kendall_radii_diff <- Kendall_radii_diff %>%
  mutate(Radii = factor(Radii, levels = c("Fixed_Radius", "Pixel_radius", "Effective_Radius")))

# Summary of the metric
 # Median
round(median(Kendall_radii_diff$Percentage_Difference),3)

  # Mean
round(mean(Kendall_radii_diff$Percentage_Difference),3)
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
  # Kendall
median_metrics_perce_radii(Kendall_radii_diff)

```

***Habitat generalists***
Calculate percentage differences from observer-location vs. bird-location
```{r}
# Extract habitat generalists
Kendall_gener <- Kendall_long[Kendall_long$Specialist == 0, ]

Kendall_radii_diff_gener <- calculate_diff_radii(Kendall_gener, metric_col = "Kendall")

# Reorder the levels of Radii
Kendall_radii_diff_gener <- Kendall_radii_diff_gener %>%
  mutate(Radii = factor(Radii, levels = c("Fixed_Radius", "Pixel_radius", "Effective_Radius")))

# Summary of the metric
 # Median
round(median(Kendall_radii_diff_gener$Percentage_Difference),3)

  # Mean
round(mean(Kendall_radii_diff_gener$Percentage_Difference),3)

```

Summarize median percentage difference of bird-location and observer-location by habitat characterizations and radius
```{r}

median_metrics_perce_radii(Kendall_radii_diff_gener)

```



***Habitat specialists***
```{r}
# Extract habitat specialists
Kendall_specia <- Kendall_long[Kendall_long$Specialist == 1, ]

Kendall_radii_diff_special <- calculate_diff_radii(Kendall_specia, metric_col = "Kendall")

# Reorder the levels of Radii
Kendall_radii_diff_special <- Kendall_radii_diff_special %>%
  mutate(Radii = factor(Radii, levels = c("Fixed_Radius", "Pixel_radius", "Effective_Radius")))

# Summary of the metric
summary(Kendall_radii_diff_special$Percentage_Difference)

```


Summarize median percentage difference of bird-location and observer-location by habitat characterizations and radius
```{r}

median_metrics_perce_radii(Kendall_radii_diff_special)

```

***Plot percentage difference box plot***
- All species + generalists + specialists
- Plot all three graphs in one figure
- Move common legend at the buttom

Add panel labes
```{r}
# Open a PNG device with space for the legend
png("/Volumes/T7/eBird_Oregon2020/distance/Distance-Sampling/Evaluation/Evaluation_Metric/Oregon2020/Figures/Kendall_diff_plot.png", 
    width = 1800, height = 3500, res = 300)  

# Set up a 3-row layout with extra outer margins at the bottom for the legend
par(mfrow = c(3, 1), mar = c(4, 8, 3, 2), oma = c(6, 2, 4, 5))  # Outer margins for the whole plot (bottom, left, top, right)

cb_palette <- palette.colors(palette = "Okabe-Ito")[c(2, 3, 7)]  
label = c("Fixed", "Pixel", "Effective")
label_positions <- seq_along(unique(Kendall_radii_diff_gener$Radii))  

plot_boxplot <- function(data, y_label, panel_title, add_xlab, panel_label) {
  boxplot(data$Percentage_Difference ~ data$Radii,
          col = cb_palette,
          ylab = y_label,  
          xlab = "",  
          cex.axis = 1.5,
          cex.lab = 2,
          xaxt = "n",
          whisklty = 1,
          ylim = c(-30, 100))  
  abline(h = 0, lty = 2, lwd = 2, col = "blue") 

  # Add panel title
  mtext(panel_title, side = 3, line = 1, cex = 1.5, font = 2)  

  # Add panel label (A, B, C) in the top-left corner
  mtext(panel_label, side = 3, line = 1, adj = -0.1, cex = 1.5, font = 2)

  # Add x-axis labels only for the last plot
  if (add_xlab) {
    text(x = label_positions, 
         y = par("usr")[3] - 6,  
         labels = label, 
         adj = 0.5,                
         xpd = TRUE,             
         cex = 1.8 )  

    # Add bold "Radii" as x-axis label
    mtext("Radii", side = 1, line = 3, cex = 1.5, font = 2)  
  }
}

plot_boxplot(Kendall_radii_diff, "Percentage differences in\nKendall's", "All species", FALSE, "(A)")
plot_boxplot(Kendall_radii_diff_gener, "Percentage differences in\nKendall's", "Generalists", FALSE, "(B)")
plot_boxplot(Kendall_radii_diff_special, "Percentage differences in\nKendall's", "Specialists", TRUE, "(C)")

# Add the shared legend outside the plot, at the bottom
par(xpd = NA)  # Allow plotting outside the figure area
# Add common legend at buttom
legend(
  x = "bottomleft", 
  inset = c(-0.2, -0.4),  # Move it below the figure
  legend = c("Fixed Radius", "Pixel Radius", "Effective Radius"),  
  fill = cb_palette,  
  horiz = TRUE,  
  cex = 2,  
  bty = "n",  
  x.intersp = 0.3,  
  text.width = 1
)    

# Close the file device
dev.off()


```
