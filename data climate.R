#load path
rm(list=ls())
setwd("")

# Install and load required packages
install.packages("sf")
install.packages("raster")
install.packages("exactextractr")
install.packages("SPEI")
install.packages("zoo")
library(sf)
library(raster)
library(exactextractr)
library(SPEI)
library(zoo)

# Load shapefiles
subdistricts <- st_read("VNM_adm3.shp")
daklak <- subset(subdistricts,ID_1=="5")
hue <- subset(subdistricts,ID_1=="56")
hatinh <- subset(subdistricts,ID_1=="25")
tvsep <- rbind(daklak, hue, hatinh)
# Load gridded data (assuming these are NetCDF or similar raster format)
#precipitation_raster <- stack('precip.mon.total.nc')
precipitation_rt <- stack('precip.mon.total.v2020.nc')
temperature_raster <- stack('air.mon.mean.nc')

# Convert temperature from Kelvin to Celsius
#temperature_raster_c <- temperature_raster - 273.15

# Calculate centroids of sub-districts
centroids <- st_centroid(tvsep)
coords <- st_coordinates(centroids)
latitudes <- coords[, "Y"]

# Extract area-weighted average temperature for each sub-district
avg_temp_tvsep <- exact_extract(temperature_raster, tvsep, 'mean')
avg_temp <- avg_temp_tvsep - 273.15
avg_temp <- avg_temp[,1:864]
# Extract area-weighted average precipitation for each sub-district
avg_precip_d <- exact_extract(precipitation_rt, tvsep, 'mean')
avg_precip <- avg_precip_d[,685:1548]
# Assuming your data is monthly, create a time series for temperature and precipitation
start_date <- as.Date("1948-01-01")  # replace with your actual start date
freq <- 12  # monthly data


temperature_ts_list <- lapply(1:nrow(avg_temp), function(i) {
  ts(avg_temp[i, ], start = c(as.numeric(format(start_date, "%Y")), as.numeric(format(start_date, "%m"))), frequency = freq)
})

temperature_ts_unlist <- unlist(temperature_ts_list)
# Initialize an empty list to store grouped data
temp_list <- list()
# Calculate the number of complete groups of 456
num_groups <- length(temperature_ts_unlist) %/% 864
# Split the flattened data into groups of 456
for (i in 1:num_groups) {
  start_index <- (i - 1) * 864 + 1
  end_index <- start_index + 863
  temp_list[[i]] <- temperature_ts_unlist[start_index:end_index]
}

# If there are remaining elements (incomplete group), you can handle them as needed
remaining_elements <- length(temperature_ts_unlist) %% 864

# calculate PET values
calculate_pet_thornthwaite <- function(temp_ts, lat) {
  temp_ts <- na.omit(temp_ts)  # Remove NAs from temperature time series
  thornthwaite(temp_ts, lat = lat, na.rm = TRUE)
}

# Apply the function to each pair of temperature vector and latitude
PET_list <- mapply(calculate_pet_thornthwaite, temp_list, latitudes, SIMPLIFY = FALSE)
print(PET_list)
PET_list[1]

# List precipitation
precipitation_ts_list <- lapply(1:nrow(avg_precip), function(i) {
  ts(avg_precip[i, ], start = c(as.numeric(format(start_date, "%Y")), as.numeric(format(start_date, "%m"))), frequency = freq)
})
precipitation_ts_unlist <- unlist(precipitation_ts_list)
# Initialize an empty list to store grouped data
precip_list <- list()
# Calculate the number of complete groups of 456
num_groups <- length(precipitation_ts_unlist) %/% 864
# Split the flattened data into groups of 456
for (i in 1:num_groups) {
  start_index <- (i - 1) * 864 + 1
  end_index <- start_index + 863
  precip_list[[i]] <- precipitation_ts_unlist[start_index:end_index]
}

# If there are remaining elements (incomplete group), you can handle them as needed
remaining_elements <- length(precipitation_ts_unlist) %% 864

# SPEI calculate
calculate_spei <- function(precip_ts, pet_ts) {
  water_balance <- precip_ts - pet_ts
  spei(water_balance, scale = 6)  # replace scale if needed
}

spei_list <- mapply(calculate_spei, precip_list, PET_list, SIMPLIFY = FALSE)

# Example to print and plot SPEI for the first sub-district
print(spei_list[[1]])

### SOME CHECK FOR NA
# Check for NA in each time series
na_check_results <- lapply(temperature_ts_list, check_na_in_ts)
# Print results
for (i in seq_along(na_check_results)) {
  if (na_check_results[[i]]) {
    cat("Time series", i, "contains NA values.\n")
  } else {
    cat("Time series", i, "does not contain NA values.\n")
  }
}

# Extract SPEI values and combine into a data frame
spei_values_list <- lapply(spei_list, function(spei_obj) {
  as.numeric(spei_obj$fitted)
})

# Combine SPEI values into a single data frame
combined_spei_df <- do.call(rbind, spei_values_list)

# Generate column names representing months from January 1982 onward
start_date <- as.Date("1948-01-01")
num_months <- ncol(combined_spei_df)
month_labels <- format(seq.Date(start_date, by = "month", length.out = num_months), "%Y-%m")

# Add column names to the data frame
colnames(combined_spei_df) <- month_labels

# Add the daklak to the data frame
combined_spei_df <- cbind(tvsep, combined_spei_df)


# Filter columns to keep only those from 1996 to 2017
start_filter <- as.Date("2000-01-01")
end_filter <- as.Date("2019-12-31")
filtered_month_labels <- format(seq.Date(start_filter, by = "month", length.out = (12 * (2019 - 2000 + 1))), "X%Y.%m")

# Keep only the ID column and the filtered month columns
combined_spei_df_filtered <- combined_spei_df[, c("ID_1","NAME_1","ID_2","NAME_2","ID_3","NAME_3", filtered_month_labels)]

#CALCULATE MONTHS DROUGHT
# Initialize a matrix or data frame to store results
result_df <- data.frame(ID1 = combined_spei_df_filtered$ID_1,NAME1 = combined_spei_df_filtered$NAME_1,
                        ID2 = combined_spei_df_filtered$ID_2,NAME2 = combined_spei_df_filtered$NAME_2,
                        ID = combined_spei_df_filtered$ID_3,NAME3 = combined_spei_df_filtered$NAME_3)  # Assuming ID is a column in combined_spei_df_filtered

# Loop through each year and calculate drought months
for (year in 2000:2019) {
  # Filter columns for the current year
  year_labels <- format(seq.Date(as.Date(paste0(year, "-01-01")), by = "month", length.out = 12), "X%Y.%m")
  year_labels <- year_labels[year_labels %in% colnames(combined_spei_df_filtered)]
  
  # Extract columns for the current year and convert to numeric
  spei_year <- combined_spei_df_filtered[, year_labels]
  spei_year <- data.frame(lapply(spei_year, function(x) as.numeric(as.character(x))))
  # Calculate drought months (SPEI <= -1) for the current year
  result_df[[paste0("Months_D_", year)]] <- rowSums(spei_year <= -1, na.rm = TRUE)
  result_df[[paste0("Months_MD_", year)]] <- rowSums(spei_year <= -1.5, na.rm = TRUE)
  result_df[[paste0("Months_SD_", year)]] <- rowSums(spei_year <= -2, na.rm = TRUE)
  #result_df[[paste0("Mean_spei_", year)]] <- rowMeans(spei_year, na.rm = TRUE)
}

# Print or view the result_df
print(result_df)
# comebine the ID with subdistrict id in tvsep
library(readxl)
subdistr_id <- read_excel("shapefiles_to_tvsep_subdistr_id.xlsx")
# merge tvsep subdistr id to data spei
library(dplyr)
spei_tvsep <- inner_join(subdistr_id, result_df, by = "ID") #inner_join: keep only matched data
# Calculate the total D_Months for each column (excluding the ID column)
column_totals <- colSums(spei_tvsep[, grep("Months_D_", colnames(spei_tvsep))])
# Create a data frame for the totals row
totals_row <- data.frame(ID = "Total", t(column_totals))
library(tidyr)
# Convert totals_row to long format
totals_row_long <- gather(totals_row, key = "Year", value = "Count", -ID)
years <- c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
           "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")
totals_row_long$Year <- years
totals_row_long$mean <- totals_row_long$Count/110
# View the totals_row_long data frame
print(totals_row_long)
# Load necessary package
library(ggplot2)

# Create a bar plot
ggplot(totals_row_long, aes(x = Year, y = mean)) +
  geom_bar(stat = "identity", fill = "#e89c4b", width = 0.6) +  # Orange color specified by hexadecimal code
  labs(title = "",
       x = "",
       y = "Average Drought Months per year") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Calculate drought by months
# Example data frame structure (replace with your actual data)
# This example assumes you have columns named as per your description
# Replace `df` with your actual data frame name

# Step 1: Create a vector of month names (01 to 12)
months <- sprintf("%02d", 1:12)  # Creates "01", "02", ..., "12"

# Step 2: Initialize an empty data frame to store results
monthly_counts <- data.frame(Month = months, Drought_Count = numeric(length(months)))
month_d <- data.frame(ID1 = combined_spei_df_filtered$ID_1,NAME1 = combined_spei_df_filtered$NAME_1,
                        ID2 = combined_spei_df_filtered$ID_2,NAME2 = combined_spei_df_filtered$NAME_2,
                        ID = combined_spei_df_filtered$ID_3,NAME3 = combined_spei_df_filtered$NAME_3)  # Assuming ID is a column in combined_spei_df_filtered

# Step 3: Calculate drought counts for each month
for (month in months) {
  # Construct column names for the current month across all years
  col_names <- grep(paste0("^X\\d{4}\\.", month, "$"), colnames(combined_spei_df_filtered), value = TRUE, ignore.case = TRUE)
  # Subset the data frame to include only the columns for the current month
  month_labels <- col_names[col_names %in% colnames(combined_spei_df_filtered)]
  # Extract columns for the current month and convert to numeric
  spei_month <- combined_spei_df_filtered[, month_labels]
  spei_month <- data.frame(lapply(spei_month, function(x) as.numeric(as.character(x))))
  month_d[[paste0("D_", month)]] <- rowSums(spei_month <= -1, na.rm = TRUE)
  month_d[[paste0("MD_", month)]] <- rowSums(spei_month <= -1.5, na.rm = TRUE)
}

library(dplyr)
month_tvsep <- inner_join(subdistr_id, month_d, by = "ID") #inner_join: keep only matched data
# Calculate the total D_Months for each column (excluding the ID column)
column_totals <- colSums(month_tvsep[, grep("D_", colnames(month_tvsep))])
# Create a data frame for the totals row
totals_row <- data.frame(ID = "Total", t(column_totals))
library(tidyr)
# Convert totals_row to long format
totals_row_long <- gather(totals_row, key = "Month", value = "Count", -ID)
library(stringr)
matches <- str_match(totals_row_long$Month, "^([A-Za-z]+)_([0-9]+)$")
# Create a new data frame with extracted parts
new_df <- data.frame(
  Month = totals_row_long$Month,
  type = matches[, 2],  # Extracted initial letter
  month = matches[, 3],    # Extracted numeric part
  count = totals_row_long$Count
)
# Extract month number and type (D or MD)
total_month_d_md <- new_df %>%
  group_by(month) %>%
  summarise(
    count_D = sum(count[type == "D"]),    # Sum counts where type is "D"
    count_MD = sum(count[type == "MD"])   # Sum counts where type is "MD"
  ) %>%
  ungroup()

# Calculate frequency using dplyr
# Step 1: Calculate total_count for each month
total_counts <- total_month_d_md %>%
  summarise(total_count = sum(count_D))
total_counts_MD <- total_month_d_md %>%
  summarise(total_count = sum(count_MD))

# Step 2: Calculate percentage of D_count relative to total_count
percentage_df <- total_month_d_md %>%
  mutate(Per_D_count = (count_D / 6669) * 100) %>% 
  mutate(Per_MD_count = (count_MD / 2627) * 100)

# Load necessary package
library(ggplot2)

# Create a bar plot
ggplot(percentage_df, aes(x = month, y = Per_D_count)) +
  geom_bar(stat = "identity", fill = "#e89c4b", width = 0.6) +  # Orange color specified by hexadecimal code
  labs(title = "",
       x = "",
       y = "Frequency Drought Months") +
  theme_minimal()


## DRAW THE MAP OF SURVEY YEAR WITH PROVINCE AND DROUGHT MONTHS
# Summarize total drought months across all years (example)
spei_tvsep$Total_D_Months <- rowSums(spei_tvsep[ , grep("Months_D_", names(spei_tvsep))])
spei_tvsep$Mean_D_Months <- spei_tvsep$Total_D_Months/20

spei_tvsep_2010 <- spei_tvsep %>% select(ID1, NAME1, ID2, NAME2, ID, NAME3, Months_D_2010)
spei_tvsep_2010$ID_3 <- spei_tvsep_2010$ID

#shapefiles: subdistricts, daklak, hue, hatinh, tvsep

# Merge data frame with shapefile based on ID
merged_data_daklak <- daklak %>%
  left_join(spei_tvsep, by = c("ID_3" = "ID"))
# Plotting the map with ggplot2
c <- ggplot(data = merged_data_daklak) +
  geom_sf(aes(fill = Mean_D_Months)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0.5, 4.3)) +
  labs(title = "Dak Lak",
       fill = "Average D Months") +
  theme(
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  )
##hue
merged_data_hue <- hue %>%
  left_join(spei_tvsep, by = c("ID_3" = "ID"))
# Plotting the map with ggplot2
b <- ggplot(data = merged_data_hue) +
  geom_sf(aes(fill = Mean_D_Months)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0.5, 4.3)) +
  labs(title = "Thua Thien Hue",
       fill = "Average D Months") +
  theme(
    legend.position = "none",       # Remove legend
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  ) 

##hatinh
merged_data_hatinh <- hatinh %>%
  left_join(spei_tvsep, by = c("ID_3" = "ID"))
# Plotting the map with ggplot2
a <- ggplot(data = merged_data_hatinh) +
  geom_sf(aes(fill = Mean_D_Months)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0.5, 4.3)) +
  labs(title = "Ha Tinh",
       fill = "Average D Months") +
  theme(
    legend.position = "none",       # Remove legend
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)# Remove right axis text
  )

library(tmap)
library(patchwork)
combined_plot_D <- (a+b+c) + plot_layout(guides = "collect") & 
  theme(legend.position = "bottom") # Move legend to the bottom for better fit
combined_plot_D

### SD
spei_tvsep$Total_MD_Months <- rowSums(spei_tvsep[ , grep("Months_MD_", names(spei_tvsep))])
spei_tvsep$Mean_MD_Months <- spei_tvsep$Total_MD_Months/20

#shapefiles: subdistricts, daklak, hue, hatinh, tvsep

# Merge data frame with shapefile based on ID
merged_data_daklak <- daklak %>%
  left_join(spei_tvsep, by = c("ID_3" = "ID"))
# Plotting the map with ggplot2
c <- ggplot(data = merged_data_daklak) +
  geom_sf(aes(fill = Mean_MD_Months)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0.5, 4.3)) +
  labs(title = "Dak Lak",
       fill = "Average SD Months") +
  theme(
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  )
##hue
merged_data_hue <- hue %>%
  left_join(spei_tvsep, by = c("ID_3" = "ID"))
# Plotting the map with ggplot2
b <- ggplot(data = merged_data_hue) +
  geom_sf(aes(fill = Mean_MD_Months)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0.5, 4.3)) +
  labs(title = "Thua Thien Hue",
       fill = "Average SD Months") +
  theme(
    legend.position = "none",       # Remove legend
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  ) 

##hatinh
merged_data_hatinh <- hatinh %>%
  left_join(spei_tvsep, by = c("ID_3" = "ID"))
# Plotting the map with ggplot2
a <- ggplot(data = merged_data_hatinh) +
  geom_sf(aes(fill = Mean_MD_Months)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0.5, 4.3)) +
  labs(title = "Ha Tinh",
       fill = "Average SD Months") +
  theme(
    legend.position = "none",       # Remove legend
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)# Remove right axis text
  )

library(tmap)
library(patchwork)
combined_plot_MD <- (a+b+c) + plot_layout(guides = "collect") & 
  theme(legend.position = "bottom") # Move legend to the bottom for better fit
combined_plot_MD

### ALL DISTRICT IN THREE PROVINCE
### TOTAL AVERAGE DROUGHT MONTHS PER YEAR
# Calculate the total D_Months for each column (excluding the ID column)
column_totals <- colSums(result_df[, grep("Months_D_", colnames(result_df))])
# Create a data frame for the totals row
totals_row <- data.frame(ID = "Total", t(column_totals))
library(tidyr)
# Convert totals_row to long format
totals_row_long <- gather(totals_row, key = "Year", value = "Count", -ID)
years <- c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
           "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")
totals_row_long$Year <- years
totals_row_long$mean <- totals_row_long$Count/579
# View the totals_row_long data frame
print(totals_row_long)
# Load necessary package
library(ggplot2)

# Create a bar plot
ggplot(totals_row_long, aes(x = Year, y = mean)) +
  geom_bar(stat = "identity", fill = "#e89c4b", width = 0.6) +  # Orange color specified by hexadecimal code
  labs(title = "",
       x = "",
       y = "Average Drought Months per year") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

## DRAW THE MAP OF SURVEY YEAR WITH PROVINCE AND DROUGHT MONTHS
# Summarize total drought months across all years (example)
result_df$Total_D_Months <- rowSums(result_df[ , grep("Months_D_", names(result_df))])
result_df$Mean_D_Months <- result_df$Total_D_Months/20

#spei_tvsep_2010 <- spei_tvsep %>% select(ID1, NAME1, ID2, NAME2, ID, NAME3, Months_D_2010)
#spei_tvsep_2010$ID_3 <- spei_tvsep_2010$ID

#shapefiles: subdistricts, daklak, hue, hatinh, tvsep

# Merge data frame with shapefile based on ID
merged_data_daklak <- daklak %>%
  left_join(result_df, by = c("ID_3" = "ID"))
# Plotting the map with ggplot2
c <- ggplot(data = merged_data_daklak) +
  geom_sf(aes(fill = Mean_D_Months)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0.5, 4.3)) +
  labs(title = "Dak Lak",
       fill = "Average D Months") +
  theme(
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  )
##hue
merged_data_hue <- hue %>%
  left_join(result_df, by = c("ID_3" = "ID"))
# Plotting the map with ggplot2
b <- ggplot(data = merged_data_hue) +
  geom_sf(aes(fill = Mean_D_Months)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0.5, 4.3)) +
  labs(title = "Thua Thien Hue",
       fill = "Average D Months") +
  theme(
    legend.position = "none",       # Remove legend
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  ) 

##hatinh
merged_data_hatinh <- hatinh %>%
  left_join(result_df, by = c("ID_3" = "ID"))
# Plotting the map with ggplot2
a <- ggplot(data = merged_data_hatinh) +
  geom_sf(aes(fill = Mean_D_Months)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0.5, 4.3)) +
  labs(title = "Ha Tinh",
       fill = "Average D Months") +
  theme(
    legend.position = "none",       # Remove legend
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)# Remove right axis text
  )

library(patchwork)
combined_plot_D <- (a+b+c) + plot_layout(guides = "collect") & 
  theme(legend.position = "bottom") # Move legend to the bottom for better fit
combined_plot_D

### SD
result_df$Total_MD_Months <- rowSums(result_df[ , grep("Months_MD_", names(result_df))])
result_df$Mean_MD_Months <- result_df$Total_MD_Months/20

#spei_tvsep_2010 <- spei_tvsep %>% select(ID1, NAME1, ID2, NAME2, ID, NAME3, Months_D_2010)
#spei_tvsep_2010$ID_3 <- spei_tvsep_2010$ID

#shapefiles: subdistricts, daklak, hue, hatinh, tvsep

# Merge data frame with shapefile based on ID
merged_data_daklak <- daklak %>%
  left_join(result_df, by = c("ID_3" = "ID"))
# Plotting the map with ggplot2
c <- ggplot(data = merged_data_daklak) +
  geom_sf(aes(fill = Mean_MD_Months)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0.5, 4.3)) +
  labs(title = "Dak Lak",
       fill = "Average SD Months") +
  theme(
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  )
##hue
merged_data_hue <- hue %>%
  left_join(result_df, by = c("ID_3" = "ID"))
# Plotting the map with ggplot2
b <- ggplot(data = merged_data_hue) +
  geom_sf(aes(fill = Mean_MD_Months)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0.5, 4.3)) +
  labs(title = "Thua Thien Hue",
       fill = "Average SD Months") +
  theme(
    legend.position = "none",       # Remove legend
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  ) 

##hatinh
merged_data_hatinh <- hatinh %>%
  left_join(result_df, by = c("ID_3" = "ID"))
# Plotting the map with ggplot2
a <- ggplot(data = merged_data_hatinh) +
  geom_sf(aes(fill = Mean_MD_Months)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0.5, 4.3)) +
  labs(title = "Ha Tinh",
       fill = "Average SD Months") +
  theme(
    legend.position = "none",       # Remove legend
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)# Remove right axis text
  )

library(patchwork)
combined_plot_MD <- (a+b+c) + plot_layout(guides = "collect") & 
  theme(legend.position = "bottom") # Move legend to the bottom for better fit
combined_plot_MD

### PLOT D EACH SURVEY YEAR: 2010, 2013, 2016, 2017
#result_df$Months_D_2017 <- result_df %>% select(ID1, NAME1, ID2, NAME2, ID, NAME3, Months_D_2010)

#shapefiles: subdistricts, daklak, hue, hatinh, tvsep

# Merge data frame with shapefile based on ID
merged_data_daklak <- daklak %>%
  left_join(result_df, by = c("ID_3" = "ID"))
# Plotting the map with ggplot2
daklak_2010 <- ggplot(data = merged_data_daklak) +
  geom_sf(aes(fill = Months_D_2010)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0, 9)) +
  labs(title = "Dak Lak - 2010",
       fill = "Total D Months") +
  theme(
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  )
##hue
merged_data_hue <- hue %>%
  left_join(result_df, by = c("ID_3" = "ID"))
# Plotting the map with ggplot2
hue_2010 <- ggplot(data = merged_data_hue) +
  geom_sf(aes(fill = Months_D_2010)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0, 9)) +
  labs(title = "Thua Thien Hue - 2010",
       fill = "Total D Months") +
  theme(
    legend.position = "none",       # Remove legend
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  ) 

##hatinh
merged_data_hatinh <- hatinh %>%
  left_join(result_df, by = c("ID_3" = "ID"))
# Plotting the map with ggplot2
hatinh_2010 <- ggplot(data = merged_data_hatinh) +
  geom_sf(aes(fill = Months_D_2010)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0, 9)) +
  labs(title = "Ha Tinh - 2010",
       fill = "Total D Months") +
  theme(
    legend.position = "none",       # Remove legend
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)# Remove right axis text
  )
#2008
daklak_2008 <- ggplot(data = merged_data_daklak) +
  geom_sf(aes(fill = Months_D_2008)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0, 9)) +
  labs(title = "Dak Lak - 2008",
       fill = "Total D Months") +
  theme(
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  )
##hue
merged_data_hue <- hue %>%
  left_join(result_df, by = c("ID_3" = "ID"))
# Plotting the map with ggplot2
hue_2008 <- ggplot(data = merged_data_hue) +
  geom_sf(aes(fill = Months_D_2008)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0, 9)) +
  labs(title = "Thua Thien Hue - 2008",
       fill = "Total D Months") +
  theme(
    legend.position = "none",       # Remove legend
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  ) 

##hatinh
merged_data_hatinh <- hatinh %>%
  left_join(result_df, by = c("ID_3" = "ID"))
# Plotting the map with ggplot2
hatinh_2008 <- ggplot(data = merged_data_hatinh) +
  geom_sf(aes(fill = Months_D_2008)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0, 9)) +
  labs(title = "Ha Tinh - 2008",
       fill = "Total D Months") +
  theme(
    legend.position = "none",       # Remove legend
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)# Remove right axis text
  )
#2013
##hatinh
daklak_2013 <- ggplot(data = merged_data_daklak) +
  geom_sf(aes(fill = Months_D_2013)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0, 9)) +
  labs(title = "Dak Lak - 2013",
       fill = "Total D Months") +
  theme(
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  )

daklak_2016 <- ggplot(data = merged_data_daklak) +
  geom_sf(aes(fill = Months_D_2016)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0, 9)) +
  labs(title = "Dak Lak - 2016",
       fill = "Total D Months") +
  theme(
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  )
daklak_2017 <- ggplot(data = merged_data_daklak) +
  geom_sf(aes(fill = Months_D_2017)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0, 9)) +
  labs(title = "Dak Lak - 2017",
       fill = "Total D Months") +
  theme(
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  )
#hatinh
hatinh_2013 <- ggplot(data = merged_data_hatinh) +
  geom_sf(aes(fill = Months_D_2013)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0, 9)) +
  labs(title = "Ha Tinh - 2013",
       fill = "Total D Months") +
  theme(
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  )
hatinh_2016 <- ggplot(data = merged_data_hatinh) +
  geom_sf(aes(fill = Months_D_2016)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0, 9)) +
  labs(title = "Ha Tinh - 2016",
       fill = "Total D Months") +
  theme(
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  )
hatinh_2017 <- ggplot(data = merged_data_hatinh) +
  geom_sf(aes(fill = Months_D_2017)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0, 9)) +
  labs(title = "Ha Tinh - 2017",
       fill = "Total D Months") +
  theme(
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  )
#hue
hue_2013 <- ggplot(data = merged_data_hue) +
  geom_sf(aes(fill = Months_D_2013)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0, 9)) +
  labs(title = "Thua Thien Hue - 2013",
       fill = "Total D Months") +
  theme(
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  )
hue_2016 <- ggplot(data = merged_data_hue) +
  geom_sf(aes(fill = Months_D_2016)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0, 9)) +
  labs(title = "Thua Thien Hue - 2016",
       fill = "Total D Months") +
  theme(
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  )
hue_2017 <- ggplot(data = merged_data_hue) +
  geom_sf(aes(fill = Months_D_2017)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0, 9)) +
  labs(title = "Thua Thien Hue - 2017",
       fill = "Total D Months") +
  theme(
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  )

library(patchwork)
combined_plot_2008 <- (hatinh_2008 + hue_2008 + daklak_2008) + plot_layout(guides = "collect") & theme(legend.position = "none") # Move legend to the bottom for better fit
combined_plot_2010 <- (hatinh_2010 + hue_2010 + daklak_2010) + plot_layout(guides = "collect") & theme(legend.position = "none") # Move legend to the bottom for better fit
combined_plot_2013 <- (hatinh_2013 + hue_2013 + daklak_2013) + plot_layout(guides = "collect") & theme(legend.position = "none") # Move legend to the bottom for better fit
combined_plot_2016 <- (hatinh_2016 + hue_2016 + daklak_2016) + plot_layout(guides = "collect") & theme(legend.position = "none") # Move legend to the bottom for better fit
combined_plot_2017 <- (hatinh_2017 + hue_2017 + daklak_2017) + plot_layout(guides = "collect") & theme(legend.position = "bottom") # Move legend to the bottom for better fit

### MD: Medium drought
merged_data_daklak <- daklak %>%
  left_join(result_df, by = c("ID_3" = "ID"))
# Plotting the map with ggplot2
md_daklak_2010 <- ggplot(data = merged_data_daklak) +
  geom_sf(aes(fill = Months_MD_2010)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0, 9)) +
  labs(title = "Dak Lak - 2010",
       fill = "Total SD Months") +
  theme(
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  )
##hue
merged_data_hue <- hue %>%
  left_join(result_df, by = c("ID_3" = "ID"))
# Plotting the map with ggplot2
md_hue_2010 <- ggplot(data = merged_data_hue) +
  geom_sf(aes(fill = Months_MD_2010)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0, 9)) +
  labs(title = "Thua Thien Hue - 2010",
       fill = "Total SD Months") +
  theme(
    legend.position = "none",       # Remove legend
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  ) 

##hatinh
merged_data_hatinh <- hatinh %>%
  left_join(result_df, by = c("ID_3" = "ID"))
# Plotting the map with ggplot2
md_hatinh_2010 <- ggplot(data = merged_data_hatinh) +
  geom_sf(aes(fill = Months_MD_2010)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0, 9)) +
  labs(title = "Ha Tinh - 2010",
       fill = "Total SD Months") +
  theme(
    legend.position = "none",       # Remove legend
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)# Remove right axis text
  )
#2008
md_daklak_2008 <- ggplot(data = merged_data_daklak) +
  geom_sf(aes(fill = Months_MD_2008)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0, 9)) +
  labs(title = "Dak Lak - 2008",
       fill = "Total SD Months") +
  theme(
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  )
##hue
merged_data_hue <- hue %>%
  left_join(result_df, by = c("ID_3" = "ID"))
# Plotting the map with ggplot2
md_hue_2008 <- ggplot(data = merged_data_hue) +
  geom_sf(aes(fill = Months_MD_2008)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0, 9)) +
  labs(title = "Thua Thien Hue - 2008",
       fill = "Total SD Months") +
  theme(
    legend.position = "none",       # Remove legend
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  ) 

##hatinh
merged_data_hatinh <- hatinh %>%
  left_join(result_df, by = c("ID_3" = "ID"))
# Plotting the map with ggplot2
md_hatinh_2008 <- ggplot(data = merged_data_hatinh) +
  geom_sf(aes(fill = Months_MD_2008)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0, 9)) +
  labs(title = "Ha Tinh - 2008",
       fill = "Total SD Months") +
  theme(
    legend.position = "none",       # Remove legend
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)# Remove right axis text
  )
#2013
##hatinh
md_daklak_2013 <- ggplot(data = merged_data_daklak) +
  geom_sf(aes(fill = Months_MD_2013)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0, 9)) +
  labs(title = "Dak Lak - 2013",
       fill = "Total SD Months") +
  theme(
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  )

md_daklak_2016 <- ggplot(data = merged_data_daklak) +
  geom_sf(aes(fill = Months_MD_2016)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0, 9)) +
  labs(title = "Dak Lak - 2016",
       fill = "Total SD Months") +
  theme(
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  )
md_daklak_2017 <- ggplot(data = merged_data_daklak) +
  geom_sf(aes(fill = Months_MD_2017)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0, 9)) +
  labs(title = "Dak Lak - 2017",
       fill = "Total SD Months") +
  theme(
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  )
#hatinh
md_hatinh_2013 <- ggplot(data = merged_data_hatinh) +
  geom_sf(aes(fill = Months_MD_2013)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0, 9)) +
  labs(title = "Ha Tinh - 2013",
       fill = "Total SD Months") +
  theme(
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  )
md_hatinh_2016 <- ggplot(data = merged_data_hatinh) +
  geom_sf(aes(fill = Months_MD_2016)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0, 9)) +
  labs(title = "Ha Tinh - 2016",
       fill = "Total SD Months") +
  theme(
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  )
md_hatinh_2017 <- ggplot(data = merged_data_hatinh) +
  geom_sf(aes(fill = Months_MD_2017)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0, 9)) +
  labs(title = "Ha Tinh - 2017",
       fill = "Total SD Months") +
  theme(
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  )
#hue
md_hue_2013 <- ggplot(data = merged_data_hue) +
  geom_sf(aes(fill = Months_MD_2013)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0, 9)) +
  labs(title = "Thua Thien Hue - 2013",
       fill = "Total SD Months") +
  theme(
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  )
md_hue_2016 <- ggplot(data = merged_data_hue) +
  geom_sf(aes(fill = Months_MD_2016)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0, 9)) +
  labs(title = "Thua Thien Hue - 2016",
       fill = "Total SD Months") +
  theme(
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  )
md_hue_2017 <- ggplot(data = merged_data_hue) +
  geom_sf(aes(fill = Months_MD_2017)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "#F5F5F5",limits = c(0, 9)) +
  labs(title = "Thua Thien Hue - 2017",
       fill = "Total SD Months") +
  theme(
    axis.line.x.top = element_blank(), # Remove top axis line
    axis.line.y.right = element_blank(), # Remove right axis line
    axis.ticks.x.top = element_blank(),  # Remove top axis ticks
    axis.ticks.y.right = element_blank(), # Remove right axis ticks
    axis.text.x.top = element_blank(),    # Remove top axis text
    axis.text.y.right = element_blank(),
    axis.text.x = element_text(angle = 90)   # Remove right axis text
  )

library(patchwork)
md_combined_plot_2008 <- (md_hatinh_2008 + md_hue_2008 + md_daklak_2008) + plot_layout(guides = "collect") & theme(legend.position = "none") # Move legend to the bottom for better fit
md_combined_plot_2010 <- (md_hatinh_2010 + md_hue_2010 + md_daklak_2010) + plot_layout(guides = "collect") & theme(legend.position = "none") # Move legend to the bottom for better fit
md_combined_plot_2013 <- (md_hatinh_2013 + md_hue_2013 + md_daklak_2013) + plot_layout(guides = "collect") & theme(legend.position = "none") # Move legend to the bottom for better fit
md_combined_plot_2016 <- (md_hatinh_2016 + md_hue_2016 + md_daklak_2016) + plot_layout(guides = "collect") & theme(legend.position = "none") # Move legend to the bottom for better fit
md_combined_plot_2017 <- (md_hatinh_2017 + md_hue_2017 + md_daklak_2017) + plot_layout(guides = "collect") & theme(legend.position = "bottom") # Move legend to the bottom for better fit

