# Load necessary libraries
require(dplyr)
require(tidyr)
require(fst)
require(sf)
require(lubridate)
require(ggplot2)

# Define the function
process_date_of_interest <- function(data, date_of_interest, prop_out_threshold = 0.05, cust_out_threshold = 10000) {
  # Convert date_of_interest to POSIXct
  date_of_interest <- as.POSIXct(date_of_interest, tz = "America/New_York")
  
  # Filter for a wide window around the date of interest
  filtered_data <- data %>%
    filter(datetime_eastern >= (date_of_interest - hours(72)),
           datetime_eastern <= (date_of_interest + hours(72)))
  
  # Identify the first time any ID exceeds the threshold for this event
  treatment_start_time <- filtered_data %>%
    filter(datetime_eastern >= date_of_interest) %>%
    filter(prop_out >= prop_out_threshold | customers_out >= cust_out_threshold) %>%
    summarize(first_time = min(datetime_eastern, na.rm = TRUE)) %>%
    pull(first_time)
  
  # Skip processing if no treatment event is found
  if (is.na(treatment_start_time)) {
    message("No treatment event found for ", date_of_interest)
    return(NULL)
  }
  
  # Define exact pre- and post-treatment time blocks
  pre_treatment_blocks <- seq(from = treatment_start_time - hours(48),
                              by = "6 hours", length.out = 8)
  
  post_treatment_blocks <- seq(from = treatment_start_time,
                               by = "6 hours", length.out = 9)
  
  # Combine pre- and post-treatment blocks
  time_blocks <- c(pre_treatment_blocks, post_treatment_blocks)
  
  print(time_blocks)
  print(treatment_start_time)
  
  # Filter data to the exact blocks
  filtered_data <- filtered_data %>%
    filter(datetime_eastern >= min(time_blocks),
           datetime_eastern <= max(time_blocks))
  
  # Identify treated IDs for this specific event
  treated_ids <- filtered_data %>%
    group_by(dps_id) %>%
    summarize(max_prop_out = max(prop_out, na.rm = TRUE),
              max_cust_out = max(customers_out, na.rm = TRUE)) %>%
    filter(max_prop_out >= prop_out_threshold | max_cust_out >= cust_out_threshold) %>%
    pull(dps_id)
  
  # Identify control IDs with `prop_out == 0` for the time after outage starts
  control_ids <- filtered_data %>%
    filter(datetime_eastern >= treatment_start_time) %>%
    group_by(dps_id) %>%
    summarize(max_prop_out = max(prop_out, na.rm = TRUE),
              max_cust_out = max(customers_out, na.rm = TRUE)) %>%
    filter(max_prop_out <= 0.005  & max_cust_out <= 100) %>%
    pull(dps_id)
  
  # Assign treatment indicators for treated and control IDs
  treated_data <- filtered_data %>%
    filter(dps_id %in% treated_ids) %>%
    mutate(
      treat_ind = if_else(datetime_eastern >= treatment_start_time, 1, 0),
      treatment_c = cumsum(treat_ind),
      treatment_c = if_else(treatment_c > 0, 1, 0),
      t_c = "t"
    )
  
  control_data <- filtered_data %>%
    filter(dps_id %in% control_ids) %>%
    mutate(
      treat_ind = 0,
      treatment_c = 0,
      t_c = "c"
    )
  
  # Identify the end time when all treated units drop below the threshold
  treatment_end_time <- treated_data %>%
    filter(datetime_eastern >= treatment_start_time) %>%
    group_by(datetime_eastern) %>%
    summarize(all_below_threshold = all(prop_out <= prop_out_threshold & customers_out <= cust_out_threshold, na.rm = TRUE)) %>%
    filter(all_below_threshold) %>%
    summarize(first_below_time = min(datetime_eastern, na.rm = TRUE)) %>%
    pull(first_below_time)
  
 # identify max_out
  treated_max <- filtered_data %>%
    filter(dps_id %in% treated_ids) %>%
    group_by(datetime_eastern) %>%
    summarize(
      sum_out = sum(customers_out, na.rm = T)) %>%
    ungroup() %>%
    summarize(maximum_out = max(sum_out, na.rm = T)) %>%
    pull(maximum_out)
  
  
  # Combine treated and control data
  combined_data <- bind_rows(treated_data, control_data)
  
  # Assign time blocks explicitly
  combined_data <- combined_data %>%
    mutate(
      time_block = cut(
        datetime_eastern,
        breaks = time_blocks,
        include.lowest = TRUE,
        labels = as.character(head(time_blocks, -1)) # Exclude the last break
      ),
      time_block = as.POSIXct(time_block)
    )
  
  
  # Aggregate into time blocks
  aggregated_data <- combined_data %>%
    group_by(dps_id, time_block) %>%
    summarize(
      sum_ed = sum(asthma_ed, na.rm = TRUE),
      mean_temp = mean(temperature, na.rm = TRUE),
      max_temp = max(temperature, na.rm = TRUE),
      mean_rh = mean(rh, na.rm = TRUE),
      max_ws = max(abs_wind_speed_knots, na.rm = TRUE),
      total_precip = mean(total_precipitation, na.rm = TRUE),
      trt = max(treatment_c),
      t_c = first(t_c),
      customers = mean(customers),
      customers_out_max = max(customers_out),
      customers_out_mean = mean(customers_out)
    ) %>%
    ungroup() %>%
    mutate(event = as.character(date_of_interest),
           start = treatment_start_time,
           end = treatment_end_time,
           max_sum_out = treated_max)
  
  return(aggregated_data)
}

# 1. Read in the data
setwd("~/outagesxasthma/data")
data <- read.fst("sparcs_outages_for_gsynth.fst") %>%
  drop_na(dps_id)

# 2. List of dates of interest
dates_of_interest <- c("2018-03-02", "2019-07-13", "2019-07-21", "2020-08-04")

# 3. Apply the function for all dates of interest
all_results <- lapply(dates_of_interest, function(date) {
  process_date_of_interest(data, date)
})

# 4. Combine results into a single dataset
final_dataset <- bind_rows(all_results, .id = "event_id")

setwd("~/outagesxasthma/data")

# 5. Save the final dataset
write.fst(final_dataset, "processed_data_for_augsynth.fst")

# 6. Visualization
# Filter for treated data
final_dataset_t <- final_dataset %>%
  filter(t_c == "t")

# Plot
ggplot(final_dataset_t, aes(x = time_block, y = dps_id, color = factor(trt), group = dps_id)) +
  geom_line(size = 1) +
  geom_point(size = 2, aes(shape = factor(trt))) +
  facet_wrap(~ event, scales = "free_x", ncol = 1) +
  labs(
    title = "Treatment Start Timing by ID and Event",
    x = "Time",
    y = "ID",
    color = "Treatment Started",
    shape = "Treatment Started"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 8)
  )
