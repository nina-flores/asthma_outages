# Load necessary libraries
require(dplyr)
require(tidyr)
require(fst)
require(sf)
require(lubridate)
require(ggplot2)
require(ggpubr)
require(scales)
require(purrr)

### Load augmented synthetic control functions
setwd("~/outagesxasthma/scripts/augmented synthetic control/aug-synth-package-r-files")
source("run-all.R")

### Read in the data
setwd("~/outagesxasthma/data")
data <- read.fst("analytical_data_for_augsynth.fst") 

sum(data$sum_ed)


### Set up locality shapefile
setwd("~/outagesxasthma/data/locality")
locality <- st_read("e_locality.shp")

dps <- unique(data$dps_id)


### Custom palette
custom_palette <- c("unexposed" = "#9D7AAD", "exposed" = "#59234E", "excluded" = "white")

### Helper function to process each event
process_event <- function(event_data, event_name, locality) {
  # Identify treated areas
  data_map_t <- event_data %>% filter(t_c == "t")
  data_map_c <- event_data %>% filter(t_c == "c")
  
  
  locality <- locality %>%
    filter(PRIME_DPS_ %in% dps) %>% 
    mutate(
      treated = case_when(
        PRIME_DPS_ %in% data_map_t$dps_id ~ "exposed",
        PRIME_DPS_ %in% data_map_c$dps_id ~ "unexposed",
        !PRIME_DPS_ %in% data_map_t$dps_id & !PRIME_DPS_ %in% data_map_c$dps_id ~ "excluded"
      )
    )
  
  
  # Create map
  map_plot <- ggplot(locality) +
    geom_sf(aes(fill = treated), color = "black") +
    scale_fill_manual(values = custom_palette) +
    theme_void() +
    labs(fill = "") +
    theme(text = element_text(size = 11), legend.position = "bottom") +
    ggtitle(paste0("Map for: ", event_name))
  
  start_time <- unique(event_data$start)
  
  
  # Run model
  asyn_model <- augsynth(
    sum_ed ~ trt | mean_temp + total_precip + max_ws + mean_rh +
       Daily.Mean.PM2.5.Concentration + annual_pm,
    unit = dps_id,
    time = time_block,
    t_int = start_time,
    data = event_data,
    progfunc = "ridge",
    scm = TRUE,
    fixedeff = TRUE
  )
  

  
  # Model summary
  print(summary(asyn_model))
  
  # Plot model results
  # Replace "Inf" in 'end' with a reasonable maximum
  
  start_time <- unique(event_data$start)
  end_time <- unique(event_data$end)
  
  print(start_time)
  print(end_time)
  
  
  model_plot <- plot(asyn_model) +
    xlab("Time (six hour increments)") +
    ylab("Difference in ED visits per 1,000 \n(exposed - synthetic)") +
    theme(
      text = element_text(size = 11),
      axis.text.x = element_text(angle = 90, vjust = 0.5)
    ) +
    scale_x_datetime(
      breaks = unique(event_data$time_block),
      date_labels = "%d %l:%M %p"
    ) +
   # scale_y_continuous(labels = scales::percent) +
    ggtitle(paste0("Model results for: ", event_name, ", max out: ", round(unique(event_data$max_sum_out)))) +
    geom_rect(aes(xmin = start_time, xmax = end_time, ymin = -Inf, ymax = Inf), fill = "grey90", alpha = 0.02)
  
  list(map_plot = map_plot, model_plot = model_plot)
}

### Process all events
event_list <- unique(data$event)
results <- lapply(event_list, function(event_name) {
  event_data <- data %>% filter(event == event_name) %>%
    mutate(sum_ed = (sum_ed/customers)*1000)
  process_event(event_data, event_name, locality)
})




### Combine plots for all events
map_plots <- lapply(results, `[[`, "map_plot")
model_plots <- lapply(results, `[[`, "model_plot")

# Arrange each event's results side by side
combined_plots <- mapply(
  function(map, model) ggarrange(map, model, ncol = 2, nrow = 1, widths = c(2, 3)),
  map_plots, model_plots, SIMPLIFY = FALSE
)

# Combine all events into one plot
all_plots <- ggarrange(
  plotlist = combined_plots,
  nrow = 4
) 


# Save the combined plot
setwd("~/outagesxasthma/figures")
png("ASC_all_events_rates.png", width = 8.5, height =11.2, unit = "in", res = 300)
print(all_plots)
dev.off()
