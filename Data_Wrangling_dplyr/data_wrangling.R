# Data Wrangling with dplyr ---------------------------------------------

library(dplyr)
library(nycflights13)

# Filter United Airlines flights to San Francisco on June 22.
ua_june_22_sfo <- flights %>%
  filter(
    carrier == "UA",
    month == 6,
    day == 22,
    dest == "SFO"
  ) %>%
  arrange(desc(arr_delay)) %>%
  rename(
    dep_time_actual = dep_time,
    dep_time_scheduled = sched_dep_time,
    departure_delay = dep_delay,
    arr_time_actual = arr_time
  )

# Inspect missing values in the selected data.
missing_by_variable <- colSums(is.na(ua_june_22_sfo))

# Summarize arrival and departure delays.
delay_summary <- ua_june_22_sfo %>%
  summarise(
    mean_arrival_delay = mean(arr_delay, na.rm = TRUE),
    mean_departure_delay = mean(departure_delay, na.rm = TRUE),
    sd_arrival_delay = sd(arr_delay, na.rm = TRUE),
    sd_departure_delay = sd(departure_delay, na.rm = TRUE)
  )

# Export the prepared file if an output copy is needed.
write.csv(ua_june_22_sfo, "ua_june_22_sfo.csv", row.names = FALSE)

print(missing_by_variable)
print(delay_summary)
