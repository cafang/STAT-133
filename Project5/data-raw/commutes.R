# Run this script once from the package root to collect a new snapshot.
# This script calls the API; rendering project.qmd does not.
library(devtools)
library(usethis)
library(dplyr)

load_all()
stations <- bart_stations()
trips <- lapply(stations$abbr, bart_trip) |>
  bind_rows()

commutes <- stations |>
  left_join(trips, by = c("abbr" = "orig")) |>
  mutate(weekly_cost = 2 * 5 * fare)

use_data(commutes, overwrite = TRUE)
file.size("data/commutes.rda") / 1e6
