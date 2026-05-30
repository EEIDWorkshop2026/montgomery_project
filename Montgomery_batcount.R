batcount <- read.csv("../materials/data/bat_count.csv")

View(batcount)

library(tidyverse)

# transforming from long to wide
bat_wide <- batcount %>%
  pivot_wider(
    names_from = "species", values_from = "count",
  )
View(bat_wide)

species <- unique(batcount$species)

# transforming from wide to long
bat_long <- bat_wide %>%
  pivot_longer(
    cols = all_of(species),
    names_to = "species",
    values_to = "count"
  )

# swapping column order
bat_long <- bat_long[, c(1, 3, 2, 4)]

View(bat_long)
