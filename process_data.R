library(tidyverse)

data("billboard", package = "tidyr")

billboard_long <- billboard |>
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    names_prefix = "wk",
    names_transform = list(week = as.integer),
    values_to = "rank",
    values_drop_na = TRUE
  ) |>
  mutate(chart_date = as.Date(date.entered) + weeks(week - 1))

july_top_five <- billboard_long |>
  filter(chart_date <= as.Date("2000-07-01")) |>
  group_by(artist, track) |>
  slice_max(chart_date, n = 1, with_ties = FALSE) |>
  ungroup() |>
  slice_min(rank, n = 5, with_ties = FALSE) |>
  transmute(artist, track, song_artist = paste0(track, " (", artist, ")"))

top_songs <- billboard_long |>
  inner_join(july_top_five, by = c("artist", "track")) |>
  filter(chart_date >= as.Date("2000-01-01"),
         chart_date <= as.Date("2001-01-01")) 

         write_rds(top_songs, file = "clean_data.rds") 
         