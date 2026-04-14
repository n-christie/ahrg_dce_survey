# table_01_attributes.R
# Table 1: DCE Attributes and their corresponding levels.
# No model or survey data needed — all values are hard-coded from the study design.
# Output: output/tables/table_01_attributes.html

library(pacman)
p_load(here, flextable, tibble)

# Attribute/level table ----
attributes_df <- tibble(
  Attribute = c(
    "Distance to green areas",
    "Distance to shops",
    "Distance to public transport",
    "Parking",
    "Housing cost (vs current)"
  ),
  Levels = c(
    "500 m, 5 km, 15 km",
    "500 m, 5 km, 15 km",
    "300 m, 600 m, 900 m",
    "None, Reserved space, Reserved garage",
    "−20%, −10%, 0%, +10%, +20%"
  )
)

# Build flextable ----
ft <- flextable(attributes_df) |>
  bold(part = "header") |>
  autofit() |>
  theme_booktabs() |>
  set_caption(caption = "Table 1. DCE attributes and their corresponding levels")

# Save as HTML ----
save_as_html(ft, path = here("output/tables", "table_01_attributes.html"))


message("Table 1 saved to output/tables/table_01_attributes.html")
