# 00_data_prep.R
# Shared data loading and cleaning for all paper table scripts.
# Source this file at the top of each table script that requires survey data.
#
# Output objects created:
#   df_model  - cleaned row-level DCE data (one row per choice occasion)
#   df_owner  - subset: owners only
#   df_renter - subset: renters only

library(pacman)
p_load(tidyverse, here, haven, logitr)

# Load raw model data ----
df_model <- readRDS(here("data/formr", "df_model.rds"))

# Core variable transformations ----
df_model <- df_model |>
  mutate(
    # Price variables
    price_pp             = as.numeric(price) / 100,
    cost_new             = planed_cost * (1 + price_pp),
    cost_diff            = planed_cost * price_pp,
    price_pct            = as.numeric(price),
    price_num            = price_num / 100,
    proportion_of_income = planed_cost / income,

    # Set attribute reference levels
    dist_trans = factor(dist_trans, levels = c("900", "600", "300")),
    dist_green = factor(dist_green, levels = c("15km", "5km", "500 meter")),
    dist_shops = factor(dist_shops, levels = c("15km", "5km", "500 meter")),

    # Age
    age       = floor(Age_T3),
    age_group = factor(case_when(
      age >= 55 & age < 65 ~ "55-64",
      age >= 65 & age < 75 ~ "65-74",
      age >= 75             ~ "75+"
    ))
  ) |>
  mutate(
    # Categorical / label recodes
    ägandebostad = haven::as_factor(ägandebostad),
    bostadstyp   = haven::as_factor(bostadstyp),
    Sex          = haven::as_factor(Sex),
    Civil        = haven::as_factor(civil_status_T2),
    civil_d      = factor(if_else(civil_status_T2 == 1, "Partnered", "Not partnered")),
    Own          = factor(if_else(ägandebostad == "Ja", "Owner", "Renter")),
    Hus          = factor(if_else(
      bostadstyp %in% c("Friliggande villa/hus/gård", "Radhus/kedjehus/parhus"),
      "House", "Apartment/Condo"
    )),
    downsize     = if_else(planed_cost < monthcost, 1, 0),
    Retired      = haven::as_factor(VAR174_8),

    # Derived binary indicators
    G65_74   = as.integer(age_group == "65-74"),
    G75p     = as.integer(age_group == "75+"),
    income   = if_else(income == 0, NA_real_, as.numeric(income)),
    Sex      = factor(if_else(Sex == "Man", "Men", "Women")),
    Men      = as.integer(Sex == "Men"),
    Health   = case_when(
      VAR035 %in% c(1, 2, 3) ~ "Not very good",
      VAR035 %in% c(4, 5)    ~ "Very good",
      TRUE                   ~ NA_character_
    ),
    Good     = as.integer(Health == "Very good"),
    Old      = as.integer(age >= 72),
    High_age = as.integer(Age_T3 > median(Age_T3, na.rm = TRUE)),

    # Location
    location = case_when(
      VAR010 == 1 ~ "City/town",
      VAR010 == 2 ~ "Urban/countryside",
      TRUE        ~ NA_character_
    )
  )

# Tenure subsets ----
df_owner  <- df_model |> filter(Own == "Owner")
df_renter <- df_model |> filter(Own == "Renter")
