library(pacman)

p_load(tidyverse, here, stringr, formr, logitr, cbcTools, texreg, likert, tidyr, haven, kableExtra,ggrepel, sjlabelled, summarytools,gtsummary,texreg)

# load data ----

df_model <- readRDS(here("data/formr", "df_model.rds"))

## Load data

library(dplyr)
library(broom)
library(purrr)
library(texreg)
library(stringr)

cost_name   <- "price_num"
set.seed(12345)  # or any fixed number

# Data cleaning ----

## Change factor levels ----

# -- Variable transformations, factor baselines, age groups, and derived vars --

df_model <- df_model %>%
  mutate(
    price_pp   = as.numeric(price) / 100,
    cost_new   = planed_cost * (1 + price_pp),
    cost_diff  = planed_cost * price_pp,
    price_pct  = as.numeric(price),
    proportion_of_income = planed_cost / income,
    age = floor(Age_T3),
    age_group = factor(case_when(
      age >= 55 & age < 65 ~ "55-64",
      age >= 65 & age < 75 ~ "65-74",
      age >= 75 ~ "75+"
    )),
    price_num = price_num / 100,
    
    # Set reference levels
    dist_trans = factor(dist_trans, levels = c("900", "600", "300")),
    dist_green = factor(dist_green, levels = c("15km", "5km", "500 meter")),
    dist_shops = factor(dist_shops, levels = c("15km", "5km", "500 meter"))
  ) %>%
  mutate(
    # Demographics
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
    downsize = if_else(planed_cost < monthcost, 1, 0),
    Retired = haven::as_factor(VAR174_8),  ) %>%
  # Age group dummies
  mutate(
    G65_74 = as.integer(age_group == "65-74"),
    G75p   = as.integer(age_group == "75+"),
    income = if_else(income == 0, NA, income),
    Sex = factor(if_else(Sex == "Man", "Men","Women")),
    Men = if_else(Sex == "Men", 1,0),
    Health = case_when( VAR035 %in% c(1,2,3) ~ "Not very good",
                        VAR035 %in% c(4,5) ~ "Very good",
                        TRUE ~ NA_character_)
  ) 


df_dummies <- model.matrix(~ dist_green + dist_shops + dist_trans + parking - 1, data = df_model) %>%
  as_tibble()

df_model <- bind_cols(df_model, df_dummies)

df_model <- df_model %>%
  rename(
    dist_green5km   = `dist_green5km`,
    dist_green500m  = `dist_green500 meter`,
    dist_shops5km   = `dist_shops5km`,
    dist_shops500m  = `dist_shops500 meter`,
    dist_trans600   = `dist_trans600`,
    dist_trans300   = `dist_trans300`,
    park_space      = `parkingreserverad P-plats`,
    park_garage     = `parkingreserverad garageplats`
  )

df_model <- df_model %>%
  mutate(
    green5km_Men   = dist_green5km  * Men,
    green500_Men   = dist_green500m * Men,
    shops5km_Men   = dist_shops5km  * Men,
    shops500_Men   = dist_shops500m * Men,
    trans600_Men   = dist_trans600  * Men,
    trans300_Men   = dist_trans300  * Men,
    park_space_Men = park_space     * Men,
    park_garage_Men= park_garage    * Men,
    price_Men      = price_num      * Men
  )

# --- Define main and interaction parameter names again ---
attr_vars <- c(
  "dist_green5km", "dist_green500m",
  "dist_shops5km", "dist_shops500m",
  "dist_trans600", "dist_trans300",
  "park_space", "park_garage",
  "price_num"
)

inter_vars <- c(
  "green5km_Men", "green500_Men",
  "shops5km_Men", "shops500_Men",
  "trans600_Men", "trans300_Men",
  "park_space_Men", "park_garage_Men",
  "price_Men"
)


## Create renter/owner subsets ----

df_owner <- df_model %>% 
  filter(Own == "Owner" )


df_renter <- df_model %>% 
  filter(Own == "Renter")


mxl_isex_ret <- logitr(
  data = as.data.frame(df_renter ),
  outcome = "choice",
  obsID = "obsID",
  panelID = "panelID",
  pars = c(
    "dist_green5km", "dist_green500m",
    "dist_shops5km", "dist_shops500m",
    "dist_trans600", "dist_trans300",
    "park_space", "park_garage",
    "price_num",
    "green5km_Men", "green500_Men",
    "shops5km_Men", "shops500_Men",
    "trans600_Men", "trans300_Men",
    "park_space_Men", "park_garage_Men",
    "price_Men"
  ),
  randPars = c(
    dist_green5km   = "n",
    dist_green500m  = "n",
    dist_shops5km   = "n",
    dist_shops500m  = "n",
    dist_trans600   = "n",
    dist_trans300   = "n",
    park_space      = "n",
    park_garage     = "n"
  ),
  numDraws = 100,
  numMultiStarts = 10,
  drawType = "sobol",
  correlation = TRUE
)

screenreg(mxl_isex_ret)

saveRDS(mxl_isex_ret, here("output/models", "mxl_sex_renter.rds"))


mxl_isex_own <- logitr(
  data = as.data.frame(df_owner ),
  outcome = "choice",
  obsID = "obsID",
  panelID = "panelID",
  pars = c(
    "dist_green5km", "dist_green500m",
    "dist_shops5km", "dist_shops500m",
    "dist_trans600", "dist_trans300",
    "park_space", "park_garage",
    "price_num",
    "green5km_Men", "green500_Men",
    "shops5km_Men", "shops500_Men",
    "trans600_Men", "trans300_Men",
    "park_space_Men", "park_garage_Men",
    "price_Men"
  ),
  randPars = c(
    dist_green5km   = "n",
    dist_green500m  = "n",
    dist_shops5km   = "n",
    dist_shops500m  = "n",
    dist_trans600   = "n",
    dist_trans300   = "n",
    park_space      = "n",
    park_garage     = "n"
  ),
  numDraws = 100,
  numMultiStarts = 10,
  drawType = "sobol",
  correlation = TRUE
)

screenreg(mxl_isex_own)

saveRDS(mxl_isex_own, here("output/models", "mxl_sex_owner.rds"))

