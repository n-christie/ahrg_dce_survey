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
    income = if_else(income == 0, NA, income)
  ) 
# --- Create high/low income variable ---

median_income <- median(df_model$income, na.rm = TRUE)

df_model <- df_model %>%
  mutate(
    income_group = factor(
      if_else(income >= median_income, "High", "Low"),
      levels = c("Low", "High")
    ),
    HighInc = as.integer(income_group == "High")  # dummy for interaction
  )

summary(df_model$income_group)




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
    green5km_HighInc   = dist_green5km  * HighInc,
    green500_HighInc   = dist_green500m * HighInc,
    shops5km_HighInc   = dist_shops5km  * HighInc,
    shops500_HighInc   = dist_shops500m * HighInc,
    trans600_HighInc   = dist_trans600  * HighInc,
    trans300_HighInc   = dist_trans300  * HighInc,
    park_space_HighInc = park_space     * HighInc,
    park_garage_HighInc= park_garage    * HighInc,
    price_HighInc      = price_num      * HighInc
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
  "green5km_HighInc", "green500_HighInc",
  "shops5km_HighInc", "shops500_HighInc",
  "trans600_HighInc", "trans300_HighInc",
  "park_space_HighInc", "park_garage_HighInc",
  "price_HighInc"
)


df_retired <- df_model %>%
  group_by(obsID) %>%
  filter(all(Retired == "Yes"),
         !is.na(income)) %>%
  ungroup()

df_notretired <- df_model %>%
  group_by(obsID) %>%
  filter(all(Retired == "No")) %>%
  ungroup()


# remove any suffixed duplicate columns
df_retired <- df_retired %>%
  select(-matches("\\.\\.\\d+$"))

# or, equivalently for all of df_model
df_model <- df_model %>%
  select(-matches("\\.\\.\\d+$"))

df_model$choice <- as.numeric(as.character(df_model$choice))
df_model$choice[df_model$choice != 1] <- 0


mxl_income_ret <- logitr(
  data = as.data.frame(df_model %>% filter(Retired == "Yes",
                                           !is.na(income)) ),
  outcome = "choice",
  obsID = "obsID",
  panelID = "panelID",
  pars = c(
    "dist_green5km", "dist_green500m",
    "dist_shops5km", "dist_shops500m",
    "dist_trans600", "dist_trans300",
    "park_space", "park_garage",
    "price_num",
    "green5km_HighInc", "green500_HighInc",
    "shops5km_HighInc", "shops500_HighInc",
    "trans600_HighInc", "trans300_HighInc",
    "park_space_HighInc", "park_garage_HighInc",
    "price_HighInc"
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
summary(mxl_income_ret)

saveRDS(mxl_income_ret, here("output/models", "mxl_income_retired.rds"))




mxl_income_noret <- logitr(
  data = as.data.frame(df_model %>% filter(Retired == "No",
                                           !is.na(income)) ),
  outcome = "choice",
  obsID = "obsID",
  panelID = "panelID",
  pars = c(
    "dist_green5km", "dist_green500m",
    "dist_shops5km", "dist_shops500m",
    "dist_trans600", "dist_trans300",
    "park_space", "park_garage",
    "price_num",
    "green5km_HighInc", "green500_HighInc",
    "shops5km_HighInc", "shops500_HighInc",
    "trans600_HighInc", "trans300_HighInc",
    "park_space_HighInc", "park_garage_HighInc",
    "price_HighInc"
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
summary(mxl_income_noret)

saveRDS(mxl_income_noret, here("output/models", "mxl_income_noretired.rds"))




