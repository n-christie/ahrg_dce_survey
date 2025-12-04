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
library(gmnl)
library(mlogit)

set.seed(12345) 

# Data cleaning ----

## Change factor levels ----

df_model <- df_model %>%
  mutate(
    price_pp   = as.numeric(price) / 100,                 # -0.20 … +0.20 (fraction)
    cost_new   = planed_cost * (1 + price_pp),            # SEK total under scenario
    cost_diff  = planed_cost * price_pp,                  # SEK delta from baseline (can be 0)
    # keep a numeric % version too if you want it
    price_pct  = as.numeric(price),                       # -20..20 (percent units)
    # set consistent factor baselines
    dist_trans = factor(dist_trans, levels = c("900","600","300")),
    dist_green = factor(dist_green, levels = c("15km", "5km", "500 meter")),
    dist_shops = factor(dist_shops, levels = c("15km", "5km", "500 meter")),
    proportion_of_income = planed_cost/income,
    age = floor(Age_T3),
    age_group = factor(case_when(
      age >= 55 & age < 65 ~ "55-64",
      age >= 65 & age < 75 ~ "65-74",
      age >= 75 ~ "75+",
    )),
    price_num = price_num/100
  ) %>% 
  mutate(
    ägandebostad = haven::as_factor(ägandebostad),
    bostadstyp = haven::as_factor(bostadstyp),
    Sex = haven::as_factor(Sex),
    Civil = haven::as_factor(civil_status_T2),
    civil_d = factor(if_else(civil_status_T2 == 1, "Partnered", "Not partnered")),
    Own = factor(if_else(ägandebostad == "Ja", "Owner", "Renter")),
    Hus = factor(if_else( bostadstyp == "Friliggande villa/hus/gård" | bostadstyp == "Radhus/kedjehus/parhus", "House", "Apartment/Condo")),
    downsize = if_else(planed_cost < monthcost, 1, 0),
    location = case_when(VAR010 == 1 ~ "City/town",
                         VAR010 == 2 | VAR010 == 2 ~ "Urban/countryside",
                         TRUE ~ NA ),
    #income_c = first(income) - mean(income, na.rm = TRUE),
    is_owner = ifelse(Own == "Owner", 1, 0),
    #is_renter = first(Hus == "Renter")
    
  )

# ==============================================================================
# CORRECT DATA SETUP FOR GMNL LATENT CLASS MODELS
# ==============================================================================

library(gmnl)
library(mlogit)
library(dplyr)
library(texreg)

# ------------------------------------------------------------------------------
# 1. PREPARE DATA FOR GMNL (CRITICAL STEP)
# ------------------------------------------------------------------------------

# Check your data structure first
head(df_model)
table(df_model$choice)  # Should have 0s and 1s
length(unique(df_model$obsID))  # Number of choice tasks
length(unique(df_model$panelID))  # Number of individuals

# IMPORTANT: gmnl/mlogit.data needs specific structure
# - Each choice task should have multiple rows (one per alternative)
# - One alternative per row should be chosen (choice = TRUE or 1)

# Convert to mlogit.data format
df_gmnl <- mlogit.data(
  data = df_model,
  choice = "choice",      # Column with TRUE/FALSE or 1/0
  shape = "long",         # Each alternative is a row
  alt.var = "altID",      # Alternative identifier within each choice task
  chid.var = "obsID",     # Choice task identifier
  id.var = "panelID",     # Individual identifier
  opposite = c("price_num")  # Variables where higher = less utility
)


# ------------------------------------------------------------------------------
# 2. FIT BASIC MULTINOMIAL LOGIT (for comparison)
# ------------------------------------------------------------------------------

mnl_base <- gmnl(
  choice ~ dist_green + dist_shops + dist_trans + parking + price_num | 0,
  data = df_gmnl,
  model = "mnl"
)

summary(mnl_base)

# ------------------------------------------------------------------------------
# 3. FIT LATENT CLASS MODELS
# ------------------------------------------------------------------------------

# 2-class model (basic - no covariates for class membership)
lc_2class <- gmnl(
  choice ~ dist_green + dist_shops + dist_trans + parking + price_num | 0 | 0 | 0 | 1,
  data = df_gmnl,
  model = "lc",
  Q = 2,
  panel = TRUE
)

summary(lc_2class)

# 3-class model
lc_3class <- gmnl(
  choice ~ dist_green + dist_shops + dist_trans + parking + price_num | 0 | 0 | 0 | 1,
  data = df_gmnl,
  model = "lc",
  Q = 3,
  panel = TRUE
)

summary(lc_3class)




# ------------------------------------------------------------------------------
# 4. MODEL WITH INDIVIDUAL CHARACTERISTICS FOR CLASS MEMBERSHIP
# ------------------------------------------------------------------------------

# First, make sure individual-specific variables are available
# They should be constant within each individual (panelID)

# Add individual characteristics if not already in df_gmnl
# These need to be added BEFORE mlogit.data conversion for class membership covariates



df_gmnl2 <- mlogit.data(
  data = df_model,
  choice = "choice",
  shape = "long",
  alt.var = "altID",
  chid.var = "obsID",
  id.var = "panelID"
)

# Model with covariates predicting class membership
# Formula: choice ~ choice_vars | 0 | 0 | 0 | class_membership_vars
lc_2class_cov <- gmnl(
  choice ~ dist_green + dist_shops + dist_trans + parking + price_num | 
    0 | 0 | 0 | 
    age + is_owner ,
  data = df_gmnl2,
  model = "lc",
  Q = 2,
  panel = TRUE,
  method = "bfgs"  # Can help with convergence
)

summary(lc_2class_cov)

# ------------------------------------------------------------------------------
# 5. EXTRACT CLASS MEMBERSHIP PROBABILITIES
# ------------------------------------------------------------------------------

# Get individual-level class probabilities
class_probs <- lc_2class$Qir
head(class_probs)

# Create data frame with class assignments
unique_panels <- df_model %>%
  distinct(panelID, .keep_all = TRUE) %>%
  select(panelID, age, Own, )

class_assignments <- data.frame(
  panelID = unique(df_gmnl$id),
  class_prob_1 = class_probs[, 1],
  class_prob_2 = class_probs[, 2]
) %>%
  mutate(assigned_class = ifelse(class_prob_1 > class_prob_2, 1, 2))

# Merge with demographics
class_profiles <- class_assignments %>%
  left_join(unique_panels, by = "panelID")

# Profile each class
summary_by_class <- class_profiles %>%
  group_by(assigned_class) %>%
  summarise(
    n = n(),
    pct_of_sample = n() / nrow(.) * 100,
    avg_age = mean(age, na.rm = TRUE),
    pct_owner = mean(Own == "Owner", na.rm = TRUE) * 100,
    pct_house = mean(Hus == "House", na.rm = TRUE) * 100,
    avg_income = mean(income, na.rm = TRUE)
  )

print(summary_by_class)

# ------------------------------------------------------------------------------
# 6. EXTRACT AND INTERPRET CLASS-SPECIFIC COEFFICIENTS
# ------------------------------------------------------------------------------

# Get coefficients
coefs <- coef(lc_2class)
n_vars <- 5  # dist_green, dist_shops, dist_trans, parking, price_num

# Class 1 coefficients (first n_vars parameters)
class1_coefs <- coefs[1:n_vars]
names(class1_coefs) <- c("dist_green", "dist_shops", "dist_trans", "parking", "price_num")

# Class 2 coefficients (next n_vars parameters)
class2_coefs <- coefs[(n_vars + 1):(2 * n_vars)]
names(class2_coefs) <- c("dist_green", "dist_shops", "dist_trans", "parking", "price_num")

# Display
coef_comparison <- data.frame(
  Variable = names(class1_coefs),
  Class1 = class1_coefs,
  Class2 = class2_coefs
)
print(coef_comparison)

# ------------------------------------------------------------------------------
# 7. WILLINGNESS TO PAY CALCULATIONS
# ------------------------------------------------------------------------------

# WTP = -beta_attribute / beta_price
wtp_class1 <- -class1_coefs[-5] / class1_coefs["price_num"]
wtp_class2 <- -class2_coefs[-5] / class2_coefs["price_num"]

wtp_comparison <- data.frame(
  Attribute = names(wtp_class1),
  WTP_Class1 = wtp_class1,
  WTP_Class2 = wtp_class2,
  Difference = wtp_class1 - wtp_class2
)
print(wtp_comparison)

# ------------------------------------------------------------------------------
# 8. MODEL COMPARISON TABLE
# ------------------------------------------------------------------------------

# Compare models
model_stats <- data.frame(
  Model = c("MNL", "LC-2", "LC-3"),
  LogLik = c(logLik(mnl_base), logLik(lc_2class), logLik(lc_3class)),
  K = c(
    length(coef(mnl_base)),
    length(coef(lc_2class)),
    length(coef(lc_3class))
  )
) %>%
  mutate(
    AIC = -2 * LogLik + 2 * K,
    BIC = -2 * LogLik + log(nrow(df_gmnl)) * K
  )

print(model_stats)

# Use texreg for publication-ready table
screenreg(
  list(mnl_base, lc_2class, lc_3class),
  custom.model.names = c("MNL", "2-Class LC", "3-Class LC")
)

# ------------------------------------------------------------------------------
# 9. FILTER BY SUBGROUP (like your Owner filter)
# ------------------------------------------------------------------------------

# Filter for owners only
df_owner <- df_model %>% filter(Own == "Owner")

df_gmnl_owner <- mlogit.data(
  data = df_owner,
  choice = "choice",
  shape = "long",
  alt.var = "altID",
  chid.var = "obsID",
  id.var = "panelID"
)

lc_2class_owner <- gmnl(
  choice ~ dist_green + dist_shops + dist_trans + parking + price_num | 0 | 0 | 0 | 1,
  data = df_gmnl_owner,
  model = "lc",
  Q = 2,
  panel = TRUE
)

summary(lc_2class_owner)