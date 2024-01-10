library(tidyverse)
library(cbcTools)
library("logitr")
library(texreg)
library(here)

# Create attributes and their levels

profiles <- cbc_profiles(
  price     = c("20% less","10% less", "Same","10% more", "20% more"), # relative to current
  dist_green = c("5km", "10km", "15km"),
  dist_shops = c("5km", "10km", "15km"),
  dist_trans = c("200m", "400m", "800m"),
  parking      = c('Garage', 'On street free', 'On street paid')
)

nrow(profiles)

# Create design

set.seed(5678)

## D opt design

design_dopt <- cbc_design(
  profiles = profiles,
  n_resp   = 1000, # Number of respondents
  n_alts   = 2,   # Number of alternatives per question
  n_q      = 10,   # Number of questions per respondent
  method   = 'orthogonal'
)
#> D-optimal design found with D-efficiency of 0.6

dim(design_dopt)
cbc_balance(design_dopt)

## Bayesian design 

design_bayesian <- cbc_design(
  profiles  = profiles,
  n_resp    = 100, # Number of respondents
  n_alts    = 2, # Number of alternatives per question
  n_q       = 10, # Number of questions per respondent
  method = 'Modfed'
)

dim(design_bayesian)

cbc_balance(design_bayesian)

# Save design
write_csv(design_dopt, here("output/formr", "choice_questions.csv"))
