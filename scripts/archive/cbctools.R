library(tidyverse)
library(cbcTools)
library("logitr")
library(texreg)

# Create attributes and their levels

profiles <- cbc_profiles(
  price     = c("20% less","10% less", "Same","10% more", "20% more"), # relative to current
  dist_green = c("5km", "10km", "15km"),
  dist_shops = c("5km", "10km", "15km"),
  dist_trans = c("5km", "10km", "15km"),
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

# Simulate data  orthogonal

data_dopt <- cbc_choices(
  design = design_dopt,
  obsID  = "obsID"
)



# Simulate data bayesian

data_bay <- cbc_choices(
  design = design_bayesian,
  obsID  = "obsID"
)



# simulate data with restrictions

data <- cbc_choices(
  design = design,
  obsID = "obsID",
  priors = list(
    price     = -0.1,
    type      = c(0.1, 0.2),
    freshness = c(0.1, 0.2)
  )
)




# Estimate a model

m1_dopt <- logitr(
  data    = data_dopt,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c( "dist_green", "dist_shops","dist_trans","parking"),
  scalePar = "price"
)

screenreg(m1_dopt)


power_dopt <- cbc_power(
  data    = data_dopt,
  pars    = c("price", "dist_green", "dist_shops", "dist_trans", "parking"),
  outcome = "choice",
  obsID   = "obsID",
  nbreaks = 10,
  n_q     = 6
)


power_bay <- cbc_power(
  data    = data_bay,
  pars    = c("price", "dist_green", "dist_shops", "dist_trans", "parking"),
  outcome = "choice",
  obsID   = "obsID",
  nbreaks = 10,
  n_q     = 6
)

plot_compare_power(power_dopt, power_bay)
