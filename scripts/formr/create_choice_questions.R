library(tidyverse)
library(cbcTools)
library("logitr")
library(texreg)
library(here)

# Create attributes and their levels

# profiles <- cbc_profiles(
#   price     = c("0","-20","-10","10","20"), # relative to current
#   dist_green = c("5km", "10km", "15km"),
#   dist_shops = c("500 meters", "5km", "15km"),
#   dist_trans = c("200", "400", "800"),
#   parking      = c('reserverad garageplats', ' reserverad P-plats', 'ingen P-plats')
# )

swe_profiles <- cbc_profiles(
  price     = c("0","-20","-10","10","20"), # relative to current
  dist_green = c("500 meter", "5km", "15km"),
  dist_shops = c("500 meter", "5km", "15km"),
  dist_trans = c("300", "600", "900"),
  parking      = c('reserverad garageplats', ' reserverad P-plats', 'ingen P-plats')
)


nrow(swe_profiles)

# Create design

set.seed(5678)

## D opt design

# design_dopt <- cbc_design(
#   profiles = profiles,
#   n_resp   = 800, # Number of respondents
#   n_alts   = 2,   # Number of alternatives per question
#   n_q      = 9,   # Number of questions per respondent
#   method   = 'orthogonal'
# )

swe_design_dopt <- cbc_design(
  profiles = swe_profiles,
  n_resp   = 800, # Number of respondents
  n_alts   = 2,   # Number of alternatives per question
  n_q      = 9,   # Number of questions per respondent
  method   = 'orthogonal'
)

swe_design_dopt_1 <- cbc_design(
  profiles = swe_profiles,
  n_resp   = 1600, # Number of respondents
  n_alts   = 2,   # Number of alternatives per question
  n_q      = 9,   # Number of questions per respondent
  method   = 'orthogonal'
)

# Random design

design_random <- cbc_design(
  profiles = swe_profiles,
  n_resp   = 800, # Number of respondents
  n_alts   = 2,   # Number of alternatives per question
  n_q      = 9,   # Number of questions per respondent
  method   = 'random' # This is the default method
)


# Descriptives and simulation ----

dim(design_dopt)
cbc_balance(swe_design_dopt)


dim(design_random)
cbc_balance(design_random)

# Simulate data 

data_dopt <- cbc_choices(
  design = design_dopt,
  obsID  = "obsID"
)

swe_data_dopt <- cbc_choices(
  design = swe_design_dopt,
  obsID  = "obsID"
)



data_rand <- cbc_choices(
  design = design_random,
  obsID  = "obsID"
) 


# Compare standard errors of simulated data between design options

power_rand <- cbc_power(
  data    = data_rand,
  pars    = c("price", "dist_green", "dist_shops", "dist_trans", "parking"),
  outcome = "choice",
  obsID   = "obsID",
  nbreaks = 10,
  n_q     = 9
)


power_dopt <- cbc_power(
  data    = data_dopt,
  pars    = c("price", "dist_green", "dist_shops", "dist_trans", "parking"),
  outcome = "choice",
  obsID   = "obsID",
  nbreaks = 10,
  n_q     = 9
)


swe_power_dopt <- cbc_power(
  data    = swe_data_dopt,
  pars    = c("price", "dist_green", "dist_shops", "dist_trans", "parking"),
  outcome = "choice",
  obsID   = "obsID",
  nbreaks = 10,
  n_q     = 9
)

plot(power_dopt)
plot(swe_power_dopt)

plot_compare_power(power_dopt,power_rand) +
   geom_vline(xintercept = 600, linetype="dotted", 
               color = "blue", size=1) +
  scale_colour_discrete(labels = c('D-opt', 'Random')) + 
  labs(title = "Estimating required sample size",
       subtitle = "Based on 9 choice sets per respondent",
       caption = "Shows standard errors (y-axis) from simulated logistic regressions with various sample sizes (x-axis).") +
  theme(plot.caption = element_text(hjust=0))
  


# Save design
write_csv(design_dopt, here("output/formr", "choice_questions.csv"))
write_csv(swe_design_dopt, here("output/formr", "swe_choice_questions_01.csv"))
write_csv(swe_design_dopt_1, here("output/formr", "swe_choice_questions_99.csv"))


