
library(pacman)

p_load(tidyverse, here, stringr, formr, logitr, cbcTools, texreg, likert, tidyr, haven, kableExtra,ggrepel, sjlabelled, summarytools,gtsummary,texreg)

# load data ----

df_model <- readRDS(here("data/formr", "df_model.rds"))

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
    price_num = price_num*0.1
    #green_x_male = (dist_green == "500 meter") * (Sex == 1)
  ) %>% 
  mutate(
    male    = as.integer(Sex == 1),
    female  = as.integer(Sex == 2),
    gar     = as.integer(parking == "reserverad garageplats"),
    splats  = as.integer(parking == "reserverad P-plats"),
    # interactions with price (in fraction units)
    gar_x_price   = gar   * price_pp,
    splats_x_price= splats* price_pp,
    green500 = as.integer(dist_green == "500 meter"),
    shops500 = as.integer(dist_shops == "500 meter"),
    # interactions with female
    green500_x_female = green500 * female,
    shops500_x_female = shops500 * female
  )



view(dfSummary(df_model %>% select(Sex,
                                   Age = Age_T3,
                                   age_group,
                                   Income = income,
                                   monthcost,
                                   planed_cost,
                                   income_qrt
                                   
                                   ) %>% mutate(Income = as.numeric(Income),
                                                monthcost= as.numeric(monthcost),
                                                planed_cost = as.numeric(planed_cost)),
          plain.ascii  = FALSE, 
          style        = "grid", 
          graph.magnif = 0.82, 
          varnumbers   = FALSE,
          valid.col    = FALSE))




# Regressions -----

## Baseline regression - Sex ----

m_male <- logitr(
  data    = df_model %>% filter(Sex == 1) ,
  outcome = "choice",
  obsID   = "obsID",
  panelID = "panelID",
  pars    = c("dist_green","dist_shops","dist_trans","parking","price_num")
)

m_a_mxl1 <- logitr(
  data    = df_model,
  outcome = "choice",
  obsID   = "obsID",
  panelID = "panelID",
  pars    = c("dist_green", "dist_shops", "dist_trans", "parking", "price_num"),
  randPars = c(dist_green = 'n', dist_shops = 'n', dist_trans = 'n', parking = 'n'),
  numMultiStarts = 10,
  drawType = 'sobol',
  numDraws = 50,
  correlation = TRUE
)


m_female <- logitr(
  data    = df_model %>% filter(Sex == 2) ,
  outcome = "choice",
  obsID   = "obsID",
  panelID = "panelID",
  pars    = c("dist_green","dist_shops","dist_trans","parking","price_num")
)

m_sex_interacition <- logitr(
  data    = df_model %>% mutate(Sex = Sex - 1,
                                price_sex = Sex*price_num),
  outcome = "choice",
  obsID   = "obsID",
  panelID = "panelID",
  pars    = c("dist_green","dist_shops","dist_trans","parking","price_num", "price_num*parking")
)


screenreg(list(m_male,m_a_mxl1),
          custom.model.names = c("M", "ML"))

## Baseline regression - age groups

m_1 <- logitr(
  data    = df_model %>% filter(age_group == "55-64") ,
  outcome = "choice",
  obsID   = "obsID",
  panelID = "panelID",
  pars    = c("dist_green","dist_shops","dist_trans","parking","price_num")
)

m_2 <- logitr(
  data    = df_model %>% filter(age_group == "65-74") ,
  outcome = "choice",
  obsID   = "obsID",
  panelID = "panelID",
  pars    = c("dist_green","dist_shops","dist_trans","parking","price_num")
)

m_3 <- logitr(
  data    = df_model %>% filter(age_group == "75+") ,
  outcome = "choice",
  obsID   = "obsID",
  panelID = "panelID",
  pars    = c("dist_green","dist_shops","dist_trans","parking","price_num")
)



screenreg(list(m_1, m_2, m_3),
          custom.model.names = c("55-64", "65-74", "75+"))

## WTP age groups ------

m_1 <- logitr(
  data    = df_model %>% filter(age_group == "55-64") ,
  outcome = "choice",
  obsID   = "obsID",
  panelID = "panelID",
  pars    = c("dist_green","dist_shops","dist_trans","parking"),
  scalePar = "price_num"
)

m_2 <- logitr(
  data    = df_model %>% filter(age_group == "65-74") ,
  outcome = "choice",
  obsID   = "obsID",
  panelID = "panelID",
  pars    = c("dist_green","dist_shops","dist_trans","parking"),
  scalePar = "price_num"
)

m_3 <- logitr(
  data    = df_model %>% filter(age_group == "75+") ,
  outcome = "choice",
  obsID   = "obsID",
  panelID = "panelID",
  pars    = c("dist_green","dist_shops","dist_trans","parking"),
  scalePar = "price_num"
)


screenreg(list(m_1, m_2, m_3),
          custom.model.names = c("55-64", "65-74", "75+"))

## Income quartiles -----


m_1 <- logitr(
  data    = df_model %>% filter(income_qrt == "Q1") ,
  outcome = "choice",
  obsID   = "obsID",
  panelID = "panelID",
  pars    = c("dist_green","dist_shops","dist_trans","parking"),
  scalePar = "price"
)

m_2 <- logitr(
  data    = df_model %>% filter(income_qrt == "Q2") ,
  outcome = "choice",
  obsID   = "obsID",
  panelID = "panelID",
  pars    = c("dist_green","dist_shops","dist_trans","parking"),
  scalePar = "price"
)

m_3 <- logitr(
  data    = df_model %>% filter(income_qrt == "Q3") ,
  outcome = "choice",
  obsID   = "obsID",
  panelID = "panelID",
  pars    = c("dist_green","dist_shops","dist_trans","parking"),
  scalePar = "price"
)


m_4 <- logitr(
  data    = df_model %>% filter(income_qrt == "Q4") ,
  outcome = "choice",
  obsID   = "obsID",
  panelID = "panelID",
  pars    = c("dist_green","dist_shops","dist_trans","parking"),
  scalePar = "cost"
)



screenreg(list(m_1, m_2, m_3),
          custom.model.names = c("Q1", "Q2", "Q3"))



screenreg(m_2)









m_a <- logitr(
  data    = df_model,
  outcome = "choice",
  obsID   = "obsID",
  panelID = "panelID",
  pars    = c("dist_green", "dist_shops", "dist_trans", "parking", "price_num")
)

m_pref <- logitr(
  data    = df_model,
  outcome = "choice",
  obsID   = "obsID",
  panelID = "panelID",
  pars    = c("dist_green", "dist_shops", "dist_trans", "parking" ),
  scalePar = "price_num",
  numMultiStarts = 10
)

m_a_mxl <- logitr(
  data    = df_model,
  outcome = "choice",
  obsID   = "obsID",
  panelID = "panelID",
  pars    = c("dist_green", "dist_shops", "dist_trans", "parking", "price_num"),
  randPars = c(dist_green = 'n', dist_shops = 'n', dist_trans = 'n', parking = 'n'),
  numMultiStarts = 10,
  drawType = 'sobol',
  numDraws = 50
)


m_a_mxl1 <- logitr(
  data    = df_model,
  outcome = "choice",
  obsID   = "obsID",
  panelID = "panelID",
  pars    = c("dist_green", "dist_shops", "dist_trans", "parking", "price_num"),
  randPars = c(dist_green = 'n', dist_shops = 'n', dist_trans = 'n', parking = 'n'),
  numMultiStarts = 10,
  drawType = 'sobol',
  numDraws = 50,
  correlation = TRUE
)


screenreg(list(m_pref      )    )

wtp(m_a_mxl1, scalePar = "price_num")


wtpCompare(m_a, m_a_mxl,  scalePar = "price_num")

plot(wtpCompare(m_a, m_pref, scalePar = "price_num"))

