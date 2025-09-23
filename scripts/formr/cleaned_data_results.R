
library(pacman)

p_load(tidyverse, here, stringr, formr, logitr, cbcTools, texreg, likert, tidyr, haven, kableExtra,ggrepel, sjlabelled, summarytools,gtsummary)

# load data ----


survey_design <-  read_csv("https://github.com/n-christie/ahrg_dce_survey/blob/main/output/formr/swe_choice_questions_01.csv?raw=true")

dce_survey_df <- readRDS(here("data/formr", "08_02_results.rds")) %>% 
  mutate(ended_survey = if_else(is.na(ended_page_4), ended_page_2, ended_page_4)) %>% 
  filter(created_page_0 > "2024-05-29",
         !is.na(ended_survey))%>% 
  distinct(user_code, .keep_all = TRUE)

sample_df <- readRDS(here("data/formr", "dce_anym_output_1.rds"))


# Add other survey data ----
t3_sur <- readRDS("~/Projects/!med/ahrg_reloc_age_dce/data/rt3_eng.rds") %>% 
  select(RespondentID,
         VAR174_8,
         VAR010,
         VAR011,
         VAR022,
         VAR028_1,
         VAR028_2,
         VAR028_3,
         VAR033_1,
         VAR033_2,
         VAR035,
         VAR162,
         VAR163
         
         
         
  )



survey_df <- sample_df %>% 
  left_join(dce_survey_df,
            by = c("Användarnamn" = "user_code")) %>% 
  left_join(t3_sur,
            by = c("RespondentID")) %>% 
  distinct(Användarnamn, .keep_all = TRUE) %>% 
  mutate(respondentID = respondentID.y)


## data tidying - Need to sort out the multiple observations!!!!! -----


# ----- Build per-person choices (one row per person × task with chosen altID) -----
cbc_cols <- grep("^cbc\\d+$", names(survey_df), value = TRUE)

df_regs <- survey_df %>%
  transmute(
    personID = as.integer(RespondentID),                              # true person ID
    designID = parse_integer(as.character(respondentID)),             # design assignment ID
    across(all_of(cbc_cols))
  ) %>%
  pivot_longer(
    cols = all_of(cbc_cols),
    names_to  = "cbc_name",
    values_to = "chosen"
  ) %>%
  mutate(
    qID    = parse_integer(str_remove(cbc_name, "^cbc")),
    chosen = as.integer(chosen)
  ) %>%
  select(personID, designID, qID, chosen) %>%
  filter(!is.na(personID), !is.na(designID), !is.na(qID))

## -------------------------
## 2) Design alternatives
## -------------------------
design_tbl <- survey_design %>%
  transmute(
    respID     = as.integer(respID),     # matches designID
    qID        = as.integer(qID),
    altID      = as.integer(altID),
    profileID,
    price,
    dist_green,
    dist_shops,
    dist_trans,
    parking
  )

##  Add vars to df_model -------------------------
## 3) Merge + IDs for logitr
## -------------------------
df_merged <- df_regs %>%
  inner_join(
    design_tbl,
    by = c("designID" = "respID", "qID"),
    relationship = "many-to-many"
  ) %>%
  mutate(
    price_num = parse_number(as.character(price)),          # numeric % price change
    choice    = as.integer(altID == chosen),                # 1 if chosen
    panelID   = as.integer(factor(personID)),               # dense 1..N person IDs
    obsID     = as.integer(factor(paste(personID, qID)))    # unique person × task
  ) %>%
  filter(!is.na(panelID), !is.na(obsID)) %>%
  arrange(panelID, obsID, altID)

## -------------------------
## 4) Attach respondent covariates -----
## -------------------------
df_covars <- survey_df %>%
  distinct(RespondentID, .keep_all = TRUE) %>%
  select(RespondentID, Sex, Age_T3, civil_status_T2, income, monthcost, planed_cost, bostadstyp, ägandebostad,         VAR174_8,
         VAR010,
         VAR011,
         VAR022,
         VAR028_1,
         VAR028_2,
         VAR028_3,
         VAR033_1,
         VAR033_2,
         VAR035,
         VAR162,
         VAR163) %>%
  mutate(panelID = as.integer(factor(RespondentID)),
         income_qrt = factor(ntile(income, 3), labels = c("Q1","Q2","Q3"))
         
  )

df_model <- df_merged %>%
  left_join(df_covars, by = "panelID") %>%
  mutate(
    cost      = monthcost * (1 + (price_num/100)),
    cost_diff = cost - monthcost,
    price_con = 1 + (price_num/100),
    price_nom = price_con * planed_cost,
    price_diff = price_nom - planed_cost,
    price_fac = factor(price, levels = c("0","-20","-10","10","20")),
    dist_trans = factor(dist_trans, levels = c("600","300","900")),
    dist_green = factor(dist_green, levels = c("15km", "500 meter", "5km")),
    dist_shops = factor(dist_shops, levels = c("15km", "500 meter", "5km")),
    #green_x_male = (dist_green == "500 meter") * (Sex == 1)
  )

saveRDS(df_model,here("data/formr", "df_model.rds"))


## -------------------------
## 5) Fit model (male example)
## -------------------------



m_male <- logitr(
  data    = df_model ,
  outcome = "choice",
  obsID   = "obsID",
  panelID = "panelID",
  pars    = c("dist_green","dist_shops","dist_trans","parking","price_nom")
)

m_male %>%
  tbl_regression(
    label = list(
      dist_green = "Distance to green areas",
      dist_shops = "Distance to shops",
      dist_trans = "Distance to transportation",
      parking    = "Type of parking",
      price_con  = "Price (%)"
    )
  )



m_wtp_int <- logitr(
  data     = df_model,
  outcome  = "choice",
  obsID    = "obsID",
  panelID  = "panelID",
  pars     = c("dist_green","dist_shops","dist_trans","parking"),
  scalePar = "price_con"
)

m_wtp_int %>% 
  tbl_regression()


## Explore unique number of individuals ----

survey_df %>% 
  filter(!is.na(Sex)) %>% 
  group_by(Sex) %>% 
  summarise(no_res = n_distinct(respondentID.x)) %>% 
  mutate(total = sum(no_res))

## Descriptive data ----



dfSum <- survey_df %>% 
  filter(!is.na(Sex)) %>% 
  # filter(planed_cost != 0,
  #        monthcost != 0,
  #        income != 0) %>%
  select(monthcost,
         income,
         planed_cost,
         ägandebostad,
         bostadstyp,
         Sex,
         civil_status_T2
         
         
  )%>% 
  mutate( ägandebostad = haven::as_factor(ägandebostad),
          bostadstyp = haven::as_factor(bostadstyp),
          Sex = haven::as_factor(Sex),
          Civil = haven::as_factor(civil_status_T2),
          civil_d = factor(if_else(civil_status_T2 == 1, "Partnered", "Not partnered")),
          Own = factor(if_else(ägandebostad == "Ja", "Owner", "Renter")),
          Hus = factor(if_else( bostadstyp == "Friliggande villa/hus/gård" | bostadstyp == "Radhus/kedjehus/parhus", "House", "Apartment/Condo"))
          
          ) %>% 
  # mutate(time_page_1 = time_page_1/60,
  #        cbc_time = (if_else(is.na(time_page_4),time_page_2,time_page_4)/60) ,
  #        total_survey_time = rowSums(across(c(time_page_1,cbc_time))) ) %>% 
  as.data.frame()


label(dfSum$monthcost) <- "Monthly costs"
label(dfSum$income) <- "Monthly household income"
# label(dfSum$cbc_time) <- "Time to complete DCE section (min)"
# label(dfSum$total_survey_time) <- "Time to complete entire experiment (min)"
label(dfSum$planed_cost) <- "Planned housing costs"




t1 <- table1::table1(~ Sex + civil_d + Own + Hus + monthcost + income + planed_cost + ägandebostad + bostadstyp  ,
                     data = dfSum,
                     na.rm = TRUE,
                     digits = 3,
                     format.number = FALSE,
                     # extra.col=list(`P-value`=pvalue),
                     caption = "Sample description") 

table1::t1flex(t1) |> 
  flextable::fontsize(size = 11) |> 
  flextable::padding(padding = 1, part = "all") 


# Regressions ----

theme_gtsummary_journal("jama")


## Standard model ----

m1 <- logitr(
  data    = df_merged %>% mutate(price = as.numeric(price)),
  outcome = "choice",
  obsID   = "obsID",
  pars    = c( "dist_green",
               "dist_shops",
               "dist_trans",
               "parking",
               "price")
)


m1_m <- logitr(
  data    = df_merged %>% mutate(price = as.numeric(price)) %>% 
    filter(sex == 1),
  outcome = "choice",
  obsID   = "obsID",
  pars    = c( "dist_green",
               "dist_shops",
               "dist_trans",
               "parking",
               "price")
)

m1_w <- logitr(
  data    = df_merged %>% mutate(price = as.numeric(price)) %>% 
    filter(sex == 2),
  outcome = "choice",
  obsID   = "obsID",
  pars    = c( "dist_green",
               "dist_shops",
               "dist_trans",
               "parking",
               "price")
)

m2 <- logitr(
  data    = df_merged %>% filter(!is.na(cost),
                                 cost != 0),
  outcome = "choice",
  obsID   = "obsID",
  pars    = c( "dist_green",
               "dist_shops",
               "dist_trans",
               "parking",
               "cost_diff")
)

m1 %>% 
  tbl_regression(
    #exponentiate = TRUE,
    label = list(
      dist_green = "Distance to green areas",
      dist_shops = "Distance to shops",
      dist_trans = "Distance to transportation",
      parking = "Type of parking",
      price = "Price")
  ) 

m1_m %>% 
  tbl_regression(
    #exponentiate = TRUE,
    label = list(
      dist_green = "Distance to green areas",
      dist_shops = "Distance to shops",
      dist_trans = "Distance to transportation",
      parking = "Type of parking",
      price = "Price")
  ) 

m1_w %>% 
  tbl_regression(
    #exponentiate = TRUE,
    label = list(
      dist_green = "Distance to green areas",
      dist_shops = "Distance to shops",
      dist_trans = "Distance to transportation",
      parking = "Type of parking",
      price = "Price")
  ) 




m2 %>% 
  tbl_regression(
    exponentiate = TRUE,
    label = list(
      dist_green = "Distance to green areas",
      dist_shops = "Distance to shops",
      dist_trans = "Distance to transportation",
      parking = "Type of parking",
      cost_diff = "Cost")
  ) 


## Visualize coeffcients

library(ggstats)
ggcoef_model(m1_m)

## WTP model ----

m1_wtp <- logitr(   
  data    = df_merged %>% mutate(price = as.numeric(cost_diff)),
  outcome = "choice",
  obsID   = "obsID",
  pars    = c( "dist_green",
               "dist_shops",
               "dist_trans",
               "parking"),
  scalePar = "cost_diff"
)

m1_alt_wtp <- wtp(m1, scalePar = "cost_diff")
# 
# vcov(m1_wtp)

m1_wtp_m <- logitr(   
  data    = df_merged %>% 
    mutate(price = as.numeric(price)) %>% 
    filter(sex == 1),
  outcome = "choice",
  obsID   = "obsID",
  pars    = c( "dist_green",
               "dist_shops",
               "dist_trans",
               "parking" ),
  scalePar = "price")

m1_wtp_w <- logitr(   
  data    = df_merged %>% 
    mutate(price = as.numeric(price))  %>% 
    filter(sex == 2),
  outcome = "choice",
  obsID   = "obsID",
  pars    = c( "dist_green",
               "dist_shops",
               "dist_trans",
               "parking" ),
  scalePar = "price")


m1_wtp %>% 
  tbl_regression(
    label = list(
      dist_green = "Distance to green areas",
      dist_shops = "Distance to shops",
      dist_trans = "Distance to transportation",
      parking = "Type of parking")
  )


m1_wtp_m %>% 
  tbl_regression(
    label = list(
      dist_green = "Distance to green areas",
      dist_shops = "Distance to shops",
      dist_trans = "Distance to transportation",
      parking = "Type of parking")
  )

m1_wtp_w %>% 
  tbl_regression(
    label = list(
      dist_green = "Distance to green areas",
      dist_shops = "Distance to shops",
      dist_trans = "Distance to transportation",
      parking = "Type of parking")
  )



















m2_wtp <- logitr(   
  data    = df_merged %>% filter(!is.na(cost),
                                 cost != 0),
  outcome = "choice",
  obsID   = "obsID",
  pars    = c( "dist_green",
               "dist_shops",
               "dist_trans",
               "parking" ),
  scalePar = "cost"
)

m2_wtp %>% 
  tbl_regression()

vcov(m2_wtp)









library(dplyr)

# Basic structure
glimpse(survey_design)

# 1) How many alternatives per task for each respondent?
survey_design %>%
  count(respID, qID, name = "n_alts_per_task") %>%
  count(n_alts_per_task)

# 2) Is there exactly one row per respondent × task × alternative?
survey_design %>%
  count(respID, qID, altID) %>%
  filter(n != 1)

# 3) Is obsID constant within respondent × task?
survey_design %>%
  group_by(respID, qID) %>%
  summarise(n_obsIDs = n_distinct(obsID), .groups = "drop") %>%
  count(n_obsIDs)

# 4) How many tasks per respondent?
survey_design %>%
  count(respID, qID) %>%
  count(n, name = "num_tasks_distribution")

# 5) Quick peek at unique attribute levels
lapply(survey_design[c("price","dist_green","dist_shops","dist_trans","parking")], unique)






df_covars %>% 
  filter(planed_cost != 0,
         income != 0) %>%
  mutate(age_qrt = factor(ntile(Age_T3, 3))) %>% 
  ggplot(aes(x = log(planed_cost), fill = age_qrt)) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Density of Planned Cost by Income Tercile",
    x = "Log planned cost",
    fill = "Income Quartile"
  ) +
  theme_minimal()
