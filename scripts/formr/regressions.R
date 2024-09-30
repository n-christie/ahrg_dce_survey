# Regressions for DCE.

# Load packages

library(pacman)

p_load(tidyverse, 
       here, 
       stringr, 
       formr, 
       logitr, 
       cbcTools, 
       texreg, 
       likert, 
       tidyr, 
       haven, 
       kableExtra,
       ggrepel, 
       sjlabelled, 
       summarytools,
       gtsummary,
       mlogit,
       nnet
       )


# Load data

survey_df <- readRDS(here("data/formr", "24_09_results.rds")) %>% 
  mutate(ended_survey = if_else(is.na(ended_page_4), ended_page_2, ended_page_4)) %>% 
  filter(created_page_0 > "2024-05-29",
         !is.na(ended_survey)) # Remove pilot testers, all surveys after 29/5/2024, and incomplete
survey_design <-  read_csv("https://github.com/n-christie/ahrg_dce_survey/blob/main/output/formr/swe_choice_questions_01.csv?raw=true")

## data tidying  ## Need to sort out the multiple observations!!!!! -----

df_regs <- survey_df %>% 
  filter(!is.na(respondentID),
         !is.na(cbc9)) %>% 
  distinct(respondentID, .keep_all = TRUE) %>% ## Data loss - distinct RESPID
  select(respID = respondentID, contains("cbc")) %>% 
  pivot_longer(cols = -respID,
               values_to = "chosen") %>% 
  mutate(qID = extract_numeric(name),
         respID = as.numeric(respID)) %>% 
  filter(!is.na(qID))

df_merged <- survey_design %>% 
  inner_join(df_regs,
             by = c("respID" ,"qID")) %>% 
  left_join(survey_df %>% 
              mutate(respondentID = as.numeric(respondentID)) %>% 
              select( respondentID, session, income, monthcost, planed_cost),
            by = c("respID" = "respondentID")) %>% 
  mutate(cost = monthcost * (1+(price/100)),
         cost_diff = cost - monthcost,
         choice = if_else(altID == chosen, 1, 0),
         price_con = (1+(price/100)),
         price = factor(price, levels = c("0","-20","-10","10","20")),
         dist_trans = factor(dist_trans, levels = c("900","600","300")),
         dist_green = factor(dist_green, levels = c("5km", "500 meter", "15km")),
         dist_shops = factor(dist_shops, levels = c("5km", "500 meter", "15km")),
  ) %>% 
  select(session,respID,profileID:parking, choice,monthcost, cost,cost_diff,price_con) %>% 
  filter(!is.na(choice)) %>% 
  distinct(qID,altID,obsID, .keep_all = TRUE)

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




## WTP model ----

m1_wtp <- logitr(   
  data    = df_merged %>% mutate(price = as.numeric(price)),
  outcome = "choice",
  obsID   = "obsID",
  pars    = c( "dist_green",
               "dist_shops",
               "dist_trans",
               "parking" ),
  scalePar = "price"
)

m1_alt_wtp <- wtp(m1, scalePar = "price")

vcov(m1_wtp)

m1_wtp %>% 
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


# test mlogit _- NOt working

math <- read.table("http://www.utstat.utoronto.ca/~brunner/data/legal/mathcat.data.txt")
math0 = math[,c(1,5)]; head(math0)

long0 = mlogit.data(math0,shape="wide",choice="passed") ; head(long0)

simple0 = mlogit(passed ~ 0 | hsgpa, data=long0)
simple0 = mlogit(passed ~ 0 | hsgpa, data=long0); summary(simple0)
summary(glm(passed~hsgpa,family=binomial,data=math))


data("Fishing",package="Ecdat")
colnames(Fishing)[4:11] <- c("pr.beach","pr.pier","pr.boat","pr.charter",
                             "ca.beach","ca.pier","ca.boat","ca.charter")

Fish <- mlogit.data(Fishing,varying=c(4:11),shape="wide",choice="mode")
summary(mlogit(mode~pr+ca-1,data=Fish))


survey_dat <- mlogit.data(df_merged %>% 
                            
                            select(choice, dist_green, dist_shops, dist_trans), shape = "wide", choice = "choice")

m1_mlog <- mlogit(choice ~ dist_green ,
                  data =survey_dat)



multinom(choice ~ dist_green + dist_shops + dist_trans + parking + price, data=df_merged)
