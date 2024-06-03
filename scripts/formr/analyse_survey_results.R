
library(pacman)

p_load(tidyverse, here, stringr, formr, logitr, cbcTools, texreg, likert, tidyr, haven, kableExtra, sjlabelled, summarytools,gtsummary)

# load data


survey_design <-  read_csv("https://github.com/n-christie/ahrg_dce_survey/blob/main/output/formr/swe_choice_questions_01.csv?raw=true")

survey_df <- readRDS(here("data/formr", "day_four_results.rds")) %>% 
  filter(created_page_0 > "2024-05-29") # Remove pilot testers, all surveys after 29/5/2024


## Descriptive data ----

dfSum <- survey_df %>% 
  select(monthcost,income,planed_cost, time_page_1,time_page_2,time_page_0,time_page_4)%>% 
  mutate(cbc_time = if_else(is.na(time_page_4),time_page_2/60,time_page_4/60) ,
         total_survey_time = rowSums(across(c(time_page_1,time_page_2,cbc_time)))/60 ) %>% 
  as.data.frame()


label(dfSum$monthcost) <- "Monthly costs"
label(dfSum$income) <- "Monthly household income"
label(dfSum$cbc_time) <- "Time to complete DCE section (min)"
label(dfSum$total_survey_time) <- "Time to complete entire experiment (min)"
label(dfSum$planed_cost) <- "Planned housing costs"




t1 <- table1::table1(~ monthcost + income + planed_cost + cbc_time + total_survey_time  ,
                     data = dfSum,
                     na.rm = TRUE,
                     digits = 3,
                     format.number = FALSE,
                     # extra.col=list(`P-value`=pvalue),
                     caption = "Sample description") 

table1::t1flex(t1) |> 
  flextable::fontsize(size = 11) |> 
  flextable::padding(padding = 1, part = "all") 


## Figures ----

dfSum %>% 
  ggplot(aes(planed_cost)) +
  geom_density(bins = 300)

dfSum %>% 
  ggplot(aes(total_survey_time)) +
  geom_histogram(bins = 300)

dfSum %>% 
  ggplot(aes(income)) +
  geom_histogram(bins = 15)

dfSum %>% 
  ggplot(aes(content)) +
  geom_histogram(bins = 5)

dfSum %>% 
  ggplot(aes(user_exp)) +
  geom_histogram(bins = 5)

# Regressions -----

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
  mutate(cost = income * (1+(price/100)),
         choice = if_else(altID == chosen, 1, 0),
         price_con = (1+(price/100)),
         price = factor(price, levels = c("0","-20","-10","10","20")),
         dist_trans = factor(dist_trans, levels = c("300","600","900")),
         dist_green = factor(dist_green, levels = c("5km", "500 meter", "15km")),
         dist_shops = factor(dist_shops, levels = c("5km", "500 meter", "15km")),
  ) %>% 
  select(session, profileID:parking, choice,income, cost,price_con) %>% 
  filter(!is.na(choice)) %>% 
  group_by(session, qID ) %>%
  mutate(obsID = cur_group_id()) %>% 
  ungroup() %>% 
  head(900) %>% 
  as.data.frame()


# regressions -----


theme_gtsummary_journal("jama")

m1_dopt <- logitr(
  data    = df_merged,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c( "dist_green", "dist_shops","dist_trans","parking", "price")
)


m1_dopt2 <- logitr(
  data    = df_merged2,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c( "dist_green", "dist_shops","dist_trans","parking", "price")
)


m2_dopt <- logitr(   #NOT WORKINF NAN IN VCOV MATRIX
  data    = df_merged,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c( "dist_green", "dist_shops","dist_trans","parking"),
  randPars = c(dist_green = 'n', dist_shops = 'n', dist_trans = 'n', parking = 'n'),
  drawType = 'sobol',
  numDraws = 200,
  numMultiStarts = 10
  
)

vcov(m2_dopt)


# Preference model -----

m3_dopt_pref <- logitr(   
  data    = df_merged2 ,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c( "dist_green",
               "dist_shops",
               "dist_trans",
               "parking",
               "price_con" )
)

vcov(m3_dopt_pref)

m3_dopt_pref %>% 
  tbl_regression(
    exponentiate = TRUE,
    label = list(
      dist_green = "Distance to green areas",
      dist_shops = "Distance to shops",
      dist_trans = "Distance to transportation",
      parking = "Type of parking",
      price_con = "Price")
  ) 

wtp(m3_dopt_pref, scalePar = "cost")

# WTP model ----

m3_dopt_wtp <- logitr(   
  data    = df_merged2 ,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c( "dist_green",
               "dist_shops",
               "dist_trans",
               "parking" ),
  scalePar = "price_con"
)

vcov(m3_dopt_wtp)

m3_dopt_wtp %>% 
  tbl_regression()



# Compare wtp results ---- 

wtpCompare(m3_dopt_pref, m3_dopt_wtp, scalePar = "price_con")

# Getting wildly different wtp results!  Potential reasons: no significant price(scaling variable)?  Cost does not work in the wtp space.














saveRDS(m1_dopt, file =  here("data/formr", "model_1.rds"))








summary(m1_dopt)
screenreg(m1_dopt,
          caption = "Logistic regression results",
          ci.force = TRUE,
          custom.coef.names = c("Distance green - 5km (base = 10km)",
                                "Distance green - 15km (base = 10km)",
                                "Distance shops - 5km (base = 10km)",
                                "Distance shops - 15km (base = 10km)",
                                "Distance trans - 200m (base = 400m)",
                                "Distance trans - 800m (base = 400m)",
                                "Parking - Garageplats (base = Ingen p-plats)",
                                "Parking - P-plats (base = Ingen p-plats)",
                                "Price -20% (base = 0)",
                                "Price -10% (base = 0)",
                                "Price 10% (base = 0)",
                                "Price 20% (base = 0)"
          ))







m2_dopt %>% 
  tbl_regression()

vcov(m2_dopt
)

