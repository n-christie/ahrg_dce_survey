
library(pacman)

p_load(tidyverse, here, stringr, formr, logitr, cbcTools, texreg, likert, tidyr, haven, kableExtra,ggrepel, sjlabelled, summarytools,gtsummary)

# load data


survey_design <-  read_csv("https://github.com/n-christie/ahrg_dce_survey/blob/main/output/formr/swe_choice_questions_01.csv?raw=true")

dce_survey_df <- readRDS(here("data/formr", "08_02_results.rds")) %>% 
  mutate(ended_survey = if_else(is.na(ended_page_4), ended_page_2, ended_page_4)) %>% 
  filter(created_page_0 > "2024-05-29",
         !is.na(ended_survey))%>% 
  distinct(user_code, .keep_all = TRUE)

sample_df <- readRDS(here("data/formr", "dce_anym_output_1.rds"))


survey_df <- sample_df %>% 
  left_join(dce_survey_df,
            by = c("Användarnamn" = "user_code")) %>% 
  distinct(Användarnamn, .keep_all = TRUE) %>% 
  mutate(respondentID = respondentID.y)

survey_df %>% 
  filter(!is.na(Sex)) %>% 
  group_by(Sex) %>% 
  summarise(no_res = n_distinct(respondentID.x)) %>% 
  mutate(total = sum(no_res))

## Descriptive data ----



dfSum <- survey_df %>% 
  # filter(planed_cost != 0,
  #        monthcost != 0,
  #        income != 0) %>%
  select(monthcost,
         income,
         planed_cost,

         )%>% 
  mutate(time_page_1 = time_page_1/60,
         cbc_time = (if_else(is.na(time_page_4),time_page_2,time_page_4)/60) ,
         total_survey_time = rowSums(across(c(time_page_1,cbc_time))) ) %>% 
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

# When took survey -----

survey_df %>%
  ggplot(aes( x = as.Date( created_page_0))) +
  geom_histogram(bins = 150,
                 color = 'black',fill = 'slateblue') +
  scale_x_date(labels = scales::date_format("%b-%d"),
               date_breaks = 'week') +
  labs(title = "Number of respondents over time",
       x = "",
       y = "Number of respondents")+
  theme_light()


survey_df %>%
  transmute(hour = strftime(created_page_0, format="%H") ) %>% 
  ggplot(aes( x = hour)) +
  geom_histogram(stat = "count",
                 color = 'black',fill = 'slateblue') +
  labs(title = "Number of respondents - time of day",
       x = "Time of day",
       y = "Number of respondents")+
  theme_light()



## Figures ----

theme_set(theme_bw())

annotations_p <- data.frame(
  x = c(round(min(dfSum %>% filter(planed_cost != 0) %>% pull(planed_cost)), 2),
        round(mean(dfSum %>% filter(planed_cost != 0) %>% pull(planed_cost)), 2),
        round(max(dfSum %>% filter(planed_cost != 0) %>% pull(planed_cost)), 2)),
  y = c(.000014, .000262, .000015),
  label = c("Min:", "Mean:", "Max:")
) 


planned_plot <- dfSum %>% 
  filter(planed_cost != 0) %>%
  ggplot(aes(x = planed_cost)) +
  geom_histogram(aes(y=..density..), bins = 70,  color = "#000000", fill = "#0099F8")+
  geom_density(color = "#000000", fill = "#F85700", alpha = 0.6) +
  scale_x_continuous(labels = scales::label_number(suffix = " sek")) +
  labs(title = "What are your planned monthly housing costs?",
       y = "",
       x = "Planned housing costs") +
  theme(axis.text.y = element_blank()) +
  geom_text(data = annotations_p, aes(x = x, y = y, label = paste(label, x)), size = 3, fontface = "bold")

planned_plot 



annotations_c <- data.frame(
  x = c(round(min(dfSum %>% filter(monthcost != 0) %>% pull(monthcost)), 2),
        round(mean(dfSum %>% filter(monthcost != 0) %>% pull(monthcost)), 2),
        round(max(dfSum %>% filter(monthcost != 0) %>% pull(monthcost)), 2)),
  y = c(.000014, .000162, .000015),
  label = c("Min:", "Mean:", "Max:")
)

current_plot <-dfSum %>% 
  filter(monthcost != 0) %>%
  ggplot(aes(monthcost)) +
  geom_histogram(aes(y=..density..), bins = 50,  color = "#000000", fill = "#0099F8")+
  geom_density(color = "#000000", fill = "#F85700", alpha = 0.6) +
  scale_x_continuous(labels = scales::label_number(suffix = " sek")) +
  labs(title = "What are your current monthly housing costs?",
       y = "",
       x = "Monthly housing costs") +
  theme(axis.text.y = element_blank()) +
  geom_text(data = annotations_c, aes(x = x, y = y, label = paste(label, x)), size = 3, fontface = "bold")

current_plot

annotations_i <- data.frame(
  x = c(round(min(dfSum %>% filter(income != 0) %>% pull(income)), 2),
        round(mean(dfSum %>% filter(income != 0) %>% pull(income)), 2),
        round(max(dfSum %>% filter(income != 0) %>% pull(income)), 2)),
  y = c(.000004, .000022262, .000005),
  label = c("Min:", "Mean:", "Max:")
)

income_plot <-dfSum %>% 
  filter(income != 0) %>%
  ggplot(aes(income)) +
  geom_histogram(aes(y=..density..), bins = 50, color = "#000000", fill = "#0099F8")+
  geom_density(color = "#000000", fill = "#F85700", alpha = 0.6) +
  #geom_vline(aes(xintercept = mean(income)),col='red',size=1)+
  scale_x_continuous(labels = scales::label_number(suffix = " sek")) +
  labs(title = "What is your current household income?",
       y = "",
       x = "Household income") +
  theme(axis.text.y = element_blank()) +
  geom_text(data = annotations_i, aes(x = x, y = y, label = paste(label, x)), size = 3, fontface = "bold")

income_plot

annotations_d <- data.frame(
  x = c(round(min(dfSum %>% 
                    filter(planed_cost != 0,
                           monthcost != 0) %>%
                    mutate(diff_cost = planed_cost - monthcost) %>%
                    pull(diff_cost)), 2),
        round(mean(dfSum %>% 
                     filter(planed_cost != 0,
                            monthcost != 0) %>%
                     mutate(diff_cost = planed_cost - monthcost) %>%
                     pull(diff_cost)), 2),
        round(max(dfSum %>% 
                    filter(planed_cost != 0,
                           monthcost != 0) %>%
                    mutate(diff_cost = planed_cost - monthcost) %>%
                    pull(diff_cost)), 2)),
  y = c(.000014, .000262, .000015),
  label = c("Min:", "Mean:", "Max:")
) 


diff_plot <- dfSum %>% 
  filter(planed_cost != 0,
         monthcost != 0) %>%
  mutate(diff_cost = planed_cost - monthcost) %>% 
  ggplot(aes(x = diff_cost)) +
  geom_histogram(aes(y=..density..), bins = 70,  color = "#000000", fill = "#0099F8")+
  geom_density(color = "#000000", fill = "#F85700", alpha = 0.6) +
  scale_x_continuous(labels = scales::label_number(suffix = " sek")) +
  labs(title = "Difference between planned and current costs",
       y = "",
       x = "Difference") +
  theme(axis.text.y = element_blank()) +
  geom_text(data = annotations_d, aes(x = x, y = y, label = paste(label, x)), size = 3, fontface = "bold")

diff_plot 


gridExtra::grid.arrange(income_plot, current_plot,planned_plot,diff_plot)


# Regressions -----

## data tidying  ## Need to sort out the multiple observations!!!!! -----

df_regs <- survey_df %>% 
  # filter(!is.na(respondentID),
  #        !is.na(cbc9)) %>% 
  distinct(respondentID, .keep_all = TRUE) %>% ## Data loss - distinct RESPID
  select(respID = respondentID, contains("cbc")) %>% 
  pivot_longer(cols = -respID,
               values_to = "chosen") %>% 
  mutate(qID = extract_numeric(name),
         respID = as.numeric(respID)) %>% 
  filter(!is.na(qID))

survey_merg <- survey_df %>% 
  mutate(respondentID = as.numeric(respondentID)) %>% 
  select( respondentID, session, income, monthcost, planed_cost,sex = Sex,
          civil = civil_status_T2
          ) %>% 
  distinct(respondentID,.keep_all = TRUE)

df_merged <- survey_design %>% 
  inner_join(df_regs,
             by = c("respID" ,"qID")) %>% 
  left_join(survey_merg,
            by = c("respID" = "respondentID")) %>% 
  mutate(cost = monthcost * (1+(price/100)),
         cost_diff = cost - monthcost,
         choice = if_else(altID == chosen, 1, 0),
         price_con = (1+(price/100)),
         price = factor(price, levels = c("0","-20","-10","10","20")),
         dist_trans = factor(dist_trans, levels = c("900","600","300")),
         dist_green = factor(dist_green, levels = c("15km", "500 meter", "5km")),
         dist_shops = factor(dist_shops, levels = c("15km", "500 meter", "5km")),
  ) %>% 
  select(session,respID,profileID:parking, choice,monthcost, cost,cost_diff,price_con,sex,civil) %>% 
  filter(!is.na(choice)) %>% 
  distinct(qID,altID,obsID, .keep_all = TRUE)


# group_by(session, qID ) %>%
#   mutate(obsID = cur_group_id()) %>% 
#   ungroup() %>% 
#   head(900) %>% 
#   as.data.frame()


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

# m1_alt_wtp <- wtp(m1, scalePar = "price")
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
