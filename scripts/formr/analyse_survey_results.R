
library(pacman)

p_load(tidyverse, here, stringr, formr, logitr, cbcTools, texreg, likert, tidyr, haven, kableExtra,ggrepel, sjlabelled, summarytools,gtsummary)

# load data


survey_design <-  read_csv("https://github.com/n-christie/ahrg_dce_survey/blob/main/output/formr/swe_choice_questions_01.csv?raw=true")

survey_df <- readRDS(here("data/formr", "08_02_results.rds")) %>% 
  mutate(ended_survey = if_else(is.na(ended_page_4), ended_page_2, ended_page_4)) %>% 
  filter(created_page_0 > "2024-05-29",
         !is.na(ended_survey))


## Descriptive data ----

dfSum <- survey_df %>% 
  # filter(planed_cost != 0,
  #        monthcost != 0,
  #        income != 0) %>%
  select(monthcost,income,planed_cost, time_page_1,time_page_2,time_page_0,time_page_4)%>% 
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

# Repspondents ----

number_respondents <- survey_df %>% 
  mutate(valid_id = case_when( nchar(user_code) == 5 ~ "Valid",
                               is.na(user_code) ~ "Missing",
                               TRUE ~ "Other")
      
         
          ) %>% 
  select(user_code, valid_id) %>% 
count(valid_id)


# Identify indivuals who have completed the survey ----


id_respondents <- survey_df %>% 
  mutate(user_code = if_else(grepl("^[0-9]{5} +[0-9]{5}$", user_code), substr(user_code, 1, 5), user_code),
         valid_id = case_when(
                               grepl("^[0-9]{5}$", user_code) ~ "Valid",
                               grepl("[a-zA-Z].*[a-zA-Z]", user_code) ~ "Name",
                               grepl("^[0-9]{5} +[0-9]{5}$", user_code) ~ "User and pass",
                               str_count(user_code, "[0-9]") > 7 ~ "Personnummer",
                               is.na(user_code) ~ "Missing",
                               TRUE ~ "Other"),
         ended_survey = if_else(is.na(ended_page_4), ended_page_2, ended_page_4),
         not_completed =  if_else(is.na(cbc9), "Not completed", "Completed")
         
         
  ) %>% 
  select(user_code, valid_id, not_completed, created_survey = created_page_0, ended_survey) 

writexl::write_xlsx(id_respondents, here("data/formr", "respondents.xlsx"))


# Create labels for the pie chart

number_respondents$label <- paste0(number_respondents$valid_id, "\n", number_respondents$percentage, "%")


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
  distinct(qID,altID,obsID, .keep_all = TRUE)


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
  numDraws = 20,
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

m1_dopt %>% 
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
  data    = df_merged ,
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

