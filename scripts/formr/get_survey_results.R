library(formr)
library(tidyverse)
library("logitr")
library(cbcTools)
library(tidyr)
source(".passwords.R")

formr::formr_connect(credentials$email,
                     credentials$password,
                     host = "http://37.27.25.127")


# get survey data

survey_design <- read_csv("https://github.com/n-christie/ahrg_dce_survey/blob/main/output/formr/choice_questions.csv?raw=true")

s_data_p1 <- formr::formr_results("reloc_dce_p1",
                                  host = "http://37.27.25.127")

s_data_p2 <- formr::formr_results("reloc_dce_p2",
                                  host = "http://37.27.25.127")

s_data_p4 <- formr::formr_results("reloc_dce_p4",
                                  host = "http://37.27.25.127")

s_data_feedback <- formr::formr_results("reloc_dce_feedbak",
                                     host = "http://37.27.25.127")

saveRDS(s_data_feedback, file = "feedback_test.rds")

# tidy feedback data (NOT WORKING)

library(likert)

df_plot <- s_data_feedback %>% 
  select(rating_button01,
         rating_button02) %>% 
  sjlabelled::label_to_colnames() %>% 
  mutate(across(where(is.numeric), ~as.factor(., levels = c("1", "2", "3", "4", "5")))) %>%
  as.data.frame() 

df_plot <- s_data_feedback %>% 
  select(rating_button01, rating_button02) %>% 
  sjlabelled::label_to_colnames() %>% 
  mutate(across(.cols = where(is.numeric), ~as.factor(.), .names = "factor_{.col}")) %>%
  as.data.frame()



plot(likert(df_plot),
     legend.position = 'right') 

s_data_feedback %>% 
  select()

# random data

data_rand <- cbc_choices(
  design = survey_design,
  obsID  = "obsID"
) 

# data tidying
df_survey <- survey_data_w_full_info %>% 
  dplyr::filter(!is.na(ended)) %>%
  select(respondentID,
         cbc1,
         cbc2,
         cbc3,
         cbc4,
         cbc5,
         cbc6,
         cbc7,
         cbc8) %>% 
  pivot_longer(cols = -respondentID,
               values_to = "chosen") %>% 
  mutate(qID = extract_numeric(name),
         respID = as.numeric(respondentID)) 

df_merged <- survey_design %>% 
  inner_join(df_survey,
             by = c("respID" ,"qID")) %>% 
  mutate(choice = if_else(altID == chosen, 1, 0)) %>% 
  select(profileID:parking, choice)



# regression

m1_dopt <- logitr(
  data    = df_merged,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c( "dist_green", "dist_shops","dist_trans","parking", "price")
)

summary(m1_dopt)
