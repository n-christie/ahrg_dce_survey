library(formr)
library(tidyverse)
library("logitr")
library(cbcTools)
library(tidyr)
library(texreg)
library(likert)
library(stringr)
library(here)
source(".passwords.R")

formr::formr_connect(credentials$email,
                     credentials$password,
                     host = "http://37.27.25.127")


# get survey data ----

survey_design <- read_csv("https://github.com/n-christie/ahrg_dce_survey/blob/main/output/formr/choice_questions.csv?raw=true")
survey_design_swe <- read_csv("https://github.com/n-christie/ahrg_dce_survey/blob/main/output/formr/swe_choice_questions.csv?raw=true")

s_data_screen <- formr::formr_results("reloc_dce_screen",
                                      host = "http://37.27.25.127") 

s_data_p1 <- formr::formr_results("reloc_dce_p1",
                                  host = "http://37.27.25.127")

s_data_p2 <- formr::formr_results("reloc_dce_p2",
                                  host = "http://37.27.25.127")

s_data_p4 <- formr::formr_results("reloc_dce_p4",
                                  host = "http://37.27.25.127")

s_data_feedback <- formr::formr_results("reloc_dce_feedbak",
                                     host = "http://37.27.25.127")

survey_df <- s_data_p1 %>%
  left_join(s_data_feedback,
            join_by(session)) %>% 
  filter(!is.na(ended.y)) %>%  # finished the survey
  #filter(str_detect(yearOfBirth,"^[:digit:]+$")) # must have 4 digit birth year
  left_join(s_data_p2,
            join_by(session)) %>% 
  left_join(s_data_p4,
            join_by(session)) %>% 
  left_join(s_data_screen) %>% 
  filter(created >= 2024-01-30 )


saveRDS(survey_df, file = here("data/formr", "ahrg_test.rds"))
saveRDS(survey_design, file = here("data/formr", "survey_design.rds"))
saveRDS(survey_design_swe, file = here("data/formr", "survey_design_swe.rds"))

# prepare tables/figures for shinydoc ----




# tidy feedback data ----

df_feed <- s_data_feedback %>% 
  filter(!is.na(ended)) %>% 
  select(feedback11) %>% 
  sjlabelled::label_to_colnames() %>% 
  mutate(across(.cols = everything(), ~factor(.x, levels = 1:2, labels = c("Mobile", "Desktop")))) %>% 
  sjlabelled::label_to_colnames() %>% 
  as.data.frame()


plot(likert(df_feed),
     legend.position = 'right') +  
  theme(legend.text = element_text(size = rel(0.7)))

png(file = here("output/dce_shiny_doc/figures","phone.png"),
    width = 1300, height = 400,)
p1
dev.off()




dff2 <- s_data_feedback %>% 
  filter(!is.na(ended)) %>% 
  select(contains ("rating_button")) %>% 
  sjlabelled::label_to_colnames() %>% 
  mutate(across(.cols = everything(), ~factor(.x, levels = 1:5))) %>% 
  sjlabelled::label_to_colnames() %>% 
  as.data.frame() 

  plot(likert(dff2))

  
## Attribute feedback
  
  
  
df_plot <- s_data_feedback %>% 
  select(rating_button01,
         rating_button02) %>% 
  sjlabelled::label_to_colnames() %>% 
  na.omit() %>% 
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
df_survey <- s_data_p4 %>% 
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
  mutate(choice = if_else(altID == chosen, 1, 0),
         price = factor(price, levels = c("0","-20","-10","10","20"))) %>% 
  select(profileID:parking, choice)


# regression

m1_dopt <- logitr(
  data    = df_merged,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c( "dist_green", "dist_shops","dist_trans","parking", "price")
)

summary(m1_dopt)
screenreg(m1_dopt)
