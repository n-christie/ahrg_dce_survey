library(formr)
library(tidyverse)
library("logitr")
library(cbcTools)
library(tidyr)
library(texreg)
library(likert)
library(lubridate)
library(stringr)
library(here)
source(".passwords.R")

formr::formr_connect(credentials_r$email,
                     credentials_r$password,
                     host = "https://www.reloc-age.org")


survey_design <- read_csv("https://github.com/n-christie/ahrg_dce_survey/blob/main/output/formr/choice_questions.csv?raw=true")
survey_design_swe <- read_csv("https://github.com/n-christie/ahrg_dce_survey/blob/main/output/formr/swe_choice_questions.csv?raw=true")

s_data_screen <- formr::formr_results("reloc_dce_screen",
                                      host = "https://www.reloc-age.org") %>% 
  mutate(time_page_1 = as.numeric(as.period(ended - created, unit = "seconds")))

s_data_p1 <- formr::formr_results("reloc_dce_p1",
                                  host = "https://www.reloc-age.org") %>% 
  mutate(time_page_2 = as.numeric(as.period(ended - created, unit = "seconds")))

s_data_p2 <- formr::formr_results("reloc_dce_p2",
                                  host = "https://www.reloc-age.org") %>% 
  mutate(time_page_3 = as.numeric(as.period(ended - created, unit = "seconds")))

s_data_p4 <- formr::formr_results("reloc_dce_p4",
                                  host = "https://www.reloc-age.org") %>% 
  mutate(time_page_4 = as.numeric(as.period(ended - created, unit = "seconds")))

s_data_feedback <- formr::formr_results("reloc_dce_feedback_swe",
                                        host = "https://www.reloc-age.org") %>% 
  mutate(time_page_feed = as.numeric(as.period(ended - created, unit = "seconds")))

survey_df <- s_data_p1 %>%
  left_join(s_data_feedback,
            join_by(session)) %>% 
  filter(!is.na(ended.y)) %>%  # finished the survey
  #filter(str_detect(yearOfBirth,"^[:digit:]+$")) # must have 4 digit birth year
  # left_join(s_data_p2,
  #           join_by(session)) %>% 
  left_join(s_data_p4,
            join_by(session)) %>% 
  left_join(s_data_screen)


saveRDS(survey_df, file = here("data/formr", "ahrg_pilot_2.rds"))


retreat_df <- survey_df %>% 
  mutate(created.x = ymd_hms(created.x)) %>% 
  filter(created.x >= "2024-02-27" )


