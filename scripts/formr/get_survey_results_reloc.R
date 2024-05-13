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


survey_design_swe <- read_csv("https://github.com/n-christie/ahrg_dce_survey/blob/main/output/formr/swe_choice_questions_01.csv?raw=true")

s_data_screen <- formr::formr_results("reloc_dce_screen",
                                      host = "https://www.reloc-age.org") %>% 
  mutate(time_page_0 = as.numeric(as.period(ended - created, unit = "seconds")))

s_data_p1 <- formr::formr_results("reloc_dce_p1",
                                  host = "https://www.reloc-age.org") %>% 
  mutate(time_page_1 = as.numeric(as.period(ended - created, unit = "seconds"))) %>% 
  select(-created, -ended, - modified, -expired, -time2 )

s_data_p2 <- formr::formr_results("reloc_dce_p2",
                                  host = "https://www.reloc-age.org") %>% 
  mutate(time_page_2 = as.numeric(as.period(ended - created, unit = "seconds"))) %>% 
  select(session, respondentID, survey, df, df_json,  contains("cbc"),time_page_2 )

s_data_p4 <- formr::formr_results("reloc_dce_p4",
                                  host = "https://www.reloc-age.org") %>% 
  mutate(time_page_4 = as.numeric(as.period(ended - created, unit = "seconds"))) %>% 
  select(session, respondentID, survey, df, df_json,  contains("cbc"),time_page_4 )

s_data_feedback <- formr::formr_results("reloc_dce_feedback_swe",
                                        host = "https://www.reloc-age.org") %>% 
  mutate(time_page_feed = as.numeric(as.period(ended - created, unit = "seconds"))) %>% 
  select( -created, -ended, - modified, -expired)

survey_df <- s_data_p1 %>%
  left_join(s_data_feedback,
            join_by(session)) %>% 
  left_join(s_data_p4,
            join_by(session)) %>% 
  left_join(s_data_p2,
            join_by(session)) %>% 
  filter(!is.na(mc_position)) %>%   # finished the survey (must have answered the first question)
  mutate(cbc1 = coalesce(cbc1.x, cbc1.y),
         cbc2 = coalesce(cbc2.x, cbc2.y),
         cbc3 = coalesce(cbc3.x, cbc3.y),
         cbc4 = coalesce(cbc4.x, cbc4.y),
         cbc5 = coalesce(cbc5.x, cbc5.y),
         cbc6 = coalesce(cbc6.x, cbc6.y),
         cbc7 = coalesce(cbc7.x, cbc7.y),
         cbc8 = coalesce(cbc8.x, cbc8.y),
         cbc9 = coalesce(cbc9.x, cbc9.y),
         respondentID = coalesce(respondentID.x, respondentID.y)
  ) %>% 
  select(-cbc1.x, -cbc2.x, -cbc3.x, -cbc4.x, -cbc5.x, -cbc6.x, -cbc7.x, -cbc8.x, -cbc9.x,
         -cbc1.y, -cbc2.y, -cbc3.y, -cbc4.y, -cbc5.y, -cbc6.y, -cbc7.y, -cbc8.y, -cbc9.y, -respondentID.x, -respondentID.y
  ) %>% 
  distinct(respondentID, .keep_all = TRUE)




saveRDS(survey_df, file = here("data/formr", "ahrg_pilot_2.rds"))


# retreat_df <- survey_df %>% 
#   mutate(created.x = ymd_hms(created.x)) %>% 
#   filter(created.x >= "2024-02-27" )


