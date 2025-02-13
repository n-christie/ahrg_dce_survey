
library(pacman)

p_load(tidyverse, here, stringr, formr, logitr, cbcTools, texreg, likert, tidyr, haven, kableExtra,ggrepel, sjlabelled, summarytools,gtsummary,readxl)

# load data


survey_design <-  read_csv("https://github.com/n-christie/ahrg_dce_survey/blob/main/output/formr/swe_choice_questions_01.csv?raw=true")

survey_df <- readRDS(here("data/formr", "04_10_results.rds")) %>% 
  mutate(ended_survey = if_else(is.na(ended_page_4), ended_page_2, ended_page_4)) %>% 
  filter(created_page_0 > "2024-05-29",
         !is.na(ended_survey))

ra_lookup <- read_excel("data/clean/ra_lookup.xlsx")
taken <- read_excel("data/formr/RELOC-AGE responses 2024-08-28.xlsx")


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
  select(user_code, valid_id, not_completed, created_survey = created_page_0, ended_survey) %>% 
  full_join(ra_lookup,
            by = c("user_code" = "Användarnamn")) %>% 
  mutate(dce_match = case_when(!is.na(ended_survey) & !is.na(Förnamn) ~ "Identified - completed - dce",
                   !is.na(ended_survey) & is.na(Förnamn) ~ "Unidentified - completed - dce",
                   is.na(created_survey) ~ "No record taken dce",
                   not_completed == "Not completed" & !is.na(Förnamn) ~ "Identified - incomplete dce",
                   not_completed == "Not completed" & valid_id == "Valid" ~ "Unidentified - incomplete dce",
                   TRUE ~ NA)) %>% 
  rename("Användarnamn" = "user_code") %>% 
  left_join(taken %>% select(Användarnamn) %>% mutate(taken_primary_survey = 1), 
                                                  by = "Användarnamn") %>% 
  mutate(taken_primary_survey = if_else(is.na(taken_primary_survey) ,  "No record of primary survey", "Taken primary survey"  ))

         

id_respondents %>% 
  count(dce_match)


incomplete_list <- id_respondents %>% filter(dce_match == "Identified - incomplete - dce")

complete_list <-id_respondents %>% filter(dce_match == "Identified - completed - dce") %>%
  select(-valid_id,-not_completed,-Lösenord) %>% 
  rename("Användarnamn" = user_code)

complete_need_to_id <- id_respondents %>% filter(dce_match == "Identified - completed - dce")
never_taken <- id_respondents %>% filter(dce_match == "Never taken")



writexl::write_xlsx(complete_list, here("data/formr", "users_completed_dce.xlsx"))
writexl::write_xlsx(id_respondents, here("data/formr", "users_dce.xlsx"))


# join taken --------

list <- id_respondents %>% 
  left_join(taken,
            by = "Användarnamn")

complete_list <-list %>% 
  filter(match == "Matched - complete") %>% 
  pull("Användarnamn")

no_take_dce <- taken %>% 
  filter( !Användarnamn %in% complete_list) %>% 
  select(`Respondent-ID`, Användarnamn, everything(), -Svarstillfälle, -ID)

writexl::write_xlsx(no_take_dce, here("data/formr", "respondents_no_dce.xlsx"))

match_id <- list %>% 
  filter(match != "Matched - complete")



list %>% 
  filter(!is.na(`Respondent-ID`)) %>% View(  )

  id_respondents %>% 
  left_join(taken,
            by = c(  ))

       
# Join from full list
  
list_1 <- ra_lookup %>%
  left_join(taken %>% select(Användarnamn) %>% mutate(taken_primary_survey = 1), 
            by = "Användarnamn") %>% 
  left_join(id_respondents  %>%  select(Användarnamn) %>% mutate(taken_dce = 1),
            by = c("Användarnamn" ))

            