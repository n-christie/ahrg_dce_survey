
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


## Descriptive data ----


dfSum <- survey_df %>% 
  filter(
    planed_cost != 0,
         monthcost != 0,
         income != 0
         ) %>%
  select(monthcost,
         income,
         planed_cost,
         
  )%>% 
  # mutate(time_page_1 = time_page_1/60,
  #        cbc_time = (if_else(is.na(time_page_4),time_page_2,time_page_4)/60) ,
  #        total_survey_time = rowSums(across(c(time_page_1,cbc_time))) ) %>% 
  as.data.frame()


label(dfSum$monthcost) <- "Monthly costs"
label(dfSum$income) <- "Monthly household income"
# label(dfSum$cbc_time) <- "Time to complete DCE section (min)"
# label(dfSum$total_survey_time) <- "Time to complete entire experiment (min)"
label(dfSum$planed_cost) <- "Planned housing costs"




t1 <- table1::table1(~ monthcost + income + planed_cost  ,
                     data = dfSum,
                     na.rm = TRUE,
                     digits = 3,
                     format.number = FALSE,
                     # extra.col=list(`P-value`=pvalue),
                     caption = "Sample description") 

table1::t1flex(t1) |> 
  flextable::fontsize(size = 11) |> 
  flextable::padding(padding = 1, part = "all") 
