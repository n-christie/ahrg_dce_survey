
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

# Add other survey data ----
t3_sur <- readRDS("~/Projects/!med/ahrg_reloc_age_dce/data/rt3_eng.rds") %>% 
  select(RespondentID,
         VAR174_8,
         VAR010,
         VAR011,
         VAR022,
         VAR028_1,
         VAR028_2,
         VAR028_3,
         VAR033_1,
         VAR033_2,
         VAR035,
         VAR162,
         VAR163
         
         
         
  )

t1_edu <- readRDS("~/Projects/!med/ahrg_dce_survey/data/clean/out.rds")


survey_df <- sample_df %>% 
  left_join(dce_survey_df,
            by = c("Användarnamn" = "user_code")) %>% 
  left_join(t3_sur,
            by = c("RespondentID")) %>% 
  left_join(t1_edu,
            by = c("RespondentID")) %>% 
  distinct(Användarnamn, .keep_all = TRUE) %>% 
  mutate(respondentID = respondentID.y)

 #write_rds(survey_df,here("data/formr", "survey_res.rds"))


## Descriptive data ----


dfSum <- survey_df %>% 
  filter(
    !is.na(Sex)
         ) %>%
  select(monthcost,
         income,
         planed_cost,
         Age = Age_T3,
         Sex,
         civil_status_T2,
         ägandebostad,
         bostadstyp,
         VAR174_8,
         VAR010,
         VAR035,
         VAR011,
         edu = VAR76
  )%>% 
  mutate(Sex = factor(if_else(Sex == 1, "Male", "Female" )),
         age_group = factor(case_when(
           Age >= 55 & Age < 65 ~ "55-64",
           Age >= 65 & Age < 75 ~ "65-74",
           Age >= 75 ~ "75+",
         )),
         civil_status_T2 = if_else(civil_status_T2 == 1, "Partnered","Not partnered"),
         ägandebostad = haven::as_factor(ägandebostad),
         bostadstyp = haven::as_factor(bostadstyp),
         ägandebostad = if_else(is.na(ägandebostad), "Nej",ägandebostad),
         planed_cost = if_else(planed_cost == 0 , NA ,planed_cost),
         monthcost = if_else(monthcost == 0 , NA ,monthcost),
         
         Own = factor(if_else(ägandebostad == "Ja", "Owner", "Renter")),
         Hus = factor(if_else( bostadstyp == "Friliggande villa/hus/gård" | bostadstyp == "Radhus/kedjehus/parhus", "House", "Apartment/Condo")),
         Retired = haven::as_factor(VAR174_8),
         location = haven::as_factor(VAR010),
         health = haven::as_factor(VAR035),
         monthcost = as.numeric(monthcost),
         income = as.numeric(income),
         planed_cost = as.numeric(planed_cost),
         VAR011_clean = case_when(
           VAR011 == 0 ~ 1,
           TRUE ~ VAR011
         ),
         VAR011_factor = case_when(
           VAR011_clean < 3 ~ as.character(VAR011_clean),
           VAR011_clean >= 3 ~ "3 or more",
           TRUE ~ NA_character_
         ),
         VAR011f = factor(VAR011),
         edu = factor(edu),
         edu = fct_recode(edu,
                          'Elementary school' = "1",
                          '2 years upper secondary school' = "2",
                          '3 or 4 years upper secondary school' = "3",
                          'Univeristy less than 3 years' = "4",
                          'University 3 years or more' = "5")

) %>% 
  select(-VAR174_8, -VAR010, -VAR035) %>% 
  as.data.frame()

label(dfSum$Hus) <- "Current housing type"
label(dfSum$VAR011_factor) <- "Number in household"
label(dfSum$edu) <- "Education"
label(dfSum$Own) <- "Current housing"
label(dfSum$age_group) <- "Age group"
label(dfSum$location) <- "Current housing location"
label(dfSum$Retired) <- "Retired"
label(dfSum$monthcost) <- "Monthly costs"
label(dfSum$income) <- "Monthly household income"
 label(dfSum$civil_status_T2) <- "Civil status"
# label(dfSum$total_survey_time) <- "Time to complete entire experiment (min)"
label(dfSum$planed_cost) <- "Planned housing costs"


options(table1_output = "latex")

rndr <- function(x, name, ...) {
  if (!is.numeric(x)) return(render.categorical.default(x))
  
  # Only apply custom format to specific variables
  if (name %in% c("income", "planed_cost")) {
    return(parse.abbrev.render.code("Mean (SD)")(x))
  } else {
    return(render.continuous.default(x))  # fallback to default for other numeric vars
  }
}


t1 <- table1::table1(~ Sex + age_group + civil_status_T2 + edu + Retired  + Hus + location  + VAR011_factor + income + planed_cost  | Own   ,
                     data = dfSum,
                     na.rm = TRUE,
                     digits = 3,
                     format.number = FALSE,
                     # extra.col=list(`P-value`=pvalue),
                     caption = "Descriptive statistics",
                     output = "latex",
                     render = rndr) 

latex_code <- table1:::doRender(t1, output = "latex")

# 4. Write LaTeX code to file
writeLines(as.character(latex_code),here("docs/elsvier/tables", "descriptive_table.tex"))


writeLines(t1, here("docs/elsvier/tables", "descriptive_table.tex"))

table1::t1flex(t1) |> 
  flextable::fontsize(size = 11) |> 
  flextable::padding(padding = 1, part = "all") 








pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

dfSum <- dfSum[!sapply(dfSum, is.list)]



t1 <- table1::table1(
  ~ Sex + age_group + civil_status_T2 + Retired + Hus + location + income + planed_cost | Own,
  data = dfSum,
  na.rm = TRUE,
  digits = 3,
  format.number = FALSE,
  overall = F,
  extra.col = list(`P-value` = pvalue),
  caption = "Descriptive statistics"
)


# Convert to flextable with formatting
table1::t1flex(t1) |> 
  flextable::fontsize(size = 11) |> 
  flextable::padding(padding = 1, part = "all")
