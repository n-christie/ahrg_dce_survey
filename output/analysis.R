

library(tidyverse)
library(here)
library(summarytools)
library(texreg)
library(MASS)
library(caret)
library(glmnet)
library(broom)
library(kableExtra)
library(likert)
library(gtools)
library(pROC)
library(flextable)
library(table1)
library(DescTools)
library(cbcTools)
library("logitr")
library(texreg)
library(mlogit)
library(nnet)
# Load data

source(here("scripts", "functions.R"))

df_att <- read_rds(here("data/clean", "attributes_df.rds")) %>%
  rename(edu = education,
         healthxxx = health) %>% 
  mutate(education = case_when(edu == "Grundskola/folkskola/realskola" ~ "Elementary school",
                               edu == "2-årigt gymnasium/yrkesskola" | edu == "3-4 årigt gymnasium/folkhögskola" ~ "Primary school",
                               edu == "Universitet/högskola längre än 3 år" | edu == "Universitet/högskola mindre än 3 år" ~ "University",
                               TRUE ~ NA
                               
                               
  ),
  health =  factor(case_when(healthxxx == "Dålig" | healthxxx == "Någorlunda" ~ "Poor/Fair",
                             healthxxx == "God" ~ "Good",
                             healthxxx == "Mycket god"  ~ "Very good",
                             healthxxx == "Utmärkt" ~ "Excellent"
  ), levels = c("Poor/Fair", "Good", "Very good", "Excellent"))) %>% 
  filter(!is.na(education),
         !is.na(civil),
         !is.na(health),
         !is.na(VAR026_1),
         !is.na(VAR028_1))

# Create dataset to send to datavis guys ----

df_eng <- read_rds(here("data/clean","df_eng.rds"))
base_dic <- labelled::generate_dictionary(df_eng)

df_vis <- df_eng %>% 
  dplyr::select(
                Bostadstyp,
                Sex,
                Postnummer,
                Stad,
                contains("VAR19_"),
                VAR24) %>% 
  tibble::rowid_to_column( "ID") %>% 
  mutate(across(contains("VAR19_")), labelled::to_factor(.))

write_sav(df_vis, here( "data/clean", "infravis.sav"))
write.csv(df_vis, here( "data/clean", "infravis.csv"), row.names=FALSE)

# Create descriptive tables. ----

dfSum <- df_att %>%
  dplyr::select(sex,
                civil,
                own_hus,
                health,
                age_group,
                Flyttat,
                diff_with_mobility,
                Bostadstyp,
                alone,
                location,
                VAR026_1,
                age = age2,
                education
  ) |> 
  as.data.frame()

# Change some lables 
label(dfSum$age_group) <- "Age group"
label(dfSum$civil) <- "Civil status"
label(dfSum$health) <- "Self-reported health"
label(dfSum$education) <- "Education"

# generate the desc table
my_table <- table1::table1(~ sex + age_group + civil + health + education ,
                           data = dfSum,
                           na.rm = TRUE,
                           digits = 1,
                           format.number = TRUE,
                           # extra.col=list(`P-value`=pvalue),
                           caption = "Sample description") 

# Transform into kable objefct

table_des <- t1kable(my_table,
                     format = "latex",
                     booktabs = TRUE)

# Saving the table


table_des %>% 
  save_kable(here("latex/elsvier/tables", "des.tex"))

#Create some figures ----

df_att %>% 
  ggplot( aes(x = age, colour = education, fill = education)) +
  geom_density(alpha = 0.5) +
  labs(x = "Age",
       y = "Density",
       fill = "Education",
       color = "Education"
       ) +
  theme_light()

ggsave(here("latex/elsvier/figures", "edu_den.png"))

# Create attribute table ----

first_col <- c(
                "Distance to green area",
                "Distance to shops",
                "Distance to public transportaion",
                "Balcony",
                "Price")
                
                
second_col <- c("1 = greenclose: within 5km \n 2 = greenmed: within 10km \n 3 = greenfar: within 15km",
                "1 = shopsclose: within 5km \n 2 = shopsmed: within 10km \n 3 = shopsfar: within 15km",
                "1 = transclose: within 5km \n 2 = transmed: within 10km \n 3 = transfar: within 15km",
                "1 = balsmall:  5m2 \n 2 = balmed: 10m2 \n 3 = ballarge: 15m2",
                "1 = 10 less than current costs \n 2 = same as curent costs \n 3 = 10 more than current costs"
                
)


table_att <- data.frame(first_col, second_col) %>% 
  mutate_all(linebreak, align = 'l') %>% 
  kable(format = "latex",
        col.names = c( "Attribute"	,"Description and levels"),
        align = "ll",
        linesep = "\\addlinespace",
        valign = "top",
        booktabs = TRUE,
        escape = FALSE,
        caption = "Attributes and descriptions \\label{tab:atts}") %>% 
  column_spec(1,"15em", latex_valign = "p") %>% 
  kable_styling(latex_options =  c("hold_position")) 


# Save table 

table_att %>% 
  save_kable(here("latex/elsvier/tables", "att.tex"))


# Regressions (simulated data) ----


df_sim <- df_att %>% 
  mutate(respID = row_number()) %>% 
  dplyr::select(respID, age_group, sex, own_hus, alone, location, education, age = age2, health)

# Create attributes and their levels

profiles <- cbc_profiles(
  price     = c(-20,-10,0,10,20), # % change of current mothly costs
  dist_green = c("5km", "10km", "15km"),
  dist_shops = c("5km", "10km", "15km"),
  dist_trans = c("5km", "10km", "15km"),
  balcony      = c('5m2', '10m2', '15m2')
)

#Create design (multiple types)

set.seed(58)

# Random design

design_random <- cbc_design(
  profiles = profiles,
  n_resp   = 1000, # Number of respondents
  n_alts   = 2,   # Number of alternatives per question
  n_q      = 10,   # Number of questions per respondent
  method   = 'random' # This is the default method
)

# D-optimal design 

design_dopt <- cbc_design(
  profiles = profiles,
  n_resp   = 800, # Number of respondents
  n_alts   = 2,   # Number of alternatives per question
  n_q      = 10,   # Number of questions per respondent
  method   = 'orthogonal'
)

# Example 
# 
# example_1 <- design_random %>% 
#   filter(respID %in% c(1,2)) %>% 
#   select(respID, qID, altID, price:balcony)

dim(design_dopt)
cbc_balance(design_dopt)

## Bayesian design 

design_bayesian <- cbc_design(
  profiles  = profiles,
  n_resp    = 800, # Number of respondents
  n_alts    = 2, # Number of alternatives per question
  n_q       = 10, # Number of questions per respondent
  method = 'Modfed'
)

# Simulate data random

data_rand <- cbc_choices(
  design = design_random,
  obsID  = "obsID"
) %>% 
  left_join(df_sim,
            by = "respID")

# Simulate data orthogonal

data_dopt <- cbc_choices(
  design = design_dopt,
  obsID  = "obsID"
) %>% 
  left_join(df_sim,
            by = "respID")


# Simulate data bayesian

data_bay <- cbc_choices(
  design = design_bayesian,
  obsID  = "obsID"
) %>% 
  left_join(df_sim,
            by = "respID")


# Compare standard errors of simulated data between design options

power_rand <- cbc_power(
  data    = data_rand,
  pars    = c("price", "dist_green", "dist_shops", "dist_trans", "balcony"),
  outcome = "choice",
  obsID   = "obsID",
  nbreaks = 10,
  n_q     = 6
)


power_dopt <- cbc_power(
  data    = data_dopt,
  pars    = c("price", "dist_green", "dist_shops", "dist_trans", "balcony"),
  outcome = "choice",
  obsID   = "obsID",
  nbreaks = 10,
  n_q     = 6
)


power_bay <- cbc_power(
  data    = data_bay,
  pars    = c("price", "dist_green", "dist_shops", "dist_trans", "balcony"),
  outcome = "choice",
  obsID   = "obsID",
  nbreaks = 10,
  n_q     = 6
)

plot(power_dopt)

plot_compare_power(power_dopt, power_bay,power_rand)


# Estimate a model

m1_dopt <- logitr(
  data    = data_rand %>% filter(sex == "Man"),
  outcome = "choice",
  obsID   = "obsID",
  pars    = c( "dist_green", "dist_shops","dist_trans","balcony", "price")
)

m2_dopt <- logitr(
  data    = data_rand %>% filter(sex == "Kvinna"),
  outcome = "choice",
  obsID   = "obsID",
  pars    = c( "dist_green", "dist_shops","dist_trans","balcony", "price")
)


m3_dopt <- logitr(
  data    = data_rand ,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c( "dist_green", "dist_shops","dist_trans","balcony", "price")
)


tex_1 <- texreg(list(m1_dopt,m2_dopt, m3_dopt),
                digits = 3,
                include.groups = FALSE,
                caption = "Baseline regressions",
                caption.above = TRUE,
                custom.coef.map = list("dist_green10km" = "$Dist to green 10km$",
                                       "dist_green15km" = "$Dist to green 15km$",
                                       "dist_shops10km" = "$Dist to shops 10km$",
                                       "dist_shops15km" = "$Dist to shops 15km$",
                                       "dist_trans10km" = "$Dist to transport 10km$",
                                       "dist_trans15km" = "$Dist to transport 15km$",
                                       "balcony10m2" = "$Balcony 10m2$",
                                       "balcony15m2" = "$Balcony 15m2$",
                                       "price0" = "$Price same$",
                                       "price10%" = "$Price 10per more$"
                ),
                float.pos = "H",
                custom.model.names = c("Men","Women","Total"),
                label = "tab:regs"
                
)

note <- "\\textbf{Note:} Table reports multiple logistice regression results from Equation 1."
tex_1

stargazernote(tex_1, here("latex/elsvier/tables", "reg.tex"),note = note )







# test mlogit

data("Fishing",package="Ecdat")
colnames(Fishing)[4:11] <- c("pr.beach","pr.pier","pr.boat","pr.charter",
                             "ca.beach","ca.pier","ca.boat","ca.charter")

Fish <- mlogit.data(Fishing,varying=c(4:11),shape="wide",choice="mode")
summary(mlogit(mode~pr+ca-1,data=Fish))


survey_dat <- mlogit.data(data_dopt, shape = "wide", choice = "choice")

m1_mlog <- mlogit(choice ~ dist_green + dist_shops + dist_trans,
                  data =survey_dat)



multinom(choice ~ dist_green + dist_shops + dist_trans + balcony,data=data_dopt)



