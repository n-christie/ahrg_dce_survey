library(pacman)
p_load(here, tidyverse, haven, table1, flextable)

# Load respondent-level survey data (already one row per respondent) ----
# survey_res.rds is a pre-joined file built in descriptive_table.R.
# df_model.rds is the DCE analysis dataset (long format, one row per choice occasion)
# and is NOT appropriate for descriptive statistics.
surv_df <- readRDS(here("data/formr", "survey_res.rds"))

# Education variable lives in a separate file; join by RespondentID
edu_df  <- readRDS(here("data/clean", "out.rds"))
surv_df <- dplyr::left_join(surv_df, edu_df, by = "RespondentID")

dfSum <- surv_df |>
  mutate(
    age       = floor(Age_T3),
    age_group = factor(case_when(
      age >= 55 & age < 65 ~ "55-64",
      age >= 65 & age < 75 ~ "65-74",
      age >= 75             ~ "75+"
    )),
    ägandebostad    = haven::as_factor(ägandebostad),
    # 6 respondents have NA ownership — treat as Renter (consistent with baseline_regs.R)
    ägandebostad    = if_else(is.na(ägandebostad), "Nej", as.character(ägandebostad)),
    bostadstyp      = haven::as_factor(bostadstyp),
    Sex             = haven::as_factor(Sex),
    Sex             = factor(if_else(Sex == "Man", "Male", "Female")),
    civil_status_T2 = if_else(civil_status_T2 == 1, "Partnered", "Not partnered"),
    Own             = factor(if_else(ägandebostad == "Ja", "Owner", "Renter")),
    Hus             = factor(if_else(
      bostadstyp %in% c("Friliggande villa/hus/gård", "Radhus/kedjehus/parhus"),
      "House", "Apartment/Condo"
    )),
    Retired         = haven::as_factor(VAR174_8),
    health = haven::as_factor(VAR035) |>
      forcats::fct_recode("Good" = "God") |>
      forcats::fct_relevel("Poor", "Fairly", "Good", "Very good", "Excellent"),
    location = case_when(
      VAR010 == 1 ~ "City/town",
      VAR010 == 2 ~ "Urban/countryside",
      TRUE        ~ NA_character_
    ),
    income      = if_else(as.numeric(income) == 0, NA_real_, as.numeric(income)),
    planed_cost = if_else(as.numeric(planed_cost) == 0, NA_real_, as.numeric(planed_cost)),
    monthcost   = if_else(as.numeric(monthcost) == 0, NA_real_, as.numeric(monthcost)),
    VAR011_factor = factor(case_when(
      VAR011 %in% c(0, 1) ~ "1",
      VAR011 == 2          ~ "2",
      VAR011 >= 3          ~ "3 or more",
      TRUE                 ~ NA_character_
    )),
    edu = factor(VAR76) |>
      forcats::fct_recode(
        "Elementary school"        = "1",
        "2 years upper secondary"  = "2",
        "3-4 years upper secondary" = "3",
        "University < 3 years"     = "4",
        "University >= 3 years"    = "5"
      )
  ) |>
  filter(!is.na(Sex)) |>
  select(Sex, age_group, civil_status_T2, edu, health, Retired,
         Hus, location, VAR011_factor, income, planed_cost, Own) |>
  as.data.frame()

# Labels ----
label(dfSum$Sex)             <- "Sex"
label(dfSum$age_group)       <- "Age group"
label(dfSum$civil_status_T2) <- "Civil status"
label(dfSum$edu)             <- "Education"
label(dfSum$health)          <- "Self-reported health"
label(dfSum$Retired)         <- "Retired"
label(dfSum$Hus)             <- "Housing type"
label(dfSum$location)        <- "Housing location"
label(dfSum$VAR011_factor)   <- "Number in household"
label(dfSum$income)          <- "Monthly household income (SEK)"
label(dfSum$planed_cost)     <- "Planned monthly housing cost (SEK)"

# When took survey -----

surv_df %>%
  ggplot(aes( x = as.Date( created_page_0))) +
  geom_histogram(bins = 150,
                 color = 'black',fill = 'slateblue') +
  scale_x_date(labels = scales::date_format("%b-%d"),
               date_breaks = '2 week') +
  labs(title = "Number of respondents over time",
       x = "",
       y = "Number of respondents")+
  theme_light()


surv_df %>%
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
  #filter(planed_cost != 0) %>%
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


dfSum %>% 
  filter(planed_cost != 0,
         income != 0) %>%
  mutate(income_qrt = factor(ntile(income, 3))) %>% 
  ggplot(aes(x = log(planed_cost), fill = income_qrt)) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Density of Planned Cost by Income Tercile",
    x = "Log planned cost",
    fill = "Income Quartile"
  ) +
  theme_minimal()

