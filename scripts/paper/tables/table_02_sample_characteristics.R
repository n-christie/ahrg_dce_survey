# table_02_sample_characteristics.R
# Table 2: Sample characteristics by tenure type (Owner vs Renter).
# Output: output/tables/table_02_sample_characteristics.html

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

# P-value function for extra column ----
# Compares only Owner vs Renter (first two strata); ignores the Overall column.
pvalue <- function(x, ...) {
  x2 <- x[1:2]  # drop Overall stratum if present
  y  <- unlist(x2)
  g  <- factor(rep(seq_along(x2), times = lengths(x2)))
  if (is.numeric(y)) {
    p <- t.test(y ~ g)$p.value
  } else {
    p <- chisq.test(table(y, g))$p.value
  }
  c("", sub("<", "&lt;", format.pval(p, digits = 3, eps = 0.001)))
}

# Build table ----
t1 <- table1::table1(
  ~ Sex + age_group + civil_status_T2 + edu + health + Retired +
    Hus + location + VAR011_factor + income + planed_cost | Own,
  data          = dfSum,
  na.rm         = TRUE,
  overall       = "Overall",
  digits        = 2,
  extra.col     = list(`P-value` = pvalue),
  caption       = "Table 2. Sample characteristics by tenure type"
)

# Save as HTML via flextable ----
ft <- table1::t1flex(t1) |>
  flextable::fontsize(size = 10) |>
  flextable::padding(padding = 2, part = "all")

save_as_html(ft, path = here("output/tables", "table_02_sample_characteristics.html"))

message("Table 2 saved to output/tables/table_02_sample_characteristics.html")
