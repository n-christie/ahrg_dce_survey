# latent_class_analysis.R
#
# Latent Class Logit Models for DCE Housing Preference Data
#
# Steps:
#   1. Load + clean data (mirrors baseline_regs.R)
#   2. Fit MNL and LC models (2, 3, 4 classes)
#   3. Compare model fit (AIC, BIC, CAIC, relative entropy)
#   4. Extract class-specific coefficients and WTP
#   5. Profile classes by respondent demographics
#   6. Optional: LC with class-membership covariates
#   7. Save models and LaTeX tables

library(pacman)
p_load(
  tidyverse, here, haven,
  gmnl, mlogit,
  texreg, kableExtra,
  broom, stringr
)

set.seed(12345)

# ==============================================================================
# 1. LOAD AND CLEAN DATA
# ==============================================================================

df_model <- readRDS(here("data/formr", "df_model.rds"))

df_model <- df_model %>%
  mutate(
    price_pp   = as.numeric(price) / 100,
    cost_new   = planed_cost * (1 + price_pp),
    cost_diff  = planed_cost * price_pp,
    price_pct  = as.numeric(price),
    dist_trans = factor(dist_trans, levels = c("900", "600", "300")),
    dist_green = factor(dist_green, levels = c("15km", "5km", "500 meter")),
    dist_shops = factor(dist_shops, levels = c("15km", "5km", "500 meter")),
    proportion_of_income = planed_cost / income,
    age        = floor(Age_T3),
    age_group  = factor(case_when(
      age >= 55 & age < 65 ~ "55-64",
      age >= 65 & age < 75 ~ "65-74",
      age >= 75             ~ "75+"
    )),
    price_num  = price_num / 100
  ) %>%
  mutate(
    ägandebostad = haven::as_factor(ägandebostad),
    bostadstyp   = haven::as_factor(bostadstyp),
    Sex          = haven::as_factor(Sex),
    Civil        = haven::as_factor(civil_status_T2),
    civil_d      = factor(if_else(civil_status_T2 == 1, "Partnered", "Not partnered")),
    Own          = factor(if_else(ägandebostad == "Ja", "Owner", "Renter")),
    Hus          = factor(if_else(
      bostadstyp %in% c("Friliggande villa/hus/gård", "Radhus/kedjehus/parhus"),
      "House", "Apartment/Condo"
    )),
    downsize     = if_else(planed_cost < monthcost, 1, 0),
    location     = case_when(
      VAR010 == 1 ~ "City/town",
      VAR010 == 2 ~ "Urban/countryside",
      TRUE        ~ NA_character_
    ),
    self_report_health = as.numeric(VAR035),
    is_owner     = as.integer(Own == "Owner"),
    is_female    = as.integer(Sex == "Kvinna"),
    is_partnered = as.integer(civil_d == "Partnered"),
    G65_74       = as.integer(age_group == "65-74"),
    G75p         = as.integer(age_group == "75+")
  )

# ==============================================================================
# 2. PREPARE DATA FOR GMNL
# ==============================================================================

# Note: no 'opposite' argument - price_num enters naturally with a negative
# coefficient (higher price → lower utility), consistent with MXL models.

df_gmnl <- mlogit.data(
  data     = df_model,
  choice   = "choice",
  shape    = "long",
  alt.var  = "altID",
  chid.var = "obsID",
  id.var   = "panelID"
)

n_ind <- length(unique(df_model$panelID))
n_obs <- length(unique(df_model$obsID))

cat("Individuals:", n_ind, " | Choice tasks:", n_obs, "\n")

# ==============================================================================
# 3. FIT MNL AND LATENT CLASS MODELS
# ==============================================================================

cat("\n--- Fitting MNL baseline ---\n")
mnl_base <- gmnl(
  choice ~ dist_green + dist_shops + dist_trans + parking + price_num | 0,
  data  = df_gmnl,
  model = "mnl"
)
summary(mnl_base)

cat("\n--- Fitting LC-2 ---\n")
lc2 <- gmnl(
  choice ~ dist_green + dist_shops + dist_trans + parking + price_num | 0 | 0 | 0 | 1,
  data   = df_gmnl,
  model  = "lc",
  Q      = 2,
  panel  = TRUE,
  method = "bfgs"
)
summary(lc2)

cat("\n--- Fitting LC-3 ---\n")
lc3 <- gmnl(
  choice ~ dist_green + dist_shops + dist_trans + parking + price_num | 0 | 0 | 0 | 1,
  data   = df_gmnl,
  model  = "lc",
  Q      = 3,
  panel  = TRUE,
  method = "bfgs"
)
summary(lc3)

cat("\n--- Fitting LC-4 ---\n")
lc4 <- gmnl(
  choice ~ dist_green + dist_shops + dist_trans + parking + price_num | 0 | 0 | 0 | 1,
  data   = df_gmnl,
  model  = "lc",
  Q      = 4,
  panel  = TRUE,
  method = "bfgs"
)
summary(lc4)

# Save models
saveRDS(mnl_base, here("output/models", "lc_mnl_base.rds"))
saveRDS(lc2,      here("output/models", "lc_2class.rds"))
saveRDS(lc3,      here("output/models", "lc_3class.rds"))
saveRDS(lc4,      here("output/models", "lc_4class.rds"))

cat("Models saved.\n")

# ==============================================================================
# 4. MODEL COMPARISON
# ==============================================================================

# Relative entropy: 1 = perfect class separation, 0 = no separation
calc_entropy <- function(model, Q, n) {
  Qir <- model$Qir
  1 + sum(Qir * log(Qir + 1e-12)) / (n * log(Q))
}

model_fit <- tibble(
  Model  = c("MNL", "LC-2", "LC-3", "LC-4"),
  Q      = c(1L, 2L, 3L, 4L),
  LogLik = c(
    as.numeric(logLik(mnl_base)),
    as.numeric(logLik(lc2)),
    as.numeric(logLik(lc3)),
    as.numeric(logLik(lc4))
  ),
  K = c(
    length(coef(mnl_base)),
    length(coef(lc2)),
    length(coef(lc3)),
    length(coef(lc4))
  )
) %>%
  mutate(
    AIC     = -2 * LogLik + 2 * K,
    BIC     = -2 * LogLik + log(n_ind) * K,
    CAIC    = -2 * LogLik + (log(n_ind) + 1) * K,
    Entropy = c(
      NA_real_,
      calc_entropy(lc2, 2, n_ind),
      calc_entropy(lc3, 3, n_ind),
      calc_entropy(lc4, 4, n_ind)
    )
  )

print(model_fit)

# ==============================================================================
# 5. EXTRACT CLASS-SPECIFIC COEFFICIENTS
# ==============================================================================

# Extract utility parameters by class using coefficient names (pattern: var.class.Q)
extract_lc_coefs <- function(model, Q) {
  sm <- summary(model)$CoefTable

  lapply(1:Q, function(q) {
    prefix  <- paste0("^class\\.", q, "\\.")
    idx     <- grep(prefix, rownames(sm))
    tibble(
      class     = q,
      variable  = sub(prefix, "", rownames(sm)[idx]),
      estimate  = sm[idx, "Estimate"],
      std_error = sm[idx, "Std. Error"],
      z_value   = sm[idx, "z-value"],
      p_value   = sm[idx, "Pr(>|z|)"]
    )
  }) %>%
    bind_rows()
}

# Extract prior class shares from the class-membership logit parameters
extract_class_shares <- function(model, Q) {
  sm <- summary(model)$CoefTable
  # Class membership params are named "(class)2", "(class)3", etc.
  delta_idx <- grep("^\\(class\\)", rownames(sm))
  deltas    <- c(0, sm[delta_idx, "Estimate"])  # reference class has delta=0
  exp_d     <- exp(deltas)
  exp_d / sum(exp_d)
}

coefs_lc2 <- extract_lc_coefs(lc2, 2)
coefs_lc3 <- extract_lc_coefs(lc3, 3)
coefs_lc4 <- extract_lc_coefs(lc4, 4)

shares_lc2 <- extract_class_shares(lc2, 2)
shares_lc3 <- extract_class_shares(lc3, 3)
shares_lc4 <- extract_class_shares(lc4, 4)

cat("\nLC-2 class shares:\n"); print(round(shares_lc2, 3))
cat("\nLC-3 class shares:\n"); print(round(shares_lc3, 3))
cat("\nLC-4 class shares:\n"); print(round(shares_lc4, 3))

# ==============================================================================
# 6. WILLINGNESS TO PAY BY CLASS
# ==============================================================================

# WTP = -(beta_attribute / beta_price) * scale
# Scale: 10% of overall median monthly housing cost

median_cost_overall <- median(
  df_model %>% distinct(panelID, .keep_all = TRUE) %>% pull(planed_cost),
  na.rm = TRUE
)
scaler <- 0.10 * median_cost_overall

cat("\nMedian planned cost:", median_cost_overall, "| WTP scale:", scaler, "\n")

compute_lc_wtp <- function(coef_df, scale) {
  price_row <- coef_df %>% filter(variable == "price_num")
  coef_df %>%
    filter(variable != "price_num") %>%
    left_join(
      price_row %>% select(class, beta_price = estimate),
      by = "class"
    ) %>%
    mutate(wtp = -(estimate / beta_price) * scale)
}

wtp_lc2 <- compute_lc_wtp(coefs_lc2, scaler)
wtp_lc3 <- compute_lc_wtp(coefs_lc3, scaler)

# ==============================================================================
# 7. CLASS MEMBERSHIP PROFILES (posterior probabilities)
# ==============================================================================

profile_classes <- function(model, Q, df, gmnl_data) {
  Qir <- as.data.frame(model$Qir)
  prob_cols <- paste0("prob_class_", 1:Q)
  names(Qir) <- prob_cols

  # Row order of Qir matches the order of unique individuals in the index
  panel_order <- as.character(unique(gmnl_data$panelID))

  Qir$panelID     <- panel_order
  Qir$modal_class <- max.col(Qir[, prob_cols])

  ind_data <- df %>%
    distinct(panelID, .keep_all = TRUE) %>%
    mutate(panelID = as.character(panelID)) %>%
    select(panelID, age, age_group, Own, Sex, civil_d, Hus, income,
           location, self_report_health)

  left_join(Qir, ind_data, by = "panelID")
}

class_profiles_lc2 <- profile_classes(lc2, 2, df_model, df_gmnl)
class_profiles_lc3 <- profile_classes(lc3, 3, df_model, df_gmnl)

summarise_profiles <- function(class_df, Q) {
  class_df %>%
    group_by(modal_class) %>%
    summarise(
      N            = n(),
      Share_pct    = round(n() / nrow(.) * 100, 1),
      Avg_age      = round(mean(age, na.rm = TRUE), 1),
      Pct_55_64    = round(mean(age_group == "55-64", na.rm = TRUE) * 100, 1),
      Pct_65_74    = round(mean(age_group == "65-74", na.rm = TRUE) * 100, 1),
      Pct_75p      = round(mean(age_group == "75+",   na.rm = TRUE) * 100, 1),
      Pct_owner    = round(mean(Own == "Owner",        na.rm = TRUE) * 100, 1),
      Pct_female   = round(mean(Sex == "Kvinna",       na.rm = TRUE) * 100, 1),
      Pct_partnered = round(mean(civil_d == "Partnered", na.rm = TRUE) * 100, 1),
      Pct_house    = round(mean(Hus == "House",        na.rm = TRUE) * 100, 1),
      Avg_health   = round(mean(self_report_health, na.rm = TRUE), 2),
      .groups = "drop"
    )
}

profiles_lc2 <- summarise_profiles(class_profiles_lc2, 2)
profiles_lc3 <- summarise_profiles(class_profiles_lc3, 3)

cat("\n--- LC-2 Class Profiles ---\n"); print(profiles_lc2)
cat("\n--- LC-3 Class Profiles ---\n"); print(profiles_lc3)

# ==============================================================================
# 8. LC WITH CLASS-MEMBERSHIP COVARIATES
# ==============================================================================
# Uses age, ownership status, and health as predictors of class membership.
# Requires a separate mlogit.data without the 'opposite' flag so individual-
# level covariates are preserved correctly.

cat("\n--- Fitting LC-2 with class-membership covariates ---\n")

lc2_cov <- gmnl(
  choice ~ dist_green + dist_shops + dist_trans + parking + price_num |
    0 | 0 | 0 |
    age + is_owner + is_female + self_report_health,
  data   = df_gmnl,
  model  = "lc",
  Q      = 2,
  panel  = TRUE,
  method = "bfgs"
)
summary(lc2_cov)
saveRDS(lc2_cov, here("output/models", "lc_2class_cov.rds"))

# ==============================================================================
# 9. PUBLICATION TABLES
# ==============================================================================

label_map <- c(
  "dist_green5km"                  = "Green space: 5 km (vs 15 km)",
  "dist_green500 meter"            = "Green space: 500 m (vs 15 km)",
  "dist_shops5km"                  = "Shops: 5 km (vs 15 km)",
  "dist_shops500 meter"            = "Shops: 500 m (vs 15 km)",
  "dist_trans600"                  = "Transit stop: 600 m (vs 900 m)",
  "dist_trans300"                  = "Transit stop: 300 m (vs 900 m)",
  "parkingreserverad garageplats"  = "Parking: reserved garage (vs none)",
  "parkingreserverad P-plats"      = "Parking: reserved space (vs none)",
  "price_num"                      = "Price"
)

# --- Helper: build a texreg object for one class ---
make_lc_texreg <- function(coef_df, q, shares, label_map) {
  d <- coef_df %>% filter(class == q)
  createTexreg(
    coef.names = label_map[d$variable],
    coef       = d$estimate,
    se         = d$std_error,
    pvalues    = d$p_value,
    gof.names  = "Class share (%)",
    gof        = round(shares[q] * 100, 1),
    gof.decimal = TRUE
  )
}

# -- LC-2 screen table --
tr_lc2 <- lapply(1:2, make_lc_texreg,
                 coef_df = coefs_lc2, shares = shares_lc2, label_map = label_map)

screenreg(
  tr_lc2,
  custom.model.names = paste0("Class ", 1:2),
  custom.coef.names  = label_map,
  stars    = c(0.001, 0.01, 0.05),
  booktabs = TRUE,
  dcolumn  = TRUE,
  caption  = "Latent Class Model: 2-Class Solution"
)

# -- LC-3 screen table --
tr_lc3 <- lapply(1:3, make_lc_texreg,
                 coef_df = coefs_lc3, shares = shares_lc3, label_map = label_map)

screenreg(
  tr_lc3,
  custom.model.names = paste0("Class ", 1:3),
  custom.coef.names  = label_map,
  stars    = c(0.001, 0.01, 0.05),
  booktabs = TRUE,
  dcolumn  = TRUE,
  caption  = "Latent Class Model: 3-Class Solution"
)

# -- LaTeX: LC-2 coefficients --
texreg(
  tr_lc2,
  custom.model.names = paste0("Class ", 1:2),
  custom.coef.names  = label_map,
  stars        = c(0.001, 0.01, 0.05),
  booktabs     = TRUE,
  dcolumn      = TRUE,
  use.packages = FALSE,
  caption      = "Latent Class Model (2-Class): Utility Coefficients",
  caption.above = TRUE,
  fontsize     = "scriptsize",
  na.replace   = "--",
  file         = here("paper/tex/tables", "lc_2class_coefs.tex")
)

# -- LaTeX: LC-3 coefficients --
texreg(
  tr_lc3,
  custom.model.names = paste0("Class ", 1:3),
  custom.coef.names  = label_map,
  stars        = c(0.001, 0.01, 0.05),
  booktabs     = TRUE,
  dcolumn      = TRUE,
  use.packages = FALSE,
  caption      = "Latent Class Model (3-Class): Utility Coefficients",
  caption.above = TRUE,
  fontsize     = "scriptsize",
  na.replace   = "--",
  file         = here("paper/tex/tables", "lc_3class_coefs.tex")
)

# -- Model fit comparison table (LaTeX) --
fit_table_out <- model_fit %>%
  mutate(
    LogLik  = round(LogLik, 1),
    AIC     = round(AIC, 1),
    BIC     = round(BIC, 1),
    CAIC    = round(CAIC, 1),
    Entropy = ifelse(is.na(Entropy), "--", sprintf("%.3f", Entropy))
  ) %>%
  select(Model, `Log Lik.` = LogLik, K, AIC, BIC, CAIC, Entropy)

kbl(
  fit_table_out,
  booktabs = TRUE,
  format   = "latex",
  caption  = "Latent Class Model Comparison",
  escape   = FALSE,
  align    = c("l", "r", "r", "r", "r", "r", "r")
) %>%
  kable_classic(full_width = FALSE, latex_options = "hold_position") %>%
  footnote(
    general = paste0(
      "BIC and CAIC computed using number of respondents (N = ", n_ind, ") as sample size. ",
      "Relative entropy measures class separation: values closer to 1 indicate cleaner separation."
    ),
    general_title = "",
    threeparttable = TRUE
  ) %>%
  save_kable(here("paper/tex/tables", "lc_model_comparison.tex"))

# -- WTP table: LC-2 (LaTeX) --
wtp_wide_lc2 <- wtp_lc2 %>%
  select(variable, class, wtp) %>%
  pivot_wider(names_from = class, values_from = wtp, names_prefix = "Class ") %>%
  mutate(
    Attribute = label_map[variable],
    across(starts_with("Class"), ~ round(.x, 0))
  ) %>%
  filter(!is.na(Attribute)) %>%
  select(Attribute, starts_with("Class"))

kbl(
  wtp_wide_lc2,
  booktabs = TRUE,
  format   = "latex",
  caption  = "Marginal Willingness to Pay by Latent Class (2-Class Solution, SEK/month)",
  escape   = FALSE
) %>%
  kable_classic(full_width = FALSE, latex_options = "hold_position") %>%
  add_header_above(c(" " = 1, "Latent Class" = 2)) %>%
  footnote(
    general = paste0(
      "WTP = $-(\\\\beta_{\\\\text{attribute}} / \\\\beta_{\\\\text{price}}) \\\\times ",
      round(scaler, 0),
      "$. Scale factor = 10\\\\% of median monthly housing cost (",
      round(median_cost_overall, 0),
      " SEK/month)."
    ),
    general_title = "",
    threeparttable = TRUE,
    escape = FALSE
  ) %>%
  save_kable(here("paper/tex/tables", "lc_wtp_2class.tex"))

# -- WTP table: LC-3 (LaTeX) --
wtp_wide_lc3 <- wtp_lc3 %>%
  select(variable, class, wtp) %>%
  pivot_wider(names_from = class, values_from = wtp, names_prefix = "Class ") %>%
  mutate(
    Attribute = label_map[variable],
    across(starts_with("Class"), ~ round(.x, 0))
  ) %>%
  filter(!is.na(Attribute)) %>%
  select(Attribute, starts_with("Class"))

kbl(
  wtp_wide_lc3,
  booktabs = TRUE,
  format   = "latex",
  caption  = "Marginal Willingness to Pay by Latent Class (3-Class Solution, SEK/month)",
  escape   = FALSE
) %>%
  kable_classic(full_width = FALSE, latex_options = "hold_position") %>%
  add_header_above(c(" " = 1, "Latent Class" = 3)) %>%
  footnote(
    general = paste0(
      "WTP = $-(\\\\beta_{\\\\text{attribute}} / \\\\beta_{\\\\text{price}}) \\\\times ",
      round(scaler, 0), "$."
    ),
    general_title = "",
    threeparttable = TRUE,
    escape = FALSE
  ) %>%
  save_kable(here("paper/tex/tables", "lc_wtp_3class.tex"))

cat("\n=== All done. Tables saved to paper/tex/tables/ ===\n")
cat("Models saved to output/models/\n")
cat("\nNext steps:\n")
cat("  1. Check model_fit table to choose optimal Q\n")
cat("  2. Inspect class profiles (profiles_lc2, profiles_lc3)\n")
cat("  3. Examine lc2_cov for covariate effects on class membership\n")
