# lc_results.R
# Loads saved LC models and produces results + tables.
# Run after latent_class_analysis.R has saved the model .rds files.

library(pacman)
p_load(tidyverse, here, haven, gmnl, mlogit, texreg, kableExtra, stringr)

set.seed(12345)

# ==============================================================================
# DATA
# ==============================================================================

df_model <- readRDS(here("data/formr", "df_model.rds"))

df_model <- df_model %>%
  mutate(
    price_pp   = as.numeric(price) / 100,
    dist_trans = factor(dist_trans, levels = c("900", "600", "300")),
    dist_green = factor(dist_green, levels = c("15km", "5km", "500 meter")),
    dist_shops = factor(dist_shops, levels = c("15km", "5km", "500 meter")),
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
    civil_d      = factor(if_else(civil_status_T2 == 1, "Partnered", "Not partnered")),
    Own          = factor(if_else(ägandebostad == "Ja", "Owner", "Renter")),
    Hus          = factor(if_else(
      bostadstyp %in% c("Friliggande villa/hus/gård", "Radhus/kedjehus/parhus"),
      "House", "Apartment/Condo"
    )),
    self_report_health = as.numeric(VAR035),
    location = case_when(
      VAR010 == 1 ~ "City/town",
      VAR010 == 2 ~ "Urban/countryside",
      TRUE        ~ NA_character_
    )
  )

df_gmnl <- mlogit.data(
  data = df_model, choice = "choice", shape = "long",
  alt.var = "altID", chid.var = "obsID", id.var = "panelID"
)

n_ind <- length(unique(df_model$panelID))

# ==============================================================================
# LOAD MODELS
# ==============================================================================

mnl  <- readRDS(here("output/models", "lc_mnl_base.rds"))
lc2  <- readRDS(here("output/models", "lc_2class.rds"))
lc3  <- readRDS(here("output/models", "lc_3class.rds"))
lc4  <- readRDS(here("output/models", "lc_4class.rds"))

# ==============================================================================
# HELPERS
# ==============================================================================

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
      p_value   = sm[idx, "Pr(>|z|)"]
    )
  }) %>% bind_rows()
}

extract_class_shares <- function(model) {
  sm        <- summary(model)$CoefTable
  delta_idx <- grep("^\\(class\\)", rownames(sm))
  deltas    <- c(0, sm[delta_idx, "Estimate"])
  exp_d     <- exp(deltas)
  round(exp_d / sum(exp_d), 3)
}

calc_entropy <- function(model, Q) {
  Qir <- model$Qir
  1 + sum(Qir * log(Qir + 1e-12)) / (n_ind * log(Q))
}

profile_classes <- function(model, Q) {
  Qir       <- as.data.frame(model$Qir)
  prob_cols <- paste0("prob_class_", 1:Q)
  names(Qir) <- prob_cols
  Qir$panelID     <- as.character(unique(df_gmnl$panelID))
  Qir$modal_class <- max.col(Qir[, prob_cols])

  ind_data <- df_model %>%
    distinct(panelID, .keep_all = TRUE) %>%
    mutate(panelID = as.character(panelID)) %>%
    select(panelID, age, age_group, Own, Sex, civil_d, Hus,
           income, location, self_report_health)

  left_join(Qir, ind_data, by = "panelID")
}

label_map <- c(
  "dist_green5km"                  = "Green space: 5 km (vs 15 km)",
  "dist_green500 meter"            = "Green space: 500 m (vs 15 km)",
  "dist_shops5km"                  = "Shops: 5 km (vs 15 km)",
  "dist_shops500 meter"            = "Shops: 500 m (vs 15 km)",
  "dist_trans600"                  = "Transit: 600 m (vs 900 m)",
  "dist_trans300"                  = "Transit: 300 m (vs 900 m)",
  "parkingreserverad garageplats"  = "Parking: reserved garage (vs none)",
  "parkingreserverad P-plats"      = "Parking: reserved space (vs none)",
  "price_num"                      = "Price"
)

# ==============================================================================
# 1. MODEL FIT TABLE
# ==============================================================================

model_fit <- tibble(
  Model  = c("MNL", "LC-2", "LC-3", "LC-4"),
  Q      = c(1L, 2L, 3L, 4L),
  LogLik = c(as.numeric(logLik(mnl)), as.numeric(logLik(lc2)),
             as.numeric(logLik(lc3)), as.numeric(logLik(lc4))),
  K      = c(length(coef(mnl)), length(coef(lc2)),
             length(coef(lc3)), length(coef(lc4)))
) %>%
  mutate(
    AIC     = round(-2 * LogLik + 2 * K, 1),
    BIC     = round(-2 * LogLik + log(n_ind) * K, 1),
    CAIC    = round(-2 * LogLik + (log(n_ind) + 1) * K, 1),
    Entropy = c(NA, round(calc_entropy(lc2, 2), 3),
                    round(calc_entropy(lc3, 3), 3),
                    round(calc_entropy(lc4, 4), 3)),
    LogLik  = round(LogLik, 1)
  )

cat("\n========================================\n")
cat("MODEL FIT COMPARISON\n")
cat("========================================\n")
print(as.data.frame(model_fit))

# ==============================================================================
# 2. CLASS SHARES
# ==============================================================================

cat("\n========================================\n")
cat("CLASS SHARES (prior probabilities)\n")
cat("========================================\n")
cat("LC-2:", paste(round(extract_class_shares(lc2) * 100, 1), "%", collapse = "  /  "), "\n")
cat("LC-3:", paste(round(extract_class_shares(lc3) * 100, 1), "%", collapse = "  /  "), "\n")
cat("LC-4:", paste(round(extract_class_shares(lc4) * 100, 1), "%", collapse = "  /  "), "\n")

# ==============================================================================
# 3. COEFFICIENTS (LC-2, LC-3, LC-4)
# ==============================================================================

coefs_lc2 <- extract_lc_coefs(lc2, 2)
coefs_lc3 <- extract_lc_coefs(lc3, 3)
coefs_lc4 <- extract_lc_coefs(lc4, 4)

cat("\n========================================\n")
cat("LC-2 COEFFICIENTS\n")
cat("========================================\n")
coefs_lc2 %>%
  mutate(label = label_map[variable], sig = case_when(
    p_value < 0.001 ~ "***", p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*", TRUE ~ ""
  )) %>%
  select(Class = class, Attribute = label, Coef = estimate, SE = std_error, Sig = sig) %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  print(n = 30)

cat("\n========================================\n")
cat("LC-3 COEFFICIENTS\n")
cat("========================================\n")
coefs_lc3 %>%
  mutate(label = label_map[variable], sig = case_when(
    p_value < 0.001 ~ "***", p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*", TRUE ~ ""
  )) %>%
  select(Class = class, Attribute = label, Coef = estimate, SE = std_error, Sig = sig) %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  print(n = 50)

cat("\n========================================\n")
cat("LC-4 COEFFICIENTS\n")
cat("========================================\n")
coefs_lc4 %>%
  mutate(label = label_map[variable], sig = case_when(
    p_value < 0.001 ~ "***", p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*", TRUE ~ ""
  )) %>%
  select(Class = class, Attribute = label, Coef = estimate, SE = std_error, Sig = sig) %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  print(n = 60)

# ==============================================================================
# 4. WTP BY CLASS
# ==============================================================================

# Pooled-sample median (not mean): Table 3 (baseline_regs.R) uses
# tenure-specific medians (owners 10,000 SEK, renters 9,000 SEK); the latent
# class models are fit on the pooled sample, so the pooled median is the
# right analogue. Reverted 2026-08-21 after a same-session edit mistakenly
# switched this to the mean based on the wrong script (interaction_regs_
# table.R generates a different table, not Table 3).
median_cost <- median(
  df_model %>% distinct(panelID, .keep_all = TRUE) %>% pull(planed_cost),
  na.rm = TRUE
)
scaler <- 0.10 * median_cost

# SEs via the delta method (matches lc_plot.R): a ratio of two jointly-
# estimated, correlated coefficients has its own sampling variance, so the
# point estimate alone doesn't say whether a WTP is distinguishable from zero.
compute_lc_wtp <- function(model, Q, scale) {
  vc <- vcov(model)
  sm <- summary(model)$CoefTable

  map_dfr(1:Q, function(q) {
    price_name <- paste0("class.", q, ".price_num")
    beta_price <- sm[price_name, "Estimate"]

    prefix   <- paste0("^class\\.", q, "\\.")
    idx      <- grep(prefix, rownames(sm))
    attr_idx <- idx[rownames(sm)[idx] != price_name]

    map_dfr(attr_idx, function(i) {
      attr_name <- rownames(sm)[i]
      beta_attr <- sm[i, "Estimate"]

      wtp  <- -(beta_attr / beta_price) * scale
      grad <- c(-1 / beta_price, beta_attr / beta_price^2) * scale
      V    <- vc[c(attr_name, price_name), c(attr_name, price_name)]
      se   <- sqrt(as.numeric(t(grad) %*% V %*% grad))

      tibble(
        class    = q,
        variable = sub(prefix, "", attr_name),
        wtp      = round(wtp, 0),
        se       = se,
        lower    = wtp - 1.96 * se,
        upper    = wtp + 1.96 * se,
        p_value  = 2 * pnorm(-abs(wtp / se))
      )
    })
  })
}

wtp_lc2 <- compute_lc_wtp(lc2, 2, scaler)
wtp_lc3 <- compute_lc_wtp(lc3, 3, scaler)
wtp_lc4 <- compute_lc_wtp(lc4, 4, scaler)

cat("\n========================================\n")
cat("WTP BY CLASS (SEK/month, scale =", round(scaler), ")\n")
cat("========================================\n")
cat("\n--- LC-2 ---\n")
wtp_lc2 %>%
  mutate(label = label_map[variable],
         WTP = sprintf("%.0f (%.0f, %.0f)", wtp, lower, upper)) %>%
  select(Class = class, Attribute = label, WTP) %>%
  pivot_wider(names_from = Class, values_from = WTP, names_prefix = "Class ") %>%
  print()

cat("\n--- LC-3 ---\n")
wtp_lc3 %>%
  mutate(label = label_map[variable],
         WTP = sprintf("%.0f (%.0f, %.0f)", wtp, lower, upper)) %>%
  select(Class = class, Attribute = label, WTP) %>%
  pivot_wider(names_from = Class, values_from = WTP, names_prefix = "Class ") %>%
  print()

cat("\n--- LC-4 ---\n")
wtp_lc4 %>%
  mutate(label = label_map[variable],
         WTP = sprintf("%.0f (%.0f, %.0f)", wtp, lower, upper)) %>%
  select(Class = class, Attribute = label, WTP) %>%
  pivot_wider(names_from = Class, values_from = WTP, names_prefix = "Class ") %>%
  print()

# ==============================================================================
# 5. CLASS PROFILES
# ==============================================================================

prof2 <- profile_classes(lc2, 2)
prof3 <- profile_classes(lc3, 3)
prof4 <- profile_classes(lc4, 4)

summarise_profiles <- function(df) {
  df %>%
    group_by(modal_class) %>%
    summarise(
      N            = n(),
      `Share %`    = round(n() / nrow(.) * 100, 1),
      `Avg age`    = round(mean(age, na.rm = TRUE), 1),
      `% 55-64`    = round(mean(age_group == "55-64", na.rm = TRUE) * 100, 1),
      `% 65-74`    = round(mean(age_group == "65-74", na.rm = TRUE) * 100, 1),
      `% 75+`      = round(mean(age_group == "75+",   na.rm = TRUE) * 100, 1),
      `% Owner`    = round(mean(Own == "Owner",        na.rm = TRUE) * 100, 1),
      `% Female`   = round(mean(Sex == "Kvinna",       na.rm = TRUE) * 100, 1),
      `% Partnered`= round(mean(civil_d == "Partnered",na.rm = TRUE) * 100, 1),
      `Avg health` = round(mean(self_report_health,    na.rm = TRUE), 2),
      .groups = "drop"
    )
}

cat("\n========================================\n")
cat("CLASS PROFILES (LC-2)\n")
cat("========================================\n")
print(as.data.frame(summarise_profiles(prof2)))

cat("\n========================================\n")
cat("CLASS PROFILES (LC-3)\n")
cat("========================================\n")
print(as.data.frame(summarise_profiles(prof3)))

cat("\n========================================\n")
cat("CLASS PROFILES (LC-4)\n")
cat("========================================\n")
print(as.data.frame(summarise_profiles(prof4)))

# ==============================================================================
# 6. SAVE LaTeX TABLES
# ==============================================================================

make_lc_texreg <- function(coef_df, q, shares, label_map) {
  d <- coef_df %>% filter(class == q)
  createTexreg(
    coef.names  = label_map[d$variable],
    coef        = d$estimate,
    se          = d$std_error,
    pvalues     = d$p_value,
    gof.names   = "Class share (%)",
    gof         = round(shares[q] * 100, 1),
    gof.decimal = TRUE
  )
}

shares2 <- extract_class_shares(lc2)
shares3 <- extract_class_shares(lc3)
shares4 <- extract_class_shares(lc4)

tr_lc2 <- lapply(1:2, make_lc_texreg, coef_df = coefs_lc2, shares = shares2, label_map = label_map)
tr_lc3 <- lapply(1:3, make_lc_texreg, coef_df = coefs_lc3, shares = shares3, label_map = label_map)
tr_lc4 <- lapply(1:4, make_lc_texreg, coef_df = coefs_lc4, shares = shares4, label_map = label_map)

texreg(
  tr_lc2,
  custom.model.names = paste0("Class ", 1:2),
  custom.coef.names  = label_map,
  stars = c(0.001, 0.01, 0.05), booktabs = TRUE, dcolumn = TRUE,
  use.packages = FALSE, caption = "Latent Class Model (2-Class): Utility Coefficients",
  caption.above = TRUE, fontsize = "scriptsize", na.replace = "--",
  file = here("paper/tex/tables", "lc_2class_coefs.tex")
)

texreg(
  tr_lc3,
  custom.model.names = paste0("Class ", 1:3),
  custom.coef.names  = label_map,
  stars = c(0.001, 0.01, 0.05), booktabs = TRUE, dcolumn = TRUE,
  use.packages = FALSE, caption = "Latent Class Model (3-Class): Utility Coefficients",
  caption.above = TRUE, fontsize = "scriptsize", na.replace = "--",
  file = here("paper/tex/tables", "lc_3class_coefs.tex")
)

texreg(
  tr_lc4,
  custom.model.names = paste0("Class ", 1:4),
  custom.coef.names  = label_map,
  stars = c(0.001, 0.01, 0.05), booktabs = TRUE, dcolumn = TRUE,
  use.packages = FALSE, caption = "Latent Class Model (4-Class): Utility Coefficients",
  caption.above = TRUE, fontsize = "scriptsize", na.replace = "--",
  file = here("paper/tex/tables", "lc_4class_coefs.tex")
)

# Model fit table
fit_out <- model_fit %>%
  mutate(Entropy = ifelse(is.na(Entropy), "--", sprintf("%.3f", Entropy))) %>%
  select(Model, `Log Lik.` = LogLik, K, AIC, BIC, CAIC, Entropy)

kbl(fit_out, booktabs = TRUE, format = "latex",
    caption = "Latent Class Model Comparison", escape = FALSE,
    align = c("l","r","r","r","r","r","r")) %>%
  kable_classic(full_width = FALSE, latex_options = "hold_position") %>%
  footnote(general = paste0("BIC and CAIC use N = ", n_ind, " respondents. Entropy closer to 1 = cleaner class separation."),
           general_title = "", threeparttable = TRUE) %>%
  save_kable(here("paper/tex/tables", "lc_model_comparison.tex"))

# WTP tables
wtp_wide <- function(wtp_df, Q) {
  wtp_df %>%
    mutate(
      Attribute = label_map[variable],
      wtp_fmt   = sprintf("%.0f (%.0f, %.0f)%s", wtp, lower, upper,
                           ifelse(p_value < 0.05, "*", ""))
    ) %>%
    filter(!is.na(Attribute)) %>%
    select(Attribute, class, wtp_fmt) %>%
    pivot_wider(names_from = class, values_from = wtp_fmt, names_prefix = "Class ")
}

kbl(wtp_wide(wtp_lc2, 2), booktabs = TRUE, format = "latex",
    caption = "MWTP by Latent Class - 2-Class Solution (SEK/month, 95\\% CI)", escape = FALSE) %>%
  kable_classic(full_width = FALSE, latex_options = "hold_position") %>%
  add_header_above(c(" " = 1, "Latent Class" = 2)) %>%
  footnote(general = paste0("WTP = -(beta_attr/beta_price) x ", round(scaler),
                             " SEK. 95% CIs from the delta method. * = CI excludes zero."),
           general_title = "", threeparttable = TRUE) %>%
  save_kable(here("paper/tex/tables", "lc_wtp_2class.tex"))

kbl(wtp_wide(wtp_lc3, 3), booktabs = TRUE, format = "latex",
    caption = "MWTP by Latent Class - 3-Class Solution (SEK/month, 95\\% CI)", escape = FALSE) %>%
  kable_classic(full_width = FALSE, latex_options = "hold_position") %>%
  add_header_above(c(" " = 1, "Latent Class" = 3)) %>%
  footnote(general = paste0("WTP = -(beta_attr/beta_price) x ", round(scaler),
                             " SEK. 95% CIs from the delta method. * = CI excludes zero."),
           general_title = "", threeparttable = TRUE) %>%
  save_kable(here("paper/tex/tables", "lc_wtp_3class.tex"))

kbl(wtp_wide(wtp_lc4, 4), booktabs = TRUE, format = "latex",
    caption = "MWTP by Latent Class - 4-Class Solution (SEK/month, 95\\% CI)", escape = FALSE) %>%
  kable_classic(full_width = FALSE, latex_options = "hold_position") %>%
  add_header_above(c(" " = 1, "Latent Class" = 4)) %>%
  footnote(general = paste0("WTP = -(beta_attr/beta_price) x ", round(scaler),
                             " SEK. 95% CIs from the delta method. * = CI excludes zero."),
           general_title = "", threeparttable = TRUE) %>%
  save_kable(here("paper/tex/tables", "lc_wtp_4class.tex"))

cat("\n=== Tables saved to paper/tex/tables/ ===\n")
