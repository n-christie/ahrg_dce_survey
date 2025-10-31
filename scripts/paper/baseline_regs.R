library(pacman)

p_load(tidyverse, here, stringr, formr, logitr, cbcTools, texreg, likert, tidyr, haven, kableExtra,ggrepel, sjlabelled, summarytools,gtsummary,texreg)

# load data ----

df_model <- readRDS(here("data/formr", "df_model.rds"))

## Load data

library(dplyr)
library(broom)
library(purrr)
library(texreg)
library(stringr)




# Data cleaning ----

## Change factor levels ----

df_model <- df_model %>%
  mutate(
    price_pp   = as.numeric(price) / 100,                 # -0.20 … +0.20 (fraction)
    cost_new   = planed_cost * (1 + price_pp),            # SEK total under scenario
    cost_diff  = planed_cost * price_pp,                  # SEK delta from baseline (can be 0)
    # keep a numeric % version too if you want it
    price_pct  = as.numeric(price),                       # -20..20 (percent units)
    # set consistent factor baselines
    dist_trans = factor(dist_trans, levels = c("900","600","300")),
    dist_green = factor(dist_green, levels = c("15km", "5km", "500 meter")),
    dist_shops = factor(dist_shops, levels = c("15km", "5km", "500 meter")),
    proportion_of_income = planed_cost/income,
    age = floor(Age_T3),
    age_group = factor(case_when(
      age >= 55 & age < 65 ~ "55-64",
      age >= 65 & age < 75 ~ "65-74",
      age >= 75 ~ "75+",
    )),
    price_num = price_num/100
  ) %>% 
  mutate(
    ägandebostad = haven::as_factor(ägandebostad),
    bostadstyp = haven::as_factor(bostadstyp),
    Sex = haven::as_factor(Sex),
    Civil = haven::as_factor(civil_status_T2),
    civil_d = factor(if_else(civil_status_T2 == 1, "Partnered", "Not partnered")),
    Own = factor(if_else(ägandebostad == "Ja", "Owner", "Renter")),
    Hus = factor(if_else( bostadstyp == "Friliggande villa/hus/gård" | bostadstyp == "Radhus/kedjehus/parhus", "House", "Apartment/Condo")),
    downsize = if_else(planed_cost < monthcost, 1, 0),
    location = case_when(VAR010 == 1 ~ "City/town",
                         VAR010 == 2 | VAR010 == 2 ~ "Urban/countryside",
                         TRUE ~ NA )
    
  )



view(dfSummary(df_model %>% select(Sex,
                                   Age = Age_T3,
                                   age_group,
                                   Income = income,
                                   monthcost,
                                   planed_cost,
                                   income_qrt
                                   
) %>% mutate(Income = as.numeric(Income),
             monthcost= as.numeric(monthcost),
             planed_cost = as.numeric(planed_cost)),
plain.ascii  = FALSE, 
style        = "grid", 
graph.magnif = 0.82, 
varnumbers   = FALSE,
valid.col    = FALSE))

# Regressions -----

## Baseline regression - Sex ----

cost_name   <- "price_num"
set.seed(12345)  # or any fixed number

mnl <- logitr(
  data    = df_model %>% filter(Own == "Renter" ) ,
  outcome = "choice",
  obsID   = "obsID",
  panelID = "panelID",
  pars    = c("dist_green","dist_shops","dist_trans","parking","price_num")
)

mxl <- logitr(
  data    = df_model %>% filter(Own == "Renter") ,
  outcome = "choice",
  obsID   = "obsID",
  panelID = "panelID",
  pars    = c("dist_green", "dist_shops", "dist_trans", "parking", "price_num"),
  randPars = c(dist_green = 'n', dist_shops = 'n', dist_trans = 'n', parking = 'n'),
  numMultiStarts = 10,
  drawType = 'sobol',
  numDraws = 100,
  correlation = TRUE
)

screenreg(list(mnl,mxl))


library(broom)
library(dplyr)
library(stringr)
library(texreg)



# function

compute_bic <- function(model) {
  ll <- as.numeric(logLik(model))
  k  <- attr(logLik(model), "df")
  n  <- tryCatch(model$n$obs, error = function(e) NA)
  if (is.na(n)) return(NA)
  k * log(n) - 2 * ll
}



# 0) Tidy outputs
t_mnl <- tidy(mnl)
t_mxl <- tidy(mxl)

# 1) Choose the row order from your MNL (drop intercept & price)
attr_terms <- t_mnl %>%
  #filter(!(term %in% c("(Intercept)", "price_num"))) %>%
  pull(term)

# 2) MNL (means)
coef1 <- setNames(t_mnl$estimate[match(attr_terms, t_mnl$term)], attr_terms)
se1   <- setNames(t_mnl$std.error[match(attr_terms, t_mnl$term)], attr_terms)
p1    <- setNames(t_mnl$p.value[match(attr_terms, t_mnl$term)], attr_terms)

# 3) ML means: same attribute rows, exclude any 'sd_' terms
t_mxl_mean <- t_mxl %>% filter(term %in% attr_terms)
coef2 <- setNames(t_mxl_mean$estimate[match(attr_terms, t_mxl_mean$term)], attr_terms)
se2   <- setNames(t_mxl_mean$std.error[match(attr_terms, t_mxl_mean$term)], attr_terms)
p2    <- setNames(t_mxl_mean$p.value[match(attr_terms, t_mxl_mean$term)], attr_terms)

# 4) ML SDs (diagonal only): rows like 'sd_<attr>_<same attr>'
t_mxl_sd <- t_mxl %>% filter(str_starts(term, "sd_"))
# extract the attribute name on the diagonal using a backreference
diag_attr <- str_match(t_mxl_sd$term, "^sd_(.*)_\\1$")[, 2]
sd_diag   <- t_mxl_sd %>% mutate(attr = diag_attr) %>% filter(!is.na(attr))

# align SDs to attr_terms order
coef3 <- setNames(rep(NA_real_, length(attr_terms)), attr_terms)
se3   <- setNames(rep(NA_real_, length(attr_terms)), attr_terms)
p3    <- setNames(rep(NA_real_, length(attr_terms)), attr_terms)

midx <- match(attr_terms, sd_diag$attr)
ok   <- !is.na(midx)
coef3[ok] <- sd_diag$estimate[midx[ok]]
se3[ok]   <- sd_diag$std.error[midx[ok]]
p3[ok]    <- sd_diag$p.value[midx[ok]]

# 5) Build three texreg columns

# For MNL
mcfadden_mnl <- 1 - (logLik(mnl) / mnl$nullLogLik)

# For Mixed Logit (MXL)
mcfadden_mxl <- 1 - (logLik(mxl) / mxl$nullLogLik)


m1 <- createTexreg(
  coef.names = attr_terms,
  coef       = as.numeric(coef1),
  se         = as.numeric(se1),
  pvalues    = as.numeric(p1),
  gof.names  = c("Num. obs.", "Log Likelihood", "AIC", "BIC", "McFadden R²"),
  gof        = c(mnl$n$obs, as.numeric(logLik(mnl)), AIC(mnl), compute_bic(mnl), mcfadden_mnl),
  gof.decimal= c(FALSE, TRUE, TRUE, TRUE, TRUE)
)


# Try to pull number of observations safely
n_obs_mxl <- tryCatch({
  if (!is.null(mxl$model)) nrow(mxl$model)
  else if (!is.null(mxl$fitted.values)) length(mxl$fitted.values)
  else NA
}, error = function(e) NA)

# Log-likelihood, AIC, BIC
ll_mxl <- as.numeric(logLik(mxl))
aic_mxl <- AIC(mxl)
bic_mxl <- compute_bic(mxl)

# Log-likelihood, AIC, BIC
ll_mnl <- as.numeric(logLik(mnl))
aic_mnl <- AIC(mnl)
bic_mnl <- compute_bic(mnl)


# McFadden R²: if mxl$nullLogLik exists, use it; otherwise approximate
null_ll_mxl <- tryCatch({
  if (!is.null(mxl$nullLogLik)) mxl$nullLogLik
  else if (!is.null(mnl$nullLogLik)) mnl$nullLogLik  # fallback to MNL's null LL
  else NA
}, error = function(e) NA)

mcfadden_mxl <- if (!is.na(null_ll_mxl)) 1 - (ll_mxl / null_ll_mxl) else NA


# --- MNL LR test ---
ll_mnl <- as.numeric(logLik(mnl))
ll_null_mnl <- mnl$nullLogLik
k_mnl <- attr(logLik(mnl), "df")
lr_mnl <- -2 * (ll_null_mnl - ll_mnl)
p_lr_mnl <- pchisq(lr_mnl, df = k_mnl, lower.tail = FALSE)

# --- MXL LR test ---
ll_mxl <- as.numeric(logLik(mxl))
ll_null_mxl <- mxl$nullLogLik  # or fallback: ll_null_mxl <- ll_null_mnl
k_mxl <- attr(logLik(mxl), "df")
lr_mxl <- -2 * (ll_null_mxl - ll_mxl)
p_lr_mxl <- pchisq(lr_mxl, df = k_mxl, lower.tail = FALSE)

gof_names <- c("Num. obs.", "Log Likelihood", "AIC", "BIC", "McFadden R²",
               paste0("LR χ² (df=", k_mnl, ")"), "p-value (LR)")

gof_mnl <- c(mnl$n$obs, ll_mnl, AIC(mnl), bic_mnl, 1 - ll_mnl / ll_null_mnl, lr_mnl, p_lr_mnl)

gof_mxl <- c(mxl$n$obs, ll_mxl, AIC(mxl), bic_mxl, 1 - ll_mxl / ll_null_mxl, lr_mxl, p_lr_mxl)

gof_decimal <- c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)

m1 <- createTexreg(
  coef.names = attr_terms,
  coef = as.numeric(coef1),
  se = as.numeric(se1),
  pvalues = as.numeric(p1),
  gof.names = gof_names,
  gof = gof_mnl,
  gof.decimal = gof_decimal
)

m2 <- createTexreg(
  coef.names = attr_terms,
  coef = as.numeric(coef2),
  se = as.numeric(se2),
  pvalues = as.numeric(p2),
  gof.names = gof_names,
  gof = gof_mxl,
  gof.decimal = gof_decimal
)


m3 <- createTexreg(
  coef.names = attr_terms,
  coef       = as.numeric(coef3),
  se         = as.numeric(se3),
  pvalues    = as.numeric(p3)
)




# all attribute terms excluding cost
attr_terms_mrs <- setdiff(names(coef2), cost_name)

mrs_vals <- sapply(attr_terms_mrs, function(a) {
  - coef2[a] / coef2[cost_name]
})


vcov_means <- vcov(mxl)[attr_terms_mrs, attr_terms_mrs]

mrs_ses <- sapply(attr_terms_mrs, function(a) {
  beta_a <- coef2[a]
  beta_c <- coef2[cost_name]
  grad <- c(-1 / beta_c, beta_a / (beta_c^2))
  names(grad) <- c(a, cost_name)
  V_sub <- vcov(mxl)[c(a, cost_name), c(a, cost_name)]
  sqrt(t(grad) %*% V_sub %*% grad)
})

mrs_lower <- mrs_vals - 1.96 * mrs_ses
mrs_upper <- mrs_vals + 1.96 * mrs_ses


# Format for screenreg/texreg display
format_ci <- function(est, lo, hi, digits = 2) {
  est_txt <- formatC(est, digits = digits, format = "f")
  ci_txt  <- paste0("(", formatC(lo, digits = digits, format = "f"), ", ",
                    formatC(hi, digits = digits, format = "f"), ")")
  paste0(est_txt, "\n", ci_txt)
}

mrs_display <- mapply(format_ci, mrs_vals, mrs_lower, mrs_upper)

m4 <- createTexreg(
  coef.names = attr_terms_mrs,
  coef       = as.numeric(mrs_vals),
  ci.low     = as.numeric(mrs_lower),
  ci.up      = as.numeric(mrs_upper),
  # ensure no stars for this column:
  se         = rep(NA_real_, length(mrs_vals)),
  pvalues    = rep(NA_real_, length(mrs_vals))
)


median_cost <- 10500
scaler <- 0.10 * median_cost  # = 1000

mwtp_vals <- mrs_vals * scaler



m5 <- createTexreg(
  coef.names = attr_terms_mrs,
  coef = as.numeric(mwtp_vals),
  #se = rep(NA, length(mwtp_vals)),  # Caplan doesn’t show SEs here
  #pvalues = rep(NA, length(mwtp_vals))  # No stars in Caplan either
)



# Optional: pretty labels for rows
label_map <- c(
  "dist_green5km"                 = "Green space: 5 km (vs 15 km)",
  "dist_green500 meter"           = "Green space: 500 m (vs 15 km)",
  "dist_shops5km"                 = "Shops: 5 km (vs 15 km)",
  "dist_shops500 meter"           = "Shops: 500 m (vs 15 km)",
  "dist_trans600"                 = "Transit stop: 600 m (vs 900 m)",
  "dist_trans300"                 = "Transit stop: 300 m (vs 900 m)",
  "parkingreserverad garageplats" = "Parking: reserved garage (vs none)",
  "parkingreserverad P-plats"     = "Parking: reserved space (vs none)"
)



screenreg(
  list(m1, m2, m3, m4, m5),
  custom.header      = list("MXL" = 2:3),
  custom.model.names = c("MNL", "Mean", "SD", "MRS", "MWTP (SEK/mo)"),
  custom.coef.names  = c(
    "Green space: 5 km (vs 15 km)",
    "Green space: 500 m (vs 15 km)",
    "Shops: 5 km (vs 15 km)",
    "Shops: 500 m (vs 15 km)",
    "Transit stop: 600 m (vs 900 m)",
    "Transit stop: 300 m (vs 900 m)",
    "Parking: reserved garage (vs none)",
    "Parking: reserved space (vs none)",
    "Price"
  ),
  digits   = 2,
  stars    = c(0.001, 0.01, 0.05),
  booktabs = TRUE,
  dcolumn  = TRUE,
  use.packages = FALSE
)

texreg(
  list(m1, m2, m3, m4, m5),
  custom.header      = list("MXL" = 2:3),
  custom.model.names = c("MNL", "Mean", "SD", "MRS", "MWTP (SEK/mo)"),
  digits   = 2,
  custom.coef.names  = c(
    "Green space: 5 km (vs 15 km)",
    "Green space: 500 m (vs 15 km)",
    "Shops: 5 km (vs 15 km)",
    "Shops: 500 m (vs 15 km)",
    "Transit stop: 600 m (vs 900 m)",
    "Transit stop: 300 m (vs 900 m)",
    "Parking: reserved garage (vs none)",
    "Parking: reserved space (vs none)",
    "Price"
  ),
  stars    = c(0.001, 0.01, 0.05),
  booktabs = TRUE,
  dcolumn  = TRUE,
  use.packages = FALSE,
  na.replace = "--",
  caption = "Mixed Logit Estimates for men : Base Specification",
  caption.above = TRUE,
  fontsize = "scriptsize",
  file = here("docs/elsvier/tables","base_reg_renter.tex")
)

htmlreg(
  list(m1, m2, m3, m4, m5),
  custom.header      = list("MXL" = 2:3),
  custom.model.names = c("MNL", "Mean", "SD", "MRS", "MWTP (SEK/mo)"),
  custom.coef.names  = c(
    "Green space: 5 km (vs 15 km)",
    "Green space: 500 m (vs 15 km)",
    "Shops: 5 km (vs 15 km)",
    "Shops: 500 m (vs 15 km)",
    "Transit stop: 600 m (vs 900 m)",
    "Transit stop: 300 m (vs 900 m)",
    "Parking: reserved garage (vs none)",
    "Parking: reserved space (vs none)",
    "Price"
  ),
  digits      = 2,
  stars       = c(0.001, 0.01, 0.05),
  doctype     = TRUE,        # self-contained HTML file
  inline.css  = TRUE,        # embed CSS so Word keeps styling
  file        = here("docs/word_doc", "base_reg_renter.html"),
  custom.note = "*** p < 0.001; ** p < 0.01; * p < 0.05"
)















