library(pacman)

p_load(tidyverse, here, stringr, formr, logitr, cbcTools, texreg, likert, tidyr, haven, kableExtra,ggrepel, sjlabelled, summarytools,gtsummary,texreg,broom,purrr)





# load data ----

df_model <- readRDS(here("data/formr", "df_model.rds"))

## Load data


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

mxl_owner <- logitr(
  data    = df_model %>% filter(Own == "Owner") ,
  outcome = "choice",
  obsID   = "obsID",
  panelID = "panelID",
  pars    = c("dist_green", "dist_shops", "dist_trans", "parking", "price_num"),
  randPars = c(dist_green = 'n', dist_shops = 'n', dist_trans = 'n', parking = 'n'),
  numMultiStarts = 10,
  drawType = 'sobol',
  numDraws = 1000,
  correlation = TRUE
)

mxl_renter <- logitr(
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

screenreg(list(mxl_owner,mxl_renter))


saveRDS(mxl_renter, here("output/models", "mxl_renter_base.rds"))

saveRDS(mxl_owner, here("output/models", "mxl_owner_base.rds"))








## Create tables ----

mxl_renter <- readRDS(here("output/models", "mxl_renter_base.rds"))

mxl_owner<- readRDS(here("output/models", "mxl_owner_base.rds"))



















# function

compute_bic <- function(model) {
  ll <- as.numeric(logLik(model))
  k  <- attr(logLik(model), "df")
  n  <- tryCatch(model$n$obs, error = function(e) NA)
  if (is.na(n)) return(NA)
  k * log(n) - 2 * ll
}




# 0) Tidy outputs
t_renter <- tidy(mxl_renter)
t_owner  <- tidy(mxl_owner)

# MEANS = terms not beginning with sd_
means_renter <- t_renter %>% filter(!str_starts(term, "sd_"))
means_owner  <- t_owner  %>% filter(!str_starts(term, "sd_"))

# SDs = diagonal random parameter SD terms: sd_x_x
extract_diagonal <- function(df) {
  df %>%
    filter(str_starts(term, "sd_")) %>%
    mutate(attr = str_match(term, "^sd_(.*)_\\1$")[,2]) %>%
    filter(!is.na(attr))
}

sds_renter <- extract_diagonal(t_renter)
sds_owner  <- extract_diagonal(t_owner)

# Attribute order taken from renter means
attr_terms <- means_renter$term


# renter
coef_renter <- means_renter$estimate[match(attr_terms, means_renter$term)]
se_renter   <- means_renter$std.error[match(attr_terms, means_renter$term)]
p_renter    <- means_renter$p.value[match(attr_terms, means_renter$term)]

sd_renter   <- sds_renter$estimate[match(attr_terms, sds_renter$attr)]
sd_renter_se <- sds_renter$std.error[match(attr_terms, sds_renter$attr)]
sd_renter_p  <- sds_renter$p.value[match(attr_terms, sds_renter$attr)]

# owner
coef_owner <- means_owner$estimate[match(attr_terms, means_owner$term)]
se_owner   <- means_owner$std.error[match(attr_terms, means_owner$term)]
p_owner    <- means_owner$p.value[match(attr_terms, means_owner$term)]

sd_owner   <- sds_owner$estimate[match(attr_terms, sds_owner$attr)]
sd_owner_se <- sds_owner$std.error[match(attr_terms, sds_owner$attr)]
sd_owner_p  <- sds_owner$p.value[match(attr_terms, sds_owner$attr)]


# Compute LR test for renter model
ll_renter     <- as.numeric(logLik(mxl_renter))
ll_null_renter <- mxl_renter$nullLogLik
k_renter      <- attr(logLik(mxl_renter), "df")

lr_renter     <- -2 * (ll_null_renter - ll_renter)
p_lr_renter   <- pchisq(lr_renter, df = k_renter, lower.tail = FALSE)

# Compute LR test for owner model
ll_owner       <- as.numeric(logLik(mxl_owner))
ll_null_owner  <- mxl_owner$nullLogLik
k_owner        <- attr(logLik(mxl_owner), "df")

lr_owner       <- -2 * (ll_null_owner - ll_owner)
p_lr_owner     <- pchisq(lr_owner, df = k_owner, lower.tail = FALSE)










# Create texreg objects for each column

# RENTER MEAN (Model 1)
m_renter_mean <- createTexreg(
  coef.names = attr_terms,
  coef       = coef_renter,
  se         = se_renter,
  pvalues    = p_renter,
  gof.names  = c(
    "Num. obs.",
    "Log Likelihood",
    "AIC",
    "BIC",
    "McFadden R2",
    paste0("LR χ² (df=", k_renter, ")"),
    "p-value (LR)"
  ),
  gof = c(
    mxl_renter$n$obs,
    ll_renter,
    AIC(mxl_renter),
    compute_bic(mxl_renter),
    1 - (ll_renter / ll_null_renter),
    lr_renter,
    p_lr_renter
  ),
  gof.decimal = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
)


# OWNER MEAN (Model 3)
m_owner_mean <- createTexreg(
  coef.names = attr_terms,
  coef       = coef_owner,
  se         = se_owner,
  pvalues    = p_owner,
  gof.names  = c(
    "Num. obs.",
    "Log Likelihood",
    "AIC",
    "BIC",
    "McFadden R2",
    paste0("LR χ² (df=", k_owner, ")"),
    "p-value (LR)"
  ),
  gof = c(
    mxl_owner$n$obs,
    ll_owner,
    AIC(mxl_owner),
    compute_bic(mxl_owner),
    1 - (ll_owner / ll_null_owner),
    lr_owner,
    p_lr_owner
  ),
  gof.decimal = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
)

# OWNER SD
m_owner_sd <- createTexreg(
  coef.names = attr_terms,
  coef       = sd_owner,
  se         = sd_owner_se,
  pvalues    = sd_owner_p
)

m_renter_sd <- createTexreg(
  coef.names = attr_terms,
  coef       = sd_renter,
  se         = sd_renter_se,
  pvalues    = sd_renter_p
)






# Pretty labels
pretty_labels <- c(
  "dist_green5km"                 = "Green space: 5 km (vs 15 km)",
  "dist_green500 meter"           = "Green space: 500 m (vs 15 km)",
  "dist_shops5km"                 = "Shops: 5 km (vs 15 km)",
  "dist_shops500 meter"           = "Shops: 500 m (vs 15 km)",
  "dist_trans600"                 = "Transit stop: 600 m (vs 900 m)",
  "dist_trans300"                 = "Transit stop: 300 m (vs 900 m)",
  "parkingreserverad garageplats" = "Parking: reserved garage (vs none)",
  "parkingreserverad P-plats"     = "Parking: reserved space (vs none)",
  "price_num"                     = "Price"
)

# Produce final table ----









screenreg(
  list(m_renter_mean, m_renter_sd, m_owner_mean, m_owner_sd),
  custom.model.names = c("Coefficient", "SD", "Coefficient", "SD"),
  custom.coef.names  = pretty_labels[attr_terms],
  custom.header = list(
    "Renters" = 1:2,
    "Owners"  = 3:4
  ),
  stars    = c(0.001, 0.01, 0.05),
  booktabs = TRUE,
  dcolumn  = TRUE,
  use.packages = FALSE,
  caption = "Mixed Logit Results: Renters and Owners",
  caption.above = TRUE,
  fontsize = "scriptsize",
  na.replace = "--",
  #file = here("docs/elsvier/tables", "mxl_base_table.tex")
)





# MWTP table -------------


library(dplyr)
library(texreg)
library(stringr)

# Extract mean coefficients (no sd_ terms) ------

t_renter <- tidy(mxl_renter) %>% filter(!str_starts(term, "sd_"))
t_owner  <- tidy(mxl_owner)  %>% filter(!str_starts(term, "sd_"))

price_term <- "price_num"

# Attribute list for MWTP
attr_terms_mrs_renter <- setdiff(t_renter$term, price_term)
attr_terms_mrs_owner  <- setdiff(t_owner$term,  price_term)

# Scaling factor (your original)
median_cost_own <- 10000
median_cost_rent <- 9000
scaler_own <- 0.10 * median_cost_own   # = 1000
scaler_rent <- 0.10 * median_cost_rent   # = 1000

compute_mwtp_delta <- function(model, t_means, attributes, price_term, scale_value) {
  
  vc <- vcov(model)
  
  out <- lapply(attributes, function(a) {
    
    beta_a <- t_means$estimate[t_means$term == a]
    beta_p <- t_means$estimate[t_means$term == price_term]
    
    # MRS
    mrs <- - beta_a / beta_p
    
    # Gradient (exact same as your original code)
    grad <- c(-1 / beta_p, beta_a / (beta_p^2))
    names(grad) <- c(a, price_term)
    
    # Subset VCOV
    V_sub <- vc[c(a, price_term), c(a, price_term)]
    
    # Delta method SE
    se_mrs <- sqrt(t(grad) %*% V_sub %*% grad)
    
    # Convert to MWTP
    mwtp <- mrs * scale_value
    se_mwtp <- se_mrs * scale_value
    
    tibble(
      attribute = a,
      mwtp = as.numeric(mwtp),
      se   = as.numeric(se_mwtp),
      lower = mwtp - 1.96 * se_mwtp,
      upper = mwtp + 1.96 * se_mwtp
    )
  })
  
  bind_rows(out)
}


compute_mrs_delta <- function(model, t_means, attributes, price_term) {
  
  vc <- vcov(model)
  
  out <- lapply(attributes, function(a) {
    
    beta_a <- t_means$estimate[t_means$term == a]
    beta_p <- t_means$estimate[t_means$term == price_term]
    
    # MRS
    mrs <- - beta_a / beta_p
    
    # Gradient (same as MWTP version)
    grad <- c(-1 / beta_p, beta_a / (beta_p^2))
    names(grad) <- c(a, price_term)
    
    V_sub <- vc[c(a, price_term), c(a, price_term)]
    se_mrs <- sqrt(t(grad) %*% V_sub %*% grad)
    
    tibble(
      attribute = a,
      mrs       = as.numeric(mrs),
      se_mrs    = as.numeric(se_mrs),
      low_mrs   = mrs - 1.96 * se_mrs,
      up_mrs    = mrs + 1.96 * se_mrs
    )
  })
  
  bind_rows(out)
}



# Compute MWTP for each group
mwtp_renter <- compute_mwtp_delta(mxl_renter, t_renter, attr_terms_mrs_renter, price_term, scaler_rent)
mwtp_owner  <- compute_mwtp_delta(mxl_owner,  t_owner,  attr_terms_mrs_owner,  price_term, scaler_own)
mrs_renter <- compute_mrs_delta(mxl_renter, t_renter, attr_terms_mrs_renter, price_term)
mrs_owner  <- compute_mrs_delta(mxl_owner,  t_owner,  attr_terms_mrs_owner,  price_term)


mwtp_table <- full_join(
  mwtp_renter %>% rename(
    mwtp_rent = mwtp, se_rent = se,
    low_rent = lower, up_rent = upper
  ),
  mwtp_owner %>% rename(
    mwtp_own = mwtp, se_own = se,
    low_own = lower, up_own = upper
  ),
  by = "attribute"
)

mwtp_full <- mwtp_table %>%
  left_join(mrs_renter %>% rename(
    mrs_r = mrs, low_mrs_r = low_mrs, up_mrs_r = up_mrs
  ), by = "attribute") %>%
  left_join(mrs_owner %>% rename(
    mrs_o = mrs, low_mrs_o = low_mrs, up_mrs_o = up_mrs
  ), by = "attribute")


fmt <- function(est, lo, hi, digits = 2) {
  sprintf("%.2f\n(%.2f, %.2f)", est, lo, hi)
}

mwtp_full <- mwtp_full %>%
  mutate(
    MRS_r_print = fmt(mrs_r, low_mrs_r, up_mrs_r),
    MRS_o_print = fmt(mrs_o, low_mrs_o, up_mrs_o),
    MWTP_r = round(mwtp_rent, 0),
    MWTP_o = round(mwtp_own, 0)
  )


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

mwtp_full$label <- label_map[mwtp_full$attribute]

# 

mwtp_table_out <- mwtp_full %>%
  select(
    Attribute = label       ,
    Renter_MRS = MRS_r_print,
    Renter_MWTP = MWTP_r,
    Owner_MRS = MRS_o_print,
    Owner_MWTP = MWTP_o
  )


# mwtp_table_out <- mwtp_table %>% 
#   select(
#     Attribute = attribute, 
#     MWTP_r = mwtp_rent,
#     `5%_r` = low_rent,
#     `95%_r` = up_rent,
#     MWTP_o = mwtp_own,
#     `5%_o` = low_own,
#     `95%_o` = up_own
#   ) %>%
#   mutate(across(where(is.numeric), ~ round(., 0)))

kbl(
  mwtp_table_out,
  booktabs = TRUE,
  caption = "MWTP Estimates (Renters and Owners, SEK/month)",
  col.names = c(
    "Attribute",
    "MRS", "MWTP",
    "MRS",
    "MWTP"
  )
) %>%
  kable_classic(full_width = FALSE) %>%
  add_header_above(c(
    " " = 1,
    "Renters" = 2,
    "Owners"  = 2
  )) %>%
  add_header_above(c(
    " " = 1,
    " " = 4  # no second-level header here
  )) 


mwtp_latex <- kbl(
  mwtp_table_out,
  booktabs = TRUE,
  caption = "MWTP Estimates (Renters and Owners, SEK/month)",
  col.names = c(
    "Attribute",
    "MRS", "MWTP",
    "MRS",
    "MWTP"
  ),
  format = "latex",
  escape = FALSE
) %>%
  kable_classic(full_width = FALSE, latex_options = "hold_position") %>%
  add_header_above(c(
    " " = 1,
    "Renters" = 2,
    "Owners"  = 2
  )) %>%
  footnote(
    general = "MWTP values computed using mixed logit mean coefficients and delta method. Values in SEK/month.",
    general_title = "",
    threeparttable = TRUE
  )

# Save to file
save_kable(mwtp_latex, here("docs/elsvier/tables", "mwtp_base.tex"))


# -------------


m_mwtp_rent <- createTexreg(
  coef.names = mwtp_table$label,
  coef = mwtp_table$mwtp_rent,
  se   = mwtp_table$se_rent,
  ci.low = mwtp_table$low_rent,
  ci.up  = mwtp_table$up_rent
)

m_mwtp_own <- createTexreg(
  coef.names = mwtp_table$label,
  coef = mwtp_table$mwtp_own,
  se   = mwtp_table$se_own,
  ci.low = mwtp_table$low_own,
  ci.up  = mwtp_table$up_own
)

texreg(
  list(m_mwtp_rent, m_mwtp_own),
  custom.model.names = c("Renters", "Owners"),
  caption = "MWTP Estimates (SEK/month)",
  caption.above = TRUE,
  digits = 1,
  booktabs = TRUE,
  dcolumn = TRUE,
  file = here("docs/elsvier/tables","mwtp_renter_owner_table.tex")
)




















# 1) Choose the row order from your MNL (drop intercept & price)
attr_terms <- t_mxl_renter %>%
  #filter(!(term %in% c("(Intercept)", "price_num"))) %>%
  pull(term)




# 2) MNL (means)
coef1 <- setNames(t_mnl$estimate[match(attr_terms, t_mnl$term)], attr_terms)
se1   <- setNames(t_mnl$std.error[match(attr_terms, t_mnl$term)], attr_terms)
p1    <- setNames(t_mnl$p.value[match(attr_terms, t_mnl$term)], attr_terms)

# 3) ML means: same attribute rows, exclude any 'sd_' terms
t_mxl_mean <- t_mxl_renter %>% filter(term %in% attr_terms)
coef2 <- setNames(t_mxl_mean$estimate[match(attr_terms, t_mxl_mean$term)], attr_terms)
se2   <- setNames(t_mxl_mean$std.error[match(attr_terms, t_mxl_mean$term)], attr_terms)
p2    <- setNames(t_mxl_mean$p.value[match(attr_terms, t_mxl_mean$term)], attr_terms)

# 4) ML SDs (diagonal only): rows like 'sd_<attr>_<same attr>'
t_mxl_sd <- t_mxl_renter %>% filter(str_starts(term, "sd_"))
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


# For Mixed Logit (MXL)
mcfadden_mxl_renter <- 1 - (logLik(mxl_renter) / mxl_renter$nullLogLik)
mcfadden_mxl_owner <- 1 - (logLik(mxl_owner) / mxl_owner$nullLogLik)



# Try to pull number of observations safely
n_obs_mxl_renter <- tryCatch({
  if (!is.null(mxl_renter$model)) nrow(mxl_renter$model)
  else if (!is.null(mxl_renter$fitted.values)) length(mxl_renter$fitted.values)
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
  list( m2, m3, m4, m5),
  #custom.header      = list("MXL" = 2:3),
  custom.model.names = c( "Mean", "SD", "MRS", "MWTP (SEK/mo)"),
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
  list( m2, m3, m4, m5),
  #custom.header      = list("MXL" = 2:3),
  custom.model.names = c( "MXL - $$∖mu$$", "MXL - SD", "MRS", "MWTP (SEK/mo)"),
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
  caption = "Base Specification - Renters",
  caption.above = TRUE,
  fontsize = "scriptsize",
  file = here("docs/elsvier/tables","base_reg_owner.tex")
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















