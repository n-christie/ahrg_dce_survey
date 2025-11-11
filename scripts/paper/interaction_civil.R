

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

cost_name   <- "price_num"
set.seed(12345)  # or any fixed number

# Data cleaning ----

## Change factor levels ----

# -- Variable transformations, factor baselines, age groups, and derived vars --

df_model <- df_model %>%
  mutate(
    price_pp   = as.numeric(price) / 100,
    cost_new   = planed_cost * (1 + price_pp),
    cost_diff  = planed_cost * price_pp,
    price_pct  = as.numeric(price),
    proportion_of_income = planed_cost / income,
    age = floor(Age_T3),
    age_group = factor(case_when(
      age >= 55 & age < 65 ~ "55-64",
      age >= 65 & age < 75 ~ "65-74",
      age >= 75 ~ "75+"
    )),
    price_num = price_num / 100,
    
    # Set reference levels
    dist_trans = factor(dist_trans, levels = c("900", "600", "300")),
    dist_green = factor(dist_green, levels = c("15km", "5km", "500 meter")),
    dist_shops = factor(dist_shops, levels = c("15km", "5km", "500 meter"))
  ) %>%
  mutate(
    # Demographics
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
    downsize = if_else(planed_cost < monthcost, 1, 0),
    Retired = haven::as_factor(VAR174_8),  ) %>%
  # Age group dummies
  mutate(
    G65_74 = as.integer(age_group == "65-74"),
    G75p   = as.integer(age_group == "75+"),
    income = if_else(income == 0, NA, income),
    Sex = factor(if_else(Sex == "Man", "Men","Women")),
    Men = if_else(Sex == "Men", 1,0),
    Health = case_when( VAR035 %in% c(1,2,3) ~ "Not very good",
                        VAR035 %in% c(4,5) ~ "Very good",
                        TRUE ~ NA_character_),
    Partnered = if_else(civil_d == "Partnered", 1, 0)
  ) 


df_dummies <- model.matrix(~ dist_green + dist_shops + dist_trans + parking - 1, data = df_model) %>%
  as_tibble()

df_model <- bind_cols(df_model, df_dummies)

df_model <- df_model %>%
  rename(
    dist_green5km   = `dist_green5km`,
    dist_green500m  = `dist_green500 meter`,
    dist_shops5km   = `dist_shops5km`,
    dist_shops500m  = `dist_shops500 meter`,
    dist_trans600   = `dist_trans600`,
    dist_trans300   = `dist_trans300`,
    park_space      = `parkingreserverad P-plats`,
    park_garage     = `parkingreserverad garageplats`
  )

df_model <- df_model %>%
  mutate(
    green5km_Partnered   = dist_green5km  * Partnered,
    green500_Partnered   = dist_green500m * Partnered,
    shops5km_Partnered   = dist_shops5km  * Partnered,
    shops500_Partnered   = dist_shops500m * Partnered,
    trans600_Partnered   = dist_trans600  * Partnered,
    trans300_Partnered   = dist_trans300  * Partnered,
    park_space_Partnered = park_space     * Partnered,
    park_garage_Partnered= park_garage    * Partnered,
    price_Partnered      = price_num      * Partnered
  )

# --- Define main and interaction parameter names again ---
attr_vars <- c(
  "dist_green5km", "dist_green500m",
  "dist_shops5km", "dist_shops500m",
  "dist_trans600", "dist_trans300",
  "park_space", "park_garage",
  "price_num"
)

inter_vars <- c(
  "green5km_Partnered", "green500_Partnered",
  "shops5km_Partnered", "shops500_Partnered",
  "trans600_Partnered", "trans300_Partnered",
  "park_space_Partnered", "park_garage_Partnered",
  "price_Partnered"
)


## Create renter/owner subsets ----

df_owner <- df_model %>% 
  filter(Own == "Owner" )


df_renter <- df_model %>% 
  filter(Own == "Renter")


mxl_isex_ret <- logitr(
  data = as.data.frame(df_renter ),
  outcome = "choice",
  obsID = "obsID",
  panelID = "panelID",
  pars = c(
    "dist_green5km", "dist_green500m",
    "dist_shops5km", "dist_shops500m",
    "dist_trans600", "dist_trans300",
    "park_space", "park_garage",
    "price_num",
    "green5km_Partnered", "green500_Partnered",
    "shops5km_Partnered", "shops500_Partnered",
    "trans600_Partnered", "trans300_Partnered",
    "park_space_Partnered", "park_garage_Partnered",
    "price_Partnered"
  ),
  randPars = c(
    dist_green5km   = "n",
    dist_green500m  = "n",
    dist_shops5km   = "n",
    dist_shops500m  = "n",
    dist_trans600   = "n",
    dist_trans300   = "n",
    park_space      = "n",
    park_garage     = "n"
  ),
  numDraws = 100,
  numMultiStarts = 10,
  drawType = "sobol",
  correlation = TRUE
)

screenreg(mxl_isex_ret)

saveRDS(mxl_isex_ret, here("output/models", "mxl_Partnered_renter.rds"))


mxl_isex_own <- logitr(
  data = as.data.frame(df_owner ),
  outcome = "choice",
  obsID = "obsID",
  panelID = "panelID",
  pars = c(
    "dist_green5km", "dist_green500m",
    "dist_shops5km", "dist_shops500m",
    "dist_trans600", "dist_trans300",
    "park_space", "park_garage",
    "price_num",
    "green5km_Partnered", "green500_Partnered",
    "shops5km_Partnered", "shops500_Partnered",
    "trans600_Partnered", "trans300_Partnered",
    "park_space_Partnered", "park_garage_Partnered",
    "price_Partnered"
  ),
  randPars = c(
    dist_green5km   = "n",
    dist_green500m  = "n",
    dist_shops5km   = "n",
    dist_shops500m  = "n",
    dist_trans600   = "n",
    dist_trans300   = "n",
    park_space      = "n",
    park_garage     = "n"
  ),
  numDraws = 100,
  numMultiStarts = 10,
  drawType = "sobol",
  correlation = TRUE
)

screenreg(mxl_isex_own)

saveRDS(mxl_isex_own, here("output/models", "mxl_Partnered_owner.rds"))

# Make regression table -----

mxl_Partnered_own<- readRDS(here("output/models", "mxl_Partnered_owner.rds"))
mxl_Partnered_ren <- readRDS(here("output/models", "mxl_Partnered_renter.rds"))


screenreg(list(mxl_Partnered_own, mxl_Partnered_ren))

# --- Settings ---
cost_name   <- "price_num"
median_cost <- 10500
scaler      <- 0.10 * median_cost  # converts MRS to SEK/month

# --- Label map for texreg output ---
label_map <- c(
  "dist_green5km"   = "Green space: 5 km (vs 15 km)",
  "dist_green500m"  = "Green space: 500 m (vs 15 km)",
  "dist_shops5km"   = "Shops: 5 km (vs 15 km)",
  "dist_shops500m"  = "Shops: 500 m (vs 15 km)",
  "dist_trans600"   = "Transit stop: 600 m (vs 900 m)",
  "dist_trans300"   = "Transit stop: 300 m (vs 900 m)",
  "park_garage"     = "Parking: reserved garage (vs none)",
  "park_space"      = "Parking: reserved space (vs none)",
  "price_num"       = "Price"
)


interaction_to_base <- c(
  "green5km_Partnered"   = "dist_green5km",
  "green500_Partnered"   = "dist_green500m",
  "shops5km_Partnered"   = "dist_shops5km",
  "shops500_Partnered"   = "dist_shops500m",
  "trans600_Partnered"   = "dist_trans600",
  "trans300_Partnered"   = "dist_trans300",
  "park_space_Partnered" = "park_space",
  "park_garage_Partnered"= "park_garage",
  "price_Partnered"      = "price_num"
)

pretty_interaction <- function(term, label_map) {
  base <- interaction_to_base[[term]]
  paste0(label_map[[base]], " × Partnered")
}

main_attrs <- c("dist_green5km","dist_green500m",
                "dist_shops5km","dist_shops500m",
                "dist_trans600","dist_trans300",
                "park_garage","park_space")

inter_terms_inc <- c("green5km_Partnered","green500_Partnered",
                     "shops5km_Partnered","shops500_Partnered",
                     "trans600_Partnered","trans300_Partnered",
                     "park_garage_Partnered","park_space_Partnered",
                     "price_Partnered")

coef_rows <- c(main_attrs, "price_num", inter_terms_inc)

pretty_labels <- c(
  unname(label_map[main_attrs]),
  label_map[["price_num"]],
  sapply(inter_terms_inc, pretty_interaction, label_map = label_map)
)


# --- Helper functions -----------------------------------------------------

mrs_for_attr_group <- function(attr, group, tidy_df, V, cost_name) {
  beta_a   <- grab_term(tidy_df, attr)[1]
  beta_c   <- grab_term(tidy_df, cost_name)[1]
  a_g_term <- if (group == "base") NA_character_
  else names(interaction_to_base)[interaction_to_base == attr & grepl(group, names(interaction_to_base))]
  c_g_term <- if (group == "base") NA_character_
  else names(interaction_to_base)[interaction_to_base == cost_name & grepl(group, names(interaction_to_base))]
  beta_a_g <- if (length(a_g_term)) grab_term(tidy_df, a_g_term)[1] else 0
  beta_c_g <- if (length(c_g_term)) grab_term(tidy_df, c_g_term)[1] else 0
  
  if (any(is.na(c(beta_a, beta_c)))) return(c(NA_real_, NA_real_, NA_real_))
  
  A <- beta_a + ifelse(group == "base", 0, beta_a_g)
  C <- beta_c + ifelse(group == "base", 0, beta_c_g)
  val <- -(A / C)  # MRS (unitless)
  
  terms <- c(attr, cost_name,
             if (group != "base") a_g_term else NULL,
             if (group != "base") c_g_term else NULL)
  terms <- terms[!is.na(terms) & nzchar(terms)]
  if (is.null(V) || !all(terms %in% rownames(V))) return(c(val, NA_real_, NA_real_))
  
  grad <- numeric(length(terms))
  grad[terms == attr]      <- -1 / C
  grad[terms == cost_name] <-  A / (C^2)
  if (group != "base") {
    if (any(terms == a_g_term)) grad[terms == a_g_term] <- -1 / C
    if (any(terms == c_g_term)) grad[terms == c_g_term] <-  A / (C^2)
  }
  
  Vsub <- V[terms, terms, drop = FALSE]
  var  <- as.numeric(t(grad) %*% Vsub %*% grad)
  if (!is.finite(var) || var < 0) return(c(val, NA_real_, NA_real_))
  se   <- sqrt(var)
  pval <- if (se == 0 || !is.finite(se)) NA_real_ else 2 * pnorm(-abs(val / se))
  c(val, se, pval)
}


grab_term <- function(tidy_df, term) {
  row <- tidy_df[tidy_df$term == term, ]
  if (nrow(row) == 0) return(c(NA_real_, NA_real_, NA_real_))
  c(row$estimate[1], row$std.error[1], row$p.value[1])
}

term_group <- function(term) {
  if (term %in% main_attrs) return("base")
  if (grepl("Partnered$", term)) return("Partnered")
  if (term %in% c("price_num")) return("price")
  if (grepl("^price_", term)) return("price_inter")
  "other"
}

term_attr <- function(term) {
  if (term %in% main_attrs) return(term)
  if (term %in% names(interaction_to_base)) return(interaction_to_base[[term]])
  NA_character_
}

mwtp_for_attr_group <- function(attr, group, tidy_df, V, scaler, cost_name) {
  beta_a   <- grab_term(tidy_df, attr)[1]
  beta_c   <- grab_term(tidy_df, cost_name)[1]
  a_g_term <- if (group == "base") NA_character_
  else names(interaction_to_base)[interaction_to_base == attr & grepl(group, names(interaction_to_base))]
  c_g_term <- if (group == "base") NA_character_
  else names(interaction_to_base)[interaction_to_base == cost_name & grepl(group, names(interaction_to_base))]
  beta_a_g <- if (length(a_g_term)) grab_term(tidy_df, a_g_term)[1] else 0
  beta_c_g <- if (length(c_g_term)) grab_term(tidy_df, c_g_term)[1] else 0
  
  if (any(is.na(c(beta_a, beta_c)))) return(c(NA_real_, NA_real_, NA_real_))
  
  A <- beta_a + ifelse(group == "base", 0, beta_a_g)
  C <- beta_c + ifelse(group == "base", 0, beta_c_g)
  val <- -(A / C) * scaler
  
  terms <- c(attr, cost_name,
             if (group != "base") a_g_term else NULL,
             if (group != "base") c_g_term else NULL)
  terms <- terms[!is.na(terms) & nzchar(terms)]
  if (is.null(V) || !all(terms %in% rownames(V))) return(c(val, NA_real_, NA_real_))
  
  grad <- numeric(length(terms))
  grad[terms == attr]       <- -1 / C * scaler
  grad[terms == cost_name]  <- (A / (C^2)) * scaler
  if (group != "base") {
    if (any(terms == a_g_term)) grad[terms == a_g_term] <- -1 / C * scaler
    if (any(terms == c_g_term)) grad[terms == c_g_term] <- (A / (C^2)) * scaler
  }
  
  Vsub <- V[terms, terms, drop = FALSE]
  var  <- as.numeric(t(grad) %*% Vsub %*% grad)
  if (!is.finite(var) || var < 0) return(c(val, NA_real_, NA_real_))
  se   <- sqrt(var)
  pval <- if (se == 0 || !is.finite(se)) NA_real_ else 2 * pnorm(-abs(val / se))
  c(val, se, pval)
}

compute_bic <- function(model) {
  ll <- as.numeric(logLik(model))
  k  <- attr(logLik(model), "df")
  n  <- tryCatch(model$n$obs, error = function(e) NA)
  if (is.na(n)) return(NA_real_)
  k * log(n) - 2 * ll
}

safe_null_ll <- function(model) {
  tryCatch(if (!is.null(model$nullLogLik)) model$nullLogLik else NA_real_, error = function(e) NA_real_)
}

lr_bits <- function(model) {
  ll  <- as.numeric(logLik(model))
  ll0 <- safe_null_ll(model)
  k   <- attr(logLik(model), "df")
  if (is.na(ll0)) return(list(lr = NA_real_, p = NA_real_, k = k))
  lr <- -2 * (ll0 - ll)
  p  <- pchisq(lr, df = k, lower.tail = FALSE)
  list(lr = lr, p = p, k = k)
}

build_pair_compact <- function(model, label_map, scaler, cost_name) {
  tdf <- broom::tidy(model)
  V   <- tryCatch(vcov(model), error = function(e) NULL)
  
  # Collect Coef/SE/p for all rows you want to display
  coef_vals <- setNames(rep(NA_real_, length(coef_rows)), coef_rows)
  se_vals   <- coef_vals
  p_vals    <- coef_vals
  for (term in coef_rows) {
    x <- grab_term(tdf, term)
    coef_vals[term] <- x[1]; se_vals[term] <- x[2]; p_vals[term] <- x[3]
  }
  
  # Compute MRS (unitless) + SE and MWTP + SE for each display row
  mrs_vals <- setNames(rep(NA_real_, length(coef_rows)), coef_rows)
  mrs_se   <- mrs_vals
  mrs_p    <- mrs_vals
  mwtp_vals <- mrs_vals
  mwtp_se   <- mrs_vals
  
  for (term in coef_rows) {
    g <- term_group(term)
    if (g %in% c("price","price_inter","other")) next
    a <- term_attr(term)
    
    # MRS
    mrs_out <- mrs_for_attr_group(a, ifelse(g == "base","base", g), tdf, V, cost_name)
    mrs_vals[term] <- mrs_out[1]; mrs_se[term] <- mrs_out[2]; mrs_p[term] <- mrs_out[3]
    
    # MWTP
    mwtp_out <- mwtp_for_attr_group(a, ifelse(g == "base","base", g), tdf, V, scaler, cost_name)
    mwtp_vals[term] <- mwtp_out[1]; mwtp_se[term] <- mwtp_out[2]
  }
  
  # Fit stats
  ll   <- as.numeric(logLik(model))
  aic  <- AIC(model)
  bic  <- compute_bic(model)
  nobs <- tryCatch(model$n$obs, error = function(e) NA_integer_)
  ll0  <- safe_null_ll(model)
  mcf  <- if (!is.na(ll0)) 1 - (ll / ll0) else NA_real_
  lr   <- lr_bits(model)
  
  gof_names <- c("Num. obs.", "Log Likelihood", "AIC", "BIC", "McFadden R²",
                 paste0("LR χ² (df=", lr$k, ")"), "p-value (LR)")
  gof_vals  <- c(nobs, ll, aic, bic, mcf, lr$lr, lr$p)
  gof_dec   <- c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  
  # ---- Columns to return
  # 1) Coef: SE + stars
  m_coef <- createTexreg(
    coef.names  = pretty_labels,
    coef        = as.numeric(coef_vals),
    se          = as.numeric(se_vals),
    pvalues     = as.numeric(p_vals),
    gof.names   = gof_names,
    gof         = gof_vals,
    gof.decimal = gof_dec
  )
  
  # 2) MRS: show 95% CI, no stars
  mrs_low <- mrs_vals - 1.96 * mrs_se
  mrs_up  <- mrs_vals + 1.96 * mrs_se
  m_mrs <- createTexreg(
    coef.names = pretty_labels,
    coef       = as.numeric(mrs_vals),
    ci.low     = as.numeric(mrs_low),
    ci.up      = as.numeric(mrs_up),
    se         = rep(NA_real_, length(mrs_vals)),
    pvalues    = rep(NA_real_, length(mrs_vals))
  )
  
  # 3) MWTP: SE only, no stars
  # 3) MWTP: show only point estimates (no SE, no stars)
  m_mwtp <- createTexreg(
    coef.names = pretty_labels,
    coef       = as.numeric(mwtp_vals)
  )
  
  
  list(m_coef = m_coef, m_mrs = m_mrs, m_mwtp = m_mwtp)
}





ret  <- build_pair_compact(mxl_Partnered_own,  label_map, scaler, cost_name)
notr <- build_pair_compact(mxl_Partnered_ren, label_map, scaler, cost_name)

custom_header      <- list("Owner" = 1:3, "Renter" = 4:6)
custom_model_names <- c("Coef.", "MRS", "MWTP", "Coef.", "MRS", "MWTP")



screenreg(
  list(ret$m_coef, ret$m_mrs, ret$m_mwtp,
       notr$m_coef, notr$m_mrs, notr$m_mwtp),
  custom.header      = custom_header,
  custom.model.names = custom_model_names,
  custom.coef.names  = pretty_labels,
  digits             = 2,
  stars              = c(0.001, 0.01, 0.05),  # only Coef. has p-values, so only there we see stars
  booktabs           = TRUE,
  dcolumn            = TRUE,
  use.packages       = FALSE,
  na.replace         = "--",
  caption            = "Mixed Logit (Partnered vs Not Partnered): Coefficients, MRS (95% CI), and MWTP",
  caption.above      = TRUE,
  fontsize           = "scriptsize"
)

texreg(
  list(ret$m_coef, ret$m_mrs, ret$m_mwtp,
       notr$m_coef, notr$m_mrs, notr$m_mwtp),
  custom.header      = custom_header,
  custom.model.names = custom_model_names,
  custom.coef.names  = pretty_labels,
  digits             = 2,
  stars              = c(0.001, 0.01, 0.05),  # only Coef. has p-values, so only there we see stars
  booktabs           = TRUE,
  dcolumn            = TRUE,
  use.packages       = FALSE,
  na.replace         = "--",
  caption            = "Interaction effects - Civil status",
  caption.above      = TRUE,
  fontsize           = "scriptsize",
  file               = here("docs/elsvier/tables", "mxl_partnered_inter.tex")
  
)
































# Plot coeffceints ----

# --- Packages ---
library(broom)
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(here)

# --- Inputs ---
mhc_list <- list("Low" = 10500, "High" = 10500)  # scale for both groups

# --- Attribute map ---
attr_map <- tibble(
  display = c("Green space: 15km", "Green space: 5km", "Green space: 500m",
              "Shops: 15km", "Shops: 5km", "Shops: 500m",
              "Transit: 900m", "Transit: 600m", "Transit: 300m",
              "Parking: None", "Parking: reserved space", "Parking: reserved garage"),
  base     = c(NA, "dist_green5km", "dist_green500m",
               NA, "dist_shops5km", "dist_shops500m",
               NA, "dist_trans600", "dist_trans300",
               NA, "park_space", "park_garage"),
  Good  = c(NA, "green5km_Partnered", "green500_Partnered",
            NA, "shops5km_Partnered", "shops500_Partnered",
            NA, "trans600_Partnered", "trans300_Partnered",
            NA, "park_space_Partnered", "park_garage_Partnered")
)

# --- Function to compute WTP per attribute and income group ---------------
compute_wtp_df <- function(model, group_label) {
  coef_df <- tidy(model) %>%
    mutate(conf.low = estimate - 1.96 * std.error,
           conf.high = estimate + 1.96 * std.error)
  coefs <- setNames(coef_df$estimate, coef_df$term)
  
  get_wtp <- function(base, interaction = NULL, group = "Low") {
    if (!(base %in% names(coefs))) {
      return(tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_))
    }
    
    est <- coefs[[base]]
    se  <- coef_df %>% filter(term == base) %>% pull(std.error)
    
    if (!is.null(interaction) && !is.na(interaction) && interaction %in% names(coefs)) {
      est <- est + coefs[[interaction]]
      se_int <- coef_df %>% filter(term == interaction) %>% pull(std.error)
      se <- sqrt(se^2 + se_int^2)
      denom <- coefs[["price_num"]] + coefs[["price_HighInc"]]
    } else {
      denom <- coefs[["price_num"]]
    }
    
    wtp <- -est / denom
    wtp_sek <- wtp * 0.10 * mhc_list[[group]]
    se_wtp <- (1.96 * se / abs(denom)) * 0.10 * mhc_list[[group]]
    
    tibble(estimate = wtp_sek,
           conf.low = wtp_sek - se_wtp,
           conf.high = wtp_sek + se_wtp)
  }
  
  expand.grid(
    attribute = attr_map$display,
    income_group = c("Low", "High"),
    stringsAsFactors = FALSE
  ) %>%
    left_join(attr_map, by = c("attribute" = "display")) %>%
    rowwise() %>%
    mutate(
      interaction_term = if_else(income_group == "High", HighInc, NA_character_),
      wtp_info = list(get_wtp(base, interaction_term, income_group))
    ) %>%
    unnest(wtp_info) %>%
    mutate(model_group = group_label)
}

# --- Compute for both models ---
plot_df_ret  <- compute_wtp_df(mxl_income_ret,  "Retired")
plot_df_notr <- compute_wtp_df(mxl_income_noret, "Not retired")
plot_df <- bind_rows(plot_df_ret, plot_df_notr)

# --- Clean up labels & remove reference rows ---
plot_df <- plot_df %>%
  mutate(
    attribute = case_when(
      grepl("garage", attribute, ignore.case = TRUE) ~ "Parking: garage (ref: none)",
      grepl("reserved", attribute, ignore.case = TRUE) ~ "Parking: reserved (ref: none)",
      grepl("300m", attribute, ignore.case = TRUE) ~ "Transit stop: 300 m (ref: 900 m)",
      grepl("600m", attribute, ignore.case = TRUE) ~ "Transit stop: 600 m (ref: 900 m)",
      grepl("500m", attribute, ignore.case = TRUE) & grepl("shop", attribute, ignore.case = TRUE) ~ "Shops: 500 m (ref: 15 km)",
      grepl("5km", attribute, ignore.case = TRUE) & grepl("shop", attribute, ignore.case = TRUE) ~ "Shops: 5 km (ref: 15 km)",
      grepl("500m", attribute, ignore.case = TRUE) & grepl("green", attribute, ignore.case = TRUE) ~ "Green space: 500 m (ref: 15 km)",
      grepl("5km", attribute, ignore.case = TRUE) & grepl("green", attribute, ignore.case = TRUE) ~ "Green space: 5 km (ref: 15 km)",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(attribute))

# --- Factor order for attributes ---
attribute_levels <- c(
  "Parking: garage (ref: none)",
  "Parking: reserved (ref: none)",
  "Transit stop: 300 m (ref: 900 m)",
  "Transit stop: 600 m (ref: 900 m)",
  "Shops: 500 m (ref: 15 km)",
  "Shops: 5 km (ref: 15 km)",
  "Green space: 500 m (ref: 15 km)",
  "Green space: 5 km (ref: 15 km)"
)
plot_df$attribute <- factor(plot_df$attribute, levels = rev(attribute_levels))

# --- Shared color palette ---
income_colors <- c("Low" = "#1b9e77", "High" = "#d95f02")

# --- Plot function ---
plot_wtp <- function(df, title) {
  ggplot(df, aes(x = estimate, y = attribute, color = income_group)) +
    geom_point(position = position_dodge(width = 0.6), size = 3) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   position = position_dodge(width = 0.6),
                   height = 0.4, linewidth = 1) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
    scale_color_manual(values = income_colors) +
    scale_x_continuous(
      limits = c(-200, 1200),
      breaks = seq(-200, 1200, by = 200)
    ) +
    labs(x = "WTP (SEK/month)", y = NULL,
         color = "Income group", title = title) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.y = element_text(size = 15),
      axis.title.x = element_text(size = 13),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 13),
      panel.grid.minor = element_blank()
    )
}

# --- Create plots ---
p_ret  <- plot_wtp(filter(plot_df, model_group == "Retired"),  "Retired")
p_notr <- plot_wtp(filter(plot_df, model_group == "Not retired"), "Not retired")

# --- Combine with shared legend ---
combined_plot <- p_ret + p_notr + plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")

# --- Display and save ---
combined_plot

ggsave(
  here("docs/elsvier/figures", "wtp_income_interactions_ret_notret_publication.png"),
  plot = combined_plot,
  width = 13,
  height = 12,
  units = "in",
  dpi = 600
)






