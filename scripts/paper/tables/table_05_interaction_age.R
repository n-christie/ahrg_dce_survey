# table_05_interaction_age.R
# Table 5: Mixed logit interaction regression — age (Old = above median age 72, vs younger).
# Uses mxl_old_owner.rds and mxl_old_renter.rds, both estimated with an "Old" binary dummy
# (Old = 1 if age >= 72, i.e. at or above the sample median age).
# Loads pre-saved models; does NOT re-estimate.
# Output: output/tables/table_05_interaction_age.html

library(pacman)
p_load(here, broom, texreg, dplyr, stringr)

# Load models ----
mxl_own  <- readRDS(here("output/models", "mxl_old_owner.rds"))
mxl_rent <- readRDS(here("output/models", "mxl_old_renter.rds"))

cost_name    <- "price_num"
scaler_own   <- 0.10 * 10000  # SEK/month — owners
scaler_rent  <- 0.10 * 9000   # SEK/month — renters

# Term definitions ----
main_attrs <- c(
  "dist_green5km", "dist_green500m",
  "dist_shops5km", "dist_shops500m",
  "dist_trans600", "dist_trans300",
  "park_garage",   "park_space"
)

inter_terms <- c(
  "green5km_Old", "green500_Old",
  "shops5km_Old", "shops500_Old",
  "trans600_Old", "trans300_Old",
  "park_garage_Old", "park_space_Old",
  "price_Old"
)

coef_rows <- c(main_attrs, cost_name, inter_terms)

# Map from interaction term -> base attribute ----
interaction_to_base <- c(
  "green5km_Old"    = "dist_green5km",
  "green500_Old"    = "dist_green500m",
  "shops5km_Old"    = "dist_shops5km",
  "shops500_Old"    = "dist_shops500m",
  "trans600_Old"    = "dist_trans600",
  "trans300_Old"    = "dist_trans300",
  "park_space_Old"  = "park_space",
  "park_garage_Old" = "park_garage",
  "price_Old"       = cost_name
)

# Label map ----
label_map <- c(
  "dist_green5km"  = "Green space: 5 km (vs 15 km)",
  "dist_green500m" = "Green space: 500 m (vs 15 km)",
  "dist_shops5km"  = "Shops: 5 km (vs 15 km)",
  "dist_shops500m" = "Shops: 500 m (vs 15 km)",
  "dist_trans600"  = "Transit stop: 600 m (vs 900 m)",
  "dist_trans300"  = "Transit stop: 300 m (vs 900 m)",
  "park_garage"    = "Parking: reserved garage (vs none)",
  "park_space"     = "Parking: reserved space (vs none)",
  "price_num"      = "Price"
)

pretty_labels <- c(
  unname(label_map[main_attrs]),
  label_map[[cost_name]],
  paste0(label_map[interaction_to_base[inter_terms]], " \u00d7 Old (age \u226572)")
)

# Helper functions ----
grab_term <- function(tidy_df, term) {
  row <- tidy_df[tidy_df$term == term, ]
  if (nrow(row) == 0) return(c(NA_real_, NA_real_, NA_real_))
  c(row$estimate[1], row$std.error[1], row$p.value[1])
}

term_group <- function(term) {
  if (term %in% main_attrs)    return("base")
  if (grepl("Old$", term))     return("Old")
  if (term == cost_name)       return("price")
  if (grepl("^price_", term))  return("price_inter")
  "other"
}

term_attr <- function(term) {
  if (term %in% main_attrs)                 return(term)
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
  beta_a_g <- if (length(a_g_term) && !is.na(a_g_term)) grab_term(tidy_df, a_g_term)[1] else 0
  beta_c_g <- if (length(c_g_term) && !is.na(c_g_term)) grab_term(tidy_df, c_g_term)[1] else 0

  if (any(is.na(c(beta_a, beta_c)))) return(c(NA_real_, NA_real_, NA_real_))

  A   <- beta_a + ifelse(group == "base", 0, beta_a_g)
  C   <- beta_c + ifelse(group == "base", 0, beta_c_g)
  val <- -(A / C) * scaler

  terms <- c(attr, cost_name,
             if (group != "base" && !is.na(a_g_term)) a_g_term else NULL,
             if (group != "base" && !is.na(c_g_term)) c_g_term else NULL)
  terms <- terms[!is.na(terms) & nzchar(terms)]
  if (is.null(V) || !all(terms %in% rownames(V))) return(c(val, NA_real_, NA_real_))

  grad <- numeric(length(terms))
  grad[terms == attr]      <- -1 / C * scaler
  grad[terms == cost_name] <- (A / C^2) * scaler
  if (group != "base") {
    if (any(terms == a_g_term)) grad[terms == a_g_term] <- -1 / C * scaler
    if (any(terms == c_g_term)) grad[terms == c_g_term] <- (A / C^2) * scaler
  }

  Vsub <- V[terms, terms, drop = FALSE]
  var  <- as.numeric(t(grad) %*% Vsub %*% grad)
  if (!is.finite(var) || var < 0) return(c(val, NA_real_, NA_real_))
  se   <- sqrt(var)
  pval <- 2 * pnorm(-abs(val / se))
  c(val, se, pval)
}

compute_bic <- function(model) {
  ll <- as.numeric(logLik(model))
  k  <- attr(logLik(model), "df")
  n  <- tryCatch(model$n$obs, error = function(e) NA)
  if (is.na(n)) return(NA_real_)
  k * log(n) - 2 * ll
}

lr_bits <- function(model) {
  ll  <- as.numeric(logLik(model))
  ll0 <- tryCatch(model$nullLogLik, error = function(e) NA_real_)
  k   <- attr(logLik(model), "df")
  if (is.na(ll0)) return(list(lr = NA_real_, p = NA_real_, k = k))
  lr  <- -2 * (ll0 - ll)
  list(lr = lr, p = pchisq(lr, df = k, lower.tail = FALSE), k = k)
}

# Build texreg object pair for one model ----
build_pair <- function(model, scaler, cost_name) {
  tdf <- broom::tidy(model)
  V   <- tryCatch(vcov(model), error = function(e) NULL)

  coef_vals <- setNames(rep(NA_real_, length(coef_rows)), coef_rows)
  se_vals   <- coef_vals;  p_vals    <- coef_vals
  mwtp_vals <- coef_vals;  mwtp_se   <- coef_vals;  mwtp_p <- coef_vals

  for (term in coef_rows) {
    x <- grab_term(tdf, term)
    coef_vals[term] <- x[1]; se_vals[term] <- x[2]; p_vals[term] <- x[3]
  }

  for (term in coef_rows) {
    g <- term_group(term)
    if (g %in% c("price", "price_inter", "other")) next
    a   <- term_attr(term)
    out <- mwtp_for_attr_group(a, ifelse(g == "base", "base", g), tdf, V, scaler, cost_name)
    mwtp_vals[term] <- out[1]; mwtp_se[term] <- out[2]; mwtp_p[term] <- out[3]
  }

  ll  <- as.numeric(logLik(model))
  ll0 <- tryCatch(model$nullLogLik, error = function(e) NA_real_)
  lr  <- lr_bits(model)
  gof_n <- c("Num. obs.", "Log Likelihood", "AIC", "BIC", "McFadden R\u00b2",
             paste0("LR \u03c7\u00b2 (df=", lr$k, ")"), "p-value (LR)")
  gof_v <- c(tryCatch(model$n$obs, error = function(e) NA_integer_),
             ll, AIC(model), compute_bic(model),
             if (!is.na(ll0)) 1 - ll / ll0 else NA_real_,
             lr$lr, lr$p)
  gof_d <- c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)

  m_coef <- createTexreg(
    coef.names  = pretty_labels,
    coef        = as.numeric(coef_vals),
    se          = as.numeric(se_vals),
    pvalues     = as.numeric(p_vals),
    gof.names   = gof_n,
    gof         = gof_v,
    gof.decimal = gof_d
  )

  m_mwtp <- createTexreg(
    coef.names = pretty_labels,
    coef       = as.numeric(mwtp_vals),
    se         = as.numeric(mwtp_se),
    pvalues    = as.numeric(mwtp_p)
  )

  list(m_coef = m_coef, m_mwtp = m_mwtp)
}

# Build and render ----
own  <- build_pair(mxl_own,  scaler_own,  cost_name)
rent <- build_pair(mxl_rent, scaler_rent, cost_name)

htmlreg(
  list(own$m_coef, own$m_mwtp, rent$m_coef, rent$m_mwtp),
  custom.header      = list("Owners" = 1:2, "Renters" = 3:4),
  custom.model.names = c("Coef.", "MWTP", "Coef.", "MWTP"),
  custom.coef.names  = pretty_labels,
  digits             = 2,
  stars              = c(0.001, 0.01, 0.05),
  na.replace         = "\u2013",
  caption            = "Table 5. Interaction regression with age (Old = age \u226572, i.e. above median; reference: younger)",
  caption.above      = TRUE,
  file               = here("output/tables", "table_05_interaction_age.html")
)

message("Table 5 saved to output/tables/table_05_interaction_age.html")
