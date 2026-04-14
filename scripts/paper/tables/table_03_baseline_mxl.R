# table_03_baseline_mxl.R
# Table 3: Mixed logit results — renters and owners (baseline models).
# Loads pre-saved models; does NOT re-estimate.
# Output: output/tables/table_03_baseline_mxl.html

library(pacman)
p_load(here, broom, texreg, dplyr, stringr)

# Load models ----
mxl_renter <- readRDS(here("output/models", "mxl_renter_base.rds"))
mxl_owner  <- readRDS(here("output/models", "mxl_owner_base.rds"))

# Helper: compute BIC ----
compute_bic <- function(model) {
  ll <- as.numeric(logLik(model))
  k  <- attr(logLik(model), "df")
  n  <- tryCatch(model$n$obs, error = function(e) NA)
  if (is.na(n)) return(NA_real_)
  k * log(n) - 2 * ll
}

# Tidy outputs ----
t_renter <- tidy(mxl_renter)
t_owner  <- tidy(mxl_owner)

# Split mean coefficients and diagonal SDs ----
means_renter <- t_renter |> filter(!str_starts(term, "sd_"))
means_owner  <- t_owner  |> filter(!str_starts(term, "sd_"))

extract_diagonal_sds <- function(df) {
  df |>
    filter(str_starts(term, "sd_")) |>
    mutate(attr = str_match(term, "^sd_(.*)_\\1$")[, 2]) |>
    filter(!is.na(attr))
}

sds_renter <- extract_diagonal_sds(t_renter)
sds_owner  <- extract_diagonal_sds(t_owner)

attr_terms <- means_renter$term

# Gather estimates ----
coef_renter   <- means_renter$estimate[match(attr_terms, means_renter$term)]
se_renter     <- means_renter$std.error[match(attr_terms, means_renter$term)]
p_renter      <- means_renter$p.value[match(attr_terms, means_renter$term)]

sd_renter     <- sds_renter$estimate[match(attr_terms, sds_renter$attr)]
sd_renter_se  <- sds_renter$std.error[match(attr_terms, sds_renter$attr)]
sd_renter_p   <- sds_renter$p.value[match(attr_terms, sds_renter$attr)]

coef_owner    <- means_owner$estimate[match(attr_terms, means_owner$term)]
se_owner      <- means_owner$std.error[match(attr_terms, means_owner$term)]
p_owner       <- means_owner$p.value[match(attr_terms, means_owner$term)]

sd_owner      <- sds_owner$estimate[match(attr_terms, sds_owner$attr)]
sd_owner_se   <- sds_owner$std.error[match(attr_terms, sds_owner$attr)]
sd_owner_p    <- sds_owner$p.value[match(attr_terms, sds_owner$attr)]

# Goodness-of-fit statistics ----
gof_stats <- function(model) {
  ll    <- as.numeric(logLik(model))
  ll0   <- model$nullLogLik
  k     <- attr(logLik(model), "df")
  lr    <- -2 * (ll0 - ll)
  p_lr  <- pchisq(lr, df = k, lower.tail = FALSE)
  list(
    nobs  = model$n$obs,
    ll    = ll,
    aic   = AIC(model),
    bic   = compute_bic(model),
    mcf   = 1 - (ll / ll0),
    lr    = lr,
    p_lr  = p_lr,
    k     = k
  )
}

gof_r <- gof_stats(mxl_renter)
gof_o <- gof_stats(mxl_owner)

make_gof <- function(g) {
  list(
    names   = c("Num. obs.", "Log Likelihood", "AIC", "BIC",
                "McFadden R²", paste0("LR χ² (df=", g$k, ")"), "p-value (LR)"),
    values  = c(g$nobs, g$ll, g$aic, g$bic, g$mcf, g$lr, g$p_lr),
    decimal = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  )
}

# Build texreg objects ----
m_renter_mean <- createTexreg(
  coef.names  = attr_terms,
  coef        = coef_renter,
  se          = se_renter,
  pvalues     = p_renter,
  gof.names   = make_gof(gof_r)$names,
  gof         = make_gof(gof_r)$values,
  gof.decimal = make_gof(gof_r)$decimal
)

m_renter_sd <- createTexreg(
  coef.names = attr_terms,
  coef       = sd_renter,
  se         = sd_renter_se,
  pvalues    = sd_renter_p
)

m_owner_mean <- createTexreg(
  coef.names  = attr_terms,
  coef        = coef_owner,
  se          = se_owner,
  pvalues     = p_owner,
  gof.names   = make_gof(gof_o)$names,
  gof         = make_gof(gof_o)$values,
  gof.decimal = make_gof(gof_o)$decimal
)

m_owner_sd <- createTexreg(
  coef.names = attr_terms,
  coef       = sd_owner,
  se         = sd_owner_se,
  pvalues    = sd_owner_p
)

# Pretty labels ----
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

# Produce HTML table ----
htmlreg(
  list(m_owner_mean, m_owner_sd, m_renter_mean, m_renter_sd),
  custom.model.names = c("Coefficient", "SD", "Coefficient", "SD"),
  custom.coef.names  = pretty_labels[attr_terms],
  custom.header      = list("Owners" = 1:2, "Renters" = 3:4),
  stars              = c(0.001, 0.01, 0.05),
  digits             = 2,
  caption            = "Table 3. Mixed logit results: renters and owners",
  caption.above      = TRUE,
  na.replace         = "–",
  file               = here("output/tables", "table_03_baseline_mxl.html")
)

message("Table 3 saved to output/tables/table_03_baseline_mxl.html")
