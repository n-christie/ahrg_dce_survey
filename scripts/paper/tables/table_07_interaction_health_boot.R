# table_07_interaction_health_boot.R
# Table 7: Mixed logit interaction regression — health (Very good vs Not very good as reference).
# Identical to table_07_interaction_health.R but uses a non-parametric bootstrap for
# MWTP confidence intervals: respondents (panelIDs) are resampled with replacement,
# the interaction MXL model is re-estimated for each bootstrap sample, and MWTP
# (for both base and Good groups) is computed. Percentile 95% CIs are reported
# in the MWTP columns of the table.
#
# Bootstrap MWTP draws are cached in output/models/bootstrap/ to avoid
# re-running the expensive loop on subsequent renders. Delete the cache
# files to force a fresh run.
#
# Output: output/tables/table_07_interaction_health_boot.html
#
# NOTE: B = 500 iterations with numDraws = 200 takes several hours to run.
#
# IMPORTANT: The health variable definition here matches interaction_regs_owner_health.R
# (VAR035 in c(1,2) = "Not very good"; VAR035 in c(3,4,5) = "Very good"), which
# differs from 00_data_prep.R. The Good indicator is overridden after sourcing data_prep.

library(pacman)
p_load(here, broom, texreg, dplyr, stringr, purrr, logitr, haven, tidyverse)

# ── Settings ──────────────────────────────────────────────────────────────────

N_BOOT   <- 500
N_DRAWS  <- 200
N_STARTS <- 1
cost_name    <- "price_num"
scaler_own   <- 0.10 * 10000
scaler_rent  <- 0.10 * 9000

# ── Data preparation ──────────────────────────────────────────────────────────

source(here("scripts/paper/tables", "00_data_prep.R"))

# Override Health/Good to match the original interaction_regs_owner_health.R definition:
# Good = 1 if VAR035 %in% c(3,4,5) (i.e., health rating >= 3)
df_model <- df_model |>
  mutate(
    Health = case_when(
      VAR035 %in% c(1, 2)    ~ "Not very good",
      VAR035 %in% c(3, 4, 5) ~ "Very good",
      TRUE                   ~ NA_character_
    ),
    Good = as.integer(Health == "Very good")
  )

df_owner  <- df_model |> filter(Own == "Owner")
df_renter <- df_model |> filter(Own == "Renter")

# Create explicit attribute dummies and health interaction terms (matches interaction_regs_owner_health.R)
add_health_interaction_vars <- function(df) {
  dummies <- model.matrix(~ dist_green + dist_shops + dist_trans + parking - 1,
                          data = df) |>
    as_tibble()

  df <- bind_cols(df, dummies) |>
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

  df |>
    mutate(
      green5km_Good    = dist_green5km  * Good,
      green500_Good    = dist_green500m * Good,
      shops5km_Good    = dist_shops5km  * Good,
      shops500_Good    = dist_shops500m * Good,
      trans600_Good    = dist_trans600  * Good,
      trans300_Good    = dist_trans300  * Good,
      park_space_Good  = park_space     * Good,
      park_garage_Good = park_garage    * Good,
      price_Good       = price_num      * Good
    )
}

# Health model filters out NA health respondents (matches original)
df_owner_hlth  <- add_health_interaction_vars(df_owner)  |> filter(!is.na(Health))
df_renter_hlth <- add_health_interaction_vars(df_renter) |> filter(!is.na(Health))

# Warm-start values from original health interaction models
start_own  <- coef(readRDS(here("output/models", "mxl_health_own.rds")))
start_rent <- coef(readRDS(here("output/models", "mxl_health_rent.rds")))

# ── Model parameter specification (must match mxl_health_own/rent) ────────────

main_pars  <- c("dist_green5km", "dist_green500m",
                "dist_shops5km", "dist_shops500m",
                "dist_trans600", "dist_trans300",
                "park_space",    "park_garage",
                "price_num")
inter_pars <- c("green5km_Good", "green500_Good",
                "shops5km_Good", "shops500_Good",
                "trans600_Good", "trans300_Good",
                "park_space_Good", "park_garage_Good",
                "price_Good")
all_pars   <- c(main_pars, inter_pars)
rand_pars  <- c(dist_green5km  = "n", dist_green500m = "n",
                dist_shops5km  = "n", dist_shops500m = "n",
                dist_trans600  = "n", dist_trans300  = "n",
                park_space     = "n", park_garage    = "n")

interaction_to_base <- c(
  "green5km_Good"    = "dist_green5km",
  "green500_Good"    = "dist_green500m",
  "shops5km_Good"    = "dist_shops5km",
  "shops500_Good"    = "dist_shops500m",
  "trans600_Good"    = "dist_trans600",
  "trans300_Good"    = "dist_trans300",
  "park_space_Good"  = "park_space",
  "park_garage_Good" = "park_garage",
  "price_Good"       = cost_name
)

main_attrs <- c("dist_green5km", "dist_green500m",
                "dist_shops5km", "dist_shops500m",
                "dist_trans600", "dist_trans300",
                "park_garage",   "park_space")

inter_terms <- c("green5km_Good", "green500_Good",
                 "shops5km_Good", "shops500_Good",
                 "trans600_Good", "trans300_Good",
                 "park_garage_Good", "park_space_Good",
                 "price_Good")

coef_rows <- c(main_attrs, cost_name, inter_terms)

# ── Bootstrap helper ──────────────────────────────────────────────────────────

resample_panels <- function(df, panel_col = "panelID", obs_col = "obsID") {
  panels   <- unique(df[[panel_col]])
  n        <- length(panels)
  samp_ids <- sample(panels, n, replace = TRUE)

  result <- lapply(seq_along(samp_ids), function(i) {
    sub     <- df[df[[panel_col]] == samp_ids[i], ]
    old_obs <- unique(sub[[obs_col]])
    obs_map <- setNames(paste0(i, "_", seq_along(old_obs)), as.character(old_obs))
    sub[[obs_col]]   <- obs_map[as.character(sub[[obs_col]])]
    sub[[panel_col]] <- as.character(i)
    sub
  })
  do.call(rbind, result)
}

compute_group_mwtp <- function(model, scaler) {
  tdf   <- broom::tidy(model)
  get_b <- function(nm) { r <- tdf[tdf$term == nm, ]; if (nrow(r) > 0) r$estimate[1] else NA_real_ }

  beta_p     <- get_b(cost_name)
  beta_pGood <- get_b("price_Good")

  out <- sapply(main_attrs, function(a) {
    beta_a     <- get_b(a)
    iterm      <- names(interaction_to_base)[interaction_to_base == a]
    beta_aGood <- if (length(iterm) && nzchar(iterm)) get_b(iterm) else 0

    A_base <- beta_a
    A_good <- beta_a + beta_aGood
    C_base <- beta_p
    C_good <- beta_p + beta_pGood

    c(
      base = if (any(is.na(c(A_base, C_base)))) NA_real_ else -(A_base / C_base) * scaler,
      good = if (any(is.na(c(A_good, C_good))))  NA_real_ else -(A_good / C_good)  * scaler
    )
  })
  setNames(as.vector(out), paste0(rep(main_attrs, each = 2), c(".base", ".good")))
}

# ── Bootstrap loop (with caching) ─────────────────────────────────────────────

boot_dir <- here("output/models/bootstrap")
dir.create(boot_dir, showWarnings = FALSE, recursive = TRUE)

cache_own  <- file.path(boot_dir, "boot_mwtp_health_own.rds")
cache_rent <- file.path(boot_dir, "boot_mwtp_health_rent.rds")

run_bootstrap <- function(df, scaler, cache_path, seed, start_vals) {
  if (file.exists(cache_path)) {
    message("Loading cached bootstrap draws from ", cache_path)
    return(readRDS(cache_path))
  }

  message("Running bootstrap (B = ", N_BOOT, ") — this may take several hours …")
  set.seed(seed)

  draws_list <- vector("list", N_BOOT)
  for (b in seq_len(N_BOOT)) {
    if (b %% 50 == 0) message("  Bootstrap iteration ", b, " / ", N_BOOT)
    df_boot <- resample_panels(df)
    df_boot  <- df_boot[!is.na(df_boot$Health), ]
    fit <- tryCatch(
      logitr(
        data           = as.data.frame(df_boot),
        outcome        = "choice",
        obsID          = "obsID",
        panelID        = "panelID",
        pars           = all_pars,
        randPars       = rand_pars,
        startVals      = start_vals,
        numDraws       = N_DRAWS,
        numMultiStarts = N_STARTS,
        drawType       = "sobol",
        correlation    = TRUE
      ),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      draws_list[[b]] <- tryCatch(
        compute_group_mwtp(fit, scaler),
        error = function(e) NULL
      )
    }
  }

  draws_ok <- draws_list[!sapply(draws_list, is.null)]
  mat <- do.call(rbind, draws_ok)
  saveRDS(mat, cache_path)
  message("Bootstrap draws saved to ", cache_path)
  mat
}

boot_own  <- run_bootstrap(df_owner_hlth,  scaler_own,  cache_own,  seed = 5836, start_vals = start_own)
boot_rent <- run_bootstrap(df_renter_hlth, scaler_rent, cache_rent, seed = 3194, start_vals = start_rent)

# ── Load pre-saved models for point estimates ─────────────────────────────────

mxl_own  <- readRDS(here("output/models", "mxl_health_own.rds"))
mxl_rent <- readRDS(here("output/models", "mxl_health_rent.rds"))

# ── Labels and helper functions ───────────────────────────────────────────────

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
  paste0(label_map[interaction_to_base[inter_terms]], " \u00d7 Very good health")
)

grab_term <- function(tidy_df, term) {
  row <- tidy_df[tidy_df$term == term, ]
  if (nrow(row) == 0) return(c(NA_real_, NA_real_, NA_real_))
  c(row$estimate[1], row$std.error[1], row$p.value[1])
}

term_group <- function(term) {
  if (term %in% main_attrs)   return("base")
  if (grepl("Good$", term))   return("Good")
  if (term == cost_name)      return("price")
  "other"
}

term_attr <- function(term) {
  if (term %in% main_attrs)                 return(term)
  if (term %in% names(interaction_to_base)) return(interaction_to_base[[term]])
  NA_character_
}

mwtp_with_boot_ci <- function(attr, group, tidy_df, boot_mat, scaler) {
  get_b   <- function(nm) { r <- tidy_df[tidy_df$term == nm, ]; if (nrow(r) > 0) r$estimate[1] else NA_real_ }
  beta_p  <- get_b(cost_name)
  beta_a  <- get_b(attr)
  iterm   <- if (group == "base") NA_character_
             else names(interaction_to_base)[interaction_to_base == attr & grepl(group, names(interaction_to_base))]
  pterm   <- if (group == "base") NA_character_
             else names(interaction_to_base)[interaction_to_base == cost_name & grepl(group, names(interaction_to_base))]
  beta_ag <- if (!is.na(iterm) && length(iterm)) get_b(iterm) else 0
  beta_pg <- if (!is.na(pterm) && length(pterm)) get_b(pterm) else 0

  if (any(is.na(c(beta_a, beta_p)))) return(c(NA_real_, NA_real_, NA_real_))
  A   <- beta_a + ifelse(group == "base", 0, beta_ag)
  C   <- beta_p + ifelse(group == "base", 0, beta_pg)
  val <- -(A / C) * scaler

  col_name <- paste0(attr, ".", tolower(group))
  if (!is.null(boot_mat) && col_name %in% colnames(boot_mat)) {
    col <- boot_mat[, col_name]
    col <- col[is.finite(col)]
    if (length(col) >= 10) {
      se   <- sd(col)
      pval <- 2 * pnorm(-abs(val / se))
      return(c(val, se, pval))
    }
  }
  c(val, NA_real_, NA_real_)
}

compute_bic <- function(model) {
  ll <- as.numeric(logLik(model)); k <- attr(logLik(model), "df")
  n  <- tryCatch(model$n$obs, error = function(e) NA)
  if (is.na(n)) return(NA_real_); k * log(n) - 2 * ll
}

lr_bits <- function(model) {
  ll  <- as.numeric(logLik(model))
  ll0 <- tryCatch(model$nullLogLik, error = function(e) NA_real_)
  k   <- attr(logLik(model), "df")
  if (is.na(ll0)) return(list(lr = NA_real_, p = NA_real_, k = k))
  lr  <- -2 * (ll0 - ll)
  list(lr = lr, p = pchisq(lr, df = k, lower.tail = FALSE), k = k)
}

build_pair <- function(model, scaler, boot_mat) {
  tdf <- broom::tidy(model)

  coef_vals <- setNames(rep(NA_real_, length(coef_rows)), coef_rows)
  se_vals   <- coef_vals;  p_vals    <- coef_vals
  mwtp_vals <- coef_vals;  mwtp_se   <- coef_vals;  mwtp_p <- coef_vals

  for (term in coef_rows) {
    x <- grab_term(tdf, term)
    coef_vals[term] <- x[1]; se_vals[term] <- x[2]; p_vals[term] <- x[3]
  }
  for (term in coef_rows) {
    g <- term_group(term)
    if (g %in% c("price", "other")) next
    a   <- term_attr(term)
    out <- mwtp_with_boot_ci(a, ifelse(g == "base", "base", g), tdf, boot_mat, scaler)
    mwtp_vals[term] <- out[1]; mwtp_se[term] <- out[2]; mwtp_p[term] <- out[3]
  }

  ll  <- as.numeric(logLik(model))
  ll0 <- tryCatch(model$nullLogLik, error = function(e) NA_real_)
  lr  <- lr_bits(model)
  gof_n <- c("Num. obs.", "Log Likelihood", "AIC", "BIC", "McFadden R\u00b2",
             paste0("LR \u03c7\u00b2 (df=", lr$k, ")"), "p-value (LR)")
  gof_v <- c(tryCatch(model$n$obs, error = function(e) NA_integer_),
             ll, AIC(model), compute_bic(model),
             if (!is.na(ll0)) 1 - ll / ll0 else NA_real_, lr$lr, lr$p)
  gof_d <- c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)

  m_coef <- createTexreg(
    coef.names = pretty_labels, coef = as.numeric(coef_vals),
    se = as.numeric(se_vals), pvalues = as.numeric(p_vals),
    gof.names = gof_n, gof = gof_v, gof.decimal = gof_d
  )
  m_mwtp <- createTexreg(
    coef.names = pretty_labels, coef = as.numeric(mwtp_vals),
    se = as.numeric(mwtp_se), pvalues = as.numeric(mwtp_p)
  )
  list(m_coef = m_coef, m_mwtp = m_mwtp)
}

own  <- build_pair(mxl_own,  scaler_own,  boot_own)
rent <- build_pair(mxl_rent, scaler_rent, boot_rent)

htmlreg(
  list(own$m_coef, own$m_mwtp, rent$m_coef, rent$m_mwtp),
  custom.header      = list("Owners" = 1:2, "Renters" = 3:4),
  custom.model.names = c("Coef.", "MWTP", "Coef.", "MWTP"),
  custom.coef.names  = pretty_labels,
  digits             = 2,
  stars              = c(0.001, 0.01, 0.05),
  na.replace         = "\u2013",
  caption            = paste0(
    "Table 7. Interaction regression with health status (Very good vs Not very good as reference). ",
    "MWTP CIs via non-parametric bootstrap (B = ", N_BOOT, ", percentile method)."
  ),
  caption.above = TRUE,
  file          = here("output/tables", "table_07_interaction_health_boot.html")
)

message("Table 7 (bootstrap) saved to output/tables/table_07_interaction_health_boot.html")
