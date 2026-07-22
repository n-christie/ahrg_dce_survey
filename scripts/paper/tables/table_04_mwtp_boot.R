# table_04_mwtp_boot.R
# Table 4: MWTP estimates — renters and owners.
# Identical to table_04_mwtp.R but uses a non-parametric bootstrap for
# confidence intervals: respondents (panelIDs) are resampled with replacement,
# the MXL model is re-estimated for each bootstrap sample, and MWTP is computed
# from each refit. Percentile 95% CIs are reported.
#
# Bootstrap MWTP draws are cached in output/models/bootstrap/ to avoid
# re-running the expensive loop on subsequent renders. Delete the cache
# files to force a fresh run.
#
# Output: output/tables/table_04_mwtp_boot.html
#
# NOTE: B = 500 bootstrap iterations with numDraws = 200 and numMultiStarts = 1
# takes several hours to run. Run this script overnight or on a compute server.

library(pacman)
p_load(here, broom, dplyr, stringr, flextable, tibble, purrr, logitr, haven, tidyverse)

# ── Settings ──────────────────────────────────────────────────────────────────

N_BOOT     <- 500   # number of bootstrap iterations
N_DRAWS    <- 200   # logitr simulation draws per bootstrap (lower for speed)
N_STARTS   <- 1     # multistart attempts per bootstrap
price_term <- "price_num"
scaler_rent <- 0.10 * 9000
scaler_own  <- 0.10 * 10000

# ── Data preparation ──────────────────────────────────────────────────────────

source(here("scripts/paper/tables", "00_data_prep.R"))

# ── Bootstrap helper ──────────────────────────────────────────────────────────

# Resample panelIDs with replacement, preserving all rows per respondent and
# all rows per choice occasion (alternatives). Returns a new data frame with
# new unique panelID and obsID values.
resample_panels <- function(df, panel_col = "panelID", obs_col = "obsID") {
  panels     <- unique(df[[panel_col]])
  n          <- length(panels)
  samp_ids   <- sample(panels, n, replace = TRUE)

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

# Compute MWTP point estimates from a fitted model
mwtp_from_model <- function(model, scaler, price_term) {
  t_means <- broom::tidy(model) |> filter(!str_starts(term, "sd_"))
  beta_p  <- t_means$estimate[t_means$term == price_term]
  attrs   <- setdiff(t_means$term, price_term)
  setNames(
    sapply(attrs, function(a) {
      -(t_means$estimate[t_means$term == a] / beta_p) * scaler
    }),
    attrs
  )
}

# ── Model specification (must match original baseline_regs.R) ─────────────────

base_pars     <- c("dist_green", "dist_shops", "dist_trans", "parking", "price_num")
base_rand_own <- c(dist_green = "n", dist_shops = "n", dist_trans = "n", parking = "n")

# Warm-start values from original models — keeps BFGS near the optimum and
# avoids occasional very slow iterations caused by poor cold starts.
mxl_owner_orig  <- readRDS(here("output/models", "mxl_owner_base.rds"))
mxl_renter_orig <- readRDS(here("output/models", "mxl_renter_base.rds"))
start_own  <- coef(mxl_owner_orig)
start_rent <- coef(mxl_renter_orig)

# ── Bootstrap loop (with caching) ─────────────────────────────────────────────

boot_dir <- here("output/models/bootstrap")
dir.create(boot_dir, showWarnings = FALSE, recursive = TRUE)

cache_own  <- file.path(boot_dir, "boot_mwtp_owner_base.rds")
cache_rent <- file.path(boot_dir, "boot_mwtp_renter_base.rds")

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
    fit <- tryCatch(
      logitr(
        data           = as.data.frame(df_boot),
        outcome        = "choice",
        obsID          = "obsID",
        panelID        = "panelID",
        pars           = base_pars,
        randPars       = base_rand_own,
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
        mwtp_from_model(fit, scaler, price_term),
        error = function(e) NULL
      )
    }
  }

  # Drop failed iterations and combine into matrix (B × n_attrs)
  draws_ok <- draws_list[!sapply(draws_list, is.null)]
  mat <- do.call(rbind, draws_ok)
  saveRDS(mat, cache_path)
  message("Bootstrap draws saved to ", cache_path)
  mat
}

boot_own  <- run_bootstrap(df_owner,  scaler_own,  cache_own,  seed = 3572, start_vals = start_own)
boot_rent <- run_bootstrap(df_renter, scaler_rent, cache_rent, seed = 8149, start_vals = start_rent)

# ── Load pre-saved models for point estimates ─────────────────────────────────

mxl_owner  <- readRDS(here("output/models", "mxl_owner_base.rds"))
mxl_renter <- readRDS(here("output/models", "mxl_renter_base.rds"))

# ── Compute MWTP CIs from bootstrap distribution ─────────────────────────────

boot_ci <- function(model, scaler, boot_mat, price_term) {
  t_means <- broom::tidy(model) |> filter(!str_starts(term, "sd_"))
  beta_p  <- t_means$estimate[t_means$term == price_term]
  attrs   <- setdiff(t_means$term, price_term)

  map_dfr(attrs, function(a) {
    mwtp_pt <- -(t_means$estimate[t_means$term == a] / beta_p) * scaler

    mwtp_lo <- NA_real_
    mwtp_hi <- NA_real_
    if (!is.null(boot_mat) && a %in% colnames(boot_mat)) {
      col     <- boot_mat[, a]
      col     <- col[is.finite(col)]
      if (length(col) >= 10) {
        mwtp_lo <- quantile(col, 0.025, names = FALSE)
        mwtp_hi <- quantile(col, 0.975, names = FALSE)
      }
    }

    tibble(
      attribute = a,
      mwtp      = round(as.numeric(mwtp_pt), 0),
      mwtp_lo   = round(mwtp_lo, 0),
      mwtp_hi   = round(mwtp_hi, 0)
    )
  })
}

mwtp_own  <- boot_ci(mxl_owner,  scaler_own,  boot_own,  price_term)
mwtp_rent <- boot_ci(mxl_renter, scaler_rent, boot_rent, price_term)

# ── Attribute labels ──────────────────────────────────────────────────────────

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

# ── Build display table ───────────────────────────────────────────────────────

fmt_mwtp <- function(v, lo, hi) {
  if (is.na(lo)) return(as.character(v))
  sprintf("%d\n(%d, %d)", as.integer(v), as.integer(lo), as.integer(hi))
}

idx <- match(mwtp_own$attribute, mwtp_rent$attribute)

display <- tibble(
  Attribute = label_map[mwtp_own$attribute],
  `Owners`  = mapply(fmt_mwtp, mwtp_own$mwtp, mwtp_own$mwtp_lo, mwtp_own$mwtp_hi),
  `Renters` = mapply(fmt_mwtp, mwtp_rent$mwtp[idx], mwtp_rent$mwtp_lo[idx], mwtp_rent$mwtp_hi[idx])
) |>
  filter(!is.na(Attribute))

# ── Build flextable ───────────────────────────────────────────────────────────

ft <- flextable(display) |>
  bold(part = "header") |>
  align(part = "header", align = "center") |>
  align(j = 2:3, align = "center") |>
  autofit() |>
  theme_booktabs() |>
  add_footer_lines(
    paste0(
      "MWTP in SEK/month (10% of median monthly housing cost: owners 10,000 SEK, renters 9,000 SEK). ",
      "95% confidence intervals in brackets, estimated via non-parametric bootstrap ",
      "(B = ", N_BOOT, " resamples of respondents; percentile method). ",
      "Point estimates from the original pre-saved models."
    )
  ) |>
  set_caption(caption = "Table 4. MWTP estimates: renters and owners (bootstrap CIs)")

# ── Save ──────────────────────────────────────────────────────────────────────

save_as_html(ft, path = here("output/tables", "table_04_mwtp_boot.html"))
message("Table 4 (bootstrap) saved to output/tables/table_04_mwtp_boot.html")
