# table_reviewer_price_attr_interactions_boot.R
#
# Reviewer request: interact the price coefficient with each attribute level.
# Identical to table_reviewer_price_attr_interactions.R but uses a non-parametric
# bootstrap for MWTP confidence intervals: respondents (panelIDs) are resampled
# with replacement, the price × attribute interaction MXL model is re-estimated
# for each bootstrap sample, and MWTP is computed from each refit. Percentile
# 95% CIs are reported.
#
# Two outputs saved to output/tables/:
#   (A) table_reviewer_price_attr_coefs_boot.html  — coefficient grid (unchanged from original)
#   (B) table_reviewer_price_attr_mwtp_boot.html   — MWTP comparison with bootstrap 95% CIs
#
# Bootstrap draws cached in output/models/bootstrap/ — delete to force re-run.
#
# NOTE: B = 500 iterations × 2 tenure groups × complex models takes many hours.
# Run this script overnight or on a compute server.

library(pacman)
p_load(here, tidyverse, haven, logitr, broom, flextable, dplyr, stringr, purrr)

N_BOOT   <- 500
N_DRAWS  <- 200
N_STARTS <- 1

# ── 1. Data prep ─────────────────────────────────────────────────────────────

df_model <- readRDS(here("data/formr", "df_model.rds"))

df_model <- df_model |>
  mutate(
    price_num  = price_num / 100,
    dist_trans = factor(dist_trans, levels = c("900", "600", "300")),
    dist_green = factor(dist_green, levels = c("15km", "5km", "500 meter")),
    dist_shops = factor(dist_shops, levels = c("15km", "5km", "500 meter")),
    ägandebostad = haven::as_factor(ägandebostad),
    Own = factor(if_else(ägandebostad == "Ja", "Owner", "Renter"))
  ) |>
  mutate(
    green5km  = as.integer(dist_green == "5km"),
    green500m = as.integer(dist_green == "500 meter"),
    shops5km  = as.integer(dist_shops == "5km"),
    shops500m = as.integer(dist_shops == "500 meter"),
    trans600  = as.integer(dist_trans == "600"),
    trans300  = as.integer(dist_trans == "300"),
    garage    = as.integer(parking == "reserverad garageplats"),
    space     = as.integer(parking == "reserverad P-plats")
  ) |>
  mutate(
    p_green5km  = price_num * green5km,
    p_green500m = price_num * green500m,
    p_shops5km  = price_num * shops5km,
    p_shops500m = price_num * shops500m,
    p_trans600  = price_num * trans600,
    p_trans300  = price_num * trans300,
    p_garage    = price_num * garage,
    p_space     = price_num * space
  )

df_owner  <- df_model |> filter(Own == "Owner")
df_renter <- df_model |> filter(Own == "Renter")

# ── 2. Load (or estimate) pre-saved models ────────────────────────────────────

path_own  <- here("output/models", "mxl_price_attr_own.rds")
path_rent <- here("output/models", "mxl_price_attr_rent.rds")

attr_pars  <- c("green5km", "green500m", "shops5km", "shops500m",
                "trans600", "trans300", "garage", "space")
price_pars <- c("price_num",
                "p_green5km", "p_green500m",
                "p_shops5km", "p_shops500m",
                "p_trans600", "p_trans300",
                "p_garage",   "p_space")
all_pars   <- c(attr_pars, price_pars)
rand_pars  <- setNames(rep("n", length(attr_pars)), attr_pars)

if (!file.exists(path_own)) {
  message("Estimating owner model with price \u00d7 attribute interactions \u2026")
  set.seed(7341)
  mxl_price_attr_own <- logitr(
    data = df_owner, outcome = "choice", obsID = "obsID", panelID = "panelID",
    pars = all_pars, randPars = rand_pars,
    numMultiStarts = 5, drawType = "sobol", numDraws = 20, correlation = TRUE
  )
  saveRDS(mxl_price_attr_own, path_own)
} else {
  mxl_price_attr_own <- readRDS(path_own)
}

if (!file.exists(path_rent)) {
  message("Estimating renter model with price \u00d7 attribute interactions \u2026")
  set.seed(2857)
  mxl_price_attr_rent <- logitr(
    data = df_renter, outcome = "choice", obsID = "obsID", panelID = "panelID",
    pars = all_pars, randPars = rand_pars,
    numMultiStarts = 5, drawType = "sobol", numDraws = 20, correlation = TRUE
  )
  saveRDS(mxl_price_attr_rent, path_rent)
} else {
  mxl_price_attr_rent <- readRDS(path_rent)
}

# ── 3. Extract coefficients for Part A (unchanged from original) ──────────────

tdf_own  <- broom::tidy(mxl_price_attr_own)  |> filter(!str_starts(term, "sd_"))
tdf_rent <- broom::tidy(mxl_price_attr_rent) |> filter(!str_starts(term, "sd_"))

sig_stars <- function(p) {
  case_when(is.na(p) ~ "", p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", TRUE ~ "")
}

fmt_coef <- function(est, se, p) {
  if (is.na(est)) return("\u2013")
  paste0(sprintf("%.2f", est), sig_stars(p), "\n(", sprintf("%.2f", se), ")")
}

inter_map <- tribble(
  ~attr_label,                          ~attr_term,   ~inter_term,
  "Green space: 5 km (vs 15 km)",       "green5km",   "p_green5km",
  "Green space: 500 m (vs 15 km)",      "green500m",  "p_green500m",
  "Shops: 5 km (vs 15 km)",             "shops5km",   "p_shops5km",
  "Shops: 500 m (vs 15 km)",            "shops500m",  "p_shops500m",
  "Transit stop: 600 m (vs 900 m)",     "trans600",   "p_trans600",
  "Transit stop: 300 m (vs 900 m)",     "trans300",   "p_trans300",
  "Parking: reserved garage (vs none)", "garage",     "p_garage",
  "Parking: reserved space (vs none)",  "space",      "p_space"
)

pull_cell <- function(tdf, term_name) {
  r <- tdf[tdf$term == term_name, ]
  if (nrow(r) == 0) return("\u2013")
  fmt_coef(r$estimate[1], r$std.error[1], r$p.value[1])
}

price_section <- tibble(
  Section = "Price", Attribute = "Price",
  Owner = pull_cell(tdf_own, "price_num"), Renter = pull_cell(tdf_rent, "price_num")
)

attr_section <- inter_map |> rowwise() |>
  mutate(Section = "Attribute coefficients", Attribute = attr_label,
         Owner = pull_cell(tdf_own, attr_term), Renter = pull_cell(tdf_rent, attr_term)) |>
  ungroup() |> select(Section, Attribute, Owner, Renter)

inter_section <- inter_map |> rowwise() |>
  mutate(Section = "Price \u00d7 attribute interactions",
         Attribute = paste0("Price \u00d7 ", attr_label),
         Owner = pull_cell(tdf_own, inter_term), Renter = pull_cell(tdf_rent, inter_term)) |>
  ungroup() |> select(Section, Attribute, Owner, Renter)

grid_df <- bind_rows(price_section, attr_section, inter_section)
n_price <- nrow(price_section); n_attr <- n_price + nrow(attr_section)
n_total <- n_attr + nrow(inter_section)

ft_grid <- flextable(grid_df |> select(-Section)) |>
  add_header_row(values = c("", "Owners", "Renters"), colwidths = c(1, 1, 1)) |>
  bold(part = "header") |> align(j = 2:3, align = "center") |>
  align(j = 2:3, align = "center", part = "header") |>
  bold(i = 1) |> bg(i = 1, bg = "#e8e8e8") |>
  bold(i = n_price + 1) |> bg(i = seq(n_price + 1, n_attr), bg = "#f7f7f7") |>
  bold(i = n_attr + 1)  |> bg(i = seq(n_attr + 1, n_total), bg = "#eef4fb") |>
  hline(i = n_price, border = officer::fp_border(width = 1)) |>
  hline(i = n_attr,  border = officer::fp_border(width = 1)) |>
  autofit() |> theme_booktabs() |>
  add_footer_lines(paste0(
    "Mixed logit with correlated random parameters. Standard errors in parentheses.\n",
    "Significance: * p<0.05  ** p<0.01  *** p<0.001."
  )) |>
  set_caption(caption = "Coefficient grid: attribute effects and price \u00d7 attribute interactions")

save_as_html(ft_grid, path = here("output/tables", "table_reviewer_price_attr_coefs_boot.html"))
message("Grid table (boot) saved to output/tables/table_reviewer_price_attr_coefs_boot.html")

# ── 4. Bootstrap helper ───────────────────────────────────────────────────────

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

cost_name <- "price_num"

# MWTP = -(beta_attr / beta_price) * scaler from a fitted interaction model
mwtp_from_fit <- function(model, scaler) {
  tdf    <- broom::tidy(model)
  get_b  <- function(nm) { r <- tdf[tdf$term == nm, ]; if (nrow(r) > 0) r$estimate[1] else NA_real_ }
  beta_p <- get_b(cost_name)
  setNames(
    sapply(inter_map$attr_term, function(a) {
      beta_a <- get_b(a)
      if (any(is.na(c(beta_a, beta_p)))) NA_real_ else -(beta_a / beta_p) * scaler
    }),
    inter_map$attr_term
  )
}

# ── 5. Bootstrap loop (with caching) ─────────────────────────────────────────

boot_dir <- here("output/models/bootstrap")
dir.create(boot_dir, showWarnings = FALSE, recursive = TRUE)

cache_own_new  <- file.path(boot_dir, "boot_mwtp_price_attr_own.rds")
cache_rent_new <- file.path(boot_dir, "boot_mwtp_price_attr_rent.rds")

run_bootstrap <- function(df, scaler, cache_path, seed, start_vals) {
  if (file.exists(cache_path)) {
    message("Loading cached bootstrap draws from ", cache_path)
    return(readRDS(cache_path))
  }

  message("Running bootstrap (B = ", N_BOOT, ") — this may take several hours \u2026")
  set.seed(seed)

  draws_list <- vector("list", N_BOOT)
  for (b in seq_len(N_BOOT)) {
    if (b %% 50 == 0) message("  Bootstrap iteration ", b, " / ", N_BOOT)
    df_boot <- resample_panels(df)
    fit <- tryCatch(
      logitr(
        data = as.data.frame(df_boot), outcome = "choice",
        obsID = "obsID", panelID = "panelID",
        pars = all_pars, randPars = rand_pars,
        startVals = start_vals,
        numDraws = N_DRAWS, numMultiStarts = N_STARTS,
        drawType = "sobol", correlation = TRUE
      ),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      draws_list[[b]] <- tryCatch(mwtp_from_fit(fit, scaler), error = function(e) NULL)
    }
  }

  draws_ok <- draws_list[!sapply(draws_list, is.null)]
  mat <- do.call(rbind, draws_ok)
  saveRDS(mat, cache_path)
  message("Bootstrap draws saved to ", cache_path)
  mat
}

scaler_own  <- 0.10 * 10000
scaler_rent <- 0.10 * 9000

# Warm-start values from original reviewer models
start_own_new  <- coef(mxl_price_attr_own)
start_rent_new <- coef(mxl_price_attr_rent)

boot_own_new  <- run_bootstrap(df_owner,  scaler_own,  cache_own_new,  seed = 6473, start_vals = start_own_new)
boot_rent_new <- run_bootstrap(df_renter, scaler_rent, cache_rent_new, seed = 1825, start_vals = start_rent_new)

# Also bootstrap baseline models for comparison CIs
cache_own_base  <- file.path(boot_dir, "boot_mwtp_owner_base.rds")
cache_rent_base <- file.path(boot_dir, "boot_mwtp_renter_base.rds")

# Baseline models use factor pars; define helper for baseline
base_pars_fac  <- c("dist_green", "dist_shops", "dist_trans", "parking", "price_num")
base_rand_fac  <- c(dist_green = "n", dist_shops = "n", dist_trans = "n", parking = "n")

base_attr_map <- c(
  "p_green5km"  = "dist_green5km",
  "p_green500m" = "dist_green500 meter",
  "p_shops5km"  = "dist_shops5km",
  "p_shops500m" = "dist_shops500 meter",
  "p_trans600"  = "dist_trans600",
  "p_trans300"  = "dist_trans300",
  "p_garage"    = "parkingreserverad garageplats",
  "p_space"     = "parkingreserverad P-plats"
)

mwtp_from_baseline_fit <- function(model, scaler) {
  tdf    <- broom::tidy(model) |> filter(!str_starts(term, "sd_"))
  get_b  <- function(nm) { r <- tdf[tdf$term == nm, ]; if (nrow(r) > 0) r$estimate[1] else NA_real_ }
  beta_p <- get_b(cost_name)
  # Return vector named by inter_term (matching inter_map)
  setNames(
    sapply(inter_map$inter_term, function(it) {
      a_term <- base_attr_map[[it]]
      beta_a <- get_b(a_term)
      if (any(is.na(c(beta_a, beta_p)))) NA_real_ else -(beta_a / beta_p) * scaler
    }),
    inter_map$inter_term
  )
}

run_base_bootstrap <- function(df, scaler, cache_path, seed, start_vals) {
  if (file.exists(cache_path)) {
    message("Loading cached bootstrap draws from ", cache_path)
    return(readRDS(cache_path))
  }

  message("Running baseline bootstrap (B = ", N_BOOT, ") \u2026")
  set.seed(seed)

  draws_list <- vector("list", N_BOOT)
  for (b in seq_len(N_BOOT)) {
    if (b %% 50 == 0) message("  Baseline bootstrap iteration ", b, " / ", N_BOOT)
    df_boot <- resample_panels(df)
    fit <- tryCatch(
      logitr(
        data = as.data.frame(df_boot), outcome = "choice",
        obsID = "obsID", panelID = "panelID",
        pars = base_pars_fac, randPars = base_rand_fac,
        startVals = start_vals,
        numDraws = N_DRAWS, numMultiStarts = N_STARTS,
        drawType = "sobol", correlation = TRUE
      ),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      draws_list[[b]] <- tryCatch(mwtp_from_baseline_fit(fit, scaler), error = function(e) NULL)
    }
  }

  draws_ok <- draws_list[!sapply(draws_list, is.null)]
  mat <- do.call(rbind, draws_ok)
  saveRDS(mat, cache_path)
  message("Baseline bootstrap draws saved to ", cache_path)
  mat
}

# Load baseline data for bootstrap (standard df_owner/df_renter from data prep)
df_own_base  <- readRDS(here("data/formr", "df_model.rds")) |>
  mutate(
    price_num  = price_num / 100,
    dist_trans = factor(dist_trans, levels = c("900", "600", "300")),
    dist_green = factor(dist_green, levels = c("15km", "5km", "500 meter")),
    dist_shops = factor(dist_shops, levels = c("15km", "5km", "500 meter")),
    ägandebostad = haven::as_factor(ägandebostad),
    Own = factor(if_else(ägandebostad == "Ja", "Owner", "Renter"))
  ) |>
  filter(Own == "Owner")

df_rent_base <- readRDS(here("data/formr", "df_model.rds")) |>
  mutate(
    price_num  = price_num / 100,
    dist_trans = factor(dist_trans, levels = c("900", "600", "300")),
    dist_green = factor(dist_green, levels = c("15km", "5km", "500 meter")),
    dist_shops = factor(dist_shops, levels = c("15km", "5km", "500 meter")),
    ägandebostad = haven::as_factor(ägandebostad),
    Own = factor(if_else(ägandebostad == "Ja", "Owner", "Renter"))
  ) |>
  filter(Own == "Renter")

# Warm-start values for baseline bootstrap from pre-saved baseline models
mxl_own_base_tmp  <- readRDS(here("output/models", "mxl_owner_base.rds"))
mxl_rent_base_tmp <- readRDS(here("output/models", "mxl_renter_base.rds"))
start_own_base  <- coef(mxl_own_base_tmp)
start_rent_base <- coef(mxl_rent_base_tmp)

boot_own_base  <- run_base_bootstrap(df_own_base,  scaler_own,  cache_own_base,  seed = 3047, start_vals = start_own_base)
boot_rent_base <- run_base_bootstrap(df_rent_base, scaler_rent, cache_rent_base, seed = 8512, start_vals = start_rent_base)

# ── 6. Compute MWTP point estimates and bootstrap CIs ────────────────────────

mxl_own_base  <- readRDS(here("output/models", "mxl_owner_base.rds"))
mxl_rent_base <- readRDS(here("output/models", "mxl_renter_base.rds"))

tdf_own_base  <- broom::tidy(mxl_own_base)  |> filter(!str_starts(term, "sd_"))
tdf_rent_base <- broom::tidy(mxl_rent_base) |> filter(!str_starts(term, "sd_"))

boot_ci <- function(boot_mat, col_name) {
  if (is.null(boot_mat) || !col_name %in% colnames(boot_mat)) return(c(NA_integer_, NA_integer_))
  col <- boot_mat[, col_name]
  col <- col[is.finite(col)]
  if (length(col) < 10) return(c(NA_integer_, NA_integer_))
  round(quantile(col, c(0.025, 0.975), names = FALSE))
}

fmt_mwtp <- function(v, lo, hi) {
  if (is.na(v)) return("\u2013")
  if (is.na(lo)) return(as.character(as.integer(v)))
  sprintf("%d\n(%d, %d)", as.integer(v), as.integer(lo), as.integer(hi))
}

# Build the comparison table
get_base_pt <- function(tdf, base_term, scaler) {
  get_b  <- function(nm) { r <- tdf[tdf$term == nm, ]; if (nrow(r) > 0) r$estimate[1] else NA_real_ }
  beta_p <- get_b(cost_name)
  beta_a <- get_b(base_term)
  if (any(is.na(c(beta_a, beta_p)))) NA_real_ else round(-(beta_a / beta_p) * scaler)
}

get_new_pt <- function(tdf, attr_term, scaler) {
  get_b  <- function(nm) { r <- tdf[tdf$term == nm, ]; if (nrow(r) > 0) r$estimate[1] else NA_real_ }
  beta_p <- get_b(cost_name)
  beta_a <- get_b(attr_term)
  if (any(is.na(c(beta_a, beta_p)))) NA_real_ else round(-(beta_a / beta_p) * scaler)
}

mwtp_compare <- inter_map |>
  rowwise() |>
  mutate(
    # Owner baseline
    own_base_pt = get_base_pt(tdf_own_base, base_attr_map[[inter_term]], scaler_own),
    own_base_ci = list(boot_ci(boot_own_base, inter_term)),
    own_base    = fmt_mwtp(own_base_pt, own_base_ci[[1]][1], own_base_ci[[1]][2]),
    # Owner interaction model
    own_inter_pt = get_new_pt(tdf_own, attr_term, scaler_own),
    own_inter_ci = list(boot_ci(boot_own_new, attr_term)),
    own_inter    = fmt_mwtp(own_inter_pt, own_inter_ci[[1]][1], own_inter_ci[[1]][2]),
    own_diff     = as.integer(own_inter_pt) - as.integer(own_base_pt),
    # Renter baseline
    rent_base_pt = get_base_pt(tdf_rent_base, base_attr_map[[inter_term]], scaler_rent),
    rent_base_ci = list(boot_ci(boot_rent_base, inter_term)),
    rent_base    = fmt_mwtp(rent_base_pt, rent_base_ci[[1]][1], rent_base_ci[[1]][2]),
    # Renter interaction model
    rent_inter_pt = get_new_pt(tdf_rent, attr_term, scaler_rent),
    rent_inter_ci = list(boot_ci(boot_rent_new, attr_term)),
    rent_inter    = fmt_mwtp(rent_inter_pt, rent_inter_ci[[1]][1], rent_inter_ci[[1]][2]),
    rent_diff     = as.integer(rent_inter_pt) - as.integer(rent_base_pt)
  ) |>
  ungroup() |>
  select(Attribute = attr_label,
         own_base, own_inter, own_diff,
         rent_base, rent_inter, rent_diff)

ft_mwtp <- flextable(mwtp_compare) |>
  set_header_labels(
    Attribute  = "Attribute",
    own_base   = "Baseline",
    own_inter  = "With price\u00d7attr",
    own_diff   = "\u0394 Difference",
    rent_base  = "Baseline",
    rent_inter = "With price\u00d7attr",
    rent_diff  = "\u0394 Difference"
  ) |>
  add_header_row(
    values    = c("", "Owners (SEK/month)", "Renters (SEK/month)"),
    colwidths = c(1, 3, 3)
  ) |>
  bold(part = "header") |>
  align(j = 2:7, align = "center") |>
  align(j = 2:7, align = "center", part = "header") |>
  colformat_int(j = c(4, 7)) |>
  bg(j = c(4, 7), bg = "#f0f0f0") |>
  color(i = ~ abs(own_diff)  > 100, j = 4, color = "#c0392b") |>
  color(i = ~ abs(rent_diff) > 100, j = 7, color = "#c0392b") |>
  autofit() |>
  theme_booktabs() |>
  add_footer_lines(paste0(
    "MWTP = \u2212(\u03b2_attribute / \u03b2_price) \u00d7 scaler, in SEK/month.\n",
    "Scaler: 10% of median monthly housing cost (owners: 10,000 SEK; renters: 9,000 SEK).\n",
    "95% confidence intervals (in parentheses) estimated via non-parametric bootstrap ",
    "(B = ", N_BOOT, " resamples of respondents; percentile method).\n",
    "Baseline: standard mixed logit. With price\u00d7attr: includes price \u00d7 attribute interaction terms.\n",
    "\u0394 Difference: point estimate (interaction model) \u2212 (baseline).\n",
    "Large differences (|diff| > 100 SEK, shown in red) suggest the price\u00d7attribute\n",
    "interaction term is absorbing part of the attribute effect."
  )) |>
  set_caption(caption = paste0(
    "MWTP comparison: baseline vs price \u00d7 attribute interaction model ",
    "(SEK/month, bootstrap 95% CIs)"
  ))

save_as_html(ft_mwtp,
             path = here("output/tables", "table_reviewer_price_attr_mwtp_boot.html"))
message("MWTP comparison table (bootstrap) saved to output/tables/table_reviewer_price_attr_mwtp_boot.html")
