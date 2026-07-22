# table_reviewer_price_attr_interactions_kr.R
#
# Reviewer request: interact the price coefficient with each attribute level.
# Identical to table_reviewer_price_attr_interactions.R but uses the Krinsky-Robb (KR)
# simulation method for MWTP confidence intervals instead of the delta method.
#
# Two outputs saved to output/tables/:
#   (A) table_reviewer_price_attr_coefs_kr.html  — coefficient grid (unchanged from original)
#   (B) table_reviewer_price_attr_mwtp_kr.html   — MWTP with KR 95% CIs
#
# Models are loaded from disk (same pre-saved models as the original script).
# If models do not yet exist, they are estimated first.

library(pacman)
p_load(here, tidyverse, haven, logitr, broom, flextable, MASS, stringr, purrr)
# Explicitly prefer dplyr::select over MASS::select
select <- dplyr::select

N_KR <- 2000  # number of Krinsky-Robb draws

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

# ── 2. Load (or estimate) models ──────────────────────────────────────────────

path_own  <- here("output/models", "mxl_price_attr_own.rds")
path_rent <- here("output/models", "mxl_price_attr_rent.rds")

attr_pars  <- c("green5km", "green500m", "shops5km", "shops500m",
                "trans600", "trans300", "garage", "space")
price_pars <- c("price_num",
                "p_green5km", "p_green500m",
                "p_shops5km", "p_shops500m",
                "p_trans600", "p_trans300",
                "p_garage",   "p_space")

if (!file.exists(path_own)) {
  message("Estimating owner model with price \u00d7 attribute interactions \u2026")
  set.seed(7341)
  mxl_price_attr_own <- logitr(
    data           = df_owner,
    outcome        = "choice",
    obsID          = "obsID",
    panelID        = "panelID",
    pars           = c(attr_pars, price_pars),
    randPars       = setNames(rep("n", length(attr_pars)), attr_pars),
    numMultiStarts = 5,
    drawType       = "sobol",
    numDraws       = 20,
    correlation    = TRUE
  )
  saveRDS(mxl_price_attr_own, path_own)
  message("Owner model saved.")
} else {
  mxl_price_attr_own <- readRDS(path_own)
  message("Owner model loaded from disk.")
}

if (!file.exists(path_rent)) {
  message("Estimating renter model with price \u00d7 attribute interactions \u2026")
  set.seed(2857)
  mxl_price_attr_rent <- logitr(
    data           = df_renter,
    outcome        = "choice",
    obsID          = "obsID",
    panelID        = "panelID",
    pars           = c(attr_pars, price_pars),
    randPars       = setNames(rep("n", length(attr_pars)), attr_pars),
    numMultiStarts = 5,
    drawType       = "sobol",
    numDraws       = 20,
    correlation    = TRUE
  )
  saveRDS(mxl_price_attr_rent, path_rent)
  message("Renter model saved.")
} else {
  mxl_price_attr_rent <- readRDS(path_rent)
  message("Renter model loaded from disk.")
}

# ── 3. Extract coefficients ───────────────────────────────────────────────────

tdf_own  <- broom::tidy(mxl_price_attr_own)  |> filter(!str_starts(term, "sd_"))
tdf_rent <- broom::tidy(mxl_price_attr_rent) |> filter(!str_starts(term, "sd_"))

sig_stars <- function(p) {
  case_when(
    is.na(p)  ~ "",
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ ""
  )
}

fmt_coef <- function(est, se, p) {
  if (is.na(est)) return("\u2013")
  paste0(sprintf("%.2f", est), sig_stars(p), "\n(", sprintf("%.2f", se), ")")
}

# ── 4. Build grid table (A): full coefficient grid (identical to original) ────

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
  Section   = "Price",
  Attribute = "Price",
  Owner     = pull_cell(tdf_own,  "price_num"),
  Renter    = pull_cell(tdf_rent, "price_num")
)

attr_section <- inter_map |>
  rowwise() |>
  mutate(
    Section   = "Attribute coefficients",
    Attribute = attr_label,
    Owner     = pull_cell(tdf_own,  attr_term),
    Renter    = pull_cell(tdf_rent, attr_term)
  ) |>
  ungroup() |>
  select(Section, Attribute, Owner, Renter)

inter_section <- inter_map |>
  rowwise() |>
  mutate(
    Section   = "Price \u00d7 attribute interactions",
    Attribute = paste0("Price \u00d7 ", attr_label),
    Owner     = pull_cell(tdf_own,  inter_term),
    Renter    = pull_cell(tdf_rent, inter_term)
  ) |>
  ungroup() |>
  select(Section, Attribute, Owner, Renter)

grid_df <- bind_rows(price_section, attr_section, inter_section)

n_price <- nrow(price_section)
n_attr  <- n_price + nrow(attr_section)
n_total <- n_attr  + nrow(inter_section)

ft_grid <- flextable(grid_df |> select(-Section)) |>
  add_header_row(values = c("", "Owners", "Renters"), colwidths = c(1, 1, 1)) |>
  bold(part = "header") |>
  align(j = 2:3, align = "center") |>
  align(j = 2:3, align = "center", part = "header") |>
  bold(i = 1) |>
  bg(i = 1, bg = "#e8e8e8") |>
  bold(i = n_price + 1) |>
  bg(i = seq(n_price + 1, n_attr), bg = "#f7f7f7") |>
  bold(i = n_attr + 1) |>
  bg(i = seq(n_attr + 1, n_total), bg = "#eef4fb") |>
  hline(i = n_price, border = officer::fp_border(width = 1)) |>
  hline(i = n_attr,  border = officer::fp_border(width = 1)) |>
  autofit() |>
  theme_booktabs() |>
  add_footer_lines(paste0(
    "Mixed logit with correlated random parameters. Standard errors in parentheses.\n",
    "Significance: * p<0.05  ** p<0.01  *** p<0.001.\n\n",
    "Attribute coefficients: mean utility of each attribute level relative to its reference.\n",
    "Price \u00d7 attribute interactions: how the marginal disutility of price changes when a\n",
    "given attribute level is present. These are the terms the reviewer requested.\n",
    "Note: these interaction coefficients are not directly interpretable as willingness\n",
    "to pay \u2014 see the MWTP table for the monetised valuation of each attribute."
  )) |>
  set_caption(caption = "Coefficient grid: attribute effects and price \u00d7 attribute interactions")

save_as_html(ft_grid,
             path = here("output/tables", "table_reviewer_price_attr_coefs_kr.html"))
message("Grid table (KR) saved to output/tables/table_reviewer_price_attr_coefs_kr.html")

# ── 5. Build MWTP table (B) with Krinsky-Robb CIs ────────────────────────────

scaler_own  <- 0.10 * 10000
scaler_rent <- 0.10 * 9000
cost_name   <- "price_num"

# KR MWTP for the price×attr interaction model: MWTP = -(beta_a / beta_p) * scaler
compute_mwtp_kr <- function(tdf, model_obj, scaler, seed) {
  beta_p <- tdf$estimate[tdf$term == cost_name]
  mu     <- setNames(tdf$estimate, tdf$term)
  vc     <- tryCatch({
    vc_full <- vcov(model_obj)
    vc_full[names(mu), names(mu)]
  }, error = function(e) NULL)

  set.seed(seed)
  draws <- if (!is.null(vc)) MASS::mvrnorm(N_KR, mu = mu, Sigma = vc) else NULL

  inter_map |>
    rowwise() |>
    mutate(
      beta_a = { r <- tdf[tdf$term == attr_term, ]; if (nrow(r) > 0) r$estimate[1] else NA_real_ },
      mwtp   = if (!is.na(beta_a)) round(-(beta_a / beta_p) * scaler) else NA_integer_,
      mwtp_lo = if (!is.null(draws) && !is.na(beta_a) && attr_term %in% colnames(draws)) {
        mwtp_sim <- -(draws[, attr_term] / draws[, cost_name]) * scaler
        round(quantile(mwtp_sim, 0.025, names = FALSE))
      } else NA_integer_,
      mwtp_hi = if (!is.null(draws) && !is.na(beta_a) && attr_term %in% colnames(draws)) {
        mwtp_sim <- -(draws[, attr_term] / draws[, cost_name]) * scaler
        round(quantile(mwtp_sim, 0.975, names = FALSE))
      } else NA_integer_
    ) |>
    ungroup() |>
    select(label = attr_label, mwtp, mwtp_lo, mwtp_hi)
}

mwtp_own_new  <- compute_mwtp_kr(tdf_own,  mxl_price_attr_own,  scaler_own,  seed = 4153)
mwtp_rent_new <- compute_mwtp_kr(tdf_rent, mxl_price_attr_rent, scaler_rent, seed = 9027)

# KR MWTP for the baseline model
mxl_own_base  <- readRDS(here("output/models", "mxl_owner_base.rds"))
mxl_rent_base <- readRDS(here("output/models", "mxl_renter_base.rds"))

tdf_own_base  <- broom::tidy(mxl_own_base)  |> filter(!str_starts(term, "sd_"))
tdf_rent_base <- broom::tidy(mxl_rent_base) |> filter(!str_starts(term, "sd_"))

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

# KR draws for baseline models
build_kr_draws <- function(tdf, model_obj, seed) {
  mu  <- setNames(tdf$estimate, tdf$term)
  vc  <- tryCatch({
    vc_full <- vcov(model_obj)
    vc_full[names(mu), names(mu)]
  }, error = function(e) NULL)
  set.seed(seed)
  if (!is.null(vc)) MASS::mvrnorm(N_KR, mu = mu, Sigma = vc) else NULL
}

draws_own_base  <- build_kr_draws(tdf_own_base,  mxl_own_base,  seed = 6284)
draws_rent_base <- build_kr_draws(tdf_rent_base, mxl_rent_base, seed = 1739)

mwtp_base <- inter_map |>
  rowwise() |>
  mutate(
    base_term   = base_attr_map[[inter_term]],
    beta_a_own  = { r <- tdf_own_base[tdf_own_base$term   == base_term, ]; if (nrow(r) > 0) r$estimate[1] else NA_real_ },
    beta_p_own  = tdf_own_base$estimate[tdf_own_base$term  == cost_name],
    beta_a_rent = { r <- tdf_rent_base[tdf_rent_base$term == base_term, ]; if (nrow(r) > 0) r$estimate[1] else NA_real_ },
    beta_p_rent = tdf_rent_base$estimate[tdf_rent_base$term == cost_name],
    mwtp_own_base  = round(-(beta_a_own  / beta_p_own)  * scaler_own),
    mwtp_rent_base = round(-(beta_a_rent / beta_p_rent) * scaler_rent),
    mwtp_own_base_lo = if (!is.null(draws_own_base) && !is.na(beta_a_own) && base_term %in% colnames(draws_own_base)) {
      sim <- -(draws_own_base[, base_term] / draws_own_base[, cost_name]) * scaler_own
      round(quantile(sim, 0.025, names = FALSE))
    } else NA_integer_,
    mwtp_own_base_hi = if (!is.null(draws_own_base) && !is.na(beta_a_own) && base_term %in% colnames(draws_own_base)) {
      sim <- -(draws_own_base[, base_term] / draws_own_base[, cost_name]) * scaler_own
      round(quantile(sim, 0.975, names = FALSE))
    } else NA_integer_,
    mwtp_rent_base_lo = if (!is.null(draws_rent_base) && !is.na(beta_a_rent) && base_term %in% colnames(draws_rent_base)) {
      sim <- -(draws_rent_base[, base_term] / draws_rent_base[, cost_name]) * scaler_rent
      round(quantile(sim, 0.025, names = FALSE))
    } else NA_integer_,
    mwtp_rent_base_hi = if (!is.null(draws_rent_base) && !is.na(beta_a_rent) && base_term %in% colnames(draws_rent_base)) {
      sim <- -(draws_rent_base[, base_term] / draws_rent_base[, cost_name]) * scaler_rent
      round(quantile(sim, 0.975, names = FALSE))
    } else NA_integer_
  ) |>
  ungroup()

# Helper: format as "X\n(lo, hi)"
fmt_mwtp <- function(v, lo, hi) {
  if (is.na(v)) return("\u2013")
  sprintf("%d\n(%d, %d)", as.integer(v), as.integer(lo), as.integer(hi))
}

mwtp_compare <- tibble(
  Attribute    = inter_map$attr_label,
  own_base     = mapply(fmt_mwtp, mwtp_base$mwtp_own_base,   mwtp_base$mwtp_own_base_lo,   mwtp_base$mwtp_own_base_hi),
  own_inter    = mapply(fmt_mwtp, mwtp_own_new$mwtp,         mwtp_own_new$mwtp_lo,          mwtp_own_new$mwtp_hi),
  own_diff     = mwtp_own_new$mwtp - mwtp_base$mwtp_own_base,
  rent_base    = mapply(fmt_mwtp, mwtp_base$mwtp_rent_base,  mwtp_base$mwtp_rent_base_lo,  mwtp_base$mwtp_rent_base_hi),
  rent_inter   = mapply(fmt_mwtp, mwtp_rent_new$mwtp,        mwtp_rent_new$mwtp_lo,         mwtp_rent_new$mwtp_hi),
  rent_diff    = mwtp_rent_new$mwtp - mwtp_base$mwtp_rent_base
)

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
    "95% confidence intervals (in parentheses) estimated via Krinsky-Robb simulation (R = ", N_KR, ").\n",
    "Baseline: standard mixed logit (Table 4). With price\u00d7attr: model includes\n",
    "price \u00d7 attribute interaction terms. \u0394 Difference: point estimate difference (interaction) \u2212 (baseline).\n",
    "Large differences (|diff| > 100 SEK, shown in red) suggest the price\u00d7attribute\n",
    "interaction term is absorbing part of the attribute effect."
  )) |>
  set_caption(caption = paste0(
    "MWTP comparison: baseline vs price \u00d7 attribute interaction model (SEK/month, KR 95% CIs)"
  ))

save_as_html(ft_mwtp,
             path = here("output/tables", "table_reviewer_price_attr_mwtp_kr.html"))
message("MWTP comparison table (KR) saved to output/tables/table_reviewer_price_attr_mwtp_kr.html")
