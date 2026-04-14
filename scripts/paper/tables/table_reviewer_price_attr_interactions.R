# table_reviewer_price_attr_interactions.R
#
# Reviewer request: interact the price coefficient with each attribute level to
# examine whether price sensitivity varies across attributes.
#
# Two outputs saved to output/tables/:
#   (A) table_reviewer_price_attr_coefs.html  — the raw coefficient grid the
#       reviewer requested (price × attribute terms for owners and renters)
#   (B) table_reviewer_price_attr_mwtp.html   — MWTP comparison table from the
#       same model, which is the interpretable answer to the reviewer's question
#
# NOTE ON INTERPRETABILITY
# ─────────────────────────────────────────────────────────────────────────────
# The price × attribute coefficient (β_pa) tells you how the marginal
# disutility of a price increase changes when a particular attribute level is
# present. In isolation these coefficients are difficult to interpret because:
#   • The sign and magnitude depend on scale, not just preference
#   • They do not directly tell you "how much someone values attribute X"
# The MWTP ratio −(β_a + β_pa·D) / (β_p + β_pa·D) is the correct monetary
# expression of that valuation and is produced in part (B).
# ─────────────────────────────────────────────────────────────────────────────
#
# Models saved to output/models/:
#   mxl_price_attr_own.rds   — owners
#   mxl_price_attr_rent.rds  — renters
# Models are only estimated if the .rds files do not already exist.

library(pacman)
p_load(here, tidyverse, haven, logitr, broom, flextable, dplyr, stringr, purrr)

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
  # Create dummy variables for each attribute level
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
  # Price × attribute interaction terms
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

# ── 2. Estimate models (skip if already saved) ────────────────────────────────

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
  message("Estimating owner model with price × attribute interactions …")
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
  message("Estimating renter model with price × attribute interactions …")
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
  if (is.na(est)) return("–")
  paste0(sprintf("%.2f", est), sig_stars(p), "\n(", sprintf("%.2f", se), ")")
}

# ── 4. Build grid table (A): full coefficient grid ───────────────────────────
#
# Three clearly labelled sections:
#   (1) Price — the baseline price_num coefficient
#   (2) Attribute coefficients — main effects for each attribute level
#   (3) Price × attribute interactions — the cross terms requested by the reviewer

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

# Helper: pull one formatted cell from a tidy df by term name
pull_cell <- function(tdf, term_name) {
  r <- tdf[tdf$term == term_name, ]
  if (nrow(r) == 0) return("–")
  fmt_coef(r$estimate[1], r$std.error[1], r$p.value[1])
}

# Section 1: price coefficient (1 row, shaded)
price_section <- tibble(
  Section   = "Price",
  Attribute = "Price",
  Owner     = pull_cell(tdf_own,  "price_num"),
  Renter    = pull_cell(tdf_rent, "price_num")
)

# Section 2: main attribute coefficients (8 rows)
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

# Section 3: price × attribute interaction terms (8 rows)
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

# Row indices for section dividers
n_price <- nrow(price_section)                          # 1
n_attr  <- n_price + nrow(attr_section)                 # 9
n_total <- n_attr  + nrow(inter_section)                # 17

ft_grid <- flextable(grid_df |> select(-Section)) |>
  add_header_row(values = c("", "Owners", "Renters"), colwidths = c(1, 1, 1)) |>
  bold(part = "header") |>
  align(j = 2:3, align = "center") |>
  align(j = 2:3, align = "center", part = "header") |>
  # Section label rows — bold + light background
  bold(i = 1) |>
  bg(i = 1, bg = "#e8e8e8") |>
  bold(i = n_price + 1) |>
  bg(i = seq(n_price + 1, n_attr), bg = "#f7f7f7") |>
  bold(i = n_attr + 1) |>
  bg(i = seq(n_attr + 1, n_total), bg = "#eef4fb") |>
  # Top border before each section
  hline(i = n_price,     border = officer::fp_border(width = 1)) |>
  hline(i = n_attr,      border = officer::fp_border(width = 1)) |>
  autofit() |>
  theme_booktabs() |>
  add_footer_lines(paste0(
    "Mixed logit with correlated random parameters. Standard errors in parentheses.\n",
    "Significance: * p<0.05  ** p<0.01  *** p<0.001.\n\n",
    "Attribute coefficients: mean utility of each attribute level relative to its reference.\n",
    "Price \u00d7 attribute interactions: how the marginal disutility of price changes when a\n",
    "given attribute level is present. These are the terms the reviewer requested.\n",
    "Note: these interaction coefficients are not directly interpretable as willingness\n",
    "to pay — see the MWTP table for the monetised valuation of each attribute."
  )) |>
  set_caption(caption = "Coefficient grid: attribute effects and price \u00d7 attribute interactions")

save_as_html(ft_grid,
             path = here("output/tables", "table_reviewer_price_attr_coefs.html"))
message("Grid table saved to output/tables/table_reviewer_price_attr_coefs.html")

# ── 5. Build MWTP table (B): the interpretable counterpart ───────────────────

scaler_own  <- 0.10 * 10000
scaler_rent <- 0.10 * 9000
cost_name   <- "price_num"

compute_mwtp <- function(tdf, scaler) {
  beta_p <- tdf$estimate[tdf$term == cost_name]

  inter_map |>
    rowwise() |>
    mutate(
      beta_a = { r <- tdf[tdf$term == attr_term,  ]; if (nrow(r) > 0) r$estimate[1] else NA_real_ },
      mwtp   = if (!is.na(beta_a)) round(-(beta_a / beta_p) * scaler) else NA_integer_
    ) |>
    ungroup() |>
    select(label = attr_label, mwtp)
}

mwtp_own_new  <- compute_mwtp(tdf_own,  scaler_own)
mwtp_rent_new <- compute_mwtp(tdf_rent, scaler_rent)

# Load baseline MWTP for comparison (from Table 4)
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

mwtp_base <- inter_map |>
  rowwise() |>
  mutate(
    base_term  = base_attr_map[[inter_term]],
    beta_a_own  = { r <- tdf_own_base[tdf_own_base$term  == base_term, ]; if (nrow(r) > 0) r$estimate[1] else NA_real_ },
    beta_p_own  = tdf_own_base$estimate[tdf_own_base$term  == cost_name],
    beta_a_rent = { r <- tdf_rent_base[tdf_rent_base$term == base_term, ]; if (nrow(r) > 0) r$estimate[1] else NA_real_ },
    beta_p_rent = tdf_rent_base$estimate[tdf_rent_base$term == cost_name],
    mwtp_own_base  = round(-(beta_a_own  / beta_p_own)  * scaler_own),
    mwtp_rent_base = round(-(beta_a_rent / beta_p_rent) * scaler_rent)
  ) |>
  ungroup()

mwtp_compare <- tibble(
  Attribute        = inter_map$attr_label,
  own_base         = mwtp_base$mwtp_own_base,
  own_inter        = mwtp_own_new$mwtp,
  own_diff         = mwtp_own_new$mwtp - mwtp_base$mwtp_own_base,
  rent_base        = mwtp_base$mwtp_rent_base,
  rent_inter       = mwtp_rent_new$mwtp,
  rent_diff        = mwtp_rent_new$mwtp - mwtp_base$mwtp_rent_base
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
  colformat_int(j = 2:7) |>
  # Shade the difference columns to make them stand out
  bg(j = c(4, 7), bg = "#f0f0f0") |>
  # Highlight large differences in red
  color(i = ~ abs(own_diff)  > 100, j = 4, color = "#c0392b") |>
  color(i = ~ abs(rent_diff) > 100, j = 7, color = "#c0392b") |>
  autofit() |>
  theme_booktabs() |>
  add_footer_lines(paste0(
    "MWTP = \u2212(\u03b2_attribute / \u03b2_price) \u00d7 scaler, in SEK/month.\n",
    "Scaler: 10% of median monthly housing cost (owners: 10,000 SEK; renters: 9,000 SEK).\n",
    "Baseline: standard mixed logit (Table 4). With price\u00d7attr: model includes\n",
    "price \u00d7 attribute interaction terms. \u0394 Difference: (interaction model) \u2212 (baseline).\n",
    "Large differences (|diff| > 100 SEK, shown in red) suggest the price\u00d7attribute\n",
    "interaction term is absorbing part of the attribute effect."
  )) |>
  set_caption(caption = "MWTP comparison: baseline vs price \u00d7 attribute interaction model (SEK/month)")

save_as_html(ft_mwtp,
             path = here("output/tables", "table_reviewer_price_attr_mwtp.html"))
message("MWTP comparison table saved to output/tables/table_reviewer_price_attr_mwtp.html")
