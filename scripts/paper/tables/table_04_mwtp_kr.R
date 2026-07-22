# table_04_mwtp_kr.R
# Table 4: MWTP estimates — renters and owners.
# Identical to table_04_mwtp.R but uses the Krinsky-Robb (KR) simulation method
# for confidence intervals instead of the delta method.
# Loads pre-saved baseline models; does NOT re-estimate.
# Output: output/tables/table_04_mwtp_kr.html

library(pacman)
p_load(here, broom, dplyr, stringr, flextable, tibble, MASS, purrr)

# Load models ----
mxl_renter <- readRDS(here("output/models", "mxl_renter_base.rds"))
mxl_owner  <- readRDS(here("output/models", "mxl_owner_base.rds"))

# Scalers (10 % of median monthly housing cost -> SEK/month) ----
scaler_rent <- 0.10 * 9000   # = 900 SEK
scaler_own  <- 0.10 * 10000  # = 1000 SEK

price_term <- "price_num"

# Krinsky-Robb MWTP ----
# Draw R vectors from MVN(beta_hat, Sigma_hat); compute MWTP for each draw;
# report original point estimate with 2.5/97.5 percentile CIs.
N_KR <- 2000

compute_mwtp_kr <- function(model, scale_value, n_draws = N_KR, seed = 4821) {
  t_means <- tidy(model) |> filter(!str_starts(term, "sd_"))
  mu      <- setNames(t_means$estimate, t_means$term)
  vc      <- vcov(model)

  # Subset vcov to mean parameters only (full matrix includes SD terms)
  vc <- vc[names(mu), names(mu)]

  set.seed(seed)
  draws <- MASS::mvrnorm(n_draws, mu = mu, Sigma = vc)

  attrs <- setdiff(t_means$term, price_term)

  map_dfr(attrs, function(a) {
    beta_a <- mu[[a]]
    beta_p <- mu[[price_term]]

    # Point estimate (same as delta method)
    mwtp <- -(beta_a / beta_p) * scale_value

    # KR simulation CIs
    mwtp_draws <- -(draws[, a] / draws[, price_term]) * scale_value
    mwtp_lo    <- quantile(mwtp_draws, 0.025, names = FALSE)
    mwtp_hi    <- quantile(mwtp_draws, 0.975, names = FALSE)

    tibble(
      attribute = a,
      mwtp      = round(as.numeric(mwtp), 0),
      mwtp_lo   = round(mwtp_lo, 0),
      mwtp_hi   = round(mwtp_hi, 0)
    )
  })
}

mwtp_rent <- compute_mwtp_kr(mxl_renter, scaler_rent)
mwtp_own  <- compute_mwtp_kr(mxl_owner,  scaler_own)

# Attribute labels ----
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

# Build display table ----
fmt_mwtp <- function(v, lo, hi) sprintf("%d\n(%d, %d)", v, lo, hi)

idx <- match(mwtp_own$attribute, mwtp_rent$attribute)

display <- tibble(
  Attribute = label_map[mwtp_own$attribute],
  `Owners`  = fmt_mwtp(mwtp_own$mwtp, mwtp_own$mwtp_lo, mwtp_own$mwtp_hi),
  `Renters` = fmt_mwtp(mwtp_rent$mwtp[idx], mwtp_rent$mwtp_lo[idx], mwtp_rent$mwtp_hi[idx])
) |>
  filter(!is.na(Attribute))

# Build flextable ----
ft <- flextable(display) |>
  bold(part = "header") |>
  align(part = "header", align = "center") |>
  align(j = 2:3, align = "center") |>
  autofit() |>
  theme_booktabs() |>
  add_footer_lines(
    paste0(
      "MWTP in SEK/month (10% of median monthly housing cost: owners 10,000 SEK, renters 9,000 SEK). ",
      "95% confidence intervals in brackets, estimated via Krinsky-Robb simulation (R = ", N_KR, ")."
    )
  ) |>
  set_caption(caption = "Table 4. MWTP estimates: renters and owners (Krinsky-Robb CIs)")

# Save as HTML ----
save_as_html(ft, path = here("output/tables", "table_04_mwtp_kr.html"))

message("Table 4 (KR) saved to output/tables/table_04_mwtp_kr.html")
