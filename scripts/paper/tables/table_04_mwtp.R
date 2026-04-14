# table_04_mwtp.R
# Table 4: MWTP estimates — renters and owners.
# Loads pre-saved baseline models; does NOT re-estimate.
# Output: output/tables/table_04_mwtp.html

library(pacman)
p_load(here, broom, dplyr, stringr, flextable, tibble)

# Load models ----
mxl_renter <- readRDS(here("output/models", "mxl_renter_base.rds"))
mxl_owner  <- readRDS(here("output/models", "mxl_owner_base.rds"))

# Scalers (10 % of median monthly housing cost -> SEK/month) ----
scaler_rent <- 0.10 * 9000   # = 900 SEK
scaler_own  <- 0.10 * 10000  # = 1000 SEK

price_term <- "price_num"

# Delta-method MWTP and MRS ----
compute_mwtp_mrs <- function(model, scale_value) {
  t_means <- tidy(model) |> filter(!str_starts(term, "sd_"))
  vc      <- vcov(model)
  attrs   <- setdiff(t_means$term, price_term)

  purrr::map_dfr(attrs, function(a) {
    beta_a <- t_means$estimate[t_means$term == a]
    beta_p <- t_means$estimate[t_means$term == price_term]

    mrs  <- -beta_a / beta_p
    mwtp <- mrs * scale_value

    # Delta method SE
    grad          <- c(-1 / beta_p, beta_a / (beta_p^2))
    names(grad)   <- c(a, price_term)
    V_sub         <- vc[c(a, price_term), c(a, price_term)]
    se_mrs        <- as.numeric(sqrt(t(grad) %*% V_sub %*% grad))
    se_mwtp       <- se_mrs * scale_value

    tibble(
      attribute = a,
      mrs       = as.numeric(mrs),
      mrs_lo    = mrs - 1.96 * se_mrs,
      mrs_hi    = mrs + 1.96 * se_mrs,
      mwtp      = round(as.numeric(mwtp), 0),
      mwtp_lo   = round(mwtp - 1.96 * se_mwtp, 0),
      mwtp_hi   = round(mwtp + 1.96 * se_mwtp, 0)
    )
  })
}

mwtp_rent <- compute_mwtp_mrs(mxl_renter, scaler_rent)
mwtp_own  <- compute_mwtp_mrs(mxl_owner,  scaler_own)

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
  Attribute     = label_map[mwtp_own$attribute],
  `Owners`      = fmt_mwtp(mwtp_own$mwtp, mwtp_own$mwtp_lo, mwtp_own$mwtp_hi),
  `Renters`     = fmt_mwtp(mwtp_rent$mwtp[idx], mwtp_rent$mwtp_lo[idx], mwtp_rent$mwtp_hi[idx])
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
    "MWTP in SEK/month (10% of median monthly housing cost: owners 10,000 SEK, renters 9,000 SEK). 
95% confidence intervals in brackets, estimated via delta method."
  ) |>
  set_caption(caption = "Table 4. MWTP estimates: renters and owners")

# Save as HTML ----
save_as_html(ft, path = here("output/tables", "table_04_mwtp.html"))

message("Table 4 saved to output/tables/table_04_mwtp.html")
