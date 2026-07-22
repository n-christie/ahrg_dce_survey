# table_07_mwtp_ci_health.R
# Table 7 (CI version): MWTP with 95% confidence intervals — health interaction.
# Shows reference (Not very good health) and Very good health MWTPs side by side.
# 95% CIs via delta method. No significance stars.
# Output: output/tables_ci/table_07_mwtp_ci_health.html

library(pacman)
p_load(here, broom, dplyr, kableExtra)

dir.create(here("output/tables_ci"), showWarnings = FALSE, recursive = TRUE)

# Load models ----
mxl_own  <- readRDS(here("output/models", "mxl_health_own.rds"))
mxl_rent <- readRDS(here("output/models", "mxl_health_rent.rds"))

cost_name   <- "price_num"
scaler_own  <- 0.10 * 10000   # SEK/month — owners
scaler_rent <- 0.10 * 9000    # SEK/month — renters
inter_group <- "Good"
ref_label   <- "Not very good health"
inter_label <- "Very good health"

# Term definitions ----
main_attrs <- c(
  "dist_green5km", "dist_green500m",
  "dist_shops5km", "dist_shops500m",
  "dist_trans600", "dist_trans300",
  "park_garage",   "park_space"
)

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

label_map <- c(
  "dist_green5km"  = "Green space: 5 km (vs 15 km)",
  "dist_green500m" = "Green space: 500 m (vs 15 km)",
  "dist_shops5km"  = "Shops: 5 km (vs 15 km)",
  "dist_shops500m" = "Shops: 500 m (vs 15 km)",
  "dist_trans600"  = "Transit stop: 600 m (vs 900 m)",
  "dist_trans300"  = "Transit stop: 300 m (vs 900 m)",
  "park_garage"    = "Parking: reserved garage (vs none)",
  "park_space"     = "Parking: reserved space (vs none)"
)

# Helpers ----
grab_term <- function(tidy_df, term) {
  row <- tidy_df[tidy_df$term == term, ]
  if (nrow(row) == 0) return(c(NA_real_, NA_real_))
  c(row$estimate[1], row$std.error[1])
}

# Returns c(mwtp, lower_95, upper_95) via delta method.
compute_mwtp_ci <- function(attr, group, tidy_df, V, scaler) {
  beta_a <- grab_term(tidy_df, attr)[1]
  beta_c <- grab_term(tidy_df, cost_name)[1]

  a_g_term <- if (group == "base") character(0) else
    names(interaction_to_base)[interaction_to_base == attr &
                                 grepl(group, names(interaction_to_base))]
  c_g_term <- if (group == "base") character(0) else
    names(interaction_to_base)[interaction_to_base == cost_name &
                                 grepl(group, names(interaction_to_base))]

  beta_a_g <- if (length(a_g_term) > 0) grab_term(tidy_df, a_g_term)[1] else 0
  beta_c_g <- if (length(c_g_term) > 0) grab_term(tidy_df, c_g_term)[1] else 0

  if (any(is.na(c(beta_a, beta_c)))) return(c(NA_real_, NA_real_, NA_real_))

  A   <- beta_a + ifelse(group == "base", 0, beta_a_g)
  C   <- beta_c + ifelse(group == "base", 0, beta_c_g)
  val <- -(A / C) * scaler

  terms <- unique(c(attr, cost_name,
                    if (length(a_g_term) > 0) a_g_term,
                    if (length(c_g_term) > 0) c_g_term))

  if (is.null(V) || !all(terms %in% rownames(V))) return(c(val, NA_real_, NA_real_))

  grad <- numeric(length(terms))
  grad[terms == attr]      <- -1 / C * scaler
  grad[terms == cost_name] <-  (A / C^2) * scaler
  if (length(a_g_term) > 0 && any(terms == a_g_term))
    grad[terms == a_g_term] <- -1 / C * scaler
  if (length(c_g_term) > 0 && any(terms == c_g_term))
    grad[terms == c_g_term] <-  (A / C^2) * scaler

  Vsub <- V[terms, terms, drop = FALSE]
  var  <- as.numeric(t(grad) %*% Vsub %*% grad)
  if (!is.finite(var) || var < 0) return(c(val, NA_real_, NA_real_))

  se <- sqrt(var)
  c(val, val - 1.96 * se, val + 1.96 * se)
}

fmt_ci <- function(v, lo, hi) {
  if (is.na(v)) return("–")
  fmt <- function(x) formatC(round(x), format = "d", big.mark = ",")
  if (is.na(lo) || is.na(hi)) return(fmt(v))
  paste0(fmt(v), "<br>[", fmt(lo), ", ", fmt(hi), "]")
}

build_cols <- function(model, scaler) {
  tdf <- broom::tidy(model)
  V   <- tryCatch(vcov(model), error = function(e) NULL)
  ref_col   <- character(length(main_attrs))
  inter_col <- character(length(main_attrs))
  for (i in seq_along(main_attrs)) {
    a <- main_attrs[i]
    r <- compute_mwtp_ci(a, "base",      tdf, V, scaler)
    s <- compute_mwtp_ci(a, inter_group, tdf, V, scaler)
    ref_col[i]   <- fmt_ci(r[1], r[2], r[3])
    inter_col[i] <- fmt_ci(s[1], s[2], s[3])
  }
  list(ref = ref_col, inter = inter_col)
}

# Build and render ----
own  <- build_cols(mxl_own,  scaler_own)
rent <- build_cols(mxl_rent, scaler_rent)

tab_df <- data.frame(
  Attribute  = unname(label_map[main_attrs]),
  own_ref    = own$ref,
  own_inter  = own$inter,
  rent_ref   = rent$ref,
  rent_inter = rent$inter,
  check.names = FALSE, stringsAsFactors = FALSE
)
names(tab_df) <- c("Attribute", ref_label, inter_label, ref_label, inter_label)

tbl_html <- kbl(tab_df,
    format  = "html",
    escape  = FALSE,
    caption = "Table 7. MWTP by health status (SEK/month; 95% CI in brackets).",
    align   = c("l", "r", "r", "r", "r")) |>
  add_header_above(c(" " = 1, "Owners" = 2, "Renters" = 2)) |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, font_size = 13) |>
  footnote(
    general = paste0(
      "MWTP in SEK/month. 95% CIs computed via the delta method. ",
      "Reference group: Not very good health."
    ),
    general_title = "Note: "
  )

out_path <- here("output/tables_ci", "table_07_mwtp_ci_health.html")
writeLines(
  c('<!DOCTYPE html><html><head><meta charset="utf-8">',
    '<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css">',
    '</head><body style="padding:20px">',
    as.character(tbl_html),
    '</body></html>'),
  con = out_path
)

message("Table 7 CI saved to ", out_path)
