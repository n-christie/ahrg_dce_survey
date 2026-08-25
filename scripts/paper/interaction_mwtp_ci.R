# interaction_mwtp_ci.R
# Computes MWTP with 95% CI (delta method) for Supplementary Tables S1
# (Age), S2 (Sex), and S3 (Health), for both the base-group rows and the
# interaction-group rows, and for owners and renters.
#
# Why this exists: S1/S2's source scripts (interaction_regs_age.R,
# interaction_regs_owner_sex.R) already compute an MWTP standard error
# internally via mwtp_for_attr_group(), but their final texreg/screenreg
# output explicitly discards it and shows only a point estimate. S3
# (Health) had NO source script at all in the repo -- interaction_regs_
# owner_health.R only fits and saves mxl_health_own.rds/mxl_health_rent.rds;
# whatever built the table that ended up in supplementary_material.docx was
# never committed. This script reconstructs that computation from the saved
# models and outputs 95% CIs for all three tables, so that Supplementary
# Tables S1-S3 report MWTP the same way as Table 3/4 (point estimate with a
# 95% CI), instead of the previous mix of point-only / SE-only formats.
#
# The point estimates reproduced here were checked against the existing
# values in supplementary_material.docx before trusting the CIs (e.g. S1
# owner green-space-5km base MWTP: 183 here vs 183.48 in the docx; S3 owner
# green-space-5km base MWTP: 147 here vs 146.68 in the docx) -- matches
# confirm this replicates the same underlying delta-method calculation.
#
# Saves: output/s1_age_mwtp_ci.csv, output/s2_sex_mwtp_ci.csv,
#        output/s3_health_mwtp_ci.csv

library(pacman)
p_load(logitr, broom, dplyr, here, purrr, tibble, readr)

main_attrs <- c("dist_green5km", "dist_green500m", "dist_shops5km", "dist_shops500m",
                "dist_trans600", "dist_trans300", "park_garage", "park_space")

grab_term <- function(tidy_df, term) {
  row <- tidy_df[tidy_df$term == term, ]
  if (nrow(row) == 0) return(c(NA_real_, NA_real_, NA_real_))
  c(row$estimate[1], row$std.error[1], row$p.value[1])
}

# Mirrors mwtp_for_attr_group() in interaction_regs_age.R /
# interaction_regs_owner_sex.R: for group == "base", MWTP uses only the
# attribute's own coefficient; for the interaction group (e.g. "Old"),
# MWTP uses the COMBINED coefficient (base + interaction), which is the
# subgroup's own total MWTP -- not just the interaction increment. The
# "Coef." column shown alongside it in the table is the raw interaction
# term alone, so Coef. and MWTP on an interaction row are not derived from
# each other the way they are on a base row. This asymmetry predates this
# script; it is preserved here deliberately, not fixed, since the task was
# to standardize the *uncertainty reporting* (SE -> 95% CI), not to
# redesign what the table's columns mean.
mwtp_for_attr_group <- function(attr, group, interaction_to_base, tidy_df, V, scaler, cost_name) {
  beta_a <- grab_term(tidy_df, attr)[1]
  beta_c <- grab_term(tidy_df, cost_name)[1]
  a_g_term <- if (group == "base") NA_character_ else names(interaction_to_base)[interaction_to_base == attr & grepl(group, names(interaction_to_base))]
  c_g_term <- if (group == "base") NA_character_ else names(interaction_to_base)[interaction_to_base == cost_name & grepl(group, names(interaction_to_base))]
  beta_a_g <- if (length(a_g_term)) grab_term(tidy_df, a_g_term)[1] else 0
  beta_c_g <- if (length(c_g_term)) grab_term(tidy_df, c_g_term)[1] else 0
  if (any(is.na(c(beta_a, beta_c)))) return(c(NA_real_, NA_real_))
  A <- beta_a + ifelse(group == "base", 0, beta_a_g)
  C <- beta_c + ifelse(group == "base", 0, beta_c_g)
  val <- -(A / C) * scaler
  terms <- c(attr, cost_name, if (group != "base") a_g_term else NULL, if (group != "base") c_g_term else NULL)
  terms <- terms[!is.na(terms) & nzchar(terms)]
  if (is.null(V) || !all(terms %in% rownames(V))) return(c(val, NA_real_))
  grad <- numeric(length(terms))
  grad[terms == attr]      <- -1 / C * scaler
  grad[terms == cost_name] <- (A / (C^2)) * scaler
  if (group != "base") {
    if (any(terms == a_g_term)) grad[terms == a_g_term] <- -1 / C * scaler
    if (any(terms == c_g_term)) grad[terms == c_g_term] <- (A / (C^2)) * scaler
  }
  Vsub <- V[terms, terms, drop = FALSE]
  var <- as.numeric(t(grad) %*% Vsub %*% grad)
  se <- if (is.finite(var) && var >= 0) sqrt(var) else NA_real_
  c(val, se)
}

compute_table <- function(model, scaler, group_suffix, tenure_label, interaction_to_base) {
  tdf <- broom::tidy(model)
  V   <- vcov(model)
  rows <- list()
  for (attr in main_attrs) {
    out_base <- mwtp_for_attr_group(attr, "base", interaction_to_base, tdf, V, scaler, "price_num")
    rows[[length(rows) + 1]] <- tibble(tenure = tenure_label, row_type = "base", attribute = attr,
                                        wtp = out_base[1], se = out_base[2])
    out_g <- mwtp_for_attr_group(attr, group_suffix, interaction_to_base, tdf, V, scaler, "price_num")
    rows[[length(rows) + 1]] <- tibble(tenure = tenure_label, row_type = group_suffix, attribute = attr,
                                        wtp = out_g[1], se = out_g[2])
  }
  bind_rows(rows)
}

finalize <- function(df) {
  df %>% mutate(wtp = round(wtp), lower = round(wtp - 1.96 * se), upper = round(wtp + 1.96 * se))
}

# Tenure-specific scalers match baseline_regs.R's Table 3 convention
# (owners: 10% of 10,000 SEK; renters: 10% of 9,000 SEK) -- confirmed by
# reproducing the docx's existing point estimates with these values before
# trusting the CIs.
SCALER_OWNER  <- 1000
SCALER_RENTER <- 900

# --- S1: Age (Old = age >= 72) ---------------------------------------------
interaction_to_base_age <- c(
  "green5km_Old" = "dist_green5km", "green500_Old" = "dist_green500m",
  "shops5km_Old" = "dist_shops5km", "shops500_Old" = "dist_shops500m",
  "trans600_Old" = "dist_trans600", "trans300_Old" = "dist_trans300",
  "park_space_Old" = "park_space", "park_garage_Old" = "park_garage",
  "price_Old" = "price_num"
)
own <- readRDS(here("output/models", "mxl_old_owner.rds"))
ren <- readRDS(here("output/models", "mxl_old_renter.rds"))
s1 <- bind_rows(
  compute_table(own, SCALER_OWNER,  "Old", "Owner",  interaction_to_base_age),
  compute_table(ren, SCALER_RENTER, "Old", "Renter", interaction_to_base_age)
) %>% finalize()
write_csv(s1, here("output", "s1_age_mwtp_ci.csv"))

# --- S2: Sex (Men) -----------------------------------------------------------
interaction_to_base_sex <- c(
  "green5km_Men" = "dist_green5km", "green500_Men" = "dist_green500m",
  "shops5km_Men" = "dist_shops5km", "shops500_Men" = "dist_shops500m",
  "trans600_Men" = "dist_trans600", "trans300_Men" = "dist_trans300",
  "park_space_Men" = "park_space", "park_garage_Men" = "park_garage",
  "price_Men" = "price_num"
)
own <- readRDS(here("output/models", "mxl_sex_owner.rds"))
ren <- readRDS(here("output/models", "mxl_sex_renter.rds"))
s2 <- bind_rows(
  compute_table(own, SCALER_OWNER,  "Men", "Owner",  interaction_to_base_sex),
  compute_table(ren, SCALER_RENTER, "Men", "Renter", interaction_to_base_sex)
) %>% finalize()
write_csv(s2, here("output", "s2_sex_mwtp_ci.csv"))

# --- S3: Health (Good = self-rated health 3-5 vs 1-2) ------------------------
# Reconstructed: interaction_regs_owner_health.R only fits and saves
# mxl_health_own.rds / mxl_health_rent.rds; no table-building script for S3
# existed in the repo prior to this file.
interaction_to_base_health <- c(
  "green5km_Good" = "dist_green5km", "green500_Good" = "dist_green500m",
  "shops5km_Good" = "dist_shops5km", "shops500_Good" = "dist_shops500m",
  "trans600_Good" = "dist_trans600", "trans300_Good" = "dist_trans300",
  "park_space_Good" = "park_space", "park_garage_Good" = "park_garage",
  "price_Good" = "price_num"
)
own <- readRDS(here("output/models", "mxl_health_own.rds"))
ren <- readRDS(here("output/models", "mxl_health_rent.rds"))
s3 <- bind_rows(
  compute_table(own, SCALER_OWNER,  "Good", "Owner",  interaction_to_base_health),
  compute_table(ren, SCALER_RENTER, "Good", "Renter", interaction_to_base_health)
) %>% finalize()
write_csv(s3, here("output", "s3_health_mwtp_ci.csv"))

cat("Saved: output/s1_age_mwtp_ci.csv, output/s2_sex_mwtp_ci.csv, output/s3_health_mwtp_ci.csv\n")
