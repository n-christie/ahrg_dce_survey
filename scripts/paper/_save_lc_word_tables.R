# _save_lc_word_tables.R
#
# Word-ready (HTML) versions of the latent class results tables.
# Mirrors _save_profiles_table.R's approach: load the saved models, build the
# table, save_kable() to paper/word/tables/ as HTML that opens/pastes cleanly
# into Word. Run after latent_class_analysis.R has saved the model .rds files.
#
# Produces:
#   paper/word/tables/lc_model_comparison.html   (MNL/LC-2/LC-3/LC-4 fit stats)
#   paper/word/tables/lc_2class_coefs_wtp.html    (coefs + MWTP, 2-class, robustness/supplement)
#   paper/word/tables/lc_3class_coefs_wtp.html    (coefs + MWTP, 3-class)
#   paper/word/tables/lc_4class_coefs_wtp.html    (coefs + MWTP, 4-class)
#
# LC-3 and LC-4 models were refit 2026-08-21 using a multi-start procedure
# (fit_lc_multistart() in latent_class_analysis.R) after an audit found the
# original single-start BFGS fits had converged to local optima meaningfully
# worse than the best-found solution (LC-3: LL -3898.94 -> -3898.72; LC-4:
# LL -3856.91 -> -3843.03, a materially different 4-class solution). Both a
# 3-class and 4-class table are produced here since the class count has not
# yet been finalized -- the corrected LC-4 now has a clearly better BIC than
# LC-3 (see lc_model_comparison.html), reversing the prior "diminishing
# returns" read that favored 3 classes.

library(here)
library(gmnl)
library(mlogit)
library(dplyr)
library(purrr)
library(tibble)
library(kableExtra)

# ==============================================================================
# LOAD MODELS
# ==============================================================================

mnl <- readRDS(here("output/models", "lc_mnl_base.rds"))
lc2 <- readRDS(here("output/models", "lc_2class.rds"))
lc3 <- readRDS(here("output/models", "lc_3class.rds"))
lc4 <- readRDS(here("output/models", "lc_4class.rds"))

n_ind <- 957  # respondents; matches lc_results.R / latent_class_analysis.R

df_model <- readRDS(here("data/formr", "df_model.rds"))
# Pooled-sample median: Table 3 (baseline_regs.R) uses tenure-specific
# medians (owners 10,000 SEK, renters 9,000 SEK); the pooled median is the
# right analogue for the pooled-sample latent class models. Reverted
# 2026-08-21 after a same-session edit mistakenly switched this to the mean.
median_cost <- median(
  df_model$planed_cost[!duplicated(df_model$panelID)],
  na.rm = TRUE
)
scaler <- 0.10 * median_cost

# ==============================================================================
# HELPERS (same logic as latent_class_analysis.R / lc_results.R)
# ==============================================================================

extract_lc_coefs <- function(model, Q) {
  sm <- summary(model)$CoefTable
  map_dfr(1:Q, function(q) {
    prefix <- paste0("^class\\.", q, "\\.")
    idx    <- grep(prefix, rownames(sm))
    tibble(
      class     = q,
      variable  = sub(prefix, "", rownames(sm)[idx]),
      estimate  = sm[idx, "Estimate"],
      std_error = sm[idx, "Std. Error"],
      p_value   = sm[idx, "Pr(>|z|)"]
    )
  })
}

extract_class_shares <- function(model) {
  sm        <- summary(model)$CoefTable
  delta_idx <- grep("^\\(class\\)", rownames(sm))
  deltas    <- c(0, sm[delta_idx, "Estimate"])
  exp_d     <- exp(deltas)
  exp_d / sum(exp_d)
}

calc_entropy <- function(model, Q, n) {
  Qir <- model$Qir
  1 + sum(Qir * log(Qir + 1e-12)) / (n * log(Q))
}

# Delta-method WTP with SE/CI (matches the fix applied to latent_class_analysis.R)
compute_lc_wtp <- function(model, Q, scale) {
  vc <- vcov(model)
  sm <- summary(model)$CoefTable

  map_dfr(1:Q, function(q) {
    price_name <- paste0("class.", q, ".price_num")
    beta_price <- sm[price_name, "Estimate"]

    prefix   <- paste0("^class\\.", q, "\\.")
    idx      <- grep(prefix, rownames(sm))
    attr_idx <- idx[rownames(sm)[idx] != price_name]

    map_dfr(attr_idx, function(i) {
      attr_name <- rownames(sm)[i]
      beta_attr <- sm[i, "Estimate"]

      wtp  <- -(beta_attr / beta_price) * scale
      grad <- c(-1 / beta_price, beta_attr / beta_price^2) * scale
      V    <- vc[c(attr_name, price_name), c(attr_name, price_name)]
      se   <- sqrt(as.numeric(t(grad) %*% V %*% grad))

      tibble(
        class    = q,
        variable = sub(prefix, "", attr_name),
        wtp      = wtp,
        se       = se,
        lower    = wtp - 1.96 * se,
        upper    = wtp + 1.96 * se,
        p_value  = 2 * pnorm(-abs(wtp / se))
      )
    })
  })
}

label_map <- c(
  "dist_green5km"                  = "Green space: 5 km (vs 15 km)",
  "dist_green500 meter"            = "Green space: 500 m (vs 15 km)",
  "dist_shops5km"                  = "Shops: 5 km (vs 15 km)",
  "dist_shops500 meter"            = "Shops: 500 m (vs 15 km)",
  "dist_trans600"                  = "Transit stop: 600 m (vs 900 m)",
  "dist_trans300"                  = "Transit stop: 300 m (vs 900 m)",
  "parkingreserverad garageplats"  = "Parking: reserved garage (vs none)",
  "parkingreserverad P-plats"      = "Parking: reserved space (vs none)",
  "price_num"                      = "Price"
)

sig_stars <- function(p) case_when(
  is.na(p)   ~ "",
  p < 0.001  ~ "***",
  p < 0.01   ~ "**",
  p < 0.05   ~ "*",
  TRUE       ~ ""
)

# ==============================================================================
# TABLE 1: MODEL COMPARISON (MNL / LC-2 / LC-3 / LC-4)
# ==============================================================================

model_fit <- tibble(
  Model  = c("MNL", "LC-2", "LC-3", "LC-4"),
  Q      = c(1L, 2L, 3L, 4L),
  LogLik = c(as.numeric(logLik(mnl)), as.numeric(logLik(lc2)),
             as.numeric(logLik(lc3)), as.numeric(logLik(lc4))),
  K      = c(length(coef(mnl)), length(coef(lc2)), length(coef(lc3)), length(coef(lc4)))
) %>%
  mutate(
    AIC     = -2 * LogLik + 2 * K,
    BIC     = -2 * LogLik + log(n_ind) * K,
    CAIC    = -2 * LogLik + (log(n_ind) + 1) * K,
    Entropy = c(NA, calc_entropy(lc2, 2, n_ind), calc_entropy(lc3, 3, n_ind), calc_entropy(lc4, 4, n_ind))
  ) %>%
  mutate(
    across(c(LogLik, AIC, BIC, CAIC), ~ round(.x, 1)),
    Entropy = ifelse(is.na(Entropy), "--", sprintf("%.3f", Entropy))
  ) %>%
  select(Model, `Log Lik.` = LogLik, K, AIC, BIC, CAIC, Entropy)

out_fit <- kbl(
  model_fit, format = "html", escape = TRUE,
  caption = "Table SX. Latent Class Model Comparison"
) %>%
  kable_classic(full_width = FALSE) %>%
  row_spec(0, bold = TRUE) %>%
  footnote(
    general = paste0(
      "BIC and CAIC computed using number of respondents (N = ", n_ind, ") as sample size. ",
      "Relative entropy measures class separation: values closer to 1 indicate cleaner separation. ",
      "LC-2/LC-3/LC-4 models each selected as the best converged log-likelihood across a ",
      "multi-start search (8+ random starting vectors per model) after an initial single-start ",
      "fit was found to converge to a materially worse local optimum for the 3- and 4-class models."
    ),
    general_title = "Note: "
  )

save_kable(out_fit, here("paper/word/tables", "lc_model_comparison.html"))
cat("Saved: paper/word/tables/lc_model_comparison.html\n")

# ==============================================================================
# TABLE 2/3: COEFFICIENTS + MWTP BY CLASS (merged, per Q)
# ==============================================================================

build_coef_wtp_table <- function(model, Q, out_file, caption) {
  coefs  <- extract_lc_coefs(model, Q)
  wtp    <- compute_lc_wtp(model, Q, scaler)
  shares <- extract_class_shares(model)

  coef_fmt <- coefs %>%
    mutate(
      label = label_map[variable],
      cell  = sprintf("%.3f%s (%.3f)", estimate, sig_stars(p_value), std_error)
    ) %>%
    filter(!is.na(label)) %>%
    select(class, label, cell) %>%
    tidyr::pivot_wider(names_from = class, values_from = cell, names_prefix = "coef")

  wtp_fmt <- wtp %>%
    mutate(
      label = label_map[variable],
      cell  = sprintf("%.0f%s (%.0f, %.0f)", wtp, sig_stars(p_value), lower, upper)
    ) %>%
    filter(!is.na(label)) %>%
    select(class, label, cell) %>%
    tidyr::pivot_wider(names_from = class, values_from = cell, names_prefix = "wtp")

  # Interleave coef/wtp columns per class, in attribute (label_map) order,
  # excluding price (no WTP for price itself)
  attr_order <- label_map[label_map %in% coef_fmt$label & label_map != "Price"]
  tab <- coef_fmt %>% left_join(wtp_fmt, by = "label")
  tab <- tab[match(attr_order, tab$label), ]

  col_order <- c("label", as.vector(rbind(paste0("coef", 1:Q), paste0("wtp", 1:Q))))
  tab <- tab[, intersect(col_order, names(tab))]

  share_row <- c("Class share (%)", as.vector(rbind(sprintf("%.1f", shares * 100), rep("", Q))))
  tab_out <- rbind(setNames(share_row, names(tab)), tab)
  names(tab_out)[1] <- "Attribute"
  names(tab_out)[-1] <- rep(c("Coef. (SE)", "MWTP (95% CI, SEK/mo)"), Q)

  header <- c(" " = 1)
  for (q in 1:Q) header <- c(header, setNames(2, paste0("Class ", q)))

  out <- kbl(
    tab_out, format = "html", escape = TRUE, row.names = FALSE,
    caption = caption
  ) %>%
    kable_classic(full_width = FALSE) %>%
    row_spec(0, bold = TRUE) %>%
    add_header_above(header) %>%
    row_spec(1, italic = TRUE, extra_css = "border-bottom: 1px solid #aaa;") %>%
    footnote(
      general = paste0(
        "Reference levels: green space/shops 15 km, transit 900 m, no reserved parking. ",
        "MWTP = -(beta_attribute / beta_price) x ", round(scaler, 0),
        " SEK (10% of median monthly housing cost, ", round(median_cost, 0), " SEK/month); ",
        "95% CIs from the delta method, accounting for the covariance between each ",
        "attribute coefficient and the price coefficient. ",
        "* p<.05, ** p<.01, *** p<.001 (coefficients); starred MWTP indicates a 95% CI excluding zero."
      ),
      general_title = "Note: "
    )

  save_kable(out, here("paper/word/tables", out_file))
  cat("Saved: paper/word/tables/", out_file, "\n", sep = "")
}

build_coef_wtp_table(
  lc3, 3, "lc_3class_coefs_wtp.html",
  "Table 4 (3-class option). Latent Class Model (3-Class Solution): Utility Coefficients and Marginal Willingness to Pay"
)

build_coef_wtp_table(
  lc4, 4, "lc_4class_coefs_wtp.html",
  "Table 4 (4-class option). Latent Class Model (4-Class Solution): Utility Coefficients and Marginal Willingness to Pay"
)

build_coef_wtp_table(
  lc2, 2, "lc_2class_coefs_wtp.html",
  "Table SX. Latent Class Model (2-Class Solution): Utility Coefficients and Marginal Willingness to Pay"
)

cat("\n=== Word-ready LC tables saved to paper/word/tables/ ===\n")
