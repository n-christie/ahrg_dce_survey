# figures.R
# Six publication figures for the DCE paper.
# Requires: output/models/ with all .rds files from prior scripts.
# Saves:    output/figures/ and paper/tex/figures/

library(pacman)
p_load(tidyverse, here, haven, logitr, gmnl, mlogit, broom, scales, patchwork)

set.seed(12345)

# ==============================================================================
# 0. CONSTANTS, HELPERS, DATA
# ==============================================================================

# SCALE is computed below (after df_model loads) as 10% of the pooled-sample
# MEDIAN planned monthly housing cost (900 SEK). Note this is a pooled
# figure, used as-is for Figs 1-4 (owner/renter and age-group MXL WTP);
# Table 3 itself (baseline_regs.R) instead uses tenure-specific medians
# (owners 10,000 SEK, renters 9,000 SEK, hardcoded there) -- that
# pooled-vs-tenure-specific difference predates this session and is not
# changed here. Fig 5 (pooled-sample latent class WTP) is the one for which
# the pooled median is unambiguously correct, since the LC models are fit
# on the pooled sample. A same-session edit briefly switched SCALE to the
# mean based on the wrong script (interaction_regs_table.R generates a
# different table, not Table 3) -- reverted 2026-08-21.
N_TOTAL   <- 957
COL_OWN   <- "#2E5D9E"
COL_RENT  <- "#C76A30"
COL_GREEN <- "#2E7D4F"
COL_BLUE  <- "#6D8FAD"
COL_ZERO  <- "grey40"
AGE_COLS  <- c("55–64" = "#8BB0D2", "65–74" = "#3A75AB", "75+" = "#1A3A55")

BASE_THEME <- theme_minimal(base_size = 10.5) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(colour = "grey40", size = 8.5),
    plot.caption     = element_text(colour = "grey50", size = 7.5),
    strip.text       = element_text(face = "bold", size = 9.5),
    strip.background = element_rect(fill = "grey94", colour = NA),
    panel.grid.minor = element_blank(),
    plot.margin      = margin(8, 12, 8, 8)
  )

save_fig <- function(p, name, w, h) {
  for (d in c(here("output","figures"), here("paper","tex","figures"))) {
    dir.create(d, showWarnings = FALSE, recursive = TRUE)
    ggsave(file.path(d, paste0(name, ".png")), p, width = w, height = h, dpi = 300)
    ggsave(file.path(d, paste0(name, ".pdf")), p, width = w, height = h, device = cairo_pdf)
  }
  cat("Saved:", name, "\n")
}

# Delta method WTP for a simple ratio: WTP = -(b_a / b_p) * scale
# Accepts vectors of coefficient names for numerator and denominator terms
# (multiple terms → their sum forms the numerator/denominator).
delta_wtp_v <- function(coefs, vc, attr_terms, price_terms, scale = SCALE) {
  ba <- sum(coefs[attr_terms])
  bp <- sum(coefs[price_terms])
  wtp  <- -(ba / bp) * scale
  # gradient w.r.t. each attr_term = -1/bp * scale
  # gradient w.r.t. each price_term = ba/bp^2 * scale
  all_terms <- c(attr_terms, price_terms)
  grad      <- c(rep(-1 / bp, length(attr_terms)),
                 rep( ba / bp^2, length(price_terms))) * scale
  V   <- vc[all_terms, all_terms]
  se  <- sqrt(as.numeric(t(grad) %*% V %*% grad))
  c(wtp = wtp, lower = wtp - 1.96 * se, upper = wtp + 1.96 * se)
}

# ── Data ──────────────────────────────────────────────────────────────────────

df_model <- readRDS(here("data/formr", "df_model.rds")) %>%
  mutate(
    price_num    = price_num / 100,
    dist_trans   = factor(dist_trans, levels = c("900","600","300")),
    dist_green   = factor(dist_green, levels = c("15km","5km","500 meter")),
    dist_shops   = factor(dist_shops, levels = c("15km","5km","500 meter")),
    ägandebostad = haven::as_factor(ägandebostad),
    bostadstyp   = haven::as_factor(bostadstyp),
    Sex          = haven::as_factor(Sex),
    civil_d      = factor(if_else(civil_status_T2 == 1, "Partnered", "Not partnered")),
    Own          = factor(if_else(ägandebostad == "Ja", "Owner", "Renter")),
    age          = floor(Age_T3),
    age_group    = factor(case_when(
      age >= 55 & age < 65 ~ "55–64",
      age >= 65 & age < 75 ~ "65–74",
      age >= 75             ~ "75+"
    ), levels = c("55–64","65–74","75+")),
    self_report_health = as.numeric(VAR035)
  )

MEDIAN_COST_OVERALL <- median(
  df_model %>% distinct(panelID, .keep_all = TRUE) %>% pull(planed_cost),
  na.rm = TRUE
)
SCALE <- round(0.10 * MEDIAN_COST_OVERALL, 0)

# mlogit format needed for LC class membership
df_gmnl <- mlogit.data(
  data = df_model, choice = "choice", shape = "long",
  alt.var = "altID", chid.var = "obsID", id.var = "panelID"
)

n_own  <- df_model %>% distinct(panelID, .keep_all = TRUE) %>% filter(Own == "Owner")  %>% nrow()
n_rent <- df_model %>% distinct(panelID, .keep_all = TRUE) %>% filter(Own == "Renter") %>% nrow()

# ── Models ────────────────────────────────────────────────────────────────────

m_own      <- readRDS(here("output/models","mxl_owner_base.rds"))
m_rent     <- readRDS(here("output/models","mxl_renter_base.rds"))
m_age      <- readRDS(here("output/models","mxl_age.rds"))
m_age_own  <- readRDS(here("output/models","mxl_age_own.rds"))
lc4        <- readRDS(here("output/models","lc_4class.rds"))

# ==============================================================================
# 1. ATTR LABELS (two naming conventions)
# ==============================================================================

# Base models (owner/renter) use long factor-level names
main_terms_base <- c(
  "dist_green5km", "dist_green500 meter",
  "dist_shops5km", "dist_shops500 meter",
  "dist_trans600", "dist_trans300",
  "parkingreserverad garageplats", "parkingreserverad P-plats"
)

# Age interaction models use short dummy names
main_terms_age <- c(
  "dist_green5km", "dist_green500m",
  "dist_shops5km", "dist_shops500m",
  "dist_trans600", "dist_trans300",
  "park_garage", "park_space"
)

attr_labels <- c(
  "dist_green5km"                 = "Green space: 5 km",
  "dist_green500 meter"           = "Green space: 500 m",
  "dist_green500m"                = "Green space: 500 m",
  "dist_shops5km"                 = "Shops: 5 km",
  "dist_shops500 meter"           = "Shops: 500 m",
  "dist_shops500m"                = "Shops: 500 m",
  "dist_trans600"                 = "Transit: 600 m",
  "dist_trans300"                 = "Transit: 300 m",
  "parkingreserverad garageplats" = "Parking: garage",
  "parkingreserverad P-plats"     = "Parking: space",
  "park_garage"                   = "Parking: garage",
  "park_space"                    = "Parking: space"
)

# Display order (bottom → top on horizontal plots)
attr_order_base <- rev(main_terms_base)
attr_order_age  <- rev(main_terms_age)

# ==============================================================================
# FIG 1: MXL COEFFICIENT FOREST PLOT — OWNERS vs RENTERS
# ==============================================================================

make_forest <- function(model, group, terms) {
  t <- broom::tidy(model)
  t[t$term %in% terms, ] %>%
    transmute(
      group   = group,
      term    = term,
      label   = factor(attr_labels[term], levels = attr_labels[rev(terms)]),
      est     = estimate,
      lower   = estimate - 1.96 * std.error,
      upper   = estimate + 1.96 * std.error
    )
}

forest_df <- bind_rows(
  make_forest(m_own,  "Owner",  main_terms_base),
  make_forest(m_rent, "Renter", main_terms_base)
) %>%
  mutate(group = factor(group, levels = c("Renter","Owner")))

fig1 <- ggplot(forest_df, aes(x = est, y = label, colour = group, shape = group)) +
  geom_vline(xintercept = 0, colour = COL_ZERO, linewidth = 0.4, linetype = "dashed") +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                position = position_dodge(0.6), width = 0.3, linewidth = 0.5) +
  geom_point(position = position_dodge(0.6), size = 2.8) +
  scale_colour_manual(values = c(Owner = COL_OWN, Renter = COL_RENT), name = NULL) +
  scale_shape_manual(values  = c(Owner = 16, Renter = 17), name = NULL) +
  labs(
    x        = "Utility coefficient (95% CI)",
    y        = NULL,
    title    = "MXL Utility Coefficients: Owners vs Renters",
    subtitle = paste0("Mean random parameters. Baseline: 15 km / 900 m / no parking. ",
                      "N(owners) = ", n_own, ", N(renters) = ", n_rent, "."),
    caption  = "Correlated mixed logit estimated via logitr, Sobol draws, 2,000 iterations."
  ) +
  BASE_THEME +
  theme(
    legend.position    = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey88", linewidth = 0.3),
    axis.text.y        = element_text(size = 9)
  )

save_fig(fig1, "fig1_coef_own_rent", w = 7.5, h = 4.5)

# ==============================================================================
# FIG 2: AGE INTERACTION GRADIENT — CONDITIONAL UTILITY BY AGE GROUP
# ==============================================================================

# Compute conditional coefficient for each attribute × age group using mxl_age
# The age model uses shortened dummy names (main_terms_age)

coefs_age <- coef(m_age)
vc_age    <- vcov(m_age)

# Map: for each attribute, base term + optional interaction suffix
age_group_levels <- c("55–64","65–74","75+")
inter_suffixes   <- list("55–64" = NULL, "65–74" = "_G65_74", "75+" = "_G75p")

# For each attribute and age group, compute conditional coef + SE (delta method)
age_gradient <- map_dfr(main_terms_age, function(a) {
  map_dfr(age_group_levels, function(g) {
    inter <- inter_suffixes[[g]]
    # find the interaction term name
    a_short <- sub("dist_green5km","green5km",
               sub("dist_green500m","green500",
               sub("dist_shops5km","shops5km",
               sub("dist_shops500m","shops500",
               sub("dist_trans600","trans600",
               sub("dist_trans300","trans300",
               sub("park_garage","park_garage",
               sub("park_space","park_space", a))))))))
    inter_term <- if (!is.null(inter)) paste0(a_short, inter) else NULL

    attr_terms <- c(a, inter_term)
    attr_terms <- attr_terms[attr_terms %in% names(coefs_age)]

    est <- sum(coefs_age[attr_terms])
    # SE via delta method (sum of terms)
    if (length(attr_terms) == 1) {
      se <- sqrt(vc_age[attr_terms, attr_terms])
    } else {
      g_vec <- rep(1, length(attr_terms))
      V     <- vc_age[attr_terms, attr_terms]
      se    <- sqrt(as.numeric(t(g_vec) %*% V %*% g_vec))
    }

    tibble(
      term      = a,
      age_group = g,
      est       = est,
      lower     = est - 1.96 * se,
      upper     = est + 1.96 * se
    )
  })
}) %>%
  mutate(
    label     = factor(attr_labels[term], levels = attr_labels[attr_order_age]),
    age_group = factor(age_group, levels = age_group_levels),
    attr_grp  = case_when(
      str_starts(term, "dist_green") ~ "Green space",
      str_starts(term, "dist_shops") ~ "Shops",
      str_starts(term, "dist_trans") ~ "Transit",
      TRUE                           ~ "Parking"
    )
  )

fig2 <- ggplot(age_gradient,
               aes(x = age_group, y = est, colour = age_group, group = term)) +
  geom_hline(yintercept = 0, colour = COL_ZERO, linewidth = 0.4, linetype = "dashed") +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.18, linewidth = 0.5) +
  geom_point(size = 2.5) +
  geom_line(colour = "grey60", linewidth = 0.4) +
  facet_wrap(~ label, nrow = 2, scales = "free_y") +
  scale_colour_manual(values = AGE_COLS, name = NULL) +
  labs(
    x        = NULL,
    y        = "Conditional utility coefficient (95% CI)",
    title    = "Attribute Preferences by Age Group",
    subtitle = paste0("Conditional coefficients from pooled MXL with age-group interactions. ",
                      "N = ", N_TOTAL, " respondents."),
    caption  = "Reference group: 55–64 years. 65–74 and 75+ estimated as base + interaction."
  ) +
  BASE_THEME +
  theme(
    legend.position    = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.3),
    axis.text.x        = element_text(size = 8.5)
  )

save_fig(fig2, "fig2_age_gradient", w = 9, h = 5.5)

# ==============================================================================
# FIG 3: WTP WITH DELTA-METHOD CIs — OWNERS vs RENTERS
# ==============================================================================

compute_wtp_group <- function(model, terms, price_term, group) {
  co <- coef(model)
  vc <- vcov(model)
  map_dfr(terms, function(a) {
    r <- delta_wtp_v(co, vc, attr_terms = a, price_terms = price_term)
    tibble(group = group, term = a, wtp = r["wtp"],
           lower = r["lower"], upper = r["upper"])
  })
}

wtp_df <- bind_rows(
  compute_wtp_group(m_own,  main_terms_base, "price_num", "Owner"),
  compute_wtp_group(m_rent, main_terms_base, "price_num", "Renter")
) %>%
  mutate(
    label = factor(attr_labels[term], levels = attr_labels[rev(main_terms_base)]),
    group = factor(group, levels = c("Renter","Owner"))
  )

fig3 <- ggplot(wtp_df, aes(x = wtp, y = label, fill = group)) +
  geom_col(position = position_dodge(0.7), width = 0.55, alpha = 0.88) +
  geom_errorbar(
    aes(xmin = lower, xmax = upper),
    position = position_dodge(0.7), width = 0.28, linewidth = 0.45, colour = "grey25"
  ) +
  geom_vline(xintercept = 0, colour = COL_ZERO, linewidth = 0.4) +
  scale_fill_manual(values = c(Owner = COL_OWN, Renter = COL_RENT), name = NULL) +
  scale_x_continuous(
    labels = label_dollar(prefix = "", suffix = " SEK"),
    breaks = seq(-200, 600, 200)
  ) +
  labs(
    x        = "Marginal WTP (SEK/month, 95% CI)",
    y        = NULL,
    title    = "Marginal WTP: Owners vs Renters",
    subtitle = paste0("WTP = −(β_attr / β_price) × ", SCALE,
                      " SEK. Delta-method CIs. Reference: 15 km / 900 m / no parking."),
    caption  = paste0("N(owners) = ", n_own, ", N(renters) = ", n_rent,
                      ". Scale = 10% of median monthly housing cost (",
                      scales::comma(round(MEDIAN_COST_OVERALL, 0)), " SEK).")
  ) +
  BASE_THEME +
  theme(
    legend.position    = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey88", linewidth = 0.3),
    axis.text.y        = element_text(size = 9)
  )

save_fig(fig3, "fig3_wtp_own_rent", w = 7.5, h = 4.5)

# ==============================================================================
# FIG 4: GREEN SPACE WTP BY AGE GROUP (pooled model + owners panel)
# ==============================================================================

# Pooled age model (mxl_age.rds) uses 3 groups: G65_74, G75p relative to 55-64
# Owner age model (mxl_age_own.rds) uses same 3-group structure
# Renter age model uses binary High_age (different spec) — excluded from this figure

compute_green_wtp_age <- function(model, group_label) {
  co <- coef(model)
  vc <- vcov(model)

  expand_grid(
    green_var = c("dist_green5km","dist_green500m"),
    age_grp   = c("55–64","65–74","75+")
  ) %>%
    mutate(
      green_short = if_else(green_var == "dist_green5km", "green5km", "green500"),
      age_suffix  = case_when(
        age_grp == "65–74" ~ "_G65_74",
        age_grp == "75+"   ~ "_G75p",
        TRUE               ~ ""
      )
    ) %>%
    rowwise() %>%
    mutate(
      a_terms = list(c(green_var,
                       if (age_suffix != "") paste0(green_short, age_suffix))),
      p_terms = list(c("price_num",
                       if (age_suffix != "") paste0("price", age_suffix))),
      r       = list(delta_wtp_v(co, vc, unlist(a_terms), unlist(p_terms)))
    ) %>%
    ungroup() %>%
    transmute(
      group       = group_label,
      green_var   = green_var,
      age_grp     = factor(age_grp, levels = c("55–64","65–74","75+")),
      wtp         = map_dbl(r, "wtp"),
      lower       = map_dbl(r, "lower"),
      upper       = map_dbl(r, "upper")
    )
}

green_wtp_all  <- compute_green_wtp_age(m_age,     "All respondents")
green_wtp_own  <- compute_green_wtp_age(m_age_own, "Owners")

green_wtp <- bind_rows(green_wtp_all, green_wtp_own) %>%
  mutate(
    green_label = factor(
      if_else(green_var == "dist_green5km", "5 km (vs 15 km)", "500 m (vs 15 km)"),
      levels = c("5 km (vs 15 km)","500 m (vs 15 km)")
    ),
    group = factor(group, levels = c("All respondents","Owners"))
  )

fig4 <- ggplot(green_wtp, aes(x = age_grp, y = wtp, fill = group)) +
  geom_col(position = position_dodge(0.7), width = 0.55, alpha = 0.88) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(0.7), width = 0.25, linewidth = 0.45, colour = "grey25"
  ) +
  geom_hline(yintercept = 0, colour = COL_ZERO, linewidth = 0.4) +
  facet_wrap(~ green_label, ncol = 2) +
  scale_fill_manual(
    values = c("All respondents" = COL_GREEN, "Owners" = COL_OWN),
    name   = NULL
  ) +
  scale_y_continuous(labels = label_dollar(prefix = "", suffix = " SEK")) +
  labs(
    x        = NULL,
    y        = "Marginal WTP (SEK/month, 95% CI)",
    title    = "Green Space WTP by Age Group",
    subtitle = paste0("Conditional WTP from pooled MXL with age-group interactions. ",
                      "Scale = ", SCALE, " SEK."),
    caption  = paste0("N = ", N_TOTAL, " (all); N(owners) = ", n_own,
                      ". Delta-method CIs. Reference: 15 km distance to green space.")
  ) +
  BASE_THEME +
  theme(
    legend.position    = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.3),
    axis.text.x        = element_text(size = 9)
  )

save_fig(fig4, "fig4_green_age_own", w = 7.5, h = 4.5)

# ==============================================================================
# FIG 5: LC-4 ALL-ATTRIBUTE WTP (mirrors lc_plot.R)
# ==============================================================================
# LC-4 (not LC-3) is used here to match the class count adopted in the paper
# after the multi-start refit (2026-08-21): single-start BFGS had converged
# to a meaningfully worse local optimum for the 4-class model, and once
# refit, LC-4 beat LC-3 on AIC/BIC/CAIC. Class names below ("Shop-seekers",
# "Nature-seekers", "Price-sensitive shop-seekers", "Car-centred") match
# Table 4 in the manuscript -- keep them in sync if the class solution or
# naming changes again.

sm_lc4  <- summary(lc4)$CoefTable
vc_lc4  <- vcov(lc4)

lc4_shares_raw <- {
  delta_idx <- grep("^\\(class\\)", rownames(sm_lc4))
  deltas     <- c(0, sm_lc4[delta_idx, "Estimate"])
  exp_d      <- exp(deltas)
  round(exp_d / sum(exp_d) * 100, 1)
}

lc4_names <- c("Shop-oriented", "Green-space-oriented", "Price-sensitive\nshop-oriented", "Parking-oriented")

# Names wrap onto their own line (Class 3's "Price-sensitive shop-seekers"
# is too long for a 4-panel facet strip otherwise).
lc4_class_labels <- setNames(
  paste0("Class ", 1:4, " ·\n", lc4_names, " (", lc4_shares_raw, "%)"),
  as.character(1:4)
)

lc4_attr_vars <- c(
  "dist_green5km","dist_green500 meter",
  "dist_shops5km","dist_shops500 meter",
  "dist_trans600","dist_trans300",
  "parkingreserverad garageplats","parkingreserverad P-plats"
)

lc4_attr_labels <- c(
  "dist_green5km"                  = "Green space\n5 km vs 15 km",
  "dist_green500 meter"            = "Green space\n500 m vs 15 km",
  "dist_shops5km"                  = "Shops\n5 km vs 15 km",
  "dist_shops500 meter"            = "Shops\n500 m vs 15 km",
  "dist_trans600"                  = "Transit stop\n600 m vs 900 m",
  "dist_trans300"                  = "Transit stop\n300 m vs 900 m",
  "parkingreserverad garageplats"  = "Parking\nGarage (vs none)",
  "parkingreserverad P-plats"      = "Parking\nSpace (vs none)"
)

lc4_attr_order <- rev(lc4_attr_vars)

wtp_lc4_df <- map_dfr(1:4, function(q) {
  p_name <- paste0("class.", q, ".price_num")
  bp     <- sm_lc4[p_name, "Estimate"]
  map_dfr(lc4_attr_vars, function(a) {
    a_name <- paste0("class.", q, ".", a)
    ba     <- sm_lc4[a_name, "Estimate"]
    wtp    <- -(ba / bp) * SCALE
    grad   <- c(-1 / bp, ba / bp^2) * SCALE
    V      <- vc_lc4[c(a_name, p_name), c(a_name, p_name)]
    se     <- sqrt(as.numeric(t(grad) %*% V %*% grad))
    tibble(class = q, variable = a, wtp = wtp,
           lower = wtp - 1.96 * se, upper = wtp + 1.96 * se)
  })
}) %>%
  mutate(
    class_f  = factor(lc4_class_labels[as.character(class)],
                      levels = lc4_class_labels),
    attr_f   = factor(lc4_attr_labels[variable],
                      levels = lc4_attr_labels[lc4_attr_order]),
    fill_col = if_else(str_starts(variable, "dist_green"), "green", "other")
  )

fig5 <- ggplot(wtp_lc4_df, aes(x = wtp, y = attr_f, fill = fill_col)) +
  geom_col(width = 0.65, alpha = 0.88) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.28, linewidth = 0.45, colour = "grey25") +
  geom_vline(xintercept = 0, colour = COL_ZERO, linewidth = 0.4) +
  facet_wrap(~ class_f, ncol = 4) +
  scale_fill_manual(values = c(green = COL_GREEN, other = COL_BLUE), guide = "none") +
  scale_x_continuous(
    labels = label_dollar(prefix = "", suffix = " SEK"),
    breaks = c(-500, 0, 500, 1000),
    limits = c(-300, 1350)
  ) +
  labs(
    x        = "Marginal WTP (SEK/month)",
    y        = NULL,
    title    = "Willingness to Pay by Latent Class",
    subtitle = paste0("Green bars = green space attributes. ",
                      "95% CI from delta method. Scale = ", SCALE, " SEK."),
    caption  = paste0("N = ", N_TOTAL, " respondents. LC-4 model estimated via gmnl.")
  ) +
  BASE_THEME +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey88", linewidth = 0.3),
    axis.text.y        = element_text(size = 8.5, lineheight = 1.1),
    axis.text.x        = element_text(size = 8)
  )

save_fig(fig5, "fig5_lc4_wtp", w = 11, h = 4.2)

# ==============================================================================
# FIG 6: LC-4 CLASS DEMOGRAPHIC PROFILES
# ==============================================================================

# Assign modal class via posterior probabilities
Qir         <- as.data.frame(lc4$Qir)
names(Qir)  <- paste0("p", 1:4)
Qir$modal_class <- max.col(Qir[, paste0("p", 1:4)])
Qir$panelID     <- as.character(unique(df_gmnl$panelID))

ind_data <- df_model %>%
  distinct(panelID, .keep_all = TRUE) %>%
  mutate(panelID = as.character(panelID)) %>%
  select(panelID, age_group, Own, Sex, civil_d, self_report_health)

prof4 <- left_join(Qir, ind_data, by = "panelID") %>%
  mutate(
    Class = paste0("Class ", modal_class, "\n", lc4_names[modal_class])
  )

# Class share labels for facet titles
class_n  <- table(prof4$modal_class)
class_pct <- round(prop.table(class_n) * 100, 1)
class_names <- setNames(
  paste0("Class ", 1:4, "\n", lc4_names, "\n(", class_pct, "%)"),
  as.character(1:4)
)
prof4 <- prof4 %>%
  mutate(class_label = factor(class_names[as.character(modal_class)],
                               levels = class_names))

# Compute profile statistics per class
profile_stats <- prof4 %>%
  group_by(class_label) %>%
  summarise(
    `Owner (%)` = round(mean(Own == "Owner", na.rm = TRUE) * 100, 1),
    `Female (%)` = round(mean(Sex == "Kvinna", na.rm = TRUE) * 100, 1),
    `Partnered (%)` = round(mean(civil_d == "Partnered", na.rm = TRUE) * 100, 1),
    `Age 55-64 (%)` = round(mean(age_group == "55–64", na.rm = TRUE) * 100, 1),
    `Age 65-74 (%)` = round(mean(age_group == "65–74", na.rm = TRUE) * 100, 1),
    `Age 75+ (%)`   = round(mean(age_group == "75+",       na.rm = TRUE) * 100, 1),
    `Avg health (1-5)` = round(mean(self_report_health, na.rm = TRUE), 2),
    .groups = "drop"
  )

# Pivot for plotting (exclude health — different scale)
profile_long <- profile_stats %>%
  pivot_longer(
    cols = c(`Owner (%)`, `Female (%)`, `Partnered (%)`,
             `Age 55-64 (%)`, `Age 65-74 (%)`, `Age 75+ (%)`),
    names_to = "characteristic", values_to = "pct"
  ) %>%
  mutate(
    characteristic = factor(characteristic, levels = c(
      "Age 55-64 (%)", "Age 65-74 (%)", "Age 75+ (%)",
      "Owner (%)", "Female (%)", "Partnered (%)"
    )),
    char_group = case_when(
      str_starts(characteristic, "Age") ~ "Age group",
      TRUE ~ "Demographics"
    )
  )

# Class colours: match the green/nature theme, plus a 4th for the
# price-sensitive shop-seekers class
class_fill <- c("#4A7AB5","#2E7D4F","#A05C2C","#8E5DA8")
names(class_fill) <- class_names

fig6 <- ggplot(profile_long,
               aes(x = characteristic, y = pct, fill = class_label)) +
  geom_col(position = position_dodge(0.75), width = 0.6, alpha = 0.88) +
  geom_hline(yintercept = 0, colour = COL_ZERO, linewidth = 0.3) +
  facet_wrap(~ char_group, scales = "free_x", nrow = 1) +
  scale_fill_manual(values = class_fill, name = NULL) +
  scale_y_continuous(
    labels = label_percent(scale = 1, suffix = "%"),
    limits = c(0, 100),
    breaks = seq(0, 100, 20)
  ) +
  labs(
    x        = NULL,
    y        = "Share of class members (%)",
    title    = "LC-4 Class Demographic Profiles",
    subtitle = paste0("Modal class assignment based on posterior class probabilities. ",
                      "N = ", N_TOTAL, " respondents."),
    caption  = paste0(
                "Health (1–5 scale, higher = better): ",
                paste0("Class ", 1:4, " = ",
                       sprintf("%.2f", profile_stats$`Avg health (1-5)`), collapse = ", "),
                ". Age group shares sum to 100% within each class."
               )
  ) +
  BASE_THEME +
  theme(
    legend.position    = "bottom",
    legend.key.size    = unit(0.5,"cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.3),
    axis.text.x        = element_text(size = 8, angle = 20, hjust = 1)
  )

save_fig(fig6, "fig6_lc4_profiles", w = 10, h = 5)

# ==============================================================================
# DONE
# ==============================================================================
cat("\n=== All 6 figures saved to output/figures/ and paper/tex/figures/ ===\n")
cat("fig1_coef_own_rent    : MXL coefficients, owners vs renters\n")
cat("fig2_age_gradient     : Conditional utility by age group\n")
cat("fig3_wtp_own_rent     : WTP with delta-method CIs, owners vs renters\n")
cat("fig4_green_age_own    : Green space WTP by age group and ownership\n")
cat("fig5_lc4_wtp          : LC-4 WTP all attributes\n")
cat("fig6_lc4_profiles     : LC-4 class demographic profiles\n")
