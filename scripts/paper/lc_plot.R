# lc_plot.R
# LC-4 WTP figure highlighting green space.
# Produces: output/figures/lc4_wtp.pdf + .png
#
# LC-4 (not LC-3) is used here to match the class count adopted in the paper
# after the multi-start refit (2026-08-21): single-start BFGS had converged
# to a meaningfully worse local optimum for the 4-class model, and once
# refit, LC-4 beat LC-3 on AIC/BIC/CAIC. Class names below match Table 4 in
# the manuscript -- keep them in sync if the class solution or naming
# changes again.

library(pacman)
p_load(tidyverse, here, haven, gmnl, mlogit)

set.seed(12345)

# ==============================================================================
# DATA + MODEL
# ==============================================================================

df_model <- readRDS(here("data/formr", "df_model.rds")) %>%
  mutate(
    price_num  = price_num / 100,
    dist_trans = factor(dist_trans, levels = c("900","600","300")),
    dist_green = factor(dist_green, levels = c("15km","5km","500 meter")),
    dist_shops = factor(dist_shops, levels = c("15km","5km","500 meter")),
    ägandebostad = haven::as_factor(ägandebostad)
  )

lc4 <- readRDS(here("output/models", "lc_4class.rds"))

# ==============================================================================
# WTP WITH DELTA-METHOD CIs
# ==============================================================================

vc    <- vcov(lc4)
sm    <- summary(lc4)$CoefTable

# Scale: 10% of the pooled-sample MEDIAN planned monthly housing cost.
# Table 3 (baseline_regs.R) uses tenure-specific medians (owners 10,000
# SEK, renters 9,000 SEK); the pooled median is the right analogue for
# these pooled-sample latent class models. A same-session edit briefly
# switched this to the mean based on the wrong script (interaction_regs_
# table.R generates a different table, not Table 3) -- reverted 2026-08-21.
median_cost_overall <- median(
  df_model %>% distinct(panelID, .keep_all = TRUE) %>% pull(planed_cost),
  na.rm = TRUE
)
scale <- round(0.10 * median_cost_overall, 0)

attr_vars <- c(
  "dist_green5km", "dist_green500 meter",
  "dist_shops5km", "dist_shops500 meter",
  "dist_trans600",  "dist_trans300",
  "parkingreserverad garageplats", "parkingreserverad P-plats"
)

wtp_df <- map_dfr(1:4, function(q) {
  price_name <- paste0("class.", q, ".price_num")
  beta_p     <- sm[price_name, "Estimate"]

  map_dfr(attr_vars, function(a) {
    attr_name <- paste0("class.", q, ".", a)
    beta_a    <- sm[attr_name, "Estimate"]

    wtp  <- -(beta_a / beta_p) * scale
    grad <- c(-1 / beta_p, beta_a / beta_p^2) * scale
    V    <- vc[c(attr_name, price_name), c(attr_name, price_name)]
    se   <- sqrt(as.numeric(t(grad) %*% V %*% grad))

    tibble(
      class    = q,
      variable = a,
      wtp      = wtp,
      lower    = wtp - 1.96 * se,
      upper    = wtp + 1.96 * se
    )
  })
})

# ==============================================================================
# LABELS AND GROUPING
# ==============================================================================

lc4_names <- c("Shop-oriented", "Green-space-oriented", "Price-sensitive\nshop-oriented", "Parking-oriented")

lc4_shares_raw <- {
  delta_idx <- grep("^\\(class\\)", rownames(sm))
  deltas    <- c(0, sm[delta_idx, "Estimate"])
  exp_d     <- exp(deltas)
  round(exp_d / sum(exp_d) * 100, 1)
}

# Names wrap onto their own line (Class 3's "Price-sensitive shop-seekers"
# is too long for a 4-panel facet strip otherwise).
class_labels <- setNames(
  paste0("Class ", 1:4, " ·\n", lc4_names, " (", lc4_shares_raw, "%)"),
  as.character(1:4)
)

class_labels_short <- setNames(
  paste0("Class ", 1:4, "\n", lc4_names, "\n(", lc4_shares_raw, "%)"),
  as.character(1:4)
)

attr_labels <- c(
  "dist_green5km"                  = "Green space\n5 km vs 15 km",
  "dist_green500 meter"            = "Green space\n500 m vs 15 km",
  "dist_shops5km"                  = "Shops\n5 km vs 15 km",
  "dist_shops500 meter"            = "Shops\n500 m vs 15 km",
  "dist_trans600"                  = "Transit stop\n600 m vs 900 m",
  "dist_trans300"                  = "Transit stop\n300 m vs 900 m",
  "parkingreserverad garageplats"  = "Parking\nGarage (vs none)",
  "parkingreserverad P-plats"      = "Parking\nSpace (vs none)"
)

# attribute order: green first, then shops, transit, parking
attr_order <- rev(c(
  "dist_green5km", "dist_green500 meter",
  "dist_shops5km", "dist_shops500 meter",
  "dist_trans600",  "dist_trans300",
  "parkingreserverad garageplats", "parkingreserverad P-plats"
))

wtp_plot <- wtp_df %>%
  mutate(
    class_f   = factor(class_labels[as.character(class)],
                       levels = class_labels),
    attr_f    = factor(attr_labels[variable], levels = attr_labels[attr_order]),
    is_green  = str_starts(variable, "dist_green"),
    fill_col  = if_else(is_green, "green", "other")
  )

# ==============================================================================
# COLOUR PALETTE
# ==============================================================================

col_green <- "#2E7D4F"   # dark forest green
col_other <- "#6D8FAD"   # muted steel blue
col_zero  <- "grey40"

# ==============================================================================
# PLOT
# ==============================================================================

p <- ggplot(wtp_plot, aes(x = wtp, y = attr_f, fill = fill_col)) +
  geom_col(width = 0.65, alpha = 0.88) +
  geom_errorbar(
    aes(xmin = lower, xmax = upper),
    width = 0.28, linewidth = 0.45, colour = "grey25"
  ) +
  geom_vline(xintercept = 0, colour = col_zero, linewidth = 0.4) +
  facet_wrap(~ class_f, ncol = 4) +
  scale_fill_manual(
    values = c(green = col_green, other = col_other),
    guide  = "none"
  ) +
  scale_x_continuous(
    labels = scales::label_dollar(prefix = "", suffix = " SEK"),
    breaks = c(-500, 0, 500, 1000),
    limits = c(-300, 1350)
  ) +
  labs(
    x       = "Marginal WTP (SEK/month)",
    y       = NULL,
    title   = "Willingness to Pay by Latent Class",
    subtitle = paste0(
      "Green bars = green space attributes. ",
      "95% CI from delta method. Scale = 10% of median monthly cost (",
      scales::comma(round(median_cost_overall, 0)), " SEK)."
    ),
    caption = "N = 957 respondents. LC-4 model estimated via gmnl."
  ) +
  theme_minimal(base_size = 10.5) +
  theme(
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(colour = "grey40", size = 8.5),
    plot.caption       = element_text(colour = "grey50", size = 7.5),
    strip.text         = element_text(face = "bold", size = 9.5),
    strip.background   = element_rect(fill = "grey94", colour = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(colour = "grey88", linewidth = 0.3),
    axis.text.y        = element_text(size = 8.5, lineheight = 1.1),
    axis.text.x        = element_text(size = 8),
    plot.margin        = margin(8, 12, 8, 8)
  )

# ==============================================================================
# SAVE
# ==============================================================================

dir.create(here("output", "figures"), showWarnings = FALSE)

ggsave(here("output", "figures", "lc4_wtp.pdf"),
       plot = p, width = 11, height = 4.2, device = cairo_pdf)

ggsave(here("output", "figures", "lc4_wtp.png"),
       plot = p, width = 11, height = 4.2, dpi = 300)

cat("Saved: output/figures/lc4_wtp.pdf + .png\n")

# ==============================================================================
# ALSO: GREEN SPACE SPOTLIGHT — compare green WTP across classes
# ==============================================================================

green_spot <- wtp_df %>%
  filter(str_starts(variable, "dist_green")) %>%
  mutate(
    class_f    = factor(class_labels_short[as.character(class)],
                        levels = class_labels_short),
    attr_short = recode(variable,
      "dist_green5km"        = "5 km (vs 15 km)",
      "dist_green500 meter"  = "500 m (vs 15 km)"
    ),
    attr_short = factor(attr_short, levels = c("5 km (vs 15 km)", "500 m (vs 15 km)"))
  )

p2 <- ggplot(green_spot, aes(x = class_f, y = wtp)) +
  geom_col(width = 0.55, alpha = 0.88, fill = col_green) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.2, linewidth = 0.5, colour = "grey25"
  ) +
  geom_hline(yintercept = 0, colour = col_zero, linewidth = 0.4) +
  facet_wrap(~ attr_short, ncol = 2) +
  scale_y_continuous(
    labels = scales::label_dollar(prefix = "", suffix = " SEK"),
    breaks = c(0, 200, 400, 600, 800, 1000)
  ) +
  labs(
    x        = NULL,
    y        = "Marginal WTP (SEK/month)",
    title    = "Green Space WTP by Latent Class",
    subtitle = "95% CI from delta method.",
    caption  = "N = 957 respondents. LC-4 model."
  ) +
  theme_minimal(base_size = 10.5) +
  theme(
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(colour = "grey40", size = 8.5),
    plot.caption       = element_text(colour = "grey50", size = 7.5),
    strip.text         = element_text(face = "bold", size = 10),
    strip.background   = element_rect(fill = "grey94", colour = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.3),
    axis.text.x        = element_text(size = 8.5, lineheight = 1.15),
    plot.margin        = margin(8, 12, 8, 8)
  )

ggsave(here("output", "figures", "lc4_green_spotlight.pdf"),
       plot = p2, width = 7, height = 4, device = cairo_pdf)

ggsave(here("output", "figures", "lc4_green_spotlight.png"),
       plot = p2, width = 7, height = 4, dpi = 300)

cat("Saved: output/figures/lc4_green_spotlight.pdf + .png\n")
