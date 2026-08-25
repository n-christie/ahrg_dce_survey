# lc_plot_plain.R
# Plain-style variant of fig5_lc4_wtp (LC-4 WTP by attribute): standard
# ggplot theme (theme_gray, default text) and standard ggplot fill palette
# (scale_fill_discrete default), with no special emphasis on green-space
# bars -- just the charts. Adapted from lc_plot.R; same underlying WTP
# calculation, only the visual styling differs.
# Produces: output/figures/fig5_lc4_wtp_plain.pdf + .png

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

# Scale: 10% of the pooled-sample MEDIAN planned monthly housing cost, same
# convention as lc_plot.R / figures.R (see CLAUDE.md "MWTP scaling").
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

# Per-class price coefficient, shown in the facet label so that Class 3's
# much larger-magnitude price sensitivity (roughly 4-5x the other classes,
# per the Results text) is visible directly on the figure.
price_coef_by_class <- sapply(1:4, function(q) sm[paste0("class.", q, ".price_num"), "Estimate"])

# "Class N ·" on its own line keeps the name + share line short enough to
# fit the facet-strip width without being clipped (a plain " · " with no
# break here previously ran the whole label together into one over-long
# line and got cut off at the panel edges).
class_labels <- setNames(
  paste0("Class ", 1:4, " ·\n", lc4_names, " (", lc4_shares_raw, "%)",
         "\nPrice coefficient = ", sprintf("%.2f", price_coef_by_class)),
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

attr_order <- rev(c(
  "dist_green5km", "dist_green500 meter",
  "dist_shops5km", "dist_shops500 meter",
  "dist_trans600",  "dist_trans300",
  "parkingreserverad garageplats", "parkingreserverad P-plats"
))

wtp_plot <- wtp_df %>%
  mutate(
    class_f = factor(class_labels[as.character(class)], levels = class_labels),
    attr_f  = factor(attr_labels[variable], levels = attr_labels[attr_order])
  )

# ==============================================================================
# PLOT -- clean, muted palette (no green-space emphasis), no subtitle/caption
# ==============================================================================
# Colour choices aim for a print-friendly, understated look appropriate for a
# gerontology/health-policy journal figure (Innovation in Aging): a single
# muted teal-blue for bars (reads fine in grayscale), a pale tint of the same
# hue for facet strips, and dark charcoal (not pure black) for text/error
# bars. No per-attribute or per-class colour coding, since nothing here is
# meant to be singled out visually; Class 3's price sensitivity is conveyed
# via the price-coefficient text in its facet label instead of colour.

col_bar    <- "#3D7A8C"  # muted teal-blue
col_err    <- "#22303A"  # dark charcoal (error bars, zero line)
col_strip  <- "#EAF2F3"  # pale teal tint (facet strip background)
col_text   <- "#22303A"  # dark charcoal (all text)

# Axis range set from the actual CI extent (with padding), not a hardcoded
# guess: Class 1's shops-at-500m upper CI runs to ~1,860 SEK, well past the
# previous fixed 1,350 cap, which silently dropped (not just visually
# cropped) every CI whisker that crossed it via scale_x_continuous(limits=).
# coord_cartesian() below only zooms the view, so nothing gets dropped even
# if the padding turns out to be tight.
x_min <- floor(min(wtp_plot$lower, na.rm = TRUE) / 50) * 50 - 50
x_max <- ceiling(max(wtp_plot$upper, na.rm = TRUE) / 50) * 50 + 50

p <- ggplot(wtp_plot, aes(x = wtp, y = attr_f)) +
  geom_col(width = 0.65, fill = col_bar) +
  geom_errorbar(
    aes(xmin = lower, xmax = upper),
    width = 0.28, linewidth = 0.45, colour = col_err
  ) +
  geom_vline(xintercept = 0, colour = col_err, linewidth = 0.4) +
  facet_wrap(~ class_f, ncol = 4) +
  coord_cartesian(xlim = c(x_min, x_max)) +
  scale_x_continuous(
    labels = scales::label_comma(),
    breaks = seq(0, x_max, by = 500)
  ) +
  labs(x = "Marginal WTP (SEK/month)", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    text                = element_text(colour = col_text),
    strip.text          = element_text(face = "bold", size = 9, colour = col_text, lineheight = 1.15),
    strip.background    = element_rect(fill = col_strip, colour = NA),
    panel.spacing       = unit(1, "lines"),
    panel.grid.minor    = element_blank(),
    panel.grid.major.y  = element_blank(),
    panel.grid.major.x  = element_line(colour = "grey88", linewidth = 0.3),
    axis.text           = element_text(colour = col_text, size = 8.5),
    axis.title.x        = element_text(colour = col_text, size = 10, margin = margin(t = 8)),
    axis.text.y         = element_text(lineheight = 1.1),
    plot.margin         = margin(8, 12, 8, 8)
  )

# ==============================================================================
# SAVE
# ==============================================================================

dir.create(here("output", "figures"), showWarnings = FALSE)

ggsave(here("output", "figures", "fig5_lc4_wtp_plain.pdf"),
       plot = p, width = 11, height = 4.2, device = cairo_pdf)

ggsave(here("output", "figures", "fig5_lc4_wtp_plain.png"),
       plot = p, width = 11, height = 4.2, dpi = 300)

cat("Saved: output/figures/fig5_lc4_wtp_plain.pdf + .png\n")
