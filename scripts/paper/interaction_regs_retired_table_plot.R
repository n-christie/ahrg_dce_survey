# --- Load models ---
mxl_income_ret    <- readRDS(here("output/models", "mxl_income_retired.rds"))
mxl_income_notret <- readRDS(here("output/models", "mxl_income_notretired.rds"))

screenreg(list(mxl_income_ret, mxl_income_notret))

# --- Settings ---
cost_name   <- "price_num"
median_cost <- 10500
scaler      <- 0.10 * median_cost  # converts MRS to SEK/month

# --- Label map for texreg output ---
label_map <- c(
  "dist_green5km"   = "Green space: 5 km (vs 15 km)",
  "dist_green500m"  = "Green space: 500 m (vs 15 km)",
  "dist_shops5km"   = "Shops: 5 km (vs 15 km)",
  "dist_shops500m"  = "Shops: 500 m (vs 15 km)",
  "dist_trans600"   = "Transit stop: 600 m (vs 900 m)",
  "dist_trans300"   = "Transit stop: 300 m (vs 900 m)",
  "park_garage"     = "Parking: reserved garage (vs none)",
  "park_space"      = "Parking: reserved space (vs none)",
  "price_num"       = "Price"
)


interaction_to_base <- c(
  "green5km_HighInc"   = "dist_green5km",
  "green500_HighInc"   = "dist_green500m",
  "shops5km_HighInc"   = "dist_shops5km",
  "shops500_HighInc"   = "dist_shops500m",
  "trans600_HighInc"   = "dist_trans600",
  "trans300_HighInc"   = "dist_trans300",
  "park_space_HighInc" = "park_space",
  "park_garage_HighInc"= "park_garage",
  "price_HighInc"      = "price_num"
)

pretty_interaction <- function(term, label_map) {
  base <- interaction_to_base[[term]]
  paste0(label_map[[base]], " × High income")
}

main_attrs <- c("dist_green5km","dist_green500m",
                "dist_shops5km","dist_shops500m",
                "dist_trans600","dist_trans300",
                "park_garage","park_space")

inter_terms_inc <- c("green5km_HighInc","green500_HighInc",
                     "shops5km_HighInc","shops500_HighInc",
                     "trans600_HighInc","trans300_HighInc",
                     "park_garage_HighInc","park_space_HighInc",
                     "price_HighInc")

coef_rows <- c(main_attrs, "price_num", inter_terms_inc)

pretty_labels <- c(
  unname(label_map[main_attrs]),
  label_map[["price_num"]],
  sapply(inter_terms_inc, pretty_interaction, label_map = label_map)
)


# --- Helper functions -----------------------------------------------------

grab_term <- function(tidy_df, term) {
  row <- tidy_df[tidy_df$term == term, ]
  if (nrow(row) == 0) return(c(NA_real_, NA_real_, NA_real_))
  c(row$estimate[1], row$std.error[1], row$p.value[1])
}

term_group <- function(term) {
  if (term %in% main_attrs) return("base")
  if (grepl("HighInc$", term)) return("HighInc")
  if (term %in% c("price_num")) return("price")
  if (grepl("^price_", term)) return("price_inter")
  "other"
}

term_attr <- function(term) {
  if (term %in% main_attrs) return(term)
  if (term %in% names(interaction_to_base)) return(interaction_to_base[[term]])
  NA_character_
}

mwtp_for_attr_group <- function(attr, group, tidy_df, V, scaler, cost_name) {
  beta_a   <- grab_term(tidy_df, attr)[1]
  beta_c   <- grab_term(tidy_df, cost_name)[1]
  a_g_term <- if (group == "base") NA_character_
  else names(interaction_to_base)[interaction_to_base == attr & grepl(group, names(interaction_to_base))]
  c_g_term <- if (group == "base") NA_character_
  else names(interaction_to_base)[interaction_to_base == cost_name & grepl(group, names(interaction_to_base))]
  beta_a_g <- if (length(a_g_term)) grab_term(tidy_df, a_g_term)[1] else 0
  beta_c_g <- if (length(c_g_term)) grab_term(tidy_df, c_g_term)[1] else 0
  
  if (any(is.na(c(beta_a, beta_c)))) return(c(NA_real_, NA_real_, NA_real_))
  
  A <- beta_a + ifelse(group == "base", 0, beta_a_g)
  C <- beta_c + ifelse(group == "base", 0, beta_c_g)
  val <- -(A / C) * scaler
  
  terms <- c(attr, cost_name,
             if (group != "base") a_g_term else NULL,
             if (group != "base") c_g_term else NULL)
  terms <- terms[!is.na(terms) & nzchar(terms)]
  if (is.null(V) || !all(terms %in% rownames(V))) return(c(val, NA_real_, NA_real_))
  
  grad <- numeric(length(terms))
  grad[terms == attr]       <- -1 / C * scaler
  grad[terms == cost_name]  <- (A / (C^2)) * scaler
  if (group != "base") {
    if (any(terms == a_g_term)) grad[terms == a_g_term] <- -1 / C * scaler
    if (any(terms == c_g_term)) grad[terms == c_g_term] <- (A / (C^2)) * scaler
  }
  
  Vsub <- V[terms, terms, drop = FALSE]
  var  <- as.numeric(t(grad) %*% Vsub %*% grad)
  if (!is.finite(var) || var < 0) return(c(val, NA_real_, NA_real_))
  se   <- sqrt(var)
  pval <- if (se == 0 || !is.finite(se)) NA_real_ else 2 * pnorm(-abs(val / se))
  c(val, se, pval)
}

compute_bic <- function(model) {
  ll <- as.numeric(logLik(model))
  k  <- attr(logLik(model), "df")
  n  <- tryCatch(model$n$obs, error = function(e) NA)
  if (is.na(n)) return(NA_real_)
  k * log(n) - 2 * ll
}

safe_null_ll <- function(model) {
  tryCatch(if (!is.null(model$nullLogLik)) model$nullLogLik else NA_real_, error = function(e) NA_real_)
}

lr_bits <- function(model) {
  ll  <- as.numeric(logLik(model))
  ll0 <- safe_null_ll(model)
  k   <- attr(logLik(model), "df")
  if (is.na(ll0)) return(list(lr = NA_real_, p = NA_real_, k = k))
  lr <- -2 * (ll0 - ll)
  p  <- pchisq(lr, df = k, lower.tail = FALSE)
  list(lr = lr, p = p, k = k)
}

build_pair_compact <- function(model, label_map, scaler, cost_name) {
  tdf <- broom::tidy(model)
  V   <- tryCatch(vcov(model), error = function(e) NULL)
  
  coef_vals <- setNames(rep(NA_real_, length(coef_rows)), coef_rows)
  se_vals   <- coef_vals
  p_vals    <- coef_vals
  for (term in coef_rows) {
    x <- grab_term(tdf, term)
    coef_vals[term] <- x[1]; se_vals[term] <- x[2]; p_vals[term] <- x[3]
  }
  
  mwtp_vals <- setNames(rep(NA_real_, length(coef_rows)), coef_rows)
  mwtp_se   <- mwtp_vals
  mwtp_p    <- mwtp_vals
  for (term in coef_rows) {
    g <- term_group(term)
    if (g %in% c("price","price_inter","other")) next
    a <- term_attr(term)
    out <- mwtp_for_attr_group(a, ifelse(g == "base","base", g), tdf, V, scaler, cost_name)
    mwtp_vals[term] <- out[1]; mwtp_se[term] <- out[2]; mwtp_p[term] <- out[3]
  }
  
  ll   <- as.numeric(logLik(model))
  aic  <- AIC(model)
  bic  <- compute_bic(model)
  nobs <- tryCatch(model$n$obs, error = function(e) NA_integer_)
  ll0  <- safe_null_ll(model)
  mcf  <- if (!is.na(ll0)) 1 - (ll / ll0) else NA_real_
  lr   <- lr_bits(model)
  
  gof_names <- c("Num. obs.", "Log Likelihood", "AIC", "BIC", "McFadden R²",
                 paste0("LR χ² (df=", lr$k, ")"), "p-value (LR)")
  gof_vals  <- c(nobs, ll, aic, bic, mcf, lr$lr, lr$p)
  gof_dec   <- c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  
  m_coef <- createTexreg(
    coef.names  = pretty_labels,
    coef        = as.numeric(coef_vals),
    se          = as.numeric(se_vals),
    pvalues     = as.numeric(p_vals),
    gof.names   = gof_names,
    gof         = gof_vals,
    gof.decimal = gof_dec
  )
  
  m_mwtp <- createTexreg(
    coef.names = pretty_labels,
    coef       = as.numeric(mwtp_vals),
    se         = as.numeric(mwtp_se),
    pvalues    = as.numeric(mwtp_p)
  )
  
  list(m_coef = m_coef, m_mwtp = m_mwtp)
}






ret  <- build_pair_compact(mxl_income_ret,  label_map, scaler, cost_name)
notr <- build_pair_compact(mxl_income_noret, label_map, scaler, cost_name)

custom_header      <- list("Retired" = 1:2, "Not retired" = 3:4)
custom_model_names <- c("Coef.", "MWTP", "Coef.", "MWTP")





texreg(
  list(ret$m_coef, ret$m_mwtp, notr$m_coef, notr$m_mwtp),
  custom.header      = custom_header,
  custom.model.names = custom_model_names,
  custom.coef.names  = pretty_labels,
  digits             = 2,
  stars              = c(0.001, 0.01, 0.05),
  booktabs           = TRUE,
  dcolumn            = TRUE,
  use.packages       = FALSE,
  na.replace         = "--",
  caption            = "Mixed Logit with Income Interactions: Retired vs Not Retired (Coefficients and MWTP per Row)",
  caption.above      = TRUE,
  fontsize           = "scriptsize",
  file               = here("docs/elsvier/tables", "mxl_income_interactions_ret_notret.tex")
)


# Plot coeffceints ----

# --- Packages ---
library(broom)
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(here)

# --- Inputs ---
mhc_list <- list("Low" = 10500, "High" = 10500)  # scale for both groups

# --- Attribute map ---
attr_map <- tibble(
  display = c("Green space: 15km", "Green space: 5km", "Green space: 500m",
              "Shops: 15km", "Shops: 5km", "Shops: 500m",
              "Transit: 900m", "Transit: 600m", "Transit: 300m",
              "Parking: None", "Parking: reserved space", "Parking: reserved garage"),
  base     = c(NA, "dist_green5km", "dist_green500m",
               NA, "dist_shops5km", "dist_shops500m",
               NA, "dist_trans600", "dist_trans300",
               NA, "park_space", "park_garage"),
  HighInc  = c(NA, "green5km_HighInc", "green500_HighInc",
               NA, "shops5km_HighInc", "shops500_HighInc",
               NA, "trans600_HighInc", "trans300_HighInc",
               NA, "park_space_HighInc", "park_garage_HighInc")
)

# --- Function to compute WTP per attribute and income group ---------------
compute_wtp_df <- function(model, group_label) {
  coef_df <- tidy(model) %>%
    mutate(conf.low = estimate - 1.96 * std.error,
           conf.high = estimate + 1.96 * std.error)
  coefs <- setNames(coef_df$estimate, coef_df$term)
  
  get_wtp <- function(base, interaction = NULL, group = "Low") {
    if (!(base %in% names(coefs))) {
      return(tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_))
    }
    
    est <- coefs[[base]]
    se  <- coef_df %>% filter(term == base) %>% pull(std.error)
    
    if (!is.null(interaction) && !is.na(interaction) && interaction %in% names(coefs)) {
      est <- est + coefs[[interaction]]
      se_int <- coef_df %>% filter(term == interaction) %>% pull(std.error)
      se <- sqrt(se^2 + se_int^2)
      denom <- coefs[["price_num"]] + coefs[["price_HighInc"]]
    } else {
      denom <- coefs[["price_num"]]
    }
    
    wtp <- -est / denom
    wtp_sek <- wtp * 0.10 * mhc_list[[group]]
    se_wtp <- (1.96 * se / abs(denom)) * 0.10 * mhc_list[[group]]
    
    tibble(estimate = wtp_sek,
           conf.low = wtp_sek - se_wtp,
           conf.high = wtp_sek + se_wtp)
  }
  
  expand.grid(
    attribute = attr_map$display,
    income_group = c("Low", "High"),
    stringsAsFactors = FALSE
  ) %>%
    left_join(attr_map, by = c("attribute" = "display")) %>%
    rowwise() %>%
    mutate(
      interaction_term = if_else(income_group == "High", HighInc, NA_character_),
      wtp_info = list(get_wtp(base, interaction_term, income_group))
    ) %>%
    unnest(wtp_info) %>%
    mutate(model_group = group_label)
}

# --- Compute for both models ---
plot_df_ret  <- compute_wtp_df(mxl_income_ret,  "Retired")
plot_df_notr <- compute_wtp_df(mxl_income_noret, "Not retired")
plot_df <- bind_rows(plot_df_ret, plot_df_notr)

# --- Clean up labels & remove reference rows ---
plot_df <- plot_df %>%
  mutate(
    attribute = case_when(
      grepl("garage", attribute, ignore.case = TRUE) ~ "Parking: garage (ref: none)",
      grepl("reserved", attribute, ignore.case = TRUE) ~ "Parking: reserved (ref: none)",
      grepl("300m", attribute, ignore.case = TRUE) ~ "Transit stop: 300 m (ref: 900 m)",
      grepl("600m", attribute, ignore.case = TRUE) ~ "Transit stop: 600 m (ref: 900 m)",
      grepl("500m", attribute, ignore.case = TRUE) & grepl("shop", attribute, ignore.case = TRUE) ~ "Shops: 500 m (ref: 15 km)",
      grepl("5km", attribute, ignore.case = TRUE) & grepl("shop", attribute, ignore.case = TRUE) ~ "Shops: 5 km (ref: 15 km)",
      grepl("500m", attribute, ignore.case = TRUE) & grepl("green", attribute, ignore.case = TRUE) ~ "Green space: 500 m (ref: 15 km)",
      grepl("5km", attribute, ignore.case = TRUE) & grepl("green", attribute, ignore.case = TRUE) ~ "Green space: 5 km (ref: 15 km)",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(attribute))

# --- Factor order for attributes ---
attribute_levels <- c(
  "Parking: garage (ref: none)",
  "Parking: reserved (ref: none)",
  "Transit stop: 300 m (ref: 900 m)",
  "Transit stop: 600 m (ref: 900 m)",
  "Shops: 500 m (ref: 15 km)",
  "Shops: 5 km (ref: 15 km)",
  "Green space: 500 m (ref: 15 km)",
  "Green space: 5 km (ref: 15 km)"
)
plot_df$attribute <- factor(plot_df$attribute, levels = rev(attribute_levels))

# --- Shared color palette ---
income_colors <- c("Low" = "#1b9e77", "High" = "#d95f02")

# --- Plot function ---
plot_wtp <- function(df, title) {
  ggplot(df, aes(x = estimate, y = attribute, color = income_group)) +
    geom_point(position = position_dodge(width = 0.6), size = 3) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   position = position_dodge(width = 0.6),
                   height = 0.4, linewidth = 1) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
    scale_color_manual(values = income_colors) +
    scale_x_continuous(
      limits = c(-200, 1200),
      breaks = seq(-200, 1200, by = 200)
    ) +
    labs(x = "WTP (SEK/month)", y = NULL,
         color = "Income group", title = title) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.y = element_text(size = 15),
      axis.title.x = element_text(size = 13),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 13),
      panel.grid.minor = element_blank()
    )
}

# --- Create plots ---
p_ret  <- plot_wtp(filter(plot_df, model_group == "Retired"),  "Retired")
p_notr <- plot_wtp(filter(plot_df, model_group == "Not retired"), "Not retired")

# --- Combine with shared legend ---
combined_plot <- p_ret + p_notr + plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")

# --- Display and save ---
combined_plot

ggsave(
  here("docs/elsvier/figures", "wtp_income_interactions_ret_notret_publication.png"),
  plot = combined_plot,
  width = 13,
  height = 12,
  units = "in",
  dpi = 600
)

