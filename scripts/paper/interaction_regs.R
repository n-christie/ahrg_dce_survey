library(pacman)

p_load(tidyverse, here, stringr, formr, logitr, cbcTools, texreg, likert, tidyr, haven, kableExtra,ggrepel, sjlabelled, summarytools,gtsummary,texreg)

# load data ----

df_model <- readRDS(here("data/formr", "df_model.rds"))

## Load data

library(dplyr)
library(broom)
library(purrr)
library(texreg)
library(stringr)

cost_name   <- "price_num"
set.seed(12345)  # or any fixed number


# Data cleaning ----

## Change factor levels ----

# -- Variable transformations, factor baselines, age groups, and derived vars --

# -- Variable transformations, factor baselines, age groups, and derived vars --

df_model <- df_model %>%
  mutate(
    price_pp   = as.numeric(price) / 100,
    cost_new   = planed_cost * (1 + price_pp),
    cost_diff  = planed_cost * price_pp,
    price_pct  = as.numeric(price),
    proportion_of_income = planed_cost / income,
    age = floor(Age_T3),
    age_group = factor(case_when(
      age >= 55 & age < 65 ~ "55-64",
      age >= 65 & age < 75 ~ "65-74",
      age >= 75 ~ "75+"
    )),
    price_num = price_num / 100,
    
    # Set reference levels
    dist_trans = factor(dist_trans, levels = c("900", "600", "300")),
    dist_green = factor(dist_green, levels = c("15km", "5km", "500 meter")),
    dist_shops = factor(dist_shops, levels = c("15km", "5km", "500 meter"))
  ) %>%
  mutate(
    # Demographics
    ägandebostad = haven::as_factor(ägandebostad),
    bostadstyp   = haven::as_factor(bostadstyp),
    Sex          = haven::as_factor(Sex),
    Civil        = haven::as_factor(civil_status_T2),
    civil_d      = factor(if_else(civil_status_T2 == 1, "Partnered", "Not partnered")),
    Own          = factor(if_else(ägandebostad == "Ja", "Owner", "Renter")),
    Hus          = factor(if_else(
      bostadstyp %in% c("Friliggande villa/hus/gård", "Radhus/kedjehus/parhus"),
      "House", "Apartment/Condo"
    )),
    downsize = if_else(planed_cost < monthcost, 1, 0)
  ) %>%
  # Age group dummies
  mutate(
    G65_74 = as.integer(age_group == "65-74"),
    G75p   = as.integer(age_group == "75+")
  )

# -- Create dummy-coded variables for attribute levels --

df_dummies <- model.matrix(~ dist_green + dist_shops + dist_trans + parking - 1, data = df_model) %>%
  as_tibble()

df_model <- bind_cols(df_model, df_dummies)

# -- Rename dummy variables to clean names --

df_model <- df_model %>%
  rename(
    dist_green5km   = `dist_green5km`,
    dist_green500m  = `dist_green500 meter`,
    dist_shops5km   = `dist_shops5km`,
    dist_shops500m  = `dist_shops500 meter`,
    dist_trans600   = `dist_trans600`,
    dist_trans300   = `dist_trans300`,
    park_space      = `parkingreserverad P-plats`,
    park_garage     = `parkingreserverad garageplats`
  )

# -- Create all interaction terms in one clean block --

df_model <- df_model %>%
  mutate(
    # Green space
    green5km_G65_74    = dist_green5km   * G65_74,
    green500_G65_74    = dist_green500m  * G65_74,
    green5km_G75p      = dist_green5km   * G75p,
    green500_G75p      = dist_green500m  * G75p,
    
    # Shops
    shops5km_G65_74    = dist_shops5km   * G65_74,
    shops500_G65_74    = dist_shops500m  * G65_74,
    shops5km_G75p      = dist_shops5km   * G75p,
    shops500_G75p      = dist_shops500m  * G75p,
    
    # Transit
    trans600_G65_74    = dist_trans600   * G65_74,
    trans300_G65_74    = dist_trans300   * G65_74,
    trans600_G75p      = dist_trans600   * G75p,
    trans300_G75p      = dist_trans300   * G75p,
    
    # Parking
    park_space_G65_74  = park_space      * G65_74,
    park_garage_G65_74 = park_garage     * G65_74,
    park_space_G75p    = park_space      * G75p,
    park_garage_G75p   = park_garage     * G75p,
    
    # Price interactions
    price_G65_74       = price_num * G65_74,
    price_G75p         = price_num * G75p
  )


  
# Age interaction regression ----

mxl_age <- logitr(
  data = df_model %>% filter(!is.na(age_group),
                             Own != "Owner"),
  outcome = "choice",
  obsID = "obsID",
  panelID = "panelID",
  pars = c(
    # main effects
    "dist_green5km", "dist_green500m",
    "dist_shops5km", "dist_shops500m",
    "dist_trans600", "dist_trans300",
    "park_space",    "park_garage",
    "price_num",
    
    # interactions with G65_74
    "green5km_G65_74",   "green500_G65_74",
    "shops5km_G65_74",   "shops500_G65_74",
    "trans600_G65_74",   "trans300_G65_74",
    "park_space_G65_74", "park_garage_G65_74",
    "price_G65_74",
    
    # interactions with G75p
    "green5km_G75p",   "green500_G75p",
    "shops5km_G75p",   "shops500_G75p",
    "trans600_G75p",   "trans300_G75p",
    "park_space_G75p", "park_garage_G75p",
    "price_G75p"
  ),
  randPars = c(
    dist_green5km   = "n",
    dist_green500m  = "n",
    dist_shops5km   = "n",
    dist_shops500m  = "n",
    dist_trans600   = "n",
    dist_trans300   = "n",
    park_space      = "n",
    park_garage     = "n"
  ),
  numDraws = 100,
  numMultiStarts = 10,
  drawType = "sobol",
  correlation = TRUE
)



screenreg(mxl_age)




### wtp ----

coefs <- coef(mxl_age)

# Create a mapping of display attributes to real base + interaction names
attr_map <- tibble::tibble(
  display = c(
    "Green space: 5km", "Green space: 500m",
    "Shops: 5km",       "Shops: 500m",
    "Transit: 600m",    "Transit: 300m",
    "Parking: reserved space", "Parking: reserved garage"
  ),
  base    = c(
    "dist_green5km",   "dist_green500m",
    "dist_shops5km",   "dist_shops500m",
    "dist_trans600",   "dist_trans300",
    "park_space",      "park_garage"
  ),
  G65_74 = c(
    "green5km_G65_74",   "green500_G65_74",
    "shops5km_G65_74",   "shops500_G65_74",
    "trans600_G65_74",   "trans300_G65_74",
    "park_space_G65_74", "park_garage_G65_74"
  ),
  G75p = c(
    "green5km_G75p",   "green500_G75p",
    "shops5km_G75p",   "shops500_G75p",
    "trans600_G75p",   "trans300_G75p",
    "park_space_G75p", "park_garage_G75p"
  )
)


group_suffix <- function(group) {
  switch(group,
         "65-74" = "G65_74",
         "75+"   = "G75p",
         "55-64" = NA)
}


wtp_by_group <- function(display_attr, group = c("55-64", "65-74", "75+")) {
  group <- match.arg(group)
  
  row <- attr_map %>% filter(display == display_attr)
  base <- row$base
  int  <- if (group == "65-74") row$G65_74 else if (group == "75+") row$G75p else NA
  
  num <- coefs[[base]]
  denom <- coefs[["price_num"]]
  
  if (!is.na(int)) {
    price_int <- paste0("price_", group_suffix(group))
    num <- num + coefs[[int]]
    denom <- denom + coefs[[price_int]]
  }
  
  - num / denom
}

attributes <- attr_map$display

# Optional: use age-specific median planned cost (example)
mhc_55_64 <- 9382
mhc_65_74 <- 11530
mhc_75p   <- 9865
mhc_list <- c(`55-64` = mhc_55_64, `65-74` = mhc_65_74, `75+` = mhc_75p)

# age_group mean_plan med_plan
# <fct>         <dbl>    <dbl>
#   1 55-64        11108.    10000
# 2 65-74        10769.    10000
# 3 75+           9696.     9000


# age_group Own    mean_plan med_plan
# <fct>     <fct>      <dbl>    <dbl>
#   1 55-64     Owner     11549.    10000
# 2 55-64     Renter     9382.     9000
# 3 65-74     Owner     10608.    10000
# 4 65-74     Renter    11530.    10000
# 5 75+       Owner      9661.     9000
# 6 75+       Renter     9865.     9000

results <- expand.grid(
  attribute = attributes,
  age_group = c("55-64", "65-74", "75+"),
  stringsAsFactors = FALSE
) %>%
  mutate(
    WTP_pct = map2_dbl(attribute, age_group, ~wtp_by_group(.x, .y)),
    WTP_sek = map2_dbl(age_group, WTP_pct, ~.y * 0.10 * mhc_list[.x])
  )

results %>%
  arrange(attribute, age_group) %>%
  mutate(
    WTP_pct = round(WTP_pct, 3),
    WTP_sek = round(WTP_sek, 1)
  ) %>%
  knitr::kable(
    format = "markdown",
    col.names = c("Attribute", "Age group", "WTP (% of cost)", "WTP (SEK/month)")
  )





# plot coeffcients -----



coef_df <- tidy(mxl_age) %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )


get_wtp <- function(base, interaction = NULL, group = "55-64") {
  if (!(base %in% names(coefs))) {
    return(tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_))
  }
  
  est <- coefs[[base]]
  se  <- coef_df %>% filter(term == base) %>% pull(std.error)
  
  if (!is.null(interaction) && !is.na(interaction) && interaction %in% names(coefs)) {
    est <- est + coefs[[interaction]]
    se_int <- coef_df %>% filter(term == interaction) %>% pull(std.error)
    se <- sqrt(se^2 + se_int^2)
    
    price_term <- paste0("price_", group_suffix(group))
    if (price_term %in% names(coefs)) {
      denom <- coefs[["price_num"]] + coefs[[price_term]]
    } else {
      denom <- coefs[["price_num"]]
    }
  } else {
    denom <- coefs[["price_num"]]
  }
  
  wtp <- -est / denom
  wtp_sek <- wtp * 0.10 * mhc_list[[group]]
  se_wtp <- (1.96 * se / abs(denom)) * 0.10 * mhc_list[[group]]
  
  return(tibble(
    estimate = wtp_sek,
    conf.low = wtp_sek - se_wtp,
    conf.high = wtp_sek + se_wtp
  ))
}

plot_df <- expand.grid(
  attribute = attr_map$display,
  age_group = c("55-64", "65-74", "75+"),
  stringsAsFactors = FALSE
) %>%
  left_join(attr_map, by = c("attribute" = "display")) %>%
  rowwise() %>%
  mutate(
    interaction_term = case_when(
      age_group == "65-74" ~ G65_74,
      age_group == "75+"   ~ G75p,
      TRUE ~ NA_character_
    ),
    wtp_info = list(get_wtp(base, interaction_term, age_group))
  ) %>%
  unnest(wtp_info)

plot_df <- plot_df %>%
  mutate(attribute = case_when(
    attribute == "Green space: 500m" ~ "Green space: 500 m (ref: 15 km)",
    attribute == "Green space: 5km"  ~ "Green space: 5 km (ref: 15 km)",
    attribute == "Shops: 500m"       ~ "Shops: 500 m (ref: 15 km)",
    attribute == "Shops: 5km"        ~ "Shops: 5 km (ref: 15 km)",
    attribute == "Transit: 300m"     ~ "Transit stop: 300 m (ref: 900 m)",
    attribute == "Transit: 600m"     ~ "Transit stop: 600 m (ref: 900 m)",
    attribute == "Parking: reserved garage" ~ "Parking: garage (ref: none)",
    attribute == "Parking: reserved space"  ~ "Parking: reserved (ref: none)",
    TRUE ~ attribute
  ))

plot_df$attribute <- factor(plot_df$attribute, levels = c(
  "Green space: 5 km (ref: 15 km)",
  "Green space: 500 m (ref: 15 km)",
  "Shops: 5 km (ref: 15 km)",
  "Shops: 500 m (ref: 15 km)",
  "Transit stop: 600 m (ref: 900 m)",
  "Transit stop: 300 m (ref: 900 m)",
  "Parking: reserved (ref: none)",
  "Parking: garage (ref: none)"
))



ggplot(plot_df, aes(x = estimate, y = attribute, color = age_group)) +
  geom_point(position = position_dodge(width = 0.6), size = 2.5) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.6), height = 0.2) +
  labs(
    x = "WTP (SEK/month)", y = NULL, color = NULL
  ) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 11),
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  )


ggsave(here("docs/elsvier/figures", "wtp_age_interactions_renter.png"), width = 6, height = 6)



## load plots to combine

library(magick)



# Load both images
img_renter <- image_read(here("docs/elsvier/figures", "wtp_age_interactions_renter.png"))
img_owner  <- image_read(here("docs/elsvier/figures", "wtp_age_interactions_owner.png"))

# Combine side by side
combined_img <- image_append(c(img_renter, img_owner), stack = FALSE)

# Save result
image_write(combined_img, path = here("docs/elsvier/figures", "wtp_combined_side_by_side.png"), format = "png")
