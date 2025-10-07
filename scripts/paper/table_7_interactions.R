library(tidyverse)
library(here)
library(broom)
library(dplyr)
library(texreg)

# load models

mxl_rent <- readRDS( here("output/models", "mxl_age_rent.rds"))
mxl_own <- readRDS( here("output/models", "mxl_age_own.rds"))



## ---- WTP for each model (rent/own) ------------------------------------------

# 1) Attribute map (matches your variable names)
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
         "55-64" = NA_character_
  )
}

# 2) Core helper that takes a MODEL and returns a WTP table
wtp_table_for_model <- function(model, mhc_list) {
  coefs <- coef(model)
  wtp_by_group <- function(display_attr, group = c("55-64", "65-74", "75+")) {
    group <- match.arg(group)
    row <- dplyr::filter(attr_map, display == display_attr)
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
  
  expand.grid(
    attribute = attributes,
    age_group = c("55-64", "65-74", "75+"),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(
      WTP_pct = purrr::map2_dbl(attribute, age_group, ~ wtp_by_group(.x, .y)),
      WTP_sek = purrr::map2_dbl(age_group, WTP_pct, ~ .y * 0.10 * mhc_list[.x])
    ) %>%
    dplyr::arrange(attribute, age_group)
}

# 3) Provide your age-specific planned-cost medians (or means) for SEK scaling
#    (use your own numbers here — these are from your example)
mhc_list <- c(`55-64` = 11108, `65-74` = 10769, `75+` = 9696)

# 4) Compute WTP tables for each model
wtp_rent <- wtp_table_for_model(mxl_rent, mhc_list) %>% mutate(model = "Renter")
wtp_own  <- wtp_table_for_model(mxl_own,  mhc_list) %>% mutate(model = "Owner")

# 5) Preview (markdown tables)
wtp_rent %>%
  mutate(WTP_pct = round(WTP_pct, 3), WTP_sek = round(WTP_sek, 1)) %>%
  knitr::kable(format = "markdown", col.names = c("Attribute","Age group","WTP (% of cost)","WTP (SEK/month)","Model"))

wtp_own %>%
  mutate(WTP_pct = round(WTP_pct, 3), WTP_sek = round(WTP_sek, 1)) %>%
  knitr::kable(format = "markdown", col.names = c("Attribute","Age group","WTP (% of cost)","WTP (SEK/month)","Model"))

# (optional) combined table:
wtp_both <- bind_rows(wtp_rent, wtp_own)




