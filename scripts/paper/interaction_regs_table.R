# scripts/retired_interactions_table.R
# Purpose: Mixed logit with Retired interactions + Caplan-style DWTP table
# Author: you
# Date: today

# --- Packages -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(logitr)
  library(broom)
  library(kableExtra)
  library(haven)
})

# --- Inputs -------------------------------------------------------------------
df_model   <- readRDS(here("data/formr", "df_model.rds"))
# Separate object for mean costs per group (you said this lives elsewhere)
sample_df  <- readRDS(here("data/formr", "survey_res.rds"))

# --- Data prep (match your pipeline) ------------------------------------------
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
    price_num  = price_num / 100,
    dist_trans = factor(dist_trans, levels = c("900", "600", "300")),
    dist_green = factor(dist_green, levels = c("15km", "5km", "500 meter")),
    dist_shops = factor(dist_shops, levels = c("15km", "5km", "500 meter"))
  ) %>%
  mutate(
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
  )

# --- Dummy-code attribute levels (same names you use) --------------------------
df_dummies <- model.matrix(~ dist_green + dist_shops + dist_trans + parking - 1, data = df_model) %>%
  as_tibble()
df_model <- bind_cols(df_model, df_dummies) %>%
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

# --- Retired dummy (robust to labels 1[Yes]/2[No]) ----------------------------
df_model <- df_model %>%
  mutate(
    VAR174_8_num = suppressWarnings(as.numeric(VAR174_8)),
    Retired = case_when(
      !is.na(VAR174_8_num) & VAR174_8_num == 1 ~ 1L,  # Yes
      !is.na(VAR174_8_num) & VAR174_8_num == 2 ~ 0L,  # No
      TRUE ~ NA_integer_
    )
  )

# --- Interactions: attribute x Retired + price x Retired ----------------------
df_model <- df_model %>%
  mutate(
    # Green
    green5km_R    = dist_green5km  * Retired,
    green500_R    = dist_green500m * Retired,
    # Shops
    shops5km_R    = dist_shops5km  * Retired,
    shops500_R    = dist_shops500m * Retired,
    # Transit
    trans600_R    = dist_trans600  * Retired,
    trans300_R    = dist_trans300  * Retired,
    # Parking
    park_space_R  = park_space     * Retired,
    park_garage_R = park_garage    * Retired,
    # Price interaction
    price_R       = price_num * Retired
  )

# --- Model (base random, interactions fixed) ----------------------------------
mxl_ret <- logitr(
  data    = df_model %>% filter(!is.na(Retired)),
  outcome = "choice",
  obsID   = "obsID",
  panelID = "panelID",
  pars    = c(
    # main effects
    "dist_green5km", "dist_green500m",
    "dist_shops5km", "dist_shops500m",
    "dist_trans600", "dist_trans300",
    "park_space",    "park_garage",
    "price_num",
    # retired interactions
    "green5km_R", "green500_R",
    "shops5km_R", "shops500_R",
    "trans600_R", "trans300_R",
    "park_space_R", "park_garage_R",
    "price_R"
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
  correlation    = TRUE,
  drawType       = "sobol",
  numDraws       = 100,
  numMultiStarts = 10
)

# quick console check (comment out in batch runs)
print(summary(mxl_ret))

# --- Group mean planned costs from separate object ----------------------------
# Replace zeros with NA before mean, and map VAR174_8 the same way.
cost_df <- sample_df %>%
  mutate(
    planed_cost = na_if(planed_cost, 0),
    VAR174_8_num = suppressWarnings(as.numeric(VAR174_8)),
    RetiredGrp = case_when(
      !is.na(VAR174_8_num) & VAR174_8_num == 1 ~ "Retired",
      !is.na(VAR174_8_num) & VAR174_8_num == 2 ~ "Not retired",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(RetiredGrp))

group_costs <- cost_df %>%
  group_by(RetiredGrp) %>%
  summarise(mean_cost = mean(planed_cost, na.rm = TRUE),
            n = n(), .groups = "drop")

mwtp_scale <- setNames(0.10 * group_costs$mean_cost, group_costs$RetiredGrp)

# --- WTP calculation helpers (with Delta-method CIs) --------------------------
co     <- broom::tidy(mxl_ret)
Sigma  <- vcov(mxl_ret)
gcoef  <- function(nm) co$estimate[match(nm, co$term)]
gse    <- function(nm) co$std.error[match(nm, co$term)]
gv     <- function(i,j) if(all(c(i,j)%in%rownames(Sigma))) Sigma[i,j] else NA_real_

# MRS (non-ret): f = -a/b
mrs_nonret <- function(a, b = "price_num") {
  a0 <- gcoef(a); b0 <- gcoef(b)
  val <- -a0/b0
  g <- c(-1/b0, a0/(b0^2))
  V <- matrix(c(gv(a,a), gv(a,b), gv(b,a), gv(b,b)), 2, 2)
  se <- sqrt(drop(t(g) %*% V %*% g))
  list(val=val, se=se)
}

# MRS (ret): f = -(a+ar)/(b+br)
mrs_ret <- function(a, ar, b="price_num", br="price_R") {
  a0 <- gcoef(a); ar0 <- gcoef(ar); b0 <- gcoef(b); br0 <- gcoef(br)
  val <- -(a0+ar0)/(b0+br0)
  g <- c(-(1/(b0+br0)), -(1/(b0+br0)), (a0+ar0)/((b0+br0)^2), (a0+ar0)/((b0+br0)^2))
  V <- matrix(c(
    gv(a,a), gv(a,ar), gv(a,b), gv(a,br),
    gv(ar,a), gv(ar,ar), gv(ar,b), gv(ar,br),
    gv(b,a), gv(b,ar), gv(b,b), gv(b,br),
    gv(br,a), gv(br,ar), gv(br,b), gv(br,br)
  ), 4, 4, byrow = TRUE)
  se <- sqrt(drop(t(g) %*% V %*% g))
  list(val=val, se=se)
}

# ΔMRS = MRS_ret - MRS_nonret
mrs_diff <- function(a, ar, b="price_num", br="price_R") {
  a0 <- gcoef(a); ar0 <- gcoef(ar); b0 <- gcoef(b); br0 <- gcoef(br)
  # f = -(a+ar)/(b+br) + a/b
  val <- -(a0+ar0)/(b0+br0) + a0/b0
  g <- c(
    # d/da
    -1/(b0+br0) + 1/b0,
    # d/dar
    -1/(b0+br0),
    # d/db
    (a0+ar0)/((b0+br0)^2) - a0/(b0^2),
    # d/dbr
    (a0+ar0)/((b0+br0)^2)
  )
  V <- matrix(c(
    gv(a,a), gv(a,ar), gv(a,b), gv(a,br),
    gv(ar,a), gv(ar,ar), gv(ar,b), gv(ar,br),
    gv(b,a), gv(b,ar), gv(b,b), gv(b,br),
    gv(br,a), gv(br,ar), gv(br,b), gv(br,br)
  ), 4, 4, byrow = TRUE)
  se <- sqrt(drop(t(g) %*% V %*% g))
  list(val=val, se=se)
}

to_ci <- function(val, se, mult=1.96) c(lo = val - mult*se, hi = val + mult*se)

# Attribute mapping (display → base + interaction names)
attr_map <- tibble::tibble(
  display = c(
    "Green space: 5km", "Green space: 500m",
    "Shops: 5km",       "Shops: 500m",
    "Transit: 600m",    "Transit: 300m",
    "Parking: reserved space", "Parking: reserved garage"
  ),
  base = c(
    "dist_green5km",   "dist_green500m",
    "dist_shops5km",   "dist_shops500m",
    "dist_trans600",   "dist_trans300",
    "park_space",      "park_garage"
  ),
  inter = c(
    "green5km_R",   "green500_R",
    "shops5km_R",   "shops500_R",
    "trans600_R",   "trans300_R",
    "park_space_R", "park_garage_R"
  )
)

# --- Build 3-column texreg table: Coef (stars), MRS, DWTP ---------------------

suppressPackageStartupMessages({
  library(texreg)
  library(broom)
  library(dplyr)
  library(stringr)
})

# 1) Attribute mapping (row order)
attr_map_ret <- tibble::tibble(
  display = c(
    "Green space: 5km", "Green space: 500m",
    "Shops: 5km",       "Shops: 500m",
    "Transit: 600m",    "Transit: 300m",
    "Parking: reserved space", "Parking: reserved garage"
  ),
  base = c(
    "dist_green5km",   "dist_green500m",
    "dist_shops5km",   "dist_shops500m",
    "dist_trans600",   "dist_trans300",
    "park_space",      "park_garage"
  ),
  inter = c(
    "green5km_R",   "green500_R",
    "shops5km_R",   "shops500_R",
    "trans600_R",   "trans300_R",
    "park_space_R", "park_garage_R"
  )
)

# 2) Pull model bits
tt   <- tidy(mxl_ret)
coef_lookup <- function(x, field = "estimate") tt[[field]][match(x, tt$term)]
beta_p      <- coef_lookup("price_num")
beta_pR     <- coef_lookup("price_R")
stopifnot(!is.na(beta_p))    # price must be there
if (is.na(beta_pR)) beta_pR <- 0  # if price_R not in model, fall back gracefully

# 3) Mean planned cost for DWTP scaling (from sample_df; 0 -> NA)
mwtp_scale_list <- sample_df %>%
  mutate(planed_cost = dplyr::na_if(planed_cost, 0),
         v8 = suppressWarnings(as.numeric(VAR174_8)),
         grp = dplyr::case_when(v8 == 1 ~ "Retired",
                                v8 == 2 ~ "Not retired",
                                TRUE    ~ NA_character_)) %>%
  filter(!is.na(grp)) %>%
  group_by(grp) %>%
  summarise(scale = 0.10*mean(planed_cost, na.rm = TRUE), .groups = "drop") %>%
  { setNames(.$scale, .$grp) }

scale_ret <- mwtp_scale_list[["Retired"]]
if (is.na(scale_ret)) stop("Could not compute Retired group's MWTP scale from sample_df.")

# 4) Compute the three columns per attribute
# Column 1: model coefficients (interaction terms) with stars
coef_vec  <- sapply(attr_map_ret$inter, coef_lookup, field = "estimate")
se_vec    <- sapply(attr_map_ret$inter, coef_lookup, field = "std.error")
p_vec     <- sapply(attr_map_ret$inter, coef_lookup, field = "p.value")

# Column 2: MRS (Retired)  = -(base + inter) / (price_num + price_R)
mrs_ret <- function(b, br) {
  b0  <- coef_lookup(b, "estimate")
  br0 <- coef_lookup(br, "estimate")
  -(b0 + br0) / (beta_p + beta_pR)
}
mrs_vec <- mapply(mrs_ret, attr_map_ret$base, attr_map_ret$inter)

# Column 3: DWTP (SEK/mo, Retired) = MRS_Ret * 0.10 * mean(planed_cost in Retired)
dwtp_vec <- as.numeric(mrs_vec) * as.numeric(scale_ret)

gof_na <- c(NA_real_, NA_real_, NA_real_, NA_real_)  # numeric NAs


# 5) Wrap each column into a "fake model" for texreg
mod_coef <- createTexreg(
  coef.names = attr_map_ret$display,
  coef       = as.numeric(coef_vec),
  se         = as.numeric(se_vec),
  pvalues    = as.numeric(p_vec),
  gof.names  = c("Observations","Log Likelihood","AIC","BIC"),
  gof        = gof_na
)

mod_mrs <- createTexreg(
  coef.names = attr_map_ret$display,
  coef       = as.numeric(mrs_vec),
  se         = rep(NA_real_, length(mrs_vec)),   # hide later with include.se = FALSE
  pvalues    = rep(1, length(mrs_vec)),          # no stars
  gof.names  = c("Observations","Log Likelihood","AIC","BIC"),
  gof        = gof_na
)

mod_dwtp <- createTexreg(
  coef.names = attr_map_ret$display,
  coef       = as.numeric(dwtp_vec),
  se         = rep(NA_real_, length(dwtp_vec)),  # hide later with include.se = FALSE
  pvalues    = rep(1, length(dwtp_vec)),         # no stars
  gof.names  = c("Observations","Log Likelihood","AIC","BIC"),
  gof        = gof_na
)


# 6) Print (or save) the 3-column table
screenreg(
  list(mod_coef, mod_mrs, mod_dwtp),
  custom.model.names = c("Coef. (Retired diff)","MRS (Retired)","DWTP (SEK/mo, Retired)"),
  custom.coef.names  = attr_map_ret$display,
  digits             = 2,
  include.se         = FALSE,           # hides SE rows for all columns
  stars              = c(0.001, 0.01, 0.05, 0.10),
  caption            = "Retired interactions: model coefficients (Δ vs non-retired), MRS and DWTP for the Retired group.",
  caption.above      = TRUE,
  fontsize           = "\\scriptsize",
  booktabs           = TRUE,
  use.packages       = FALSE,
  na.replace         = "---"
  # , file = here("docs/elsvier/tables","retired_mrs_dwtp.tex")  # uncomment to save
)
