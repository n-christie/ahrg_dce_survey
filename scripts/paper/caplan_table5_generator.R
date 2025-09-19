
# Caplan-style Table 5 Generator
# This script builds a 5-column model comparison table:
# MNL | ML Mean | ML SD | MRS (per 10%) | MWTP (SEK/month)

library(pacman)
p_load(tidyverse, here, stringr, logitr, texreg, broom, purrr)

# Load data
df_model <- readRDS(here("data/formr", "df_model.rds"))

## Change factor levels ----

df_model <- df_model %>%
  mutate(
    price_pp   = as.numeric(price) / 100,                 # -0.20 … +0.20 (fraction)
    cost_new   = planed_cost * (1 + price_pp),            # SEK total under scenario
    cost_diff  = planed_cost * price_pp,                  # SEK delta from baseline (can be 0)
    # keep a numeric % version too if you want it
    price_pct  = as.numeric(price),                       # -20..20 (percent units)
    # set consistent factor baselines
    dist_trans = factor(dist_trans, levels = c("900","600","300")),
    dist_green = factor(dist_green, levels = c("15km", "5km", "500 meter")),
    dist_shops = factor(dist_shops, levels = c("15km", "5km", "500 meter")),
    proportion_of_income = planed_cost/income,
    age = floor(Age_T3),
    age_group = factor(case_when(
      age >= 55 & age < 65 ~ "55-64",
      age >= 65 & age < 75 ~ "65-74",
      age >= 75 ~ "75+",
      
    )),
    price_num = price_num/100
    #green_x_male = (dist_green == "500 meter") * (Sex == 1)
  ) %>% 
  mutate(
    male    = as.integer(Sex == 1),
    female  = as.integer(Sex == 2),
    gar     = as.integer(parking == "reserverad garageplats"),
    splats  = as.integer(parking == "reserverad P-plats"),
    # interactions with price (in fraction units)
    gar_x_price   = gar   * price_pp,
    splats_x_price= splats* price_pp,
    green500 = as.integer(dist_green == "500 meter"),
    shops500 = as.integer(dist_shops == "500 meter"),
    # interactions with female
    green500_x_female = green500 * female,
    shops500_x_female = shops500 * female
  )


# Define model settings
cost_name     <- "price_num"
median_cost   <- 10000
MHC10         <- 0.10 * median_cost  # 10% of median cost

# Run MNL and Mixed Logit for males
mnl <- logitr(
  data    = df_model %>% filter(Sex == 1),
  outcome = "choice",
  obsID   = "obsID",
  panelID = "panelID",
  pars    = c("dist_green", "dist_shops", "dist_trans", "parking", cost_name)
)

mxl <- logitr(
  data    = df_model %>% filter(Sex == 1),
  outcome = "choice",
  obsID   = "obsID",
  panelID = "panelID",
  pars    = c("dist_green", "dist_shops", "dist_trans", "parking", cost_name),
  randPars = c(dist_green = 'n', dist_shops = 'n', dist_trans = 'n', parking = 'n'),
  numMultiStarts = 10,
  drawType = 'sobol',
  numDraws = 50,
  correlation = TRUE
)

# Extract model pieces
b_mnl <- coef(mnl)
V_mnl <- vcov(mnl)
tidy_mxl <- broom::tidy(mxl)
is_sd <- str_detect(tidy_mxl$term, "^sd_")
mxl_means <- tidy_mxl %>% filter(!is_sd)
mxl_sds   <- tidy_mxl %>% filter(is_sd)

b_mxl_mean <- setNames(mxl_means$estimate, mxl_means$term)
se_mxl_mean <- setNames(mxl_means$std.error, mxl_means$term)
b_mxl_sd <- setNames(mxl_sds$estimate, mxl_sds$term)
se_mxl_sd <- setNames(mxl_sds$std.error, mxl_sds$term)

attr_terms <- setdiff(names(b_mnl), c("(Intercept)", cost_name))
attr_terms <- intersect(attr_terms, names(b_mxl_mean))

# McFadden R2 and LR test
ll_mnl <- as.numeric(logLik(mnl)); k_mnl <- attr(logLik(mnl), "df")
ll_null_mnl <- mnl$nullLogLik
r2_mnl <- 1 - (ll_mnl / ll_null_mnl)
lr_mnl <- -2 * (ll_null_mnl - ll_mnl)
p_lr_mnl <- pchisq(lr_mnl, df = k_mnl, lower.tail = FALSE)

ll_mxl <- as.numeric(logLik(mxl)); k_mxl <- attr(logLik(mxl), "df")
ll_null_mxl <- mxl$nullLogLik
r2_mxl <- 1 - (ll_mxl / ll_null_mxl)
lr_mxl <- -2 * (ll_null_mxl - ll_mxl)
p_lr_mxl <- pchisq(lr_mxl, df = k_mxl, lower.tail = FALSE)

# Manual BIC
compute_bic <- function(model) {
  ll <- as.numeric(logLik(model))
  k <- attr(logLik(model), "df")
  n <- model$n$obs
  k * log(n) - 2 * ll
}
bic_mnl <- compute_bic(mnl)
bic_mxl <- compute_bic(mxl)

# Delta-method for MRS
dm_var_ratio <- function(V, a_name, c_name, beta) {
  a <- beta[a_name]; c <- beta[c_name]
  grad <- matrix(c(-1/c, a/(c^2)), ncol = 1)
  Vsub <- V[c(a_name, c_name), c(a_name, c_name), drop = FALSE]
  as.numeric(t(grad) %*% Vsub %*% grad)  # ✅ use Vsub (no underscore)
}



mrs_list <- lapply(attr_terms, function(a){
  mrs_hat <- - b_mxl_mean[a] / b_mxl_mean[cost_name]
  var_mrs <- dm_var_ratio(vcov(mxl), a, cost_name, b_mxl_mean)
  se_mrs  <- sqrt(var_mrs)
  tibble(MRS = mrs_hat, SE = se_mrs)
})
mrs_mat <- bind_rows(mrs_list)
rownames(mrs_mat) <- attr_terms

# MWTP
mwtp_mat <- mrs_mat %>%
  mutate(MWTP = MRS * MHC10, MWTP_SE = SE * MHC10)

# Build texreg models
m1 <- createTexreg(attr_terms, b_mnl[attr_terms], sqrt(diag(V_mnl))[attr_terms],
                   2*pnorm(-abs(b_mnl[attr_terms]/sqrt(diag(V_mnl))[attr_terms])),
                   gof.names = c("Obs", "Log Likelihood", "AIC", "BIC", "McFadden R²",
                                 paste0("LR χ² (df=", k_mnl, ")"), "p-value (LR)"),
                   gof = c(mnl$n$obs, ll_mnl, AIC(mnl), bic_mnl, r2_mnl, lr_mnl, p_lr_mnl),
                   gof.decimal = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))

m2 <- createTexreg(attr_terms, b_mxl_mean[attr_terms], se_mxl_mean[attr_terms],
                   2*pnorm(-abs(b_mxl_mean[attr_terms]/se_mxl_mean[attr_terms])),
                   gof.names = c("Obs", "Log Likelihood", "AIC", "BIC", "McFadden R²",
                                 paste0("LR χ² (df=", k_mxl, ")"), "p-value (LR)"),
                   gof = c(mxl$n$obs, ll_mxl, AIC(mxl), bic_mxl, r2_mxl, lr_mxl, p_lr_mxl),
                   gof.decimal = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))

m3 <- createTexreg(attr_terms, b_mxl_sd[attr_terms], se_mxl_sd[attr_terms],
                   2*pnorm(-abs(b_mxl_sd[attr_terms]/se_mxl_sd[attr_terms])))

m4 <- createTexreg(attr_terms, mrs_mat$MRS, mrs_mat$SE,
                   2*pnorm(-abs(mrs_mat$MRS / mrs_mat$SE)))

m5 <- createTexreg(attr_terms, mwtp_mat$MWTP, mwtp_mat$MWTP_SE,
                   2*pnorm(-abs(mwtp_mat$MWTP / mwtp_mat$MWTP_SE)))

# Display table
screenreg(list(m1, m2, m3, m4, m5),
  custom.model.names = c("MNL", "ML Mean", "ML SD", "MRS (per 10%)", "MWTP (SEK/mo)"),
  digits = 3, stars = c(0.001, 0.01, 0.05), booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE
)
