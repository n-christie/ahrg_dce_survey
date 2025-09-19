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


# Data cleaning ----

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



view(dfSummary(df_model %>% select(Sex,
                                   Age = Age_T3,
                                   age_group,
                                   Income = income,
                                   monthcost,
                                   planed_cost,
                                   income_qrt
                                   
) %>% mutate(Income = as.numeric(Income),
             monthcost= as.numeric(monthcost),
             planed_cost = as.numeric(planed_cost)),
plain.ascii  = FALSE, 
style        = "grid", 
graph.magnif = 0.82, 
varnumbers   = FALSE,
valid.col    = FALSE))

# Regressions -----

## Baseline regression - Sex ----

mnl <- logitr(
  data    = df_model %>% filter(Sex == 2) ,
  outcome = "choice",
  obsID   = "obsID",
  panelID = "panelID",
  pars    = c("dist_green","dist_shops","dist_trans","parking","price_num")
)

mxl <- logitr(
  data    = df_model %>% filter(Sex == 2),
  outcome = "choice",
  obsID   = "obsID",
  panelID = "panelID",
  pars    = c("dist_green", "dist_shops", "dist_trans", "parking", "price_num"),
  randPars = c(dist_green = 'n', dist_shops = 'n', dist_trans = 'n', parking = 'n'),
  numMultiStarts = 10,
  drawType = 'sobol',
  numDraws = 50,
  correlation = TRUE
)



library(broom)
library(dplyr)
library(stringr)
library(texreg)



# function

compute_bic <- function(model) {
  ll <- as.numeric(logLik(model))
  k  <- attr(logLik(model), "df")
  n  <- tryCatch(model$n$obs, error = function(e) NA)
  if (is.na(n)) return(NA)
  k * log(n) - 2 * ll
}



# 0) Tidy outputs
t_mnl <- tidy(mnl)
t_mxl <- tidy(mxl)

# 1) Choose the row order from your MNL (drop intercept & price)
attr_terms <- t_mnl %>%
  #filter(!(term %in% c("(Intercept)", "price_num"))) %>%
  pull(term)

# 2) MNL (means)
coef1 <- setNames(t_mnl$estimate[match(attr_terms, t_mnl$term)], attr_terms)
se1   <- setNames(t_mnl$std.error[match(attr_terms, t_mnl$term)], attr_terms)
p1    <- setNames(t_mnl$p.value[match(attr_terms, t_mnl$term)], attr_terms)

# 3) ML means: same attribute rows, exclude any 'sd_' terms
t_mxl_mean <- t_mxl %>% filter(term %in% attr_terms)
coef2 <- setNames(t_mxl_mean$estimate[match(attr_terms, t_mxl_mean$term)], attr_terms)
se2   <- setNames(t_mxl_mean$std.error[match(attr_terms, t_mxl_mean$term)], attr_terms)
p2    <- setNames(t_mxl_mean$p.value[match(attr_terms, t_mxl_mean$term)], attr_terms)

# 4) ML SDs (diagonal only): rows like 'sd_<attr>_<same attr>'
t_mxl_sd <- t_mxl %>% filter(str_starts(term, "sd_"))
# extract the attribute name on the diagonal using a backreference
diag_attr <- str_match(t_mxl_sd$term, "^sd_(.*)_\\1$")[, 2]
sd_diag   <- t_mxl_sd %>% mutate(attr = diag_attr) %>% filter(!is.na(attr))

# align SDs to attr_terms order
coef3 <- setNames(rep(NA_real_, length(attr_terms)), attr_terms)
se3   <- setNames(rep(NA_real_, length(attr_terms)), attr_terms)
p3    <- setNames(rep(NA_real_, length(attr_terms)), attr_terms)

midx <- match(attr_terms, sd_diag$attr)
ok   <- !is.na(midx)
coef3[ok] <- sd_diag$estimate[midx[ok]]
se3[ok]   <- sd_diag$std.error[midx[ok]]
p3[ok]    <- sd_diag$p.value[midx[ok]]

# 5) Build three texreg columns

# For MNL
mcfadden_mnl <- 1 - (logLik(mnl) / mnl$nullLogLik)

# For Mixed Logit (MXL)
mcfadden_mxl <- 1 - (logLik(mxl) / mxl$nullLogLik)


m1 <- createTexreg(
  coef.names = attr_terms,
  coef       = as.numeric(coef1),
  se         = as.numeric(se1),
  pvalues    = as.numeric(p1),
  gof.names  = c("Num. obs.", "Log Likelihood", "AIC", "BIC", "McFadden R²"),
  gof        = c(mnl$n$obs, as.numeric(logLik(mnl)), AIC(mnl), compute_bic(mnl), mcfadden_mnl),
  gof.decimal= c(FALSE, TRUE, TRUE, TRUE, TRUE)
)


# Try to pull number of observations safely
n_obs_mxl <- tryCatch({
  if (!is.null(mxl$model)) nrow(mxl$model)
  else if (!is.null(mxl$fitted.values)) length(mxl$fitted.values)
  else NA
}, error = function(e) NA)

# Log-likelihood, AIC, BIC
ll_mxl <- as.numeric(logLik(mxl))
aic_mxl <- AIC(mxl)
bic_mxl <- compute_bic(mxl)

# Log-likelihood, AIC, BIC
ll_mnl <- as.numeric(logLik(mnl))
aic_mnl <- AIC(mnl)
bic_mnl <- compute_bic(mnl)


# McFadden R²: if mxl$nullLogLik exists, use it; otherwise approximate
null_ll_mxl <- tryCatch({
  if (!is.null(mxl$nullLogLik)) mxl$nullLogLik
  else if (!is.null(mnl$nullLogLik)) mnl$nullLogLik  # fallback to MNL's null LL
  else NA
}, error = function(e) NA)

mcfadden_mxl <- if (!is.na(null_ll_mxl)) 1 - (ll_mxl / null_ll_mxl) else NA


# --- MNL LR test ---
ll_mnl <- as.numeric(logLik(mnl))
ll_null_mnl <- mnl$nullLogLik
k_mnl <- attr(logLik(mnl), "df")
lr_mnl <- -2 * (ll_null_mnl - ll_mnl)
p_lr_mnl <- pchisq(lr_mnl, df = k_mnl, lower.tail = FALSE)

# --- MXL LR test ---
ll_mxl <- as.numeric(logLik(mxl))
ll_null_mxl <- mxl$nullLogLik  # or fallback: ll_null_mxl <- ll_null_mnl
k_mxl <- attr(logLik(mxl), "df")
lr_mxl <- -2 * (ll_null_mxl - ll_mxl)
p_lr_mxl <- pchisq(lr_mxl, df = k_mxl, lower.tail = FALSE)

gof_names <- c("Num. obs.", "Log Likelihood", "AIC", "BIC", "McFadden R²",
               paste0("LR χ² (df=", k_mnl, ")"), "p-value (LR)")

gof_mnl <- c(mnl$n$obs, ll_mnl, AIC(mnl), bic_mnl, 1 - ll_mnl / ll_null_mnl, lr_mnl, p_lr_mnl)

gof_mxl <- c(mxl$n$obs, ll_mxl, AIC(mxl), bic_mxl, 1 - ll_mxl / ll_null_mxl, lr_mxl, p_lr_mxl)

gof_decimal <- c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)

m1 <- createTexreg(
  coef.names = attr_terms,
  coef = as.numeric(coef1),
  se = as.numeric(se1),
  pvalues = as.numeric(p1),
  gof.names = gof_names,
  gof = gof_mnl,
  gof.decimal = gof_decimal
)

m2 <- createTexreg(
  coef.names = attr_terms,
  coef = as.numeric(coef2),
  se = as.numeric(se2),
  pvalues = as.numeric(p2),
  gof.names = gof_names,
  gof = gof_mxl,
  gof.decimal = gof_decimal
)


m3 <- createTexreg(
  coef.names = attr_terms,
  coef       = as.numeric(coef3),
  se         = as.numeric(se3),
  pvalues    = as.numeric(p3)
)




# all attribute terms excluding cost
attr_terms_mrs <- setdiff(names(coef2), cost_name)

mrs_vals <- sapply(attr_terms_mrs, function(a) {
  - coef2[a] / coef2[cost_name]
})


vcov_means <- vcov(mxl)[attr_terms_mrs, attr_terms_mrs]

mrs_ses <- sapply(attr_terms_mrs, function(a) {
  beta_a <- coef2[a]
  beta_c <- coef2[cost_name]
  grad <- c(-1 / beta_c, beta_a / (beta_c^2))
  names(grad) <- c(a, cost_name)
  V_sub <- vcov(mxl)[c(a, cost_name), c(a, cost_name)]
  sqrt(t(grad) %*% V_sub %*% grad)
})

mrs_lower <- mrs_vals - 1.96 * mrs_ses
mrs_upper <- mrs_vals + 1.96 * mrs_ses


# Format for screenreg/texreg display
format_ci <- function(est, lo, hi, digits = 2) {
  est_txt <- formatC(est, digits = digits, format = "f")
  ci_txt  <- paste0("(", formatC(lo, digits = digits, format = "f"), ", ",
                    formatC(hi, digits = digits, format = "f"), ")")
  paste0(est_txt, "\n", ci_txt)
}

mrs_display <- mapply(format_ci, mrs_vals, mrs_lower, mrs_upper)

m4 <- createTexreg(
  coef.names = attr_terms_mrs,
  coef = as.numeric(mrs_vals),
  se = as.numeric(mrs_ses),
  pvalues = 2 * pnorm(-abs(mrs_vals / mrs_ses))
)


screenreg(
  list(m1, m2, m3, m4),
  custom.model.names = c("MNL", "ML Mean", "ML SD", "MRS (per 10%)"),
  digits = 2,
  stars = c(0.001, 0.01, 0.05),
  booktabs = TRUE,
  dcolumn = TRUE,
  use.packages = FALSE
)






median_cost <- 10500
scaler <- 0.10 * median_cost  # = 1000

mwtp_vals <- mrs_vals * scaler



m5 <- createTexreg(
  coef.names = attr_terms_mrs,
  coef = as.numeric(mwtp_vals),
  #se = rep(NA, length(mwtp_vals)),  # Caplan doesn’t show SEs here
  #pvalues = rep(NA, length(mwtp_vals))  # No stars in Caplan either
)



# Optional: pretty labels for rows
label_map <- c(
  "dist_green5km"                 = "Green space: 5 km (vs 15 km)",
  "dist_green500 meter"           = "Green space: 500 m (vs 15 km)",
  "dist_shops5km"                 = "Shops: 5 km (vs 15 km)",
  "dist_shops500 meter"           = "Shops: 500 m (vs 15 km)",
  "dist_trans600"                 = "Transit stop: 600 m (vs 900 m)",
  "dist_trans300"                 = "Transit stop: 300 m (vs 900 m)",
  "parkingreserverad garageplats" = "Parking: reserved garage (vs none)",
  "parkingreserverad P-plats"     = "Parking: reserved space (vs none)"
)



screenreg(
  list(m1, m2, m3, m4, m5),
  custom.header = list("MXL" = 2:3),
  custom.model.names = c("MNL", "Mean", "SD", "MRS", "MWTP (SEK/mo)"),
  custom.coef.names = c(
     "Green space: 5 km (vs 15 km)",
     "Green space: 500 m (vs 15 km)",
    "Shops: 5 km (vs 15 km)",
     "Shops: 500 m (vs 15 km)",
     "Transit stop: 600 m (vs 900 m)",
     "Transit stop: 300 m (vs 900 m)",
    "Parking: reserved garage (vs none)",
    "Parking: reserved space (vs none)",
    "Price"
  ),
  digits = 2,
  stars = c(0.001, 0.01, 0.05),
  booktabs = TRUE,
  dcolumn = TRUE,
  use.packages = FALSE
)

texreg(
  list(m1, m2, m3, m4, m5),
  custom.header = list("MXL" = 2:3),
  custom.model.names = c("MNL", "Mean", "SD", "MRS", "MWTP (SEK/mo)"),
  digits = 2,
  custom.coef.names = c(
    "Green space: 5 km (vs 15 km)",
    "Green space: 500 m (vs 15 km)",
    "Shops: 5 km (vs 15 km)",
    "Shops: 500 m (vs 15 km)",
    "Transit stop: 600 m (vs 900 m)",
    "Transit stop: 300 m (vs 900 m)",
    "Parking: reserved garage (vs none)",
    "Parking: reserved space (vs none)",
    "Price"
  ),
  stars = c(0.001, 0.01, 0.05),
  booktabs = TRUE,
  dcolumn = TRUE,
  use.packages = FALSE,
  file = "base_reg_female.tex"
  
)




































screenreg(list(mnl,mxl))


# -----------------------------
# 1) Inputs
# -----------------------------
mnl   <- mnl_male   # preference-space MNL (your object)
mxl   <- mxl_male   # mixed logit (your object)
cost_name   <- "price_num"
median_cost <- 10000
MHC10       <- 0.10 * median_cost  # 10% of median planned cost (SEK)

# Optional: nice row labels for your dummies (adjust to your factor names)
label_map <- c(
  "dist_green5km"                 = "Green space: 5 km (vs 15 km)",
  "dist_green500 meter"           = "Green space: 500 m (vs 15 km)",
  "dist_shops5km"                 = "Shops: 5 km (vs 15 km)",
  "dist_shops500 meter"           = "Shops: 500 m (vs 15 km)",
  "dist_trans600"                 = "Transit stop: 600 m (vs 900 m)",
  "dist_trans300"                 = "Transit stop: 300 m (vs 900 m)",
  "parkingreserverad garageplats" = "Parking: reserved garage (vs none)",
  "parkingreserverad P-plats"     = "Parking: reserved space (vs none)"
)

# -----------------------------
# 2) Helpers (delta method)
# -----------------------------
# var( -a/c ) with gradient [-1/c, a/c^2] wrt (a,c)
dm_var_ratio <- function(V, a_name, c_name, beta) {
  a <- beta[a_name]; c <- beta[c_name]
  grad <- matrix(c(-1/c, a/(c^2)), ncol = 1)
  Vsub <- V[c(a_name, c_name), c(a_name, c_name), drop = FALSE]
  as.numeric(t(grad) %*% Vsub %*% grad)
}

# -----------------------------
# 3) Extract pieces from models
# -----------------------------
# MNL means
b_mnl <- coef(mnl)
V_mnl <- vcov(mnl)

# MXl: means & SDs. We’ll detect SD terms by name pattern.
tidy_mxl <- broom::tidy(mxl)

# Heuristic: SD terms often start with "sd_" or contain ":sd" or similar.
# Inspect names once with: unique(tidy_mxl$term)
is_sd <- str_detect(tidy_mxl$term, regex("^sd_|_sd$|:sd|\\bsd\\b", ignore_case = TRUE))
mxl_means <- tidy_mxl %>% filter(!is_sd)
mxl_sds   <- tidy_mxl %>% filter(is_sd)

# Create named vectors for means & SDs (and their SEs)
b_mxl_mean <- setNames(mxl_means$estimate, mxl_means$term)
se_mxl_mean<- setNames(mxl_means$std.error, mxl_means$term)
b_mxl_sd   <- setNames(mxl_sds$estimate,   mxl_sds$term)
se_mxl_sd  <- setNames(mxl_sds$std.error,  mxl_sds$term)

# -----------------------------
# 4) Decide row order (attributes, no cost)
# -----------------------------
# Take attribute terms that appear in the MNL and exclude the cost
attr_terms <- names(b_mnl)
attr_terms <- setdiff(attr_terms, c("(Intercept)", cost_name))

# Keep only those we have in the mixed-logit means too (to align rows)
attr_terms <- intersect(attr_terms, names(b_mxl_mean))

# -----------------------------
# 5) Compute MRS & MWTP from MNL
# -----------------------------
mrs_list <- lapply(attr_terms, function(a){
  mrs_hat <- - b_mnl[a] / b_mnl[cost_name]
  var_mrs <- dm_var_ratio(V_mnl, a, cost_name, b_mnl)
  se_mrs  <- sqrt(var_mrs)
  tibble(MRS = mrs_hat, SE = se_mrs)
})

mrs_mat <- do.call(rbind, mrs_list)
rownames(mrs_mat) <- attr_terms

# MWTP = MRS × 10% of median planned cost (SE scales the same)
mwtp_mat <- cbind(
  MWTP   = mrs_mat[,"MRS"] * MHC10,
  SE     = mrs_mat[,"SE"]  * MHC10
)
rownames(mwtp_mat) <- attr_terms

# -----------------------------
# 6) Build texreg "models"
# -----------------------------
# Col 1: MNL means
coef1 <- b_mnl[attr_terms]
se1   <- sqrt(diag(V_mnl))[attr_terms]
p1    <- 2*pnorm(-abs(coef1/se1))

# Col 2: ML means (from mixed logit)
coef2 <- b_mxl_mean[attr_terms]
se2   <- se_mxl_mean[attr_terms]
p2    <- 2*pnorm(-abs(coef2/se2))

# Col 3: ML SDs (try to match names; SD terms often named like "sd_dist_green5km")
# We map SD rows by trying common prefixes:
name_map_sd <- setNames(
  nm = names(b_mxl_sd),
  object = str_replace(names(b_mxl_sd), regex("^sd[_\\.:]?", ignore_case = TRUE), "")
)


# Try to align: for each attr term, find sd name whose stripped name matches
sd_for_attr <- sapply(attr_terms, function(a){
  # find exact or partial match
  cand <- names(name_map_sd)[tolower(name_map_sd) == tolower(a)]
  if(length(cand) == 0) {
    # fallback: contains
    cand <- names(b_mxl_sd)[str_detect(names(b_mxl_sd), fixed(a))]
  }
  if(length(cand) > 0) cand[1] else NA_character_
})

coef3 <- setNames(rep(NA_real_, length(attr_terms)), attr_terms)
se3   <- setNames(rep(NA_real_, length(attr_terms)), attr_terms)
p3    <- setNames(rep(NA_real_, length(attr_terms)), attr_terms)

for(i in seq_along(attr_terms)){
  sd_name <- sd_for_attr[i]
  if(!is.na(sd_name)){
    coef3[i] <- b_mxl_sd[sd_name]
    se3[i]   <- se_mxl_sd[sd_name]
    p3[i]    <- 2*pnorm(-abs(coef3[i]/se3[i]))
  }
}

# Col 4: MRS (per 10%) from MNL
coef4 <- pull(mrs_mat, MRS)
se4   <- pull(mrs_mat, SE)
p4    <- 2*pnorm(-abs(coef4/se4))

# Col 5: MWTP (SEK/mo)
mwtp_mat <- mrs_mat %>%
  as.data.frame() %>%
  mutate(
    MWTP = MRS * MHC10,
    MWTP_SE = SE * MHC10
  )

coef5 <- mwtp_mat[ , "MWTP"]; names(coef5) <- attr_terms
se5   <- mwtp_mat[ , "SE"];   names(se5)   <- attr_terms
p5    <- 2*pnorm(-abs(coef5/se5))

# Pretty row names
coef_map <- setNames(
  object = ifelse(names(label_map) %in% attr_terms, label_map[names(label_map)], names(label_map)),
  nm = names(label_map)
)
# fall back to term if no label
rownames_final <- sapply(attr_terms, function(a) ifelse(!is.na(label_map[a]), label_map[a], a))

# Build texreg createTexreg objects

m1 <- createTexreg(
  coef.names = rownames_final,
  coef = as.numeric(coef1[ attr_terms ]),
  se   = as.numeric(se1  [ attr_terms ]),
  pvalues = as.numeric(p1[ attr_terms ]),
  gof.names = c("Num. obs.", "Log Likelihood", "AIC", "BIC", "McFadden R²"),
  gof = c(3852, -1889.1238786, 3796.2477571, 3852.5549000, 0.2924637),
  gof.decimal = c(FALSE, TRUE, TRUE, TRUE, TRUE)
)


m2 <- createTexreg(
  coef.names = rownames_final,
  coef = as.numeric(coef2[ attr_terms ]),
  se   = as.numeric(se2  [ attr_terms ]),
  pvalues = as.numeric(p2[ attr_terms ]),
  gof.names = c("Num. obs.", "Log Likelihood", "AIC", "BIC", "McFadden R²"),
  gof = c(3852, -1889.1238786, 3796.2477571, 3852.5549000, 0.2924637),
  gof.decimal = c(FALSE, TRUE, TRUE, TRUE, TRUE)
)

m3 <- createTexreg(
  coef.names = rownames_final,
  coef = as.numeric(coef3[ attr_terms ]),
  se   = as.numeric(se3  [ attr_terms ]),
  pvalues = as.numeric(p3[ attr_terms ])
)

m4 <- createTexreg(
  coef.names = rownames_final,
  coef = as.numeric(coef4[ attr_terms ]),
  se   = as.numeric(se4  [ attr_terms ]),
  pvalues = as.numeric(p4[ attr_terms ])
)

m5 <- createTexreg(
  coef.names = rownames_final,
  coef = as.numeric(coef5[ attr_terms ]),
  se   = as.numeric(se5  [ attr_terms ]),
  pvalues = as.numeric(p5[ attr_terms ])
)

# -----------------------------
# 7) Print table
# -----------------------------
screenreg(
  list(m1, m2, m3, m4, m5),
  custom.model.names = c("MNL", "ML Mean", "ML SD", "MRS (per 10%)", "MWTP (SEK/mo)"),
  dcolumn = TRUE, booktabs = TRUE, use.packages = FALSE,
  stars = c(0.001, 0.01, 0.05),
  digits = 3
)

# Or HTML in the Viewer:
# htmlreg(list(m1, m2, m3, m4, m5), file = "caplan_table5_males.html", doctype = TRUE)
# And LaTeX to file:
# texreg(list(m1, m2, m3, m4, m5), file = "caplan_table5_males.tex")
