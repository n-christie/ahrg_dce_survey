# _save_profiles_table.R
#
# Word-ready demographic profile-by-class tables, for both the 3-class and
# 4-class LC solutions (class count not yet finalized -- see
# _save_lc_word_tables.R note on the 2026-08-21 multi-start refit).
#
# Class labels are NOT hardcoded: they're derived from each class's own
# coefficients (top attribute + price sensitivity band) so this table can't
# silently go stale the way the old hardcoded "Shop-seekers/Nature-seekers/
# Car-centred" labels and "39%, 25%, 37%" shares did after the models were
# refit and the class order/composition changed.
#
# Produces:
#   paper/word/tables/lc_class_profiles.html    (alias of the 3-class table,
#                                                 kept for compatibility with
#                                                 existing manuscript refs)
#   paper/word/tables/lc_3class_profiles.html
#   paper/word/tables/lc_4class_profiles.html

library(here)
library(gmnl)
library(mlogit)
library(haven)
library(kableExtra)
library(dplyr)

df <- readRDS(here("data/formr", "df_model.rds"))
df$price_num    <- df$price_num / 100
df$ägandebostad <- haven::as_factor(df$ägandebostad)
df$Sex          <- haven::as_factor(df$Sex)
df$civil_d      <- ifelse(df$civil_status_T2 == 1, "Partnered", "Not partnered")
df$Own          <- ifelse(df$ägandebostad == "Ja", "Owner", "Renter")
df$age          <- floor(df$Age_T3)
df$ag           <- ifelse(df$age >= 75, "75+", ifelse(df$age >= 65, "65-74", "55-64"))
df$hlth         <- as.numeric(df$VAR035)
df$city         <- as.integer(df$VAR010 == 1)
df$bostadstyp   <- haven::as_factor(df$bostadstyp)

dg <- mlogit.data(df, choice = "choice", shape = "long",
                   alt.var = "altID", chid.var = "obsID", id.var = "panelID")

label_map <- c(
  "dist_green5km"                  = "Green space",
  "dist_green500 meter"            = "Green space",
  "dist_shops5km"                  = "Shops",
  "dist_shops500 meter"            = "Shops",
  "dist_trans600"                  = "Transit",
  "dist_trans300"                  = "Transit",
  "parkingreserverad garageplats"  = "Parking",
  "parkingreserverad P-plats"      = "Parking",
  "price_num"                      = "Price"
)

# Auto-derived short label: top non-price attribute by coefficient, plus a
# price-sensitivity note relative to the other classes in the same model.
derive_class_labels <- function(model, Q) {
  sm <- summary(model)$CoefTable
  price_est <- sapply(1:Q, function(q) sm[paste0("class.", q, ".price_num"), "Estimate"])
  price_rank <- rank(price_est)  # most negative (most price-sensitive) = rank 1

  sapply(1:Q, function(q) {
    prefix <- paste0("^class\\.", q, "\\.")
    idx    <- grep(prefix, rownames(sm))
    idx    <- idx[rownames(sm)[idx] != paste0("class.", q, ".price_num")]
    est    <- sm[idx, "Estimate"]
    nm     <- label_map[sub(prefix, "", rownames(sm)[idx])]
    top    <- nm[which.max(est)]
    sens   <- if (price_rank[q] == 1) "most price-sensitive" else if (price_rank[q] == Q) "least price-sensitive" else NA
    if (is.na(sens)) top else paste0(top, ", ", sens)
  })
}

build_profile_table <- function(model, Q, out_file, caption) {
  Qmat        <- as.data.frame(model$Qir)
  names(Qmat) <- paste0("p", 1:Q)
  Qmat$cl     <- max.col(Qmat)
  Qmat$pid    <- as.character(unique(dg$panelID))

  ind     <- df[!duplicated(df$panelID), ]
  ind$pid <- as.character(ind$panelID)
  d       <- merge(Qmat, ind, by = "pid")

  f_pct <- function(x) round(tapply(as.integer(x), d$cl, mean, na.rm = TRUE) * 100, 1)
  f_avg <- function(x, dp = 1) round(tapply(as.numeric(x), d$cl, mean, na.rm = TRUE), dp)

  n_cl <- as.integer(table(d$cl))
  pct  <- round(prop.table(table(d$cl)) * 100, 1)

  mk <- function(lbl, v) {
    row <- as.list(as.character(v))
    names(row) <- paste0("C", 1:Q)
    as.data.frame(c(list(Characteristic = lbl), row), stringsAsFactors = FALSE)
  }

  tab <- rbind(
    mk("N (modal assignment)",          n_cl),
    mk("Modal share (%)",               paste0(pct, "%")),
    mk("Average age (years)",           f_avg(d$age)),
    mk("Age 55-64 (%)",                 f_pct(d$ag == "55-64")),
    mk("Age 65-74 (%)",                 f_pct(d$ag == "65-74")),
    mk("Age 75+ (%)",                   f_pct(d$ag == "75+")),
    mk("Owner (%)",                     f_pct(d$Own == "Owner")),
    mk("Female (%)",                    f_pct(d$Sex == "Kvinna")),
    mk("Partnered (%)",                 f_pct(d$civil_d == "Partnered")),
    mk("Lives in city/town (%)",        f_pct(d$city == 1)),
    mk("Avg. self-rated health (1-5)",  f_avg(d$hlth, 2))
  )

  labels <- derive_class_labels(model, Q)
  colnames(tab) <- c("Characteristic", paste0("Class ", 1:Q, " | ", labels))

  prior_shares <- {
    sm        <- summary(model)$CoefTable
    delta_idx <- grep("^\\(class\\)", rownames(sm))
    deltas    <- c(0, sm[delta_idx, "Estimate"])
    exp_d     <- exp(deltas)
    round(exp_d / sum(exp_d) * 100, 1)
  }

  out <- kbl(tab, format = "html", escape = TRUE, caption = caption) %>%
    kable_classic(full_width = FALSE) %>%
    row_spec(0, bold = TRUE) %>%
    row_spec(2, extra_css = "border-bottom: 1px solid #aaa;") %>%
    row_spec(5, extra_css = "border-bottom: 1px solid #aaa;") %>%
    pack_rows("Age group", 3, 5) %>%
    footnote(
      general = paste0(
        "Modal class assigned by maximum posterior probability. ",
        "Prior (model-implied) class shares: ", paste0(prior_shares, "%", collapse = ", "), ". ",
        "Class labels are auto-derived (top attribute by coefficient magnitude, plus price-",
        "sensitivity rank among classes in this solution) -- treat as a starting point for ",
        "interpretation, not a final naming decision. Self-rated health: 1 = poor, 5 = excellent."
      ),
      general_title = "Note: "
    )

  save_kable(out, here("paper/word/tables", out_file))
  cat("Saved: paper/word/tables/", out_file, "\n", sep = "")
}

lc3 <- readRDS(here("output/models", "lc_3class.rds"))
lc4 <- readRDS(here("output/models", "lc_4class.rds"))

build_profile_table(
  lc3, 3, "lc_3class_profiles.html",
  "Table S5 (3-class option). Demographic profiles by latent class (LC-3 modal class assignment)"
)
build_profile_table(
  lc4, 4, "lc_4class_profiles.html",
  "Table S5 (4-class option). Demographic profiles by latent class (LC-4 modal class assignment)"
)

# Kept for compatibility with existing manuscript references to
# lc_class_profiles.html (which pointed at the 3-class version).
file.copy(
  here("paper/word/tables", "lc_3class_profiles.html"),
  here("paper/word/tables", "lc_class_profiles.html"),
  overwrite = TRUE
)
cat("Saved: paper/word/tables/lc_class_profiles.html (alias of 3-class table)\n")
cat("Open in Word via File > Open, or copy-paste the table directly.\n")
