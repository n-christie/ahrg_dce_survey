library(here)
library(gmnl)
library(mlogit)
library(haven)
library(kableExtra)

df <- readRDS(here("data/formr","df_model.rds"))
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

dg  <- mlogit.data(df, choice = "choice", shape = "long",
                   alt.var = "altID", chid.var = "obsID", id.var = "panelID")
lc3 <- readRDS(here("output/models","lc_3class.rds"))

Q        <- as.data.frame(lc3$Qir)
names(Q) <- paste0("p", 1:3)
Q$cl     <- max.col(Q)
Q$pid    <- as.character(unique(dg$panelID))

ind     <- df[!duplicated(df$panelID), ]
ind$pid <- as.character(ind$panelID)
d       <- merge(Q, ind, by = "pid")

f_pct <- function(x) round(tapply(as.integer(x), d$cl, mean, na.rm = TRUE) * 100, 1)
f_avg <- function(x, dp = 1) round(tapply(as.numeric(x), d$cl, mean, na.rm = TRUE), dp)

n_cl <- as.integer(table(d$cl))
pct  <- round(prop.table(table(d$cl)) * 100, 1)

mk <- function(lbl, v) data.frame(
  Characteristic = lbl,
  C1 = as.character(v[1]),
  C2 = as.character(v[2]),
  C3 = as.character(v[3]),
  stringsAsFactors = FALSE
)

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
  mk("Avg. self-rated health (1-5)", f_avg(d$hlth, 2))
)

colnames(tab) <- c("Characteristic",
                   "Class 1 | Shop-seekers",
                   "Class 2 | Nature-seekers",
                   "Class 3 | Car-centred")

out <- kbl(tab, format = "html", escape = TRUE,
           caption = "Table X. Demographic profiles by latent class (LC-3 modal class assignment)") %>%
  kable_classic(full_width = FALSE) %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(3, extra_css = "border-bottom: 1px solid #aaa;") %>%
  row_spec(6, extra_css = "border-bottom: 1px solid #aaa;") %>%
  pack_rows("Age group", 4, 6) %>%
  footnote(
    general = paste0(
      "Modal class assigned by maximum posterior probability. ",
      "Prior (model-implied) class shares: 39%, 25%, 37%. ",
      "Self-rated health: 1 = poor, 5 = excellent."
    ),
    general_title = "Note: "
  )

save_kable(out, here("paper/word/tables", "lc_class_profiles.html"))
cat("Saved: paper/word/tables/lc_class_profiles.html\n")
cat("Open in Word via File > Open, or copy-paste the table directly.\n")
