library(dplyr)
library(readr)

options(scipen = 999)

# =========================
# 1. Leggi i file
# =========================

setwd("~/ABI/Draft paper/Third step/Data/NEA_elaboration")
(RFs <- read_csv("RPs.csv", show_col_types = FALSE))
(ts  <- read_csv("time_series.csv", show_col_types = FALSE))

# Se i tuoi csv sono separati da ; invece che da ,
# usa queste due righe al posto delle precedenti:
# RFs <- read_delim("RFs.csv", delim = ";", show_col_types = FALSE)
# ts  <- read_delim("time_series.csv", delim = ";", show_col_types = FALSE)


# =========================
# 2. Controllo nomi colonne
# =========================

names(RFs)
names(ts)

# Mi assicuro che le colonne principali siano numeriche
RFs <- RFs %>%
  mutate(
    Blim = as.numeric(Blim),
    Fmsy = as.numeric(Fmsy)
  )

ts <- ts %>%
  mutate(
    year = as.numeric(year),
    ssb  = as.numeric(ssb),
    f    = as.numeric(f)
  )


# =========================
# 3. Prendi il quintultimo anno per ogni stock
# =========================

ts_5thlast <- ts %>%
  arrange(stock, year) %>%
  group_by(stock) %>%
  mutate(n_years = n()) %>%
  filter(row_number() == n() - 4) %>%
  ungroup() %>%
  select(stock, year_5thlast = year, ssb_5thlast = ssb, f_5thlast = f)


# =========================
# 4. Unisci con RFs usando stock
# =========================

joined <- ts_5thlast %>%
  left_join(
    RFs %>% select(stock, Blim, Fmsy),
    by = "stock"
  )


# =========================
# 5. Calcola i due indicatori
# =========================

out_F <- joined %>%
  transmute(
    stock = stock,
    name = "F_Fmsy",
    `5thlast` = f_5thlast / Fmsy
  )

out_SSB <- joined %>%
  transmute(
    stock = stock,
    name = "SSB_Blim",
    `5thlast` = ssb_5thlast / Blim
  )

final_out <- bind_rows(out_F, out_SSB) %>%
  arrange(stock, name)


# =========================
# 6. Controllo finale
# =========================

View(final_out)

# Controlla eventuali problemi di matching o divisioni non valide
check_problem <- joined %>%
  filter(
    is.na(Blim) | is.na(Fmsy) |
      is.na(ssb_5thlast) | is.na(f_5thlast) |
      Blim == 0 | Fmsy == 0
  )

View(check_problem)


# =========================
# 7. Salva il CSV finale
# =========================

write_csv(final_out, "indicators_5thlast.csv")
########################################################################################
########################################################################################
########################################################################################
###### ORA lo STESSO PER US data 


library(dplyr)

options(scipen = 999)

# =========================
# 1. Leggi i file
# =========================
setwd("~/ABI/Draft paper/Third step/Data/US_elaboration")


(US_ref <- read.csv("US_reference_points.csv"))
(US_ts <- read.csv("US_ABI_data_new.csv"))


# Se i file sono separati da ;
# usa queste due righe al posto delle precedenti:
# US_ref <- read.csv("US_reference_point.csv", sep = ";", stringsAsFactors = FALSE, check.names = FALSE)
# US_ts  <- read.csv("US_ABI_data.csv", sep = ";", stringsAsFactors = FALSE, check.names = FALSE)


# =========================
# 2. Pulisci nomi colonne
# =========================

names(US_ref) <- trimws(names(US_ref))
names(US_ts)  <- trimws(names(US_ts))

names(US_ref)
names(US_ts)


# =========================
# 3. Funzione per convertire numeri
# =========================

to_num <- function(x) {
  as.numeric(gsub(",", ".", x))
}


# =========================
# 4. Prepara reference points
# =========================

US_ref_clean <- US_ref %>%
  transmute(
    stock   = trimws(stock),
    Ftarget = to_num(Ftarget),
    Blim    = to_num(Blim)
  )


# =========================
# 5. Prepara time series
# =========================
# Usa direttamente le colonne SSB e F viste nello screenshot

US_ts_clean <- US_ts %>%
  transmute(
    stock = trimws(stock),
    year  = to_num(year),
    SSB   = to_num(SSB),
    F     = to_num(F)
  )


# =========================
# 6. Prendi il quintultimo anno per ogni stock
# =========================

US_5thlast <- US_ts_clean %>%
  filter(!is.na(year)) %>%
  arrange(stock, year) %>%
  group_by(stock) %>%
  filter(n() >= 5) %>%
  slice(n() - 4) %>%
  ungroup() %>%
  rename(
    year_5thlast = year,
    SSB_5thlast  = SSB,
    F_5thlast    = F
  )


# =========================
# 7. Unisci con reference points
# =========================

joined_US <- US_5thlast %>%
  left_join(US_ref_clean, by = "stock")


# =========================
# 8. Calcola indicatori
# =========================

out_F <- joined_US %>%
  transmute(
    stock = stock,
    name = "F/Ftarget",
    `5thlast` = F_5thlast / Ftarget
  )

out_SSB <- joined_US %>%
  transmute(
    stock = stock,
    name = "SSB/Blim",
    `5thlast` = SSB_5thlast / Blim
  )

US_final_out <- bind_rows(out_F, out_SSB) %>%
  arrange(stock, name)


# =========================
# 9. Controlli
# =========================

View(US_final_out)

# Controlla stock non matchati, NA o divisioni per zero
US_check_problem <- joined_US %>%
  filter(
    is.na(Ftarget) | is.na(Blim) |
      is.na(F_5thlast) | is.na(SSB_5thlast) |
      Ftarget == 0 | Blim == 0
  )

View(US_check_problem)


# =========================
# 10. Salva output finale
# =========================

write.csv(
  US_final_out,
  "US_indicators_5thlast_new.csv",
  row.names = FALSE
)


