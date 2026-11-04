# setup
rm(list = ls())
gc()

options(scipen = 9999)

# packages
library(FLasher)
library(FLBRP)
library(tidyverse)
library(viridis)
library(readxl)
library(DescTools)
library(patchwork)
library(forcats)
library(dplyr)
library(stringr)
library(cowplot)
library(ggplot2)
library(magick)
library(readr)
library(tidyr)
library(pROC)
library(purrr)
library(knitr)
library(tibble)
sessionInfo()

#########################################################################################################################################
# ===============================
#   FIGURE 1 —
# ===============================
#########################################################################################################################################

# ---- MED Pie (Fig1A) ----
Filtered_Data_ABI_MED <- read_csv("~/ABI/Draft paper/Third step/Data/Filtered_Data_ABI_MED.csv",
                                  show_col_types = FALSE)
head(Filtered_Data_ABI_MED)

total_species <- Filtered_Data_ABI_MED %>%
  distinct(Species) %>%
  nrow()

species_with_validation <- Filtered_Data_ABI_MED %>%
  filter(!is.na(ValidationStatus) & ValidationStatus %in% c("Qualitative", "Quantitative")) %>%
  distinct(Species) %>%
  nrow()

counts <- data.frame(
  category = c("Stock Assessment", "Unknown"),
  count = c(species_with_validation, total_species - species_with_validation)
) %>%
  mutate(
    percentage = round(count / sum(count) * 100, 1),
    label = paste0(percentage, "% (", count, ")")
  )

colors <- c("lightgreen", "lightgrey")

pMED1 <- ggplot(counts, aes(x = "", y = count, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(data = subset(counts, category == "Stock Assessment"),
            aes(label = label), position = position_stack(vjust = 0.2),
            size = 12, color = "black") +
  geom_text(data = subset(counts, category == "Unknown"),
            aes(label = label), position = position_stack(vjust = 0.8),
            size = 12, color = "black") +
  scale_fill_manual(values = colors) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 35, hjust = 0.5),
    legend.title = element_text(size = 26),
    legend.text = element_text(size = 26)
  ) +
  labs(fill = "Stock status info", title = "Mediterranean and Black Sea species")

pMED1

setwd("~/ABI/Draft paper/Third step/Figure")
ggsave(filename = "pMED1.png", width = 14, height = 12)

# ---- MED bar (22 MED Stocks) ----

setwd("~/ABI/Draft paper/Third step/Data")

MED_fig2 <- read.csv("MED_data_for_Fig_2_new.csv", sep = ",", stringsAsFactors = FALSE)[, 1:5]

# keep only one row per stock
med_stock_list <- MED_fig2 %>%
  distinct(stock) %>%
  mutate(
    stock_std = stock %>%
      str_trim() %>%
      tolower() %>%
      gsub("_", ".", ., fixed = TRUE)
  )

# ---- 2) Read lookup table with species names ----
med_lookup <- read.csv("STECF_ABI_data.csv", sep = ";", stringsAsFactors = FALSE) %>%
  transmute(
    stock_std = stock %>%
      str_trim() %>%
      tolower() %>%
      gsub("_", ".", ., fixed = TRUE),
    Species = species
  ) %>%
  distinct(stock_std, Species)

stecf_data <- med_stock_list %>%
  left_join(med_lookup, by = "stock_std")


stecf_data <- stecf_data %>%
  count(Species, name = "stecf_stock_count") %>%
  arrange(desc(stecf_stock_count))

print(stecf_data)


stecf_bar_plot <- ggplot(
  stecf_data,
  aes(x = stecf_stock_count, y = reorder(Species, stecf_stock_count))
) +
  geom_bar(stat = "identity", fill = "#1b9e77", color = "black", alpha = 0.70) +
  labs(x = "", y = "", title = "22 MED Stocks") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30),
    axis.text.y = element_text(size = 30, face = "italic"),
    axis.text.x = element_text(size = 26),
    axis.title.x = element_text(size = 30),
    panel.grid.major.y = element_blank()
  )

pMED2 <- stecf_bar_plot
pMED2

setwd("~/ABI/Draft paper/Third step/Figure")
ggsave(pMED2, filename = "pMED2.png", width = 14, height = 12)


# ---- NEA Pie ----
assessed <- 90
unassessed <- 426 - assessed

df <- data.frame(
  category = c("Stock Assessment", "Unknown"),
  count = c(assessed, unassessed)
)
df$percentage <- round(100 * df$count / sum(df$count), 1)
df$label <- paste0(df$percentage, "% (", df$count, ")")

colors <- c("Stock Assessment" = "lightgreen", "Unknown" = "lightgrey")

pNEA1 <- ggplot(df, aes(x = "", y = count, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.7),
            size = 12, color = "black") +
  scale_fill_manual(values = colors) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 35, hjust = 0.5),
    legend.title = element_text(size = 26),
    legend.text = element_text(size = 26)
  ) +
  labs(fill = "Stock status info", title = "North East Atlantic species")

pNEA1

setwd("~/ABI/Draft paper/Third step/Figure")
ggsave(pNEA1, filename = "pNEA1.png", width = 14, height = 12)

# ---- NEA bar (25 NEA Stocks) ----
setwd("~/ABI/Draft paper/Third step/Data")

dat3 <- read_csv("NEA_ABI_data_fig_3.csv", show_col_types = FALSE)
colnames(dat3)[8] <- "latin_name"

NEA_data <- dat3 %>%
  group_by(stock, latin_name) %>%
  summarise(n_stock = dplyr::n(), .groups = "drop") %>%
  group_by(latin_name) %>%
  summarise(final_n_stock = dplyr::n(), .groups = "drop") %>%
  arrange(desc(final_n_stock))

NEA_data %>% summarise(X = sum(final_n_stock))

NEA_bar_plot <- ggplot(NEA_data, aes(x = final_n_stock, y = reorder(latin_name, final_n_stock))) +
  geom_bar(stat = "identity", fill = "#d95f02", color = "black", alpha = 0.70) +
  labs(x = "", y = "", title = "25 NEA Stocks") +
  scale_x_continuous(breaks = c(0, 2, 4, 6), limits = c(0, 6)) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30),
    axis.text.y = element_text(size = 30, face = "italic"),
    axis.text.x = element_text(size = 26),
    axis.title.x = element_text(size = 30),
    panel.grid.major.y = element_blank()
  )

pNEA2 <- NEA_bar_plot
pNEA2

setwd("~/ABI/Draft paper/Third step/Figure")
ggsave(pNEA2, filename = "pNEA2.png", width = 14, height = 12)

# ---- US Pie ----
assessed <- 150
unassessed <- 280 - assessed

df <- data.frame(
  category = c("Stock Assessment", "Unknown"),
  count = c(assessed, unassessed)
)
df$percentage <- round(100 * df$count / sum(df$count), 1)
df$label <- paste0(df$percentage, "% (", df$count, ")")

colors <- c("Stock Assessment" = "lightgreen", "Unknown" = "lightgrey")

pUS1 <- ggplot(df, aes(x = "", y = count, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.6),
            size = 12, color = "black") +
  scale_fill_manual(values = colors) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 35, hjust = 0.5),
    legend.title = element_text(size = 26),
    legend.text = element_text(size = 26)
  ) +
  labs(fill = "Stock status info", title = "North East Pacific species")

pUS1

setwd("~/ABI/Draft paper/Third step/Figure")
ggsave(pUS1, filename = "pUS1.png", width = 14, height = 12)

# ---- US bar (18 NEP Stocks) ----
setwd("~/ABI/Draft paper/Third step/Data")
US <- read.csv("US_data_for_Fig_2_new.csv", sep = ",")

US <- US %>%
  mutate(
    latin_name = dplyr::case_when(
      stock == "Black rockfish - Oregon stock"                                        ~ "Sebastes melanops",
      stock == "Black rockfish - Washington stock"                                    ~ "Sebastes melanops",
      stock == "Black-and-yellow and Gopher rockfish complex"                         ~ "Sebastes chrysomelas + carnatus",
      stock == "Blackgill rockfish"                                                   ~ "Sebastes melanostictus",
      stock == "Blue deacon rockfish"                                                 ~ "Sebastes diaconus",
      stock == "Cabezon - Northern California"                                        ~ "Scorpaenichthys marmoratus",
      stock == "Cabezon - Southern California"                                        ~ "Scorpaenichthys marmoratus",
      stock == "California scorpionfish"                                              ~ "Scorpaena guttata",
      stock == "Canary rockfish"                                                      ~ "Sebastes pinniger",
      stock == "Copper rockfish - Northern California"                                ~ "Sebastes caurinus",
      stock == "Copper rockfish - Southern California"                                ~ "Sebastes caurinus",
      stock == "Cowcod - Southern California"                                         ~ "Sebastes levis",
      stock == "Longnose skate"                                                       ~ "Beringraja rhina",
      stock == "Petrale Sole"                                                         ~ "Eopsetta jordani",
      stock == "Quillback rockfish - California"                                      ~ "Sebastes maliger",
      stock == "Quillback rockfish - Washington"                                      ~ "Sebastes maliger",
      stock == "Squarespot rockfish"                                                  ~ "Sebastes hopkinsi",
      stock == "Vermilion rockfish and Sunset rockfish Complex - Northern California" ~ "Sebastes miniatus + crocotulus",
      TRUE                                                                            ~ NA_character_
    )
  )

US_data <- US %>%
  group_by(stock, latin_name) %>%
  summarise(n_stock = dplyr::n(), .groups = "drop") %>%
  group_by(latin_name) %>%
  summarise(final_n_stock = dplyr::n(), .groups = "drop") %>%
  arrange(desc(final_n_stock))

US_data %>% summarise(X = sum(final_n_stock))


US_bar_plot <- ggplot(US_data, aes(x = final_n_stock, y = reorder(latin_name, final_n_stock))) +
  geom_bar(stat = "identity", fill = "#7570b3", color = "black", alpha = 0.70) +
  labs(x = "stock", y = "", title = "18 NEP Stocks") +
  scale_x_continuous(breaks = c(0, 2, 4, 6), limits = c(0, 6)) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30),
    axis.text.y = element_text(size = 30, face = "italic"),
    axis.text.x = element_text(size = 26),
    axis.title.x = element_text(size = 30),
    panel.grid.major.y = element_blank()
  )

pUS2 <- US_bar_plot
pUS2

setwd("~/ABI/Draft paper/Third step/Figure")
ggsave(pUS2, filename = "pUS2.png", width = 14, height = 12)

# ---- Combine 2x3 panel for Figure 1 ----
setwd("~/ABI/Draft paper/Third step/Figure")

imgs <- image_read(c("pMED1.png", "pMED2.png", "pNEA1.png", "pNEA2.png", "pUS1.png", "pUS2.png"))
imgs <- image_scale(imgs, "1000x1000")

row1 <- image_append(imgs[1:2])
row2 <- image_append(imgs[3:4])
row3 <- image_append(imgs[5:6])
final <- image_append(c(row1, row2, row3), stack = TRUE)

image_write(final, "combined_2x3.png")
image_browse(final)


#########################################################################################################################################
# ===============================
#   FIGURE 2 — final value + historical range + recent trend from 5th_last to final
#   ORIGINAL DATA PROCESSING + NEW DISPLAY LABELS ONLY
# ===============================
#########################################################################################################################################

rm(list = ls())
gc()

library(tidyverse)
library(forcats)
library(stringr)
library(patchwork)
library(readr)
library(grid)

setwd("~/ABI/Draft paper/Third step/Data")


# ============================================================
# 0) Helper function to read CSV files robustly
# ============================================================

read_fig2_file <- function(file, area_name) {
  
  # Try comma first
  dat_comma <- tryCatch(
    readr::read_csv(file, show_col_types = FALSE),
    error = function(e) NULL
  )
  
  # Try semicolon
  dat_semicolon <- tryCatch(
    readr::read_delim(file, delim = ";", show_col_types = FALSE),
    error = function(e) NULL
  )
  
  required_cols <- c("stock", "5th_last", "final", "min", "max", "name")
  
  if (!is.null(dat_comma) && all(required_cols %in% names(dat_comma))) {
    
    dat <- dat_comma
    
  } else if (!is.null(dat_semicolon) &&
             all(required_cols %in% names(dat_semicolon))) {
    
    dat <- dat_semicolon
    
  } else {
    
    stop(
      "Cannot read file correctly or missing columns in: ", file,
      "\nRequired columns are: ", paste(required_cols, collapse = ", "),
      "\nColumns found with comma: ", paste(names(dat_comma), collapse = ", "),
      "\nColumns found with semicolon: ", paste(names(dat_semicolon), collapse = ", ")
    )
  }
  
  dat %>%
    mutate(area = area_name)
}


# ============================================================
# 1) Read & merge input files
# ============================================================

MED <- read_fig2_file(
  "MED_data_for_Fig_2_new.csv",
  "MED"
)

NEA <- read_fig2_file(
  "NEA_data_for_Fig_2_new.csv",
  "NEA"
)

US <- read_fig2_file(
  "US_data_for_Fig_2_new.csv",
  "US"
)

plot_dat <- bind_rows(
  MED,
  NEA,
  US
)

# Quick check
print(names(plot_dat))
print(plot_dat %>% count(area))


# ============================================================
# 2) Clean numeric columns and text columns
# ============================================================

plot_dat <- plot_dat %>%
  mutate(
    
    stock      = str_trim(as.character(stock)),
    area       = str_trim(as.character(area)),
    name       = str_trim(as.character(name)),
    
    `5th_last` = as.numeric(`5th_last`),
    final      = as.numeric(final),
    min        = as.numeric(min),
    max        = as.numeric(max)
  )


# ============================================================
# 3) ORIGINAL stock name conversions
# ============================================================
#
# IMPORTANT:
# This section is kept exactly in the original logic because
# these stock names are also used for the join with ssb_ref.
# ============================================================

plot_dat <- plot_dat %>%
  mutate(
    
    stock = case_when(
      
      str_detect(
        stock,
        "^[A-Z]{3}(?:_[0-9A-Za-z]+)+$"
      ) ~
        tolower(
          gsub(
            "_",
            ".",
            stock
          )
        ),
      
      TRUE ~ stock
    )
  )


# ORIGINAL US SHORT NAMES
# Do NOT change these because they are used in the SSB reference join

short_names <- c(
  
  "Black rockfish - Oregon stock" =
    "Bla.rockfish.Ore",
  
  "Black rockfish - Washington stock" =
    "Bla.rockfish.Wash.",
  
  "Black-and-yellow and Gopher rockfish complex" =
    "Bla.yell.Goph.rockfish",
  
  "Blackgill rockfish" =
    "Bla.gill.rockfish",
  
  "Blue deacon rockfish" =
    "Blue.deac.rockfish",
  
  "Cabezon - Northern California" =
    "Cabe.N.Calif.",
  
  "Cabezon - Southern California" =
    "Cabe.S.Calif.",
  
  "California scorpionfish" =
    "Calif.scorpionfish",
  
  "Canary rockfish" =
    "Canary.rockfish",
  
  "Copper rockfish - Northern California" =
    "Copp.rockfish.N.Calif.",
  
  "Copper rockfish - Southern California" =
    "Copp.rockfish.S.Calif.",
  
  "Cowcod - Southern California" =
    "Cowcod.S.Calif.",
  
  "Longnose skate" =
    "Longn.skate",
  
  "Petrale Sole" =
    "Petra.Sole",
  
  "Quillback rockfish - California" =
    "Quill.rockfish.Calif.",
  
  "Quillback rockfish - Washington" =
    "Quill.rockfish.Wash.",
  
  "Squarespot rockfish" =
    "Squares.rockfish",
  
  "Vermilion rockfish and Sunset rockfish Complex - Northern California" =
    "Verm.Sun.rockfish.N.Calif."
)


plot_dat <- plot_dat %>%
  mutate(
    stock = recode(
      stock,
      !!!short_names
    )
  )


# ============================================================
# 4) Indicator name conversions
# ============================================================
#
# MED has F/F01
# NEA has F/Fmsy
# US has F/Fmgt
#
# All are standardised to F/F[Ref] for plotting
# ============================================================

plot_dat <- plot_dat %>%
  mutate(
    
    name = case_when(
      
      name %in% c(
        "ABImsy",
        "ABI_msy",
        "ABI[MSY]",
        "ABIref",
        "ABI_ref"
      ) ~
        "ABI[ref]",
      
      
      name %in% c(
        "ABI0",
        "ABIO",
        "ABI[0]"
      ) ~
        "ABI[0]",
      
      
      name %in% c(
        "F/F01",
        "F/F0.1",
        "F/Fmsy",
        "F/FMSY",
        "F/Fmgt",
        "F/F[Ref]",
        "F/Fref",
        "F/FRef"
      ) ~
        "F/F[Ref]",
      
      
      name %in% c(
        "SSB/Blim",
        "SSB_Blim",
        "SSB/B[lim]"
      ) ~
        "SSB/B[lim]",
      
      
      name %in% c(
        "SSB/Bref",
        "SSB_Bref",
        "SSB/B[ref]",
        "SSB/Bmsy",
        "SSB/BMSY",
        "SSB/Bmgt"
      ) ~
        "SSB/B[ref]",
      
      
      name %in% c(
        "SSB/B0",
        "SSB/B[0]"
      ) ~
        "SSB/B[0]",
      
      
      TRUE ~ name
    )
    
  ) %>%
  
  filter(
    name %in% c(
      "ABI[ref]",
      "F/F[Ref]",
      "SSB/B[lim]",
      "SSB/B[ref]"
    )
  )


# Check indicator names

print(
  plot_dat %>%
    count(
      area,
      name
    )
)


# ============================================================
# 5) Read SSB reference file
# ============================================================
#
# EXACTLY AS IN ORIGINAL SCRIPT
# ============================================================

ssb_ref <- read.csv(
  "ssb_ref_data.csv",
  sep = ",",
  stringsAsFactors = FALSE
) %>%
  
  mutate(
    
    stock  = str_trim(stock),
    area   = str_trim(area),
    
    Blim   = as.numeric(Blim),
    SSBref = as.numeric(SSBref)
    
  ) %>%
  
  select(
    stock,
    area,
    Blim,
    SSBref
  )


# ============================================================
# 6) Join reference values
# ============================================================
#
# EXACT ORIGINAL JOIN
# ============================================================

plot_dat <- plot_dat %>%
  
  mutate(
    
    stock = str_trim(stock),
    area  = str_trim(area)
    
  ) %>%
  
  left_join(
    
    ssb_ref,
    
    by = c(
      "stock",
      "area"
    )
  )


# Check missing SSB references

missing_ssb_ref <- plot_dat %>%
  
  filter(
    
    name == "SSB/B[lim]" &
      (
        is.na(Blim) |
          is.na(SSBref)
      )
    
  ) %>%
  
  select(
    area,
    stock,
    name,
    Blim,
    SSBref
  )


if (nrow(missing_ssb_ref) > 0) {
  
  print(missing_ssb_ref)
  
  warning(
    "Some SSB/Blim rows have missing Blim or SSBref. ",
    "These rows cannot be converted to SSB/Bref."
  )
}


# ============================================================
# 7) Convert biomass indicator when input is SSB/Blim
# ============================================================
#
# EXACT ORIGINAL DATA CONVERSION
#
# SSB/SSBref = (SSB/Blim) * (Blim/SSBref)
# ============================================================

plot_dat <- plot_dat %>%
  
  mutate(
    
    final = if_else(
      
      name == "SSB/B[lim]" &
        !is.na(Blim) &
        !is.na(SSBref),
      
      final * (Blim / SSBref),
      
      final
    ),
    
    
    min = if_else(
      
      name == "SSB/B[lim]" &
        !is.na(Blim) &
        !is.na(SSBref),
      
      min * (Blim / SSBref),
      
      min
    ),
    
    
    max = if_else(
      
      name == "SSB/B[lim]" &
        !is.na(Blim) &
        !is.na(SSBref),
      
      max * (Blim / SSBref),
      
      max
    ),
    
    
    `5th_last` = if_else(
      
      name == "SSB/B[lim]" &
        !is.na(Blim) &
        !is.na(SSBref),
      
      `5th_last` * (Blim / SSBref),
      
      `5th_last`
    ),
    
    
    name = if_else(
      
      name == "SSB/B[lim]" &
        !is.na(Blim) &
        !is.na(SSBref),
      
      "SSB/B[ref]",
      
      name
    )
    
  )


# ########################################################################
# ONLY NEW PART
# ########################################################################
#
# From this point onward we create graphical labels.
#
# IMPORTANT:
# stock itself is NOT modified.
# Therefore none of the calculations or joins above are affected.
# ########################################################################


# ============================================================
# 8) Mediterranean DISPLAY labels
# ============================================================

med_display_names <- c(
  
  "ara.18.19.20" =
    "Blue-red.shrimp.18-20.C.Med",
  
  "ara.6.7" =
    "Blue-red.shrimp.6-7.W.Med",
  
  
  "ars.18.19.20" =
    "Giant.red.shrimp.18-20.C.Med",
  
  "ars.9.10.11" =
    "Giant.red.shrimp.9-11.W.Med",
  
  
  "dps.1" =
    "Deep-water.rose.shrimp.1.W.Med",
  
  "dps.8.9.10.11" =
    "Deep-water.rose.shrimp.8-11.W.Med",
  
  
  "hke.1.5.6.7" =
    "Hake.1,5-7.W.Med",
  
  "hke.19" =
    "Hake.19.C.Med",
  
  "hke.20" =
    "Hake.20.C.Med",
  
  "hke.22" =
    "Hake.22.E.Med",
  
  "hke.8.9.10.11" =
    "Hake.8-11.W.Med",
  
  
  "mur.15.16" =
    "Striped.red.mullet.15-16.C.Med",
  
  
  "mut.1" =
    "Red.mullet.1.W.Med",
  
  "mut.19" =
    "Red.mullet.19.C.Med",
  
  "mut.22" =
    "Red.mullet.22.E.Med",
  
  "mut.6" =
    "Red.mullet.6.W.Med",
  
  "mut.7" =
    "Red.mullet.7.W.Med",
  
  "mut.9" =
    "Red.mullet.9.W.Med",
  
  
  "nep.15.16" =
    "Norway.lobster.15-16.C.Med",
  
  "nep.6" =
    "Norway.lobster.6.W.Med",
  
  "nep.9" =
    "Norway.lobster.9.W.Med",
  
  
  "sol.17" =
    "Common.sole.17.C.Med"
)


# ============================================================
# 9) Northeast Atlantic DISPLAY labels
# ============================================================

nea_display_names <- c(
  
  "ank.27.78abd" =
    "Black-bellied.angler.Celtic-Biscay",
  
  "bss.27.4bc7ad-h" =
    "Seabass.N.Sea-Celtic",
  
  
  "cod.21.1" =
    "Cod.W.Greenland",
  
  "cod.27.1-2" =
    "Cod.NE.Arctic",
  
  "cod.27.5a" =
    "Cod.Iceland",
  
  "cod.27.7e-k" =
    "Cod.Celtic.Seas",
  
  
  "had.27.1-2" =
    "Haddock.NE.Arctic",
  
  "had.27.5a" =
    "Haddock.Iceland",
  
  
  "hke.27.3a46-8abd" =
    "Hake.N.Stock",
  
  "hke.27.8c9a" =
    "Hake.S.Stock",
  
  
  "ldb.27.8c9a" =
    "Four-spot.megrim.Iberian",
  
  
  "lin.27.5a" =
    "Ling.Iceland",
  
  "lin.27.5b" =
    "Ling.Faroes",
  
  
  "meg.27.7b-k8abd" =
    "Megrim.Celtic-Biscay",
  
  
  "ple.27.21-23" =
    "Plaice.Kattegat-Belt",
  
  "ple.27.420" =
    "Plaice.N.Sea-Skagerrak",
  
  
  "pok.27.1-2" =
    "Saithe.NE.Arctic",
  
  "pok.27.3a46" =
    "Saithe.N.Sea-Scotland",
  
  "pok.27.5b" =
    "Saithe.Faroes",
  
  
  "reg.27.561214" =
    "Golden.redfish.Iceland-Faroes",
  
  
  "sol.27.4" =
    "Sole.N.Sea",
  
  "sol.27.7a" =
    "Sole.Irish.Sea",
  
  "sol.27.7fg" =
    "Sole.Celtic.Sea",
  
  
  "tur.27.4" =
    "Turbot.N.Sea",
  
  
  "wit.27.3a47d" =
    "Witch.N.Sea"
)


# ============================================================
# 10) Combine DISPLAY labels
# ============================================================

display_names <- c(
  
  med_display_names,
  nea_display_names
)


# ============================================================
# 11) Create stock_label ONLY FOR DISPLAY
# ============================================================
#
# US names remain exactly as generated by the original script.
# MED and NEA are replaced only visually.
# ============================================================

plot_dat <- plot_dat %>%
  
  mutate(
    
    stock_label = recode(
      
      stock,
      
      !!!display_names,
      
      .default = stock
    )
  )


# Check display mapping

print(
  
  plot_dat %>%
    
    distinct(
      area,
      stock,
      stock_label
    ) %>%
    
    arrange(
      area,
      stock
    )
)


# ============================================================
# 12) Build plotting data
# ============================================================
#
# ORIGINAL LOGIC
# ============================================================

eps <- 1e-12


# Changes smaller than about 2% are treated as stable
# and no arrow is shown

trend_tol <- log(1.02)


plot_dat1 <- plot_dat %>%
  
  mutate(
    
    area = factor(
      
      area,
      
      levels = c(
        "MED",
        "NEA",
        "US"
      )
    )
  ) %>%
  
  
  arrange(
    area,
    stock
  ) %>%
  
  
  mutate(
    
    stock_factor =
      fct_inorder(stock) %>%
      fct_rev()
    
  ) %>%
  
  
  mutate(
    
    min_ok =
      pmin(
        min,
        max,
        na.rm = TRUE
      ),
    
    
    max_ok =
      pmax(
        min,
        max,
        na.rm = TRUE
      ),
    
    
    x_final =
      log(
        pmax(
          final,
          eps
        )
      ),
    
    
    x_5th_last =
      log(
        pmax(
          `5th_last`,
          eps
        )
      ),
    
    
    x_min =
      log(
        pmax(
          min_ok,
          eps
        )
      ),
    
    
    x_max =
      log(
        pmax(
          max_ok,
          eps
        )
      ),
    
    
    equal_range =
      x_min == x_max,
    
    
    x_min =
      if_else(
        
        equal_range,
        
        x_min - 0.01,
        
        x_min
      ),
    
    
    x_max =
      if_else(
        
        equal_range,
        
        x_max + 0.01,
        
        x_max
      ),
    
    
    delta_log =
      x_final -
      x_5th_last,
    
    
    trend_status = case_when(
      
      is.na(delta_log) ~
        NA_character_,
      
      
      abs(delta_log) <= trend_tol ~
        "stable",
      
      
      # For F/Fref, lower values are better
      
      name == "F/F[Ref]" &
        delta_log < 0 ~
        "improving",
      
      
      name == "F/F[Ref]" &
        delta_log > 0 ~
        "deteriorating",
      
      
      # For ABIref and SSB/Bref, higher values are better
      
      name != "F/F[Ref]" &
        delta_log > 0 ~
        "improving",
      
      
      name != "F/F[Ref]" &
        delta_log < 0 ~
        "deteriorating",
      
      
      TRUE ~
        NA_character_
    ),
    
    
    # Arrow direction
    
    arrow_status = case_when(
      
      trend_status == "stable" ~
        NA_character_,
      
      
      name == "F/F[Ref]" &
        trend_status == "improving" ~
        "←",
      
      
      name == "F/F[Ref]" &
        trend_status == "deteriorating" ~
        "→",
      
      
      name != "F/F[Ref]" &
        trend_status == "improving" ~
        "→",
      
      
      name != "F/F[Ref]" &
        trend_status == "deteriorating" ~
        "←",
      
      
      TRUE ~
        NA_character_
    )
    
  ) %>%
  
  select(
    -equal_range
  )


# ============================================================
# 13) Graphical label lookup
# ============================================================
#
# Maps the ORIGINAL stock factor to the new DISPLAY text.
# Nothing in the data is changed.
# ============================================================

stock_label_lookup <- plot_dat1 %>%
  
  distinct(
    stock,
    stock_label
  ) %>%
  
  {
    setNames(
      .$stock_label,
      .$stock
    )
  }


# ============================================================
# 14) Save data used for Figure 2
# ============================================================

write.csv(
  
  plot_dat1,
  
  "~/ABI/Draft paper/Third step/Output/Fig_2.csv",
  
  row.names = FALSE
)


# Check final plotting data

print(
  
  plot_dat1 %>%
    
    count(
      area,
      name
    )
)


print(
  
  plot_dat1 %>%
    
    count(
      name,
      trend_status
    )
)


# ============================================================
# 15) Custom legend glyph for trend arrows
# ============================================================
#
# ORIGINAL
# ============================================================

draw_key_arrow <- function(
    data,
    params,
    size
) {
  
  col <- data$colour
  
  if (
    is.null(col) ||
    is.na(col)
  ) {
    
    col <- "black"
  }
  
  
  alpha <- data$alpha
  
  if (
    is.null(alpha) ||
    is.na(alpha)
  ) {
    
    alpha <- 1
  }
  
  
  text_size <- data$size
  
  if (
    is.null(text_size) ||
    is.na(text_size)
  ) {
    
    text_size <- 5
  }
  
  
  grid::textGrob(
    
    label = "→",
    
    x = 0.5,
    
    y = 0.5,
    
    gp = grid::gpar(
      
      col =
        scales::alpha(
          col,
          alpha
        ),
      
      fontsize =
        text_size *
        ggplot2::.pt
    )
  )
}


# ============================================================
# 16) Plot function
# ============================================================
#
# ORIGINAL FUNCTION
# with ONE graphical addition only:
#
# scale_y_discrete(labels = stock_label_lookup)
# ============================================================

base_plot <- function(
    data,
    rect_color1,
    rect_color2,
    show_y_axis = TRUE,
    xlim = c(-10, 10)
) {
  
  ggplot(
    
    data,
    
    aes(
      x = x_final,
      y = stock_factor
    )
    
  ) +
    
    
    theme_bw() +
    
    
    scale_x_continuous(
      
      breaks =
        seq(
          xlim[1],
          xlim[2],
          1
        ),
      
      limits =
        xlim
    ) +
    
    
    # ========================================================
  # ONLY graphical change:
  # replace stock IDs by new display labels
  # ========================================================
  
  scale_y_discrete(
    
    labels =
      stock_label_lookup
  ) +
    
    
    facet_wrap(
      
      ~ name,
      
      labeller =
        label_parsed
    ) +
    
    
    geom_rect(
      
      xmin =
        xlim[1],
      
      xmax =
        0,
      
      ymin =
        -Inf,
      
      ymax =
        Inf,
      
      fill =
        rect_color1,
      
      alpha =
        0.01,
      
      inherit.aes =
        FALSE
    ) +
    
    
    geom_rect(
      
      xmin =
        0,
      
      xmax =
        xlim[2],
      
      ymin =
        -Inf,
      
      ymax =
        Inf,
      
      fill =
        rect_color2,
      
      alpha =
        0.01,
      
      inherit.aes =
        FALSE
    ) +
    
    
    geom_errorbar(
      
      aes(
        xmin = x_min,
        xmax = x_max
      ),
      
      width =
        0.4,
      
      linewidth =
        0.5,
      
      na.rm =
        FALSE,
      
      show.legend =
        FALSE
    ) +
    
    
    # Final value points
    
    geom_point(
      
      aes(
        fill = area
      ),
      
      size =
        3.5,
      
      shape =
        21,
      
      alpha =
        0.9,
      
      colour =
        "grey10",
      
      show.legend =
        c(
          fill = TRUE,
          colour = FALSE
        )
    ) +
    
    
    # Trend arrows
    
    geom_text(
      
      data =
        data %>%
        filter(
          
          !is.na(
            arrow_status
          ),
          
          trend_status %in%
            c(
              "improving",
              "deteriorating"
            )
        ),
      
      
      aes(
        
        x =
          x_final,
        
        y =
          stock_factor,
        
        label =
          arrow_status,
        
        colour =
          trend_status
      ),
      
      
      inherit.aes =
        FALSE,
      
      
      vjust =
        -0.15,
      
      
      size =
        5,
      
      
      na.rm =
        TRUE,
      
      
      show.legend =
        c(
          colour = TRUE,
          fill = FALSE
        ),
      
      
      key_glyph =
        draw_key_arrow
    ) +
    
    
    # ORIGINAL REGION SCALE
    
    scale_fill_brewer(
      
      palette =
        "Set2",
      
      name =
        "Region:"
    ) +
    
    
    # ORIGINAL TREND SCALE
    
    scale_colour_manual(
      
      name =
        "Trend:",
      
      values =
        c(
          
          improving =
            "darkgreen",
          
          deteriorating =
            "red3"
        ),
      
      breaks =
        c(
          "improving",
          "deteriorating"
        ),
      
      labels =
        c(
          "Improving",
          "Deteriorating"
        ),
      
      na.translate =
        FALSE,
      
      drop =
        FALSE
    ) +
    
    
    guides(
      
      fill =
        guide_legend(
          
          ncol =
            1,
          
          order =
            2,
          
          override.aes =
            list(
              
              shape =
                21,
              
              size =
                3.5,
              
              colour =
                "grey10"
            )
        ),
      
      
      colour =
        guide_legend(
          
          ncol =
            1,
          
          order =
            1,
          
          override.aes =
            list(
              
              shape =
                NA,
              
              fill =
                NA,
              
              size =
                5
            )
        )
    ) +
    
    
    theme(
      
      text =
        element_text(
          size = 13
        ),
      
      
      legend.position =
        "right",
      
      
      strip.background =
        element_blank(),
      
      
      strip.text =
        element_text(
          
          face =
            "bold",
          
          size =
            14
        ),
      
      
      axis.title.x =
        element_blank()
      
    ) +
    
    
    {
      
      if (!show_y_axis) {
        
        theme(
          
          axis.text.y =
            element_blank(),
          
          axis.ticks.y =
            element_blank(),
          
          axis.title.y =
            element_blank()
        )
        
      } else {
        
        theme()
      }
    }
}


# ============================================================
# 17) Build three panels
# ============================================================
#
# EXACT ORIGINAL LOGIC
# ============================================================

x_limits <- c(
  -10,
  10
)


# ABIref

t_bignew1 <- base_plot(
  
  filter(
    plot_dat1,
    name == "ABI[ref]"
  ),
  
  rect_color1 =
    "#FF9999",
  
  rect_color2 =
    "#99FF99",
  
  show_y_axis =
    TRUE,
  
  xlim =
    x_limits
  
) +
  
  ylab(
    "Stock ID"
  )


# F/Fref

t_bignew2 <- base_plot(
  
  filter(
    plot_dat1,
    name == "F/F[Ref]"
  ),
  
  rect_color1 =
    "#99FF99",
  
  rect_color2 =
    "#FF9999",
  
  show_y_axis =
    FALSE,
  
  xlim =
    x_limits
)


# SSB/Bref

t_bignew3 <- base_plot(
  
  filter(
    plot_dat1,
    name == "SSB/B[ref]"
  ),
  
  rect_color1 =
    "#FF9999",
  
  rect_color2 =
    "#99FF99",
  
  show_y_axis =
    FALSE,
  
  xlim =
    x_limits
)


# ============================================================
# 18) Combine panels
# ============================================================
#
# EXACT ORIGINAL PATCHWORK LOGIC
# ============================================================

t_final <-
  
  (
    t_bignew1 |
      t_bignew2 |
      t_bignew3
  ) +
  
  plot_layout(
    
    guides =
      "collect"
  ) +
  
  plot_annotation(
    
    caption =
      "Log (value)",
    
    theme =
      theme(
        
        plot.caption.position =
          "plot",
        
        plot.caption =
          element_text(
            
            size =
              16,
            
            hjust =
              0.5,
            
            margin =
              margin(
                t = 8
              )
          )
      )
  )


# Display figure

t_final


# ============================================================
# 19) Save figure
# ============================================================

setwd(
  "~/ABI/Draft paper/Third step/Figure"
)


ggsave(
  
  t_final,
  
  filename =
    "Figure_2_new_last.png",
  
  width =
    14,
  
  height =
    12,
  
  dpi =
    300
)


#########################################################################################################################################
# ===============================
#   FIGURE 3 —
#   Density distributions of ABIref, F/Fref and SSB/Bref by region
# ===============================
#########################################################################################################################################

region_cols <- c(
  "MED" = "#1b9e77",
  "NEA" = "#d95f02",
  "US"  = "#7570b3"
)

# ---- 1) Keep only final values for each stock x indicator ----
fig3_dat <- plot_dat %>%
  filter(
    name %in% c("ABI[ref]", "F/F[Ref]", "SSB/B[ref]"),
    is.finite(final),
    final > 0
  ) %>%
  mutate(
    area = factor(area, levels = c("MED", "NEA", "US"))
  ) %>%
  group_by(area, stock, name) %>%
  summarise(value = unique(final)[1], .groups = "drop") %>%
  mutate(
    name_lab = case_when(
      name == "ABI[ref]"   ~ "ABIref",
      name == "F/F[Ref]"   ~ "F/Fref",
      name == "SSB/B[ref]" ~ "SSB/Bref",
      TRUE ~ as.character(name)
    ),
    name_lab = factor(name_lab, levels = c("ABIref", "F/Fref", "SSB/Bref"))
  )

# ---- 2) Common theme ----
theme_fig3 <- theme_bw() +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    text = element_text(size = 16),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 16)
  )

# ---- 3) ABIref panel ----
Fig3a <- ggplot(
  data = filter(fig3_dat, name_lab == "ABIref"),
  aes(x = value, fill = area)
) +
  geom_density(alpha = 0.35, linewidth = 0.8, colour = "grey50") +
  scale_fill_manual(values = region_cols, name = "Region:") +
  guides(fill = guide_legend(nrow = 1)) +
  scale_x_continuous(breaks = seq(0, 7, 1), limits = c(0, 7)) +
  labs(
    x = expression(ABI[ref]),
    y = NULL
  ) +
  geom_vline(xintercept = 1, colour = "grey30", linetype = "dashed", linewidth = 1) +
  theme_fig3

# ---- 4) F/Fref panel ----
Fig3b <- ggplot(
  data = filter(fig3_dat, name_lab == "F/Fref"),
  aes(x = value, fill = area)
) +
  geom_density(alpha = 0.35, linewidth = 0.8, colour = "grey50") +
  scale_fill_manual(values = region_cols, name = "Region:") +
  scale_x_continuous(breaks = seq(0, 7, 1), limits = c(0, 7)) +
  labs(
    x = expression(F/F[ref]),
    y = "Density"
  ) +
  geom_vline(xintercept = 1, colour = "grey30", linetype = "dashed", linewidth = 1) +
  theme_fig3 +
  theme(legend.position = "none")

# ---- 5) SSB/Bref panel ----
Fig3c <- ggplot(
  data = filter(fig3_dat, name_lab == "SSB/Bref"),
  aes(x = value, fill = area)
) +
  geom_density(alpha = 0.35, linewidth = 0.8, colour = "grey50") +
  scale_fill_manual(values = region_cols, name = "Region:") +
  scale_x_continuous(breaks = seq(0, 7, 1), limits = c(0, 7)) +
  labs(
    x = expression(SSB/B[ref]),
    y = NULL
  ) +
  geom_vline(xintercept = 1, colour = "grey30", linetype = "dashed", linewidth = 1) +
  theme_fig3 +
  theme(
    legend.position = "none",
    axis.title.y = element_blank()
  )

# ---- 6) Combine Figure 3 ----
Figure_3 <- (Fig3a / Fig3b / Fig3c) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "top",
    legend.direction = "horizontal"
  )

Figure_3

# ---- 7) Save ----
setwd("~/ABI/Draft paper/Third step/Figure")
ggsave(Figure_3, filename = "Figure_3.png", width = 8, height = 10, dpi = 300)

######################################################################
######## Figure 5 + SM Figure S6
# ============================================================
# Build roc_dat from Fig2 inputs, then:
#  - Figure 4: AUC + TSS points (AUC top, TSS bottom)
#  - SM Fig S6: ROC curves with 95% bootstrap CI ribbons
# ============================================================

# ------------------------------------------------------------
# 1) PREPARE roc_dat (wide, numeric, no duplicates)
# ------------------------------------------------------------

(roc_dat <- plot_dat %>%
  select(stock, area, name, final) %>%
  mutate(
    stock = str_trim(stock),
    area  = str_trim(area),
    final = as.numeric(gsub(",", ".", as.character(final)))
  ) %>%
  group_by(stock, area, name) %>%
  summarise(final = mean(final, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = name, values_from = final))

# choose biomass column automatically
biom_col <- if ("SSB/B[ref]" %in% names(roc_dat)) "SSB/B[ref]" else "SSB/B[lim]"
attr(roc_dat, "biom_col_used") <- biom_col
message("Biomass column used for classification/label: ", biom_col)

(roc_dat <- roc_dat %>%
  rename(
    ABI_ref = `ABI[ref]`,
    F_Fref  = `F/F[Ref]`
  ) %>%
  mutate(
    SSB_ratio = .data[[biom_col]],
    area = factor(area, levels = c("MED","NEA","US")),
    true_status = ifelse(SSB_ratio > 1 & F_Fref < 1, "Healthy", "Unhealthy"),
    true_status = factor(true_status, levels = c("Unhealthy","Healthy")),
    ABI_ref   = as.numeric(ABI_ref),
    F_Fref    = as.numeric(F_Fref),
    SSB_ratio = as.numeric(SSB_ratio)
  ))

print(roc_dat %>% count(area, true_status))

# ------------------------------------------------------------
# 2) Helper: compute AUC + TSS 
# ------------------------------------------------------------

calc_auc_tss_ref <- function(truth, raw_score, auc_score = raw_score,
                             threshold = 1,
                             healthy_rule = c("higher", "lower")) {
  
  healthy_rule <- match.arg(healthy_rule)
  
  truth <- factor(truth, levels = c("Unhealthy", "Healthy"))
  raw_score <- as.numeric(raw_score)
  auc_score <- as.numeric(auc_score)
  
  if (length(unique(truth[!is.na(truth)])) < 2) {
    return(tibble(AUC = NA_real_, TSS = NA_real_))
  }
  
  # AUC: threshold-independent, with score oriented so higher = more Healthy
  r <- pROC::roc(
    response  = truth,
    predictor = auc_score,
    levels    = c("Unhealthy", "Healthy"),
    direction = "<",
    quiet     = TRUE
  )
  
  auc_val <- as.numeric(pROC::auc(r))
  
  # TSS: calculated at the theoretical/reference threshold
  pred_status <- if (healthy_rule == "higher") {
    ifelse(raw_score >= threshold, "Healthy", "Unhealthy")
  } else {
    ifelse(raw_score <= threshold, "Healthy", "Unhealthy")
  }
  
  pred_status <- factor(pred_status, levels = c("Unhealthy", "Healthy"))
  
  TP <- sum(pred_status == "Healthy"   & truth == "Healthy",   na.rm = TRUE)
  TN <- sum(pred_status == "Unhealthy" & truth == "Unhealthy", na.rm = TRUE)
  FP <- sum(pred_status == "Healthy"   & truth == "Unhealthy", na.rm = TRUE)
  FN <- sum(pred_status == "Unhealthy" & truth == "Healthy",   na.rm = TRUE)
  
  sens <- ifelse((TP + FN) > 0, TP / (TP + FN), NA_real_)
  spec <- ifelse((TN + FP) > 0, TN / (TN + FP), NA_real_)
  
  tss_val <- sens + spec - 1
  
  tibble(AUC = auc_val, TSS = tss_val)
}



# ------------------------------------------------------------
# 3) Compute skill by area and indicator (NO rownames issues)
# ------------------------------------------------------------

(biom_label <- ifelse(attr(roc_dat, "biom_col_used") == "SSB/B[ref]", "SSB/B[ref]", "SSB/B[lim]"))

(areas <- levels(roc_dat$area))

skill <- purrr::map_dfr(areas, function(a) {
  
  sub <- roc_dat %>% filter(area == a)
  
  bind_rows(
    tibble(area = a, indicator = "ABI[ref]") %>%
      bind_cols(
        calc_auc_tss_ref(
          truth = sub$true_status,
          raw_score = sub$ABI_ref,
          auc_score = sub$ABI_ref,
          threshold = 1,
          healthy_rule = "higher"
        )
      ),
    
    tibble(area = a, indicator = biom_label) %>%
      bind_cols(
        calc_auc_tss_ref(
          truth = sub$true_status,
          raw_score = sub$SSB_ratio,
          auc_score = sub$SSB_ratio,
          threshold = 1,
          healthy_rule = "higher"
        )
      ),
    
    tibble(area = a, indicator = "F/F[Ref]") %>%
      bind_cols(
        calc_auc_tss_ref(
          truth = sub$true_status,
          raw_score = sub$F_Fref,
          auc_score = -sub$F_Fref,
          threshold = 1,
          healthy_rule = "lower"
        )
      )
  )
}) %>%
  mutate(
    area = factor(area, levels = c("MED", "NEA", "US")),
    indicator = factor(indicator, levels = c("ABI[ref]", biom_label, "F/F[Ref]")),
    AUC = as.numeric(AUC),
    TSS = as.numeric(TSS)
  )
# ------------------------------------------------------------
# 4) Long for plotting (facet by metric)
# ------------------------------------------------------------

(skill_long <- skill %>%
  pivot_longer(cols = c(AUC, TSS), names_to = "metric", values_to = "value") %>%
  mutate(
    metric = factor(metric, levels = c("AUC","TSS")),
    area   = factor(area, levels = c("MED","NEA","US"))
  ))

# ------------------------------------------------------------
# 5) Plot Figure 5
# ------------------------------------------------------------

region_cols <- c(
  "MED" = "#1b9e77",
  "NEA" = "#d95f02",
  "US"  = "#7570b3"
)

p_fig5 <- ggplot(skill_long, aes(x = value, y = indicator, color = area)) +
  geom_point(size = 4) +
  
  facet_grid(metric ~ ., scales = "free_x") +
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  
  scale_color_manual(values = region_cols, drop = FALSE, name = "Region") +
  
  scale_y_discrete(
    labels = c(
      "ABI[ref]"   = expression(ABI[ref]),
      "SSB/B[ref]" = expression(SSB/B[ref]),
      "F/F[Ref]"   = expression(F/F[ref])
    )
  ) +
  
  scale_x_continuous(
    limits = c(-0.30, 1.05),
    breaks = seq(-0.25, 1, 0.25),
    expand = c(0, 0)
  ) +
  
  labs(
    x = "Skill value",
    y = "Indicator"
  ) +
  
  theme_bw(base_size = 11) +
  
  theme(
    legend.position = "right",
    strip.background = element_rect(fill = "grey95", color = NA),
    
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    
    strip.text = element_text(size = 14)
  )

print(p_fig5)

ggsave("Figure_5.png", p_fig5, width = 7.2, height = 4.6, dpi = 300)



#########################################################
########### FIGURE 4 + SUPPLEMENTARY MATERIAL
#########################################################

#########################################################################################################################################
# SM FIG S1 / S2 / S3 + Combined Figure (ABIref)
#########################################################################################################################################


# ------------------------------------------------------------
# Helper: make sure we always have ABIref column
# ------------------------------------------------------------

ensure_ABIref <- function(df) {
  
  if ("ABIref" %in% names(df)) return(df)
  
  if ("ABImsy" %in% names(df))
    return(dplyr::rename(df, ABIref = ABImsy))
  
  if ("ABI_ref" %in% names(df))
    return(dplyr::rename(df, ABIref = ABI_ref))
  
  if ("ABI[ref]" %in% names(df))
    return(dplyr::rename(df, ABIref = `ABI[ref]`))
  
  stop(
    "Cannot find ABIref/ABImsy/ABI_ref/ABI[ref] column in this dataset."
  )
}


# ============================================================
# Mediterranean DISPLAY labels
# Same labels used in Figure 2
# ============================================================

med_display_names <- c(
  
  "ara.18.19.20" =
    "Blue-red.shrimp.18-20.C.Med",
  
  "ara.6.7" =
    "Blue-red.shrimp.6-7.W.Med",
  
  
  "ars.18.19.20" =
    "Giant.red.shrimp.18-20.C.Med",
  
  "ars.9.10.11" =
    "Giant.red.shrimp.9-11.W.Med",
  
  
  "dps.1" =
    "Deep-water.rose.shrimp.1.W.Med",
  
  "dps.8.9.10.11" =
    "Deep-water.rose.shrimp.8-11.W.Med",
  
  
  "hke.1.5.6.7" =
    "Hake.1,5-7.W.Med",
  
  "hke.19" =
    "Hake.19.C.Med",
  
  "hke.20" =
    "Hake.20.C.Med",
  
  "hke.22" =
    "Hake.22.E.Med",
  
  "hke.8.9.10.11" =
    "Hake.8-11.W.Med",
  
  
  "mur.15.16" =
    "Striped.red.mullet.15-16.C.Med",
  
  
  "mut.1" =
    "Red.mullet.1.W.Med",
  
  "mut.19" =
    "Red.mullet.19.C.Med",
  
  "mut.22" =
    "Red.mullet.22.E.Med",
  
  "mut.6" =
    "Red.mullet.6.W.Med",
  
  "mut.7" =
    "Red.mullet.7.W.Med",
  
  "mut.9" =
    "Red.mullet.9.W.Med",
  
  
  "nep.15.16" =
    "Norway.lobster.15-16.C.Med",
  
  "nep.6" =
    "Norway.lobster.6.W.Med",
  
  "nep.9" =
    "Norway.lobster.9.W.Med",
  
  
  "sol.17" =
    "Common.sole.17.C.Med"
)


# ------------------------------------------------------------
# SM FIG S1 — MED
# ------------------------------------------------------------

setwd("~/ABI/Draft paper/Third step/Data")


(dat1 <- read.csv(
  "STECF_ABI_data.csv",
  sep = ";"
))


(dat1 <- ensure_ABIref(dat1))


# ------------------------------------------------------------
# Standardise original MED stock IDs
# ------------------------------------------------------------

dat1 <- dat1 %>%
  
  mutate(
    
    stock = case_when(
      
      str_detect(
        stock,
        "^[A-Z]{3}(?:_[0-9A-Za-z]+)+$"
      ) ~
        tolower(
          gsub(
            "_",
            ".",
            stock
          )
        ),
      
      TRUE ~ stock
    )
  )


# ------------------------------------------------------------
# Create DISPLAY label only
# Original stock ID remains unchanged
# ------------------------------------------------------------

dat1 <- dat1 %>%
  
  mutate(
    
    stock_label = recode(
      
      stock,
      
      !!!med_display_names,
      
      .default = stock
    )
  )


# Check labels

print(
  dat1 %>%
    distinct(
      stock,
      stock_label
    ) %>%
    arrange(stock)
)


# ------------------------------------------------------------
# Background values
# ------------------------------------------------------------

values <- data.frame(
  
  xmin = c(
    -Inf,
    -Inf
  ),
  
  xmax = c(
    Inf,
    Inf
  ),
  
  ymin = c(
    0,
    1
  ),
  
  ymax = c(
    1,
    Inf
  ),
  
  fill = c(
    "#FF9999",
    "#99FF99"
  )
)


val1 <- values[1, ]

val2 <- values[2, ]


# ------------------------------------------------------------
# Plot
# ------------------------------------------------------------

g1 <- ggplot() +
  
  # IMPORTANT:
  # use stock_label only for facet titles
  facet_wrap(
    ~ stock_label,
    scales = "free_x"
  ) +
  
  theme_bw() +
  
  xlab(
    "Year"
  ) +
  
  ylab(
    bquote(
      ABI[ref]
    )
  ) +
  
  geom_hline(
    yintercept = 1,
    col = "black",
    linetype = "dashed"
  ) +
  
  geom_rect(
    
    data = val1,
    
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    ),
    
    alpha = 0.05,
    fill = "red1"
  ) +
  
  geom_rect(
    
    data = val2,
    
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    ),
    
    alpha = 0.05,
    fill = "seagreen1"
  ) +
  
  geom_line(
    
    data = dat1,
    
    aes(
      x = year,
      y = ABIref
    ),
    
    linewidth = 1.1
  ) +
  
  scale_y_continuous(
    
    breaks = seq(
      0,
      1.5,
      0.5
    ),
    
    limits = c(
      0,
      1.5
    ),
    
    expand = c(
      0,
      0
    )
  ) +
  
  theme(
    
    text = element_text(
      size = 14
    )
  )


g1


# ------------------------------------------------------------
# Save
# ------------------------------------------------------------

setwd(
  "~/ABI/Draft paper/Third step/Figure"
)


ggsave(
  
  g1,
  
  filename =
    "SM_MED_Fig_S1.png",
  
  width =
    18,
  
  height =
    12
)



################################################################################################
# SM FIG S2 — US
################################################################################################

setwd("~/ABI/Draft paper/Third step/Data/US_elaboration")

dat2 <- read.csv(
  "US_ABI_data_new.csv",
  sep = ",",
  check.names = FALSE
)

dat2 <- ensure_ABIref(dat2)


# ============================================================
# US DISPLAY labels
# Same labels used in Figure 2
# IMPORTANT: stock itself is NOT modified
# ============================================================

us_display_names <- c(
  
  "Black rockfish - Oregon stock" =
    "Bla.rockfish.Ore",
  
  "Black rockfish - Washington stock" =
    "Bla.rockfish.Wash.",
  
  "Black-and-yellow and Gopher rockfish complex" =
    "Bla.yell.Goph.rockfish",
  
  "Blackgill rockfish" =
    "Bla.gill.rockfish",
  
  "Blue deacon rockfish" =
    "Blue.deac.rockfish",
  
  "Cabezon - Northern California" =
    "Cabe.N.Calif.",
  
  "Cabezon - Southern California" =
    "Cabe.S.Calif.",
  
  "California scorpionfish" =
    "Calif.scorpionfish",
  
  "Canary rockfish" =
    "Canary.rockfish",
  
  "Copper rockfish - Northern California" =
    "Copp.rockfish.N.Calif.",
  
  "Copper rockfish - Southern California" =
    "Copp.rockfish.S.Calif.",
  
  "Cowcod - Southern California" =
    "Cowcod.S.Calif.",
  
  "Longnose skate" =
    "Longn.skate",
  
  "Petrale Sole" =
    "Petra.Sole",
  
  "Quillback rockfish - California" =
    "Quill.rockfish.Calif.",
  
  "Quillback rockfish - Washington" =
    "Quill.rockfish.Wash.",
  
  "Squarespot rockfish" =
    "Squares.rockfish",
  
  "Vermilion rockfish and Sunset rockfish Complex - Northern California" =
    "Verm.Sun.rockfish.N.Calif."
)


# ------------------------------------------------------------
# Create DISPLAY label only
#
# IMPORTANT:
# use base R assignment because dat2 contains at least one
# empty/NA column name and mutate() would fail.
# ------------------------------------------------------------

dat2$stock_label <- dplyr::recode(
  dat2$stock,
  !!!us_display_names,
  .default = dat2$stock
)


# Check labels

print(
  unique(
    dat2[c("stock", "stock_label")]
  )
)


# ------------------------------------------------------------
# Background values
# ------------------------------------------------------------

values <- data.frame(
  xmin = c(-Inf, -Inf),
  xmax = c(Inf, Inf),
  ymin = c(0, 1),
  ymax = c(1, Inf),
  fill = c("#FF9999", "#99FF99")
)

val1 <- values[1, ]
val2 <- values[2, ]


# ------------------------------------------------------------
# Plot
# ------------------------------------------------------------

g2 <- ggplot() +
  
  facet_wrap(
    ~ stock_label,
    scales = "free_x"
  ) +
  
  theme_bw() +
  
  xlab("Year") +
  
  ylab(
    bquote(ABI[ref])
  ) +
  
  geom_hline(
    yintercept = 1,
    col = "black",
    linetype = "dashed"
  ) +
  
  geom_rect(
    data = val1,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    ),
    alpha = 0.05,
    fill = "red1"
  ) +
  
  geom_rect(
    data = val2,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    ),
    alpha = 0.05,
    fill = "seagreen1"
  ) +
  
  geom_line(
    data = dat2,
    aes(
      x = year,
      y = ABIref
    ),
    linewidth = 1.1
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 3.5, 0.5),
    limits = c(0, 3.5),
    expand = c(0, 0)
  ) +
  
  theme(
    text = element_text(size = 14)
  )


g2


setwd("~/ABI/Draft paper/Third step/Figure")

ggsave(
  g2,
  filename = "SM_US_Fig_S2.png",
  width = 18,
  height = 12
)



################################################################################################
# SM FIG S3 — NEA
################################################################################################

setwd("~/ABI/Draft paper/Third step/Data")

dat3 <- read_csv(
  "NEA_ABI_data_fig_3.csv",
  show_col_types = FALSE
)

dat3 <- ensure_ABIref(dat3)


# ============================================================
# NEA DISPLAY labels
# Same labels used in Figure 2
# ============================================================

nea_display_names <- c(
  
  "ank.27.78abd" =
    "Black-bellied.angler.Celtic-Biscay",
  
  "bss.27.4bc7ad-h" =
    "Seabass.N.Sea-Celtic",
  
  
  # Cod
  "cod.21.1" =
    "Cod.W.Greenland",
  
  "cod.27.1-2" =
    "Cod.NE.Arctic",
  
  "cod.27.5a" =
    "Cod.Iceland",
  
  "cod.27.7e-k" =
    "Cod.Celtic.Seas",
  
  
  # Haddock
  "had.27.1-2" =
    "Haddock.NE.Arctic",
  
  "had.27.5a" =
    "Haddock.Iceland",
  
  
  # Hake
  "hke.27.3a46-8abd" =
    "Hake.N.Stock",
  
  "hke.27.8c9a" =
    "Hake.S.Stock",
  
  
  # Four-spot megrim
  "ldb.27.8c9a" =
    "Four-spot.megrim.Iberian",
  
  
  # Ling
  "lin.27.5a" =
    "Ling.Iceland",
  
  "lin.27.5b" =
    "Ling.Faroes",
  
  
  # Megrim
  "meg.27.7b-k8abd" =
    "Megrim.Celtic-Biscay",
  
  
  # Plaice
  "ple.27.21-23" =
    "Plaice.Kattegat-Belt",
  
  "ple.27.420" =
    "Plaice.N.Sea-Skagerrak",
  
  
  # Saithe
  "pok.27.1-2" =
    "Saithe.NE.Arctic",
  
  "pok.27.3a46" =
    "Saithe.N.Sea-Scotland",
  
  "pok.27.5b" =
    "Saithe.Faroes",
  
  
  # Golden redfish
  "reg.27.561214" =
    "Golden.redfish.Iceland-Faroes",
  
  
  # Sole
  "sol.27.4" =
    "Sole.N.Sea",
  
  "sol.27.7a" =
    "Sole.Irish.Sea",
  
  "sol.27.7fg" =
    "Sole.Celtic.Sea",
  
  
  # Turbot
  "tur.27.4" =
    "Turbot.N.Sea",
  
  
  # Witch
  "wit.27.3a47d" =
    "Witch.N.Sea"
)


# ------------------------------------------------------------
# Create DISPLAY label only
# ------------------------------------------------------------

dat3 <- dat3 %>%
  mutate(
    
    stock_label = dplyr::recode(
      stock,
      !!!nea_display_names,
      .default = stock
    )
  )


# Check labels

print(
  dat3 %>%
    distinct(
      stock,
      stock_label
    ) %>%
    arrange(stock)
)


# ------------------------------------------------------------
# Background values
# ------------------------------------------------------------

values <- data.frame(
  xmin = c(-Inf, -Inf),
  xmax = c(Inf, Inf),
  ymin = c(0, 1),
  ymax = c(1, Inf),
  fill = c("#FF9999", "#99FF99")
)

val1 <- values[1, ]
val2 <- values[2, ]


# ------------------------------------------------------------
# Plot
# ------------------------------------------------------------

g3 <- ggplot() +
  
  facet_wrap(
    ~ stock_label,
    scales = "free_x"
  ) +
  
  theme_bw() +
  
  xlab("Year") +
  
  ylab(
    bquote(ABI[ref])
  ) +
  
  geom_hline(
    yintercept = 1,
    col = "black",
    linetype = "dashed"
  ) +
  
  geom_rect(
    data = val1,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    ),
    alpha = 0.05,
    fill = "red1"
  ) +
  
  geom_rect(
    data = val2,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    ),
    alpha = 0.05,
    fill = "seagreen1"
  ) +
  
  geom_line(
    data = dat3,
    aes(
      x = year,
      y = ABIref
    ),
    linewidth = 1.1
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 4, 0.5),
    limits = c(0, 4),
    expand = c(0, 0)
  ) +
  
  theme(
    text = element_text(size = 14)
  )


g3


setwd("~/ABI/Draft paper/Third step/Figure")

ggsave(
  g3,
  filename = "SM_NEA_Fig_S3.png",
  width = 18,
  height = 12
)



################################################################################################
# COMBINED MAIN FIGURE — FIGURE 4
################################################################################################
#
# IMPORTANT:
# stock remains the original ID used for stock selection.
# stock_label is used ONLY for facet display.
################################################################################################


# ------------------------------------------------------------
# Standardise minimal columns
# ------------------------------------------------------------
#
# dat1 and dat3 can use dplyr normally.
#
# dat2 is handled with BASE R because it contains at least one
# unnamed column in the original input dataset.
# ------------------------------------------------------------


dat1a <- dat1 %>%
  select(
    stock,
    stock_label,
    year,
    ABIref
  ) %>%
  mutate(
    area = "MED"
  )


# US — use BASE R to avoid empty/NA column-name problem

dat2a <- dat2[
  c(
    "stock",
    "stock_label",
    "year",
    "ABIref"
  )
]

dat2a$area <- "US"


dat3a <- dat3 %>%
  select(
    stock,
    stock_label,
    year,
    ABIref
  ) %>%
  mutate(
    area = "NEA"
  )


# ------------------------------------------------------------
# Merge regions
# ------------------------------------------------------------

datX <- bind_rows(
  dat1a,
  dat2a,
  dat3a
)


# ------------------------------------------------------------
# Choose the 15 stocks
# 5 per region
#
# IMPORTANT:
# selections still use ORIGINAL stock IDs
# ------------------------------------------------------------

med_stocks <- c(
  "hke.8.9.10.11",
  "sol.17",
  "dps.8.9.10.11",
  "ars.9.10.11",
  "mut.22"
)


us_stocks <- c(
  "Canary rockfish",
  "Cowcod - Southern California",
  "Quillback rockfish - Washington",
  "California scorpionfish",
  "Squarespot rockfish"
)


nea_stocks <- c(
  "cod.27.1-2",
  "ank.27.78abd",
  "pok.27.3a46",
  "sol.27.7fg",
  "lin.27.5a"
)


sel_stocks <- c(
  med_stocks,
  us_stocks,
  nea_stocks
)


# ------------------------------------------------------------
# Build display-label order corresponding to sel_stocks
# ------------------------------------------------------------

selected_labels <- datX %>%
  
  distinct(
    stock,
    stock_label
  ) %>%
  
  filter(
    stock %in% sel_stocks
  )


selected_labels <- selected_labels[
  match(
    sel_stocks,
    selected_labels$stock
  ),
]


print(selected_labels)


# ------------------------------------------------------------
# Select data
# ------------------------------------------------------------

datXa <- datX %>%
  
  filter(
    stock %in% sel_stocks
  ) %>%
  
  mutate(
    
    stock = factor(
      stock,
      levels = sel_stocks
    ),
    
    stock_label = factor(
      stock_label,
      levels = selected_labels$stock_label
    ),
    
    area = factor(
      area,
      levels = c(
        "MED",
        "US",
        "NEA"
      )
    )
  )


# ------------------------------------------------------------
# Check selected stocks and labels
# ------------------------------------------------------------

print(
  datXa %>%
    distinct(
      area,
      stock,
      stock_label
    ) %>%
    arrange(stock)
)


# ------------------------------------------------------------
# Background zones
# ------------------------------------------------------------

values <- data.frame(
  xmin = c(-Inf, -Inf),
  xmax = c(Inf, Inf),
  ymin = c(0, 1),
  ymax = c(1, Inf),
  fill = c("#FF9999", "#99FF99")
)

val1 <- values[1, ]
val2 <- values[2, ]


# ------------------------------------------------------------
# Figure 4 only:
# wrap the two longest facet labels
# ------------------------------------------------------------

fig4_labeller <- function(x) {
  
  x <- as.character(x)
  
  dplyr::recode(
    x,
    
    "Deep-water.rose.shrimp.8-11.W.Med" =
      "Deep-water.rose.shrimp.\n8-11.W.Med",
    
    "Black-bellied.angler.Celtic-Biscay" =
      "Black-bellied.angler.\nCeltic-Biscay",
    
    .default = x
  )
}


# ------------------------------------------------------------
# FIGURE 4
# ------------------------------------------------------------

gX <- ggplot() +
  
  facet_wrap(
    ~ stock_label,
    ncol = 5,
    scales = "free_x",
    labeller = labeller(
      stock_label = fig4_labeller
    )
  ) +
  
  theme_bw() +
  
  xlab("Year") +
  
  ylab(
    bquote(ABI[ref])
  ) +
  
  
  geom_hline(
    yintercept = 1,
    col = "black",
    linetype = "dashed"
  ) +
  
  
  geom_rect(
    
    data = val1,
    
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    ),
    
    inherit.aes = FALSE,
    
    alpha = 0.20,
    
    fill = val1$fill
  ) +
  
  
  geom_rect(
    
    data = val2,
    
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    ),
    
    inherit.aes = FALSE,
    
    alpha = 0.20,
    
    fill = val2$fill
  ) +
  
  
  geom_line(
    
    data = datXa,
    
    aes(
      x = year,
      y = ABIref,
      color = area
    ),
    
    linewidth = 1.1
  ) +
  
  
  scale_color_manual(
    
    name = "Region",
    
    values = c(
      MED = "#1b9e77",
      US  = "#7570b3",
      NEA = "#d95f02"
    )
  ) +
  
  
  scale_y_continuous(
    
    breaks = seq(
      0,
      4,
      0.5
    ),
    
    limits = c(
      0,
      4
    ),
    
    expand = c(
      0,
      0
    )
  ) +
  
  
  scale_x_continuous(
    
    breaks = scales::breaks_extended(
      n = 3
    ),
    
    labels = function(x)
      as.integer(x),
    
    guide = guide_axis(
      angle = 45
    )
  ) +
  
  
  theme(
    
    text = element_text(
      size = 18
    ),
    
    
    strip.background = element_rect(
      fill = "grey95",
      color = "grey70"
    ),
    
    
    strip.text = element_text(
      face = "bold",
      size = 16,
      lineheight = 0.90
    ),
    
    
    axis.title.x = element_text(
      size = 32
    ),
    
    
    axis.text.x = element_text(
      size = 16,
      hjust = 1
    ),
    
    
    axis.title.y = element_text(
      size = 32
    ),
    
    
    axis.text.y = element_text(
      size = 18
    ),
    
    
    legend.position = "right",
    
    
    legend.title = element_text(
      size = 32
    ),
    
    
    legend.text = element_text(
      size = 22
    ),
    
    
    panel.grid.minor = element_blank()
  )


# ------------------------------------------------------------
# Show Figure 4
# ------------------------------------------------------------

gX


# ------------------------------------------------------------
# Save Figure 4
# ------------------------------------------------------------

setwd(
  "~/ABI/Draft paper/Third step/Figure"
)


ggsave(
  gX,
  filename = "Figure_4.png",
  width = 20,
  height = 12,
  dpi = 300
)



# ------------------------------------------------------------
# SM Figure S4 — Taxonomic Order check (WORMS)
# ------------------------------------------------------------

# we decided to not use this by taxonomic order

# #################### MED
# setwd("~/ABI/Draft paper/Third step/Data")
# MED <- read.csv("MED_data_for_Fig_2.csv", sep = ",")[, c(1:5, 7)]
# names(MED)
# MED <- MED %>%
#   mutate(Order = case_when(
#     str_detect(species, "Aristeus antennatus")      ~ "Decapoda",
#     str_detect(species, "Aristaeomorpha foliacea")  ~ "Decapoda",
#     str_detect(species, "Parapenaeus longirostris") ~ "Decapoda",
#     str_detect(species, "Merluccius merluccius")    ~ "Gadiformes",
#     str_detect(species, "Mullus surmuletus")        ~ "Mulliformes",
#     str_detect(species, "Mullus barbatus")          ~ "Mulliformes",
#     str_detect(species, "Nephrops norvegicus")      ~ "Decapoda",
#     str_detect(species, "Solea solea")              ~ "Pleuronectiformes",
#     TRUE                                            ~ "Unknown"
#   ))
# 
# unique(MED$Order)
# MED <- MED[, c(1,2,3,4,5,7)]
# MED$area <- "MED"
# 
# #################### NEA
# NEA <- read.csv("NEA_data_for_Fig_2.csv", sep = ",")[, 1:5]
# 
# NEA <- NEA %>%
#   mutate(Order = case_when(
#     str_detect(stock, "^ank")               ~ "Lophiiformes",
#     str_detect(stock, "^bss")               ~ "Perciformes",
#     str_detect(stock, "^(cod|had|hke|lin|pok)") ~ "Gadiformes",
#     str_detect(stock, "^(ldb|meg|ple|sol|tur|wit)") ~ "Pleuronectiformes",
#     str_detect(stock, "^reg")               ~ "Scorpaeniformes",
#     TRUE                                    ~ "Unknown"
#   ))
# 
# unique(NEA$Order)
# NEA$area <- "NEA"
# 
# #################### US
# US <- read.csv("US_data_for_Fig_2.csv", sep = ",")
# 
# US <- US %>%
#   mutate(
#     Order = dplyr::case_when(
#       stock == "Black rockfish - Oregon stock"                                        ~ "Perciformes",  # Sebastes melanops
#       stock == "Black rockfish - Washington stock"                                    ~ "Perciformes",  # Sebastes melanops
#       stock == "Black-and-yellow and Gopher rockfish complex"                         ~ "Perciformes",  # Sebastes chrysomelas + S. carnatus
#       stock == "Blackgill rockfish"                                                   ~ "Perciformes",  # Sebastes melanostictus
#       stock == "Blue deacon rockfish"                                                 ~ "Perciformes",  # Sebastes diaconus
#       stock == "Cabezon - Northern California"                                        ~ "Perciformes",  # Scorpaenichthys marmoratus
#       stock == "Cabezon - Southern California"                                        ~ "Perciformes",  # Scorpaenichthys marmoratus
#       stock == "California scorpionfish"                                              ~ "Perciformes",  # Scorpaena guttata
#       stock == "Canary rockfish"                                                      ~ "Perciformes",  # Sebastes pinniger
#       stock == "Copper rockfish - Northern California"                                ~ "Perciformes",  # Sebastes caurinus
#       stock == "Copper rockfish - Southern California"                                ~ "Perciformes",  # Sebastes caurinus
#       stock == "Cowcod - Southern California"                                         ~ "Perciformes",  # Sebastes levis
#       stock == "Longnose skate"                                                       ~ "Rajiformes",   # Beringraja (Raia) rhina
#       stock == "Petrale Sole"                                                         ~ "Pleuronectiformes", # Eopsetta jordani
#       stock == "Quillback rockfish - California"                                      ~ "Perciformes",  # Sebastes maliger
#       stock == "Quillback rockfish - Washington"                                      ~ "Perciformes",  # Sebastes maliger
#       stock == "Squarespot rockfish"                                                  ~ "Perciformes",  # Sebastes hopkinsi
#       stock == "Vermilion rockfish and Sunset rockfish Complex - Northern California" ~ "Perciformes",  # Sebastes miniatus + S. crocotulus
#       TRUE                                                                            ~ NA_character_
#     )
#   )
# 
# US$area <- "US"
# 
# #################### bind + cleanup
# plot_dat <- bind_rows(MED, NEA, US) %>%
#   mutate(stock = str_trim(stock))   # remove accidental trailing spaces
# 
# # shorter names
# plot_dat <- plot_dat %>%
#   mutate(
#     stock = case_when(
#       str_detect(stock, "^[A-Z]{3}(?:_[0-9A-Za-z]+)+$") ~ tolower(gsub("_", ".", stock)),
#       TRUE ~ stock
#     )
#   )
# 
# # ------------------------------------------------------------
# # Standardize indicator names (ABIref instead of ABImsy)
# # ------------------------------------------------------------
# plot_dat <- plot_dat %>%
#   mutate(
#     name = case_when(
#       name %in% c("ABIref","ABI_ref","ABIREF")        ~ "ABI[ref]",
#       name %in% c("ABImsy","ABI_msy","ABIMSY")        ~ "ABI[ref]",   # <-- map old to new
#       name %in% c("F/F01","F/Fmsy","F/Fmgt")          ~ "F/F[Ref]",
#       name %in% c("SSB/Blim","SSB_Blim")              ~ "SSB/B[lim]",
#       name %in% c("SSB/B01")                          ~ "SSB/B[0.1]",
#       name %in% c("ABI0")                             ~ "ABI[0]",
#       name %in% c("SSB/B0")                           ~ "SSB/B[0]",
#       TRUE                                            ~ name
#     )
#   ) %>%
#   filter(!name %in% c("ABI[0]","SSB/B[0]"))
# 
# # ---- keep only ABIref if this figure is meant to show ABI only ----
# # (uncomment if needed)
# # plot_dat <- plot_dat %>% filter(name == "ABI[ref]")
# 
# # ---- x-range for background bands ----
# xlim <- c(-5, 5)
# 
# # ------------------------------------------------------------
# # Option 5a — by area (coloured)
# # ------------------------------------------------------------
# t_order_box <- ggplot(plot_dat, aes(x = log(final), y = Order, fill = area)) +
#   theme_bw() +
#   geom_rect(xmin = xlim[1], xmax = 0, ymin = -Inf, ymax = Inf,
#             inherit.aes = FALSE, fill = "#FF9999", alpha = 0.006) +
#   geom_rect(xmin = 0, xmax = xlim[2], ymin = -Inf, ymax = Inf,
#             inherit.aes = FALSE, fill = "#99FF99", alpha = 0.006) +
#   geom_vline(xintercept = 0, colour = "grey30", linetype = "dashed") +
#   geom_boxplot(position = position_dodge(width = 0.8),
#                width = 0.7, outlier.alpha = 0.4, alpha = 0.7, colour = "grey20") +
#   scale_fill_manual(
#     name = "Region:",
#     values = c(MED = "#1b9e77", NEA = "#d95f02", US = "#7570b3")
#   ) +
#   scale_x_continuous(breaks = seq(xlim[1], xlim[2], 1), limits = xlim) +
#   labs(x = expression(log(ABI[ref])), y = "Taxonomic Order") +
#   theme(
#     text = element_text(size = 14),
#     panel.grid.minor = element_blank(),
#     strip.background = element_blank(),
#     legend.position = "top"
#   )
# 
# t_order_box
# 
# setwd("~/ABI/Draft paper/Third step/Figure")
# ggsave(t_order_box, filename = "SM_Fig_S4.png", width = 14, height = 12)
# 
# # ------------------------------------------------------------
# # Option S4b — areas merged
# # ------------------------------------------------------------
# t_order_box2 <- ggplot(plot_dat, aes(x = log(final), y = Order)) +
#   theme_bw() +
#   geom_rect(xmin = xlim[1], xmax = 0, ymin = -Inf, ymax = Inf,
#             inherit.aes = FALSE, fill = "#FF9999", alpha = 0.006) +
#   geom_rect(xmin = 0, xmax = xlim[2], ymin = -Inf, ymax = Inf,
#             inherit.aes = FALSE, fill = "#99FF99", alpha = 0.006) +
#   geom_vline(xintercept = 0, colour = "grey30", linetype = "dashed") +
#   geom_boxplot(width = 0.7, outlier.alpha = 0.4, alpha = 0.7, colour = "grey20") +
#   scale_x_continuous(breaks = seq(xlim[1], xlim[2], 1), limits = xlim) +
#   labs(x = expression(log(ABI[ref])), y = "Taxonomic Order") +
#   theme(
#     text = element_text(size = 14),
#     panel.grid.minor = element_blank(),
#     strip.background = element_blank(),
#     legend.position = "top"
#   )
# 
# t_order_box2
# 
# setwd("~/ABI/Draft paper/Third step/Figure")
# ggsave(t_order_box2, filename = "SM_Fig_S4b.png", width = 14, height = 12)

# ------------------------------------------------------------
# SM Figure S5 — ROC curves with 95% bootstrap ribbons
# ------------------------------------------------------------

set.seed(123)

cols <- region_cols
fpr_grid <- seq(0, 1, length.out = 201)

interp_tpr <- function(roc_obj, fpr_grid) {
  fpr <- 1 - roc_obj$specificities
  tpr <- roc_obj$sensitivities
  
  ord <- order(fpr, tpr)
  fpr <- fpr[ord]; tpr <- tpr[ord]
  
  df <- data.frame(fpr = fpr, tpr = tpr) %>%
    group_by(fpr) %>%
    summarise(tpr = max(tpr), .groups = "drop") %>%
    arrange(fpr)
  
  if (min(df$fpr) > 0) df <- bind_rows(data.frame(fpr = 0, tpr = 0), df)
  if (max(df$fpr) < 1) df <- bind_rows(df, data.frame(fpr = 1, tpr = 1))
  
  approx(
    x = df$fpr, y = df$tpr, xout = fpr_grid,
    method = "linear", ties = "ordered", rule = 2
  )$y
}

bootstrap_roc_band <- function(df, n_boot = 10000, fpr_grid = fpr_grid) {
  
  roc_base <- pROC::roc(
    df$true_status, df$ABI_ref,
    levels = c("Unhealthy", "Healthy"),
    direction = "auto",
    quiet = TRUE
  )
  
  base_dir <- roc_base$direction
  auc_base <- as.numeric(pROC::auc(roc_base))
  
  tpr_mat  <- matrix(NA_real_, nrow = n_boot, ncol = length(fpr_grid))
  auc_boot <- rep(NA_real_, n_boot)
  
  for (b in seq_len(n_boot)) {
    boot <- df[sample.int(nrow(df), replace = TRUE), ]
    
    # skip if one class only
    if (nlevels(droplevels(boot$true_status)) < 2) next
    
    roc_b <- pROC::roc(
      boot$true_status, boot$ABI_ref,
      levels = c("Unhealthy", "Healthy"),
      direction = base_dir,
      quiet = TRUE
    )
    
    auc_boot[b] <- as.numeric(pROC::auc(roc_b))
    tpr_mat[b, ] <- interp_tpr(roc_b, fpr_grid)
  }
  
  tpr_mean <- apply(tpr_mat, 2, mean, na.rm = TRUE)
  tpr_lo   <- apply(tpr_mat, 2, quantile, probs = 0.025, na.rm = TRUE)
  tpr_hi   <- apply(tpr_mat, 2, quantile, probs = 0.975, na.rm = TRUE)
  
  auc_ci <- quantile(auc_boot, c(0.025, 0.975), na.rm = TRUE)
  
  list(
    auc = auc_base,
    auc_ci = auc_ci,
    band = data.frame(fpr = fpr_grid, tpr = tpr_mean, lo = tpr_lo, hi = tpr_hi)
  )
}

res <- roc_dat %>%
  group_by(area) %>%
  group_split() %>%
  setNames(levels(roc_dat$area)) %>%
  lapply(bootstrap_roc_band, n_boot = 10000, fpr_grid = fpr_grid)

band_df <- bind_rows(lapply(names(res), function(a) {
  out <- res[[a]]
  out$band %>%
    mutate(
      area = a,
      title = sprintf("%s (AUC = %.2f [%.2f–%.2f])", a, out$auc, out$auc_ci[1], out$auc_ci[2])
    )
}))

p_s5 <- ggplot(band_df, aes(x = fpr, y = tpr)) +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = area), alpha = 0.25, colour = NA) +
  geom_line(aes(color = area), linewidth = 1.1) +
  geom_abline(linetype = "dashed") +
  facet_wrap(~ title, nrow = 1) +
  coord_equal() +
  scale_color_manual(values = cols, guide = "none") +
  scale_fill_manual(values = cols, guide = "none") +
  labs(
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  ) +
  theme_bw()

print(p_s5)
ggsave("ROC_curves_bootstrap_S5.png", p_s5, width = 10.5, height = 3.6, dpi = 300)

table(roc_dat$true_status, roc_dat$area)
##################################

############################################################
# TABLE 1 only
# TP, TN, FP, FN, Sensitivity, Specificity, TSS, BSS
############################################################

setwd("~/ABI/Draft paper/Third step/Data")


############################################################
# LOAD DATASETS
############################################################

(dat1 <- read.csv(
  "US_ABI_data_new.csv",
  sep = ",",
  stringsAsFactors = FALSE
))

(dat2 <- read.csv(
  "NEA_ABI_data_fig_3.csv",
  sep = ",",
  stringsAsFactors = FALSE
))

(dat3 <- read.csv(
  "STECF_ABI_data.csv",
  sep = ";",
  stringsAsFactors = FALSE
))

(dat4 <- read.csv(
  "ssb_ref_data.csv",
  sep = ",",
  stringsAsFactors = FALSE
))


############################################################
# US STOCK DISPLAY NAMES
############################################################

us_short_names <- c(
  
  "Black rockfish - Oregon stock" =
    "Bla.rockfish.Ore",
  
  "Black rockfish - Washington stock" =
    "Bla.rockfish.Wash.",
  
  "Black-and-yellow and Gopher rockfish complex" =
    "Bla.yell.Goph.rockfish",
  
  "Blackgill rockfish" =
    "Bla.gill.rockfish",
  
  "Blue deacon rockfish" =
    "Blue.deac.rockfish",
  
  "Cabezon - Northern California" =
    "Cabe.N.Calif.",
  
  "Cabezon - Southern California" =
    "Cabe.S.Calif.",
  
  "California scorpionfish" =
    "Calif.scorpionfish",
  
  "Canary rockfish" =
    "Canary.rockfish",
  
  "Copper rockfish - Northern California" =
    "Copp.rockfish.N.Calif.",
  
  "Copper rockfish - Southern California" =
    "Copp.rockfish.S.Calif.",
  
  "Cowcod - Southern California" =
    "Cowcod.S.Calif.",
  
  "Longnose skate" =
    "Longn.skate",
  
  "Petrale Sole" =
    "Petra.Sole",
  
  "Quillback rockfish - California" =
    "Quill.rockfish.Calif.",
  
  "Quillback rockfish - Washington" =
    "Quill.rockfish.Wash.",
  
  "Squarespot rockfish" =
    "Squares.rockfish",
  
  "Vermilion rockfish and Sunset rockfish Complex - Northern California" =
    "Verm.Sun.rockfish.N.Calif."
)


############################################################
# NEA DISPLAY NAMES
# Same nomenclature used in Figures 2, 4 and S3
############################################################

nea_display_names <- c(
  
  "ank.27.78abd" =
    "Black-bellied.angler.Celtic-Biscay",
  
  "bss.27.4bc7ad-h" =
    "Seabass.N.Sea-Celtic",
  
  "cod.21.1" =
    "Cod.W.Greenland",
  
  "cod.27.1-2" =
    "Cod.NE.Arctic",
  
  "cod.27.5a" =
    "Cod.Iceland",
  
  "cod.27.7e-k" =
    "Cod.Celtic.Seas",
  
  "had.27.1-2" =
    "Haddock.NE.Arctic",
  
  "had.27.5a" =
    "Haddock.Iceland",
  
  "hke.27.3a46-8abd" =
    "Hake.N.Stock",
  
  "hke.27.8c9a" =
    "Hake.S.Stock",
  
  "ldb.27.8c9a" =
    "Four-spot.megrim.Iberian",
  
  "lin.27.5a" =
    "Ling.Iceland",
  
  "lin.27.5b" =
    "Ling.Faroes",
  
  "meg.27.7b-k8abd" =
    "Megrim.Celtic-Biscay",
  
  "ple.27.21-23" =
    "Plaice.Kattegat-Belt",
  
  "ple.27.420" =
    "Plaice.N.Sea-Skagerrak",
  
  "pok.27.1-2" =
    "Saithe.NE.Arctic",
  
  "pok.27.3a46" =
    "Saithe.N.Sea-Scotland",
  
  "pok.27.5b" =
    "Saithe.Faroes",
  
  "reg.27.561214" =
    "Golden.redfish.Iceland-Faroes",
  
  "sol.27.4" =
    "Sole.N.Sea",
  
  "sol.27.7a" =
    "Sole.Irish.Sea",
  
  "sol.27.7fg" =
    "Sole.Celtic.Sea",
  
  "tur.27.4" =
    "Turbot.N.Sea",
  
  "wit.27.3a47d" =
    "Witch.N.Sea"
)


############################################################
# MED DISPLAY NAMES
# Same nomenclature used in Figures 2, 4 and S1
############################################################

med_display_names <- c(
  
  "ara.18.19.20" =
    "Blue-red.shrimp.18-20.C.Med",
  
  "ara.6.7" =
    "Blue-red.shrimp.6-7.W.Med",
  
  "ars.18.19.20" =
    "Giant.red.shrimp.18-20.C.Med",
  
  "ars.9.10.11" =
    "Giant.red.shrimp.9-11.W.Med",
  
  "dps.1" =
    "Deep-water.rose.shrimp.1.W.Med",
  
  "dps.8.9.10.11" =
    "Deep-water.rose.shrimp.8-11.W.Med",
  
  "hke.1.5.6.7" =
    "Hake.1.5-7.W.Med",
  
  "hke.19" =
    "Hake.19.C.Med",
  
  "hke.20" =
    "Hake.20.C.Med",
  
  "hke.22" =
    "Hake.22.E.Med",
  
  "hke.8.9.10.11" =
    "Hake.8-11.W.Med",
  
  "mur.15.16" =
    "Striped.red.mullet.15-16.C.Med",
  
  "mut.1" =
    "Red.mullet.1.W.Med",
  
  "mut.19" =
    "Red.mullet.19.C.Med",
  
  "mut.22" =
    "Red.mullet.22.E.Med",
  
  "mut.6" =
    "Red.mullet.6.W.Med",
  
  "mut.7" =
    "Red.mullet.7.W.Med",
  
  "mut.9" =
    "Red.mullet.9.W.Med",
  
  "nep.15.16" =
    "Norway.lobster.15-16.C.Med",
  
  "nep.6" =
    "Norway.lobster.6.W.Med",
  
  "nep.9" =
    "Norway.lobster.9.W.Med",
  
  "sol.17" =
    "Common.sole.17.C.Med"
)


############################################################
# HELPER TO STANDARDIZE STOCK NAMES
############################################################

std_stock <- function(x) {
  
  x %>%
    stringr::str_trim() %>%
    gsub("_", ".", ., fixed = TRUE)
}


############################################################
# PREPARE US DATA
############################################################
#
# stock      = original stock name
# stock_join = name used in reference-data joins
# stock_std  = standardized computational ID
# stock_label = name ONLY for display
############################################################

us <- dat1 %>%
  
  transmute(
    
    area   = "US",
    stock  = stock,
    year   = year,
    SSB    = as.numeric(SSB),
    F      = as.numeric(F),
    ABImsy = as.numeric(ABImsy)
  ) %>%
  
  mutate(
    
    stock_join = dplyr::recode(
      stock,
      !!!us_short_names,
      .default = stock
    ),
    
    stock_std = std_stock(stock_join),
    
    # For US the standardized short name is also
    # the graphical/display label
    stock_label = stock_std
  )


############################################################
# PREPARE NEA DATA
############################################################

nea <- dat2 %>%
  
  transmute(
    
    area   = "NEA",
    stock  = stock,
    year   = year,
    SSB    = as.numeric(ssb),
    F      = as.numeric(f),
    ABImsy = as.numeric(ABImsy)
  ) %>%
  
  mutate(
    
    # Keep original ID for computations/joins
    stock_join = stock,
    
    stock_std = std_stock(stock_join),
    
    # New label ONLY for display
    stock_label = dplyr::recode(
      stock_std,
      !!!nea_display_names,
      .default = stock_std
    )
  )


############################################################
# PREPARE MED DATA
############################################################

med <- dat3 %>%
  
  transmute(
    
    area   = "MED",
    stock  = stock,
    year   = year,
    SSB    = as.numeric(SSB),
    F      = as.numeric(F),
    ABImsy = as.numeric(ABImsy)
  ) %>%
  
  mutate(
    
    # Example:
    # HKE_8_9_10_11 -> hke_8_9_10_11
    stock_join = tolower(stock),
    
    # hke_8_9_10_11 -> hke.8.9.10.11
    stock_std = std_stock(stock_join),
    
    # New label ONLY for display
    stock_label = dplyr::recode(
      stock_std,
      !!!med_display_names,
      .default = stock_std
    )
  )


############################################################
# CHECK STOCK NAMES
############################################################

cat("\n--- US ---\n")
print(
  us %>%
    distinct(
      stock,
      stock_std,
      stock_label
    ) %>%
    arrange(stock_std)
)


cat("\n--- NEA ---\n")
print(
  nea %>%
    distinct(
      stock,
      stock_std,
      stock_label
    ) %>%
    arrange(stock_std)
)


cat("\n--- MED ---\n")
print(
  med %>%
    distinct(
      stock,
      stock_std,
      stock_label
    ) %>%
    arrange(stock_std)
)



############################################################
# PREPARE REFERENCE POINTS
############################################################

(ref <- dat4 %>%
   transmute(
     stock_ref = stock,
     area_ref  = area,
     stock_std = std_stock(stock),
     SSBref    = as.numeric(SSBref),
     Fref      = as.numeric(Fref)
   ))


############################################################
# MERGE ALL AREAS
############################################################

(dat_all <- bind_rows(us, nea, med))


############################################################
# KEEP ONLY MOST RECENT YEAR PER STOCK
############################################################

(dat_latest <- dat_all %>%
   group_by(area, stock_std) %>%
   slice_max(
     order_by = year,
     n = 1,
     with_ties = FALSE
   ) %>%
   ungroup())


############################################################
# JOIN REFERENCE POINTS
############################################################

(dat_latest <- dat_latest %>%
   left_join(
     ref %>%
       select(
         stock_std,
         SSBref,
         Fref
       ),
     by = "stock_std"
   ))


############################################################
# CHECK MISSING REFERENCE POINTS
############################################################

(missing_refs <- dat_latest %>%
   filter(
     is.na(SSBref) |
       is.na(Fref)
   ) %>%
   select(
     area,
     stock,
     stock_label,
     stock_join,
     stock_std
   ) %>%
   distinct())


cat(
  "Numero missing refs =",
  nrow(missing_refs),
  "\n"
)


############################################################
# BUILD TABLE 1 DATA
############################################################

(roc_dat <- dat_latest %>%
   filter(
     !is.na(SSBref),
     !is.na(Fref),
     !is.na(SSB),
     !is.na(F),
     !is.na(ABImsy)
   ) %>%
   mutate(
     
     SSB_ratio = SSB / SSBref,
     
     F_ratio = F / Fref,
     
     true_status = ifelse(
       SSB_ratio >= 1 &
         F_ratio <= 1,
       "Healthy",
       "Unhealthy"
     ),
     
     pred_status = ifelse(
       ABImsy >= 1,
       "Healthy",
       "Unhealthy"
     )
   ))


roc_dat$true_status <- factor(
  roc_dat$true_status,
  levels = c(
    "Unhealthy",
    "Healthy"
  )
)

roc_dat$pred_status <- factor(
  roc_dat$pred_status,
  levels = c(
    "Unhealthy",
    "Healthy"
  )
)


############################################################
# CHECK FINAL COUNTS
############################################################

cat(
  "Numero finale stock =",
  nrow(roc_dat),
  "\n"
)


roc_dat %>%
  count(area) %>%
  print()


table(
  roc_dat$true_status,
  roc_dat$area
)


############################################################
# FUNCTION TO CALCULATE TABLE 1
############################################################

calc_table1 <- function(df) {
  
  TP <- sum(
    df$pred_status == "Healthy" &
      df$true_status == "Healthy",
    na.rm = TRUE
  )
  
  TN <- sum(
    df$pred_status == "Unhealthy" &
      df$true_status == "Unhealthy",
    na.rm = TRUE
  )
  
  FP <- sum(
    df$pred_status == "Healthy" &
      df$true_status == "Unhealthy",
    na.rm = TRUE
  )
  
  FN <- sum(
    df$pred_status == "Unhealthy" &
      df$true_status == "Healthy",
    na.rm = TRUE
  )
  
  
  sensitivity <- ifelse(
    (TP + FN) > 0,
    TP / (TP + FN),
    NA_real_
  )
  
  
  specificity <- ifelse(
    (TN + FP) > 0,
    TN / (TN + FP),
    NA_real_
  )
  
  
  TSS <- sensitivity + specificity - 1
  
  BSS <- (sensitivity + specificity) / 2
  
  
  tibble(
    
    TP = TP,
    TN = TN,
    FP = FP,
    FN = FN,
    
    Sensitivity = round(
      sensitivity,
      3
    ),
    
    Specificity = round(
      specificity,
      3
    ),
    
    TSS = round(
      TSS,
      3
    ),
    
    BSS = round(
      BSS,
      3
    )
  )
}


############################################################
# TABLE 1 BY AREA
############################################################

table1 <- roc_dat %>%
  
  group_by(area) %>%
  
  group_modify(
    ~ calc_table1(.x)
  ) %>%
  
  ungroup()


print(table1)


############################################################
# CHECK TOTAL TP+TN+FP+FN
############################################################

table1_total_n <- table1 %>%
  
  summarise(
    total = sum(
      TP +
        TN +
        FP +
        FN
    )
  ) %>%
  
  pull(total)


cat(
  "Somma totale TP+TN+FP+FN =",
  table1_total_n,
  "\n"
)


############################################################
# SAVE
############################################################

setwd(
  "~/ABI/Draft paper/Third step/Output"
)


write.csv(
  table1,
  "Table1_TSS_BSS_updated.csv",
  row.names = FALSE
)


############################################################
# PRINT LATEX TABLE
############################################################

(latex_code <- knitr::kable(
  
  table1,
  
  format = "latex",
  
  booktabs = TRUE,
  
  align = "lcccccccc",
  
  caption =
    "True Skill Score (TSS) and Balanced Skill Score (BSS) for ABI$_{ref}$ at the theoretical threshold of 1 across regions."
))


############################################################
# RAPID DIAGNOSTIC CHECK AFTER TABLE 1
# Are ABI false positives associated with recruitment failure?
#
# Logic:
# FP = ABI says Healthy, but conventional status says Unhealthy
#
# Then we check whether these FP stocks also show recent
# low recruitment.
#
# A stock could have high ABI because it still contains
# relatively old individuals, but at the same time experience
# recent recruitment failure. If present, these cases should
# not automatically be interpreted as fully healthy/rebuilt.
############################################################


############################################################
# 1. Helper functions
############################################################

as_num_safely <- function(x) {
  
  as.numeric(
    gsub(
      ",",
      ".",
      as.character(x)
    )
  )
}


get_num_col <- function(
    df,
    candidates,
    dataset_name = "dataset"
) {
  
  nm <- intersect(
    candidates,
    names(df)
  )
  
  
  if (length(nm) == 0) {
    
    warning(
      paste0(
        "No recruitment column found in ",
        dataset_name,
        ". Returning NA. Available columns are: ",
        paste(
          names(df),
          collapse = ", "
        )
      )
    )
    
    
    return(
      rep(
        NA_real_,
        nrow(df)
      )
    )
  }
  
  
  as_num_safely(
    df[[nm[1]]]
  )
}


rec_cols <- c(
  "R",
  "Rec",
  "rec",
  "Recruitment",
  "recruitment",
  "Recruit",
  "recruit",
  "recruits",
  "Recruits",
  "recruitment_mean",
  "Recruitment_mean"
)


############################################################
# 2. Reconstruct recruitment dataset from original inputs
#
# This does NOT change previous dat_all / roc_dat / table1.
#
# stock_std is used for matching.
# stock_label is used ONLY for display.
############################################################


############################################################
# US recruitment
############################################################

rec_us <- tibble(
  
  area = "US",
  
  stock = dat1$stock,
  
  year = as.integer(
    dat1$year
  ),
  
  R = get_num_col(
    dat1,
    rec_cols,
    "US_ABI_data_new.csv"
  )
  
) %>%
  
  mutate(
    
    stock_join = dplyr::recode(
      stock,
      !!!us_short_names,
      .default = stock
    ),
    
    stock_std = std_stock(
      stock_join
    ),
    
    stock_label = stock_std
  )


############################################################
# NEA recruitment
############################################################

rec_nea <- tibble(
  
  area = "NEA",
  
  stock = dat2$stock,
  
  year = as.integer(
    dat2$year
  ),
  
  R = get_num_col(
    dat2,
    rec_cols,
    "NEA_ABI_data_fig_3.csv"
  )
  
) %>%
  
  mutate(
    
    stock_join = stock,
    
    stock_std = std_stock(
      stock_join
    ),
    
    stock_label = dplyr::recode(
      stock_std,
      !!!nea_display_names,
      .default = stock_std
    )
  )


############################################################
# MED recruitment
############################################################

rec_med <- tibble(
  
  area = "MED",
  
  stock = dat3$stock,
  
  year = as.integer(
    dat3$year
  ),
  
  R = get_num_col(
    dat3,
    rec_cols,
    "STECF_ABI_data.csv"
  )
  
) %>%
  
  mutate(
    
    stock_join = tolower(
      stock
    ),
    
    stock_std = std_stock(
      stock_join
    ),
    
    stock_label = dplyr::recode(
      stock_std,
      !!!med_display_names,
      .default = stock_std
    )
  )


############################################################
# Combine recruitment datasets
############################################################

rec_all <- bind_rows(
  rec_us,
  rec_nea,
  rec_med
)


############################################################
# 3. Add TP / TN / FP / FN classification to roc_dat
############################################################

roc_dat_class <- roc_dat %>%
  
  mutate(
    
    class = case_when(
      
      pred_status == "Healthy" &
        true_status == "Healthy" ~
        "TP",
      
      pred_status == "Unhealthy" &
        true_status == "Unhealthy" ~
        "TN",
      
      pred_status == "Healthy" &
        true_status == "Unhealthy" ~
        "FP",
      
      pred_status == "Unhealthy" &
        true_status == "Healthy" ~
        "FN",
      
      TRUE ~
        NA_character_
    ),
    
    
    unhealthy_reason = case_when(
      
      SSB_ratio < 1 &
        F_ratio > 1 ~
        "SSB below ref and F above ref",
      
      SSB_ratio < 1 &
        F_ratio <= 1 ~
        "SSB below ref only",
      
      SSB_ratio >= 1 &
        F_ratio > 1 ~
        "F above ref only",
      
      SSB_ratio >= 1 &
        F_ratio <= 1 ~
        "Healthy",
      
      TRUE ~
        NA_character_
    )
  )


############################################################
# 4. Extract only ABI false positives
############################################################

abi_fp <- roc_dat_class %>%
  
  filter(
    class == "FP"
  ) %>%
  
  select(
    
    area,
    
    stock,
    
    stock_label,
    
    stock_std,
    
    year,
    
    ABImsy,
    
    SSB,
    
    F,
    
    SSBref,
    
    Fref,
    
    SSB_ratio,
    
    F_ratio,
    
    true_status,
    
    pred_status,
    
    class,
    
    unhealthy_reason
  ) %>%
  
  rename(
    
    terminal_year = year,
    
    ABIref_terminal = ABImsy
  )


cat(
  "\nNumber of ABI false positives =",
  nrow(abi_fp),
  "\n"
)


print(
  
  abi_fp %>%
    
    select(
      
      area,
      
      stock_label,
      
      terminal_year,
      
      ABIref_terminal,
      
      SSB_ratio,
      
      F_ratio,
      
      unhealthy_reason
    ),
  
  n = Inf
)


############################################################
# 5. Recruitment diagnostic only for ABI false positives
############################################################

# IMPORTANT:
#
# If zero recruitment values are real estimates,
# set this to FALSE.
#
# If zero values are fillers / missing values,
# keep TRUE.

drop_zero_R <- TRUE


rec_fp <- rec_all %>%
  
  semi_join(
    
    abi_fp %>%
      select(
        area,
        stock_std
      ),
    
    by = c(
      "area",
      "stock_std"
    )
  ) %>%
  
  filter(
    !is.na(R),
    !is.na(year)
  )


if (drop_zero_R) {
  
  rec_fp <- rec_fp %>%
    
    filter(
      R > 0
    )
}




############################################################
# 6. RECRUITMENT DIAGNOSTIC FOR BOTH ABI FALSE POSITIVES
#    AND FALSE NEGATIVES
#
# Biological logic:
#
# FALSE POSITIVE (FP):
# ABI >= 1 but conventional status = Unhealthy
# Potential mechanism:
# unusually LOW recent recruitment reduces young fish,
# increasing the relative proportion of older fish
# and potentially inflating ABI.
#
# Diagnostic:
# recent recruitment < historical P20
#
#
# FALSE NEGATIVE (FN):
# ABI < 1 but conventional status = Healthy
# Potential mechanism:
# unusually HIGH recent recruitment increases young fish,
# reducing the relative proportion of older fish
# and potentially depressing ABI.
#
# Diagnostic:
# recent recruitment > historical P80
#
#
# Both diagnostics are calculated using:
#   - 3-year geometric mean recruitment
#   - 5-year geometric mean recruitment
#
# Historical rolling windows overlapping with the recent
# window are excluded from the reference distribution.
############################################################


############################################################
# 6.1 Extract ABI FALSE NEGATIVES
############################################################

abi_fn <- roc_dat_class %>%
  filter(class == "FN") %>%
  select(
    area,
    stock,
    stock_label,
    stock_std,
    year,
    ABImsy,
    SSB,
    F,
    SSBref,
    Fref,
    SSB_ratio,
    F_ratio,
    true_status,
    pred_status,
    class,
    unhealthy_reason
  ) %>%
  rename(
    terminal_year = year,
    ABIref_terminal = ABImsy
  )


cat(
  "\nNumber of ABI false negatives =",
  nrow(abi_fn),
  "\n"
)


print(
  abi_fn %>%
    select(
      area,
      stock_label,
      terminal_year,
      ABIref_terminal,
      SSB_ratio,
      F_ratio
    ),
  n = Inf
)


############################################################
# 6.2 Combine FP and FN
############################################################

abi_errors <- bind_rows(
  
  abi_fp %>%
    mutate(
      error_type = "False positive"
    ),
  
  abi_fn %>%
    mutate(
      error_type = "False negative"
    )
)


abi_errors$error_type <- factor(
  abi_errors$error_type,
  levels = c(
    "False positive",
    "False negative"
  )
)


cat(
  "\nTotal ABI classification errors included in recruitment diagnostic =",
  nrow(abi_errors),
  "\n"
)


print(
  abi_errors %>%
    count(error_type)
)


############################################################
# 6.3 Recruitment data for ALL FP + FN stocks
############################################################

rec_error <- rec_all %>%
  
  semi_join(
    
    abi_errors %>%
      select(
        area,
        stock_std
      ),
    
    by = c(
      "area",
      "stock_std"
    )
    
  ) %>%
  
  filter(
    !is.na(R),
    !is.na(year)
  )


# Same rule already defined above:
#
# drop_zero_R = TRUE if zero recruitment values are fillers
# rather than real estimates

if (drop_zero_R) {
  
  rec_error <- rec_error %>%
    filter(
      R > 0
    )
}


############################################################
# 6.4 Helper: geometric mean
############################################################

geom_mean <- function(x) {
  
  x <- x[
    is.finite(x) &
      !is.na(x) &
      x > 0
  ]
  
  
  if (length(x) == 0) {
    
    return(
      NA_real_
    )
  }
  
  
  exp(
    mean(
      log(x)
    )
  )
}


############################################################
# 6.5 Helper: rolling geometric means
#
# Only consecutive-year windows are retained.
############################################################

rolling_geom_mean <- function(
    year,
    R,
    k
) {
  
  ord <- order(year)
  
  year <- year[ord]
  R    <- R[ord]
  
  
  keep <-
    !is.na(year) &
    !is.na(R) &
    is.finite(R) &
    R > 0
  
  
  year <- year[keep]
  R    <- R[keep]
  
  
  if (length(R) < k) {
    
    return(
      tibble(
        start_year = integer(0),
        end_year   = integer(0),
        R_roll     = numeric(0)
      )
    )
  }
  
  
  out <- vector(
    "list",
    length(R) - k + 1
  )
  
  
  counter <- 1
  
  
  for (i in k:length(R)) {
    
    idx <- (i - k + 1):i
    
    years_window <- year[idx]
    R_window     <- R[idx]
    
    
    # Require consecutive years
    
    if (
      all(
        diff(years_window) == 1
      )
    ) {
      
      out[[counter]] <- tibble(
        
        start_year = min(
          years_window
        ),
        
        end_year = max(
          years_window
        ),
        
        R_roll = geom_mean(
          R_window
        )
      )
      
      
      counter <- counter + 1
    }
  }
  
  
  bind_rows(
    out
  )
}


############################################################
# 6.6 Helper: recruitment diagnostic for ONE window length
#
# Returns BOTH lower-tail and upper-tail diagnostics:
#
# Low R  = recent R < historical P20
# High R = recent R > historical P80
############################################################

calc_window_diagnostic <- function(
    year,
    R,
    k,
    min_hist_windows = 5
) {
  
  
  roll <- rolling_geom_mean(
    year = year,
    R    = R,
    k    = k
  )
  
  
  ##########################################################
  # No valid rolling window
  ##########################################################
  
  if (nrow(roll) == 0) {
    
    return(
      tibble(
        
        window_years = k,
        
        recent_start_year = NA_integer_,
        recent_end_year   = NA_integer_,
        
        R_recent = NA_real_,
        
        n_hist_windows = 0L,
        
        R_hist_median = NA_real_,
        R_hist_p20    = NA_real_,
        R_hist_p80    = NA_real_,
        
        R_ratio_median    = NA_real_,
        R_hist_percentile = NA_real_,
        
        low_recent_R  = NA,
        high_recent_R = NA
      )
    )
  }
  
  
  ##########################################################
  # Most recent valid k-year window
  ##########################################################
  
  recent <- roll %>%
    slice_max(
      order_by = end_year,
      n = 1,
      with_ties = FALSE
    )
  
  
  recent_start <- recent$start_year
  recent_end   <- recent$end_year
  recent_R     <- recent$R_roll
  
  
  ##########################################################
  # Historical baseline
  #
  # Exclude ALL historical windows overlapping with
  # the recent window.
  ##########################################################
  
  hist <- roll %>%
    filter(
      end_year < recent_start
    )
  
  
  ##########################################################
  # Require enough historical windows
  ##########################################################
  
  if (nrow(hist) < min_hist_windows) {
    
    return(
      tibble(
        
        window_years = k,
        
        recent_start_year = recent_start,
        recent_end_year   = recent_end,
        
        R_recent = recent_R,
        
        n_hist_windows = nrow(hist),
        
        R_hist_median = NA_real_,
        R_hist_p20    = NA_real_,
        R_hist_p80    = NA_real_,
        
        R_ratio_median    = NA_real_,
        R_hist_percentile = NA_real_,
        
        low_recent_R  = NA,
        high_recent_R = NA
      )
    )
  }
  
  
  ##########################################################
  # Historical statistics
  ##########################################################
  
  hist_median <- median(
    hist$R_roll,
    na.rm = TRUE
  )
  
  
  hist_p20 <- quantile(
    hist$R_roll,
    probs = 0.20,
    na.rm = TRUE,
    names = FALSE
  )
  
  
  hist_p80 <- quantile(
    hist$R_roll,
    probs = 0.80,
    na.rm = TRUE,
    names = FALSE
  )
  
  
  ##########################################################
  # Effect size:
  # recent R relative to historical median
  ##########################################################
  
  ratio_median <- ifelse(
    
    hist_median > 0,
    
    recent_R / hist_median,
    
    NA_real_
  )
  
  
  ##########################################################
  # Empirical percentile of recent recruitment
  #
  # 0.05 = around lower 5% of historical distribution
  # 0.95 = around upper 5% of historical distribution
  ##########################################################
  
  hist_percentile <- mean(
    hist$R_roll <= recent_R,
    na.rm = TRUE
  )
  
  
  ##########################################################
  # LOWER-TAIL diagnostic for FP
  ##########################################################
  
  low_recent <- recent_R < hist_p20
  
  
  ##########################################################
  # UPPER-TAIL diagnostic for FN
  ##########################################################
  
  high_recent <- recent_R > hist_p80
  
  
  ##########################################################
  # Return
  ##########################################################
  
  tibble(
    
    window_years = k,
    
    recent_start_year = recent_start,
    recent_end_year   = recent_end,
    
    R_recent = recent_R,
    
    n_hist_windows = nrow(hist),
    
    R_hist_median = hist_median,
    R_hist_p20    = hist_p20,
    R_hist_p80    = hist_p80,
    
    R_ratio_median    = ratio_median,
    R_hist_percentile = hist_percentile,
    
    low_recent_R  = low_recent,
    high_recent_R = high_recent
  )
}


############################################################
# 6.7 Calculate BOTH 3-year and 5-year diagnostics
#     for every FP and FN
############################################################

rec_diag_errors <- rec_error %>%
  
  left_join(
    
    abi_errors %>%
      select(
        area,
        stock_std,
        terminal_year
      ),
    
    by = c(
      "area",
      "stock_std"
    )
  ) %>%
  
  
  # No recruitment information after ABI terminal year
  
  filter(
    year <= terminal_year
  ) %>%
  
  
  arrange(
    area,
    stock_std,
    year
  ) %>%
  
  
  # IMPORTANT:
  # stock_std remains the computational/matching ID.
  # stock_label is added to grouping so that the harmonised
  # label is retained in the final diagnostic output.
  
  group_by(
    area,
    stock_std,
    stock_label
  ) %>%
  
  
  group_modify(
    
    ~ {
      
      
      yy <- .x$year
      rr <- .x$R
      
      
      basic <- tibble(
        
        R_first_year = min(
          yy,
          na.rm = TRUE
        ),
        
        R_last_year = max(
          yy,
          na.rm = TRUE
        ),
        
        n_R_years = length(rr),
        
        R_terminal =
          rr[
            which.max(yy)
          ]
      )
      
      
      ######################################################
      # 3-year diagnostic
      ######################################################
      
      diag3 <- calc_window_diagnostic(
        year = yy,
        R = rr,
        k = 3,
        min_hist_windows = 5
      )
      
      
      ######################################################
      # 5-year diagnostic
      ######################################################
      
      diag5 <- calc_window_diagnostic(
        year = yy,
        R = rr,
        k = 5,
        min_hist_windows = 5
      )
      
      
      ######################################################
      # Put both diagnostics into one row
      ######################################################
      
      tibble(
        
        R_first_year =
          basic$R_first_year,
        
        R_last_year =
          basic$R_last_year,
        
        n_R_years =
          basic$n_R_years,
        
        R_terminal =
          basic$R_terminal,
        
        
        ####################################################
        # 3-YEAR
        ####################################################
        
        R3_start_year =
          diag3$recent_start_year,
        
        R3_end_year =
          diag3$recent_end_year,
        
        R_recent_3yr =
          diag3$R_recent,
        
        n_hist_windows_3yr =
          diag3$n_hist_windows,
        
        R_hist_median_3yr =
          diag3$R_hist_median,
        
        R_hist_p20_3yr =
          diag3$R_hist_p20,
        
        R_hist_p80_3yr =
          diag3$R_hist_p80,
        
        R_ratio_median_3yr =
          diag3$R_ratio_median,
        
        R_percentile_3yr =
          diag3$R_hist_percentile,
        
        low_recent_R_3yr =
          diag3$low_recent_R,
        
        high_recent_R_3yr =
          diag3$high_recent_R,
        
        
        ####################################################
        # 5-YEAR
        ####################################################
        
        R5_start_year =
          diag5$recent_start_year,
        
        R5_end_year =
          diag5$recent_end_year,
        
        R_recent_5yr =
          diag5$R_recent,
        
        n_hist_windows_5yr =
          diag5$n_hist_windows,
        
        R_hist_median_5yr =
          diag5$R_hist_median,
        
        R_hist_p20_5yr =
          diag5$R_hist_p20,
        
        R_hist_p80_5yr =
          diag5$R_hist_p80,
        
        R_ratio_median_5yr =
          diag5$R_ratio_median,
        
        R_percentile_5yr =
          diag5$R_hist_percentile,
        
        low_recent_R_5yr =
          diag5$low_recent_R,
        
        high_recent_R_5yr =
          diag5$high_recent_R
      )
    }
  ) %>%
  
  ungroup()



















############################################################
# 7. JOIN RECRUITMENT DIAGNOSTIC TO FP + FN
############################################################

abi_error_recruitment_check <- abi_errors %>%
  
  left_join(
    
    # stock_label is already present in abi_errors.
    # Remove it here from rec_diag_errors to avoid
    # stock_label.x / stock_label.y after the join.
    
    rec_diag_errors %>%
      select(
        -stock_label
      ),
    
    by = c(
      "area",
      "stock_std"
    )
  ) %>%
  
  mutate(
    
    ########################################################
    # Does recruitment show the expected anomaly?
    #
    # FP -> LOW recruitment
    # FN -> HIGH recruitment
    ########################################################
    
    recruitment_signal_3yr = case_when(
      
      error_type == "False positive" &
        low_recent_R_3yr == TRUE ~
        "Low",
      
      error_type == "False negative" &
        high_recent_R_3yr == TRUE ~
        "High",
      
      !is.na(R_percentile_3yr) ~
        "No",
      
      TRUE ~
        NA_character_
    ),
    
    
    recruitment_signal_5yr = case_when(
      
      error_type == "False positive" &
        low_recent_R_5yr == TRUE ~
        "Low",
      
      error_type == "False negative" &
        high_recent_R_5yr == TRUE ~
        "High",
      
      !is.na(R_percentile_5yr) ~
        "No",
      
      TRUE ~
        NA_character_
    ),
    
    
    ########################################################
    # TRUE/FALSE version of the diagnostic
    ########################################################
    
    diagnostic_match_3yr = case_when(
      
      error_type == "False positive" ~
        low_recent_R_3yr,
      
      error_type == "False negative" ~
        high_recent_R_3yr,
      
      TRUE ~
        NA
    ),
    
    
    diagnostic_match_5yr = case_when(
      
      error_type == "False positive" ~
        low_recent_R_5yr,
      
      error_type == "False negative" ~
        high_recent_R_5yr,
      
      TRUE ~
        NA
    ),
    
    
    ########################################################
    # Agreement between 3-year and 5-year windows
    ########################################################
    
    recruitment_pattern = case_when(
      
      diagnostic_match_3yr == TRUE &
        diagnostic_match_5yr == TRUE &
        error_type == "False positive" ~
        "Low R in both 3-yr and 5-yr windows",
      
      diagnostic_match_3yr == TRUE &
        diagnostic_match_5yr == TRUE &
        error_type == "False negative" ~
        "High R in both 3-yr and 5-yr windows",
      
      diagnostic_match_3yr == TRUE &
        diagnostic_match_5yr == FALSE ~
        "Recruitment anomaly in 3-yr window only",
      
      diagnostic_match_3yr == FALSE &
        diagnostic_match_5yr == TRUE ~
        "Recruitment anomaly in 5-yr window only",
      
      diagnostic_match_3yr == FALSE &
        diagnostic_match_5yr == FALSE ~
        "No corresponding recruitment anomaly",
      
      TRUE ~
        "Insufficient recruitment history"
    )
  ) %>%
  
  arrange(
    error_type,
    area,
    R_percentile_5yr
  )


############################################################
# 8. PRINT DETAILED FP + FN DIAGNOSTIC
############################################################

cat(
  "\nABI FP + FN recruitment diagnostic:\n"
)


print(
  
  abi_error_recruitment_check %>%
    
    select(
      
      error_type,
      
      area,
      
      # Harmonised stock name for display
      stock_label,
      
      terminal_year,
      
      ABIref_terminal,
      
      SSB_ratio,
      
      F_ratio,
      
      R_first_year,
      
      R_last_year,
      
      n_R_years,
      
      R_ratio_median_3yr,
      
      R_percentile_3yr,
      
      low_recent_R_3yr,
      
      high_recent_R_3yr,
      
      recruitment_signal_3yr,
      
      R_ratio_median_5yr,
      
      R_percentile_5yr,
      
      low_recent_R_5yr,
      
      high_recent_R_5yr,
      
      recruitment_signal_5yr,
      
      diagnostic_match_3yr,
      
      diagnostic_match_5yr,
      
      recruitment_pattern
    ),
  
  n = Inf
)


############################################################
# 9. SUMMARY BY ERROR TYPE AND AREA
############################################################

error_recruitment_summary <- abi_error_recruitment_check %>%
  
  group_by(
    error_type,
    area
  ) %>%
  
  summarise(
    
    n_cases = n(),
    
    n_matching_3yr =
      sum(
        diagnostic_match_3yr == TRUE,
        na.rm = TRUE
      ),
    
    n_matching_5yr =
      sum(
        diagnostic_match_5yr == TRUE,
        na.rm = TRUE
      ),
    
    n_matching_both =
      sum(
        diagnostic_match_3yr == TRUE &
          diagnostic_match_5yr == TRUE,
        na.rm = TRUE
      ),
    
    n_different_result =
      sum(
        !is.na(diagnostic_match_3yr) &
          !is.na(diagnostic_match_5yr) &
          diagnostic_match_3yr != diagnostic_match_5yr
      ),
    
    .groups = "drop"
  )


cat(
  "\nSummary by error type and area:\n"
)


print(
  error_recruitment_summary,
  n = Inf
)


############################################################
# 9B. OVERALL SUMMARY BY ERROR TYPE
############################################################

overall_error_summary <- abi_error_recruitment_check %>%
  
  group_by(
    error_type
  ) %>%
  
  summarise(
    
    n_cases = n(),
    
    n_matching_3yr =
      sum(
        diagnostic_match_3yr == TRUE,
        na.rm = TRUE
      ),
    
    n_matching_5yr =
      sum(
        diagnostic_match_5yr == TRUE,
        na.rm = TRUE
      ),
    
    n_matching_both =
      sum(
        diagnostic_match_3yr == TRUE &
          diagnostic_match_5yr == TRUE,
        na.rm = TRUE
      ),
    
    n_different_result =
      sum(
        !is.na(diagnostic_match_3yr) &
          !is.na(diagnostic_match_5yr) &
          diagnostic_match_3yr != diagnostic_match_5yr
      ),
    
    .groups = "drop"
  )


cat(
  "\nOverall FP/FN recruitment diagnostic:\n"
)


print(
  overall_error_summary,
  n = Inf
)


############################################################
# 10. COMPACT TABLE FOR INSPECTION
#     Includes both 3-year and 5-year diagnostics
############################################################

recruitment_table <- abi_error_recruitment_check %>%
  
  select(
    
    error_type,
    
    area,
    
    # Harmonised stock name
    stock_label,
    
    ABIref_terminal,
    
    SSB_ratio,
    
    F_ratio,
    
    R_ratio_median_3yr,
    
    R_percentile_3yr,
    
    recruitment_signal_3yr,
    
    R_ratio_median_5yr,
    
    R_percentile_5yr,
    
    recruitment_signal_5yr,
    
    recruitment_pattern
  ) %>%
  
  mutate(
    
    across(
      
      c(
        ABIref_terminal,
        SSB_ratio,
        F_ratio,
        R_ratio_median_3yr,
        R_percentile_3yr,
        R_ratio_median_5yr,
        R_percentile_5yr
      ),
      
      ~ round(
        .x,
        3
      )
    )
  )


cat(
  "\nCompact FP + FN recruitment diagnostic table:\n"
)


print(
  recruitment_table,
  n = Inf
)


############################################################
# 11. SAVE FULL DIAGNOSTIC OUTPUTS
############################################################

write.csv(
  abi_error_recruitment_check,
  "ABI_FP_FN_recruitment_check_3yr_5yr.csv",
  row.names = FALSE
)


write.csv(
  recruitment_table,
  "ABI_FP_FN_recruitment_table_3yr_5yr.csv",
  row.names = FALSE
)


write.csv(
  error_recruitment_summary,
  "ABI_FP_FN_recruitment_summary_3yr_5yr.csv",
  row.names = FALSE
)


############################################################
# 12. MAIN PAPER TABLE 2
#
# Primary analysis = 5-year recruitment window
#
# FALSE POSITIVES:
# assess unusually LOW recent recruitment (< P20)
#
# FALSE NEGATIVES:
# assess unusually HIGH recent recruitment (> P80)
############################################################

table2_recruitment <- abi_error_recruitment_check %>%
  
  mutate(
    
    ########################################################
    # Force desired order:
    # False positives FIRST
    # False negatives SECOND
    ########################################################
    
    error_order = case_when(
      
      error_type == "False positive" ~ 1,
      
      error_type == "False negative" ~ 2
    ),
    
    
    area_order = case_when(
      
      area == "MED" ~ 1,
      
      area == "NEA" ~ 2,
      
      area == "US" ~ 3,
      
      TRUE ~ 4
    ),
    
    
    ########################################################
    # Recruitment signal for the table
    ########################################################
    
    R_signal = case_when(
      
      error_type == "False positive" &
        low_recent_R_5yr == TRUE ~
        "Low",
      
      error_type == "False negative" &
        high_recent_R_5yr == TRUE ~
        "High",
      
      !is.na(R_percentile_5yr) ~
        "No",
      
      TRUE ~
        "NA"
    )
  ) %>%
  
  
  ##########################################################
# Order rows using the harmonised stock label
##########################################################

arrange(
  error_order,
  area_order,
  stock_label
) %>%
  
  
  ##########################################################
# Build final Table 2
##########################################################

transmute(
  
  ErrorType =
    as.character(
      error_type
    ),
  
  
  Region =
    area,
  
  
  # IMPORTANT:
  # Use harmonised display name
  Stock =
    stock_label,
  
  
  ABIref =
    round(
      ABIref_terminal,
      3
    ),
  
  
  SSB_Bref =
    round(
      SSB_ratio,
      3
    ),
  
  
  F_Fref =
    round(
      F_ratio,
      3
    ),
  
  
  RecentR_MedianR =
    round(
      R_ratio_median_5yr,
      3
    ),
  
  
  # Keep percentile on 0-1 scale.
  # Three decimals retained for consistency with calculations.
  
  R_Percentile =
    round(
      R_percentile_5yr,
      3
    ),
  
  
  R_Signal =
    R_signal
)


############################################################
# PRINT MAIN TABLE
############################################################

cat(
  "\nMAIN PAPER TABLE 2:\n"
)


print(
  table2_recruitment,
  n = Inf
)


############################################################
# SAVE MAIN TABLE AS CSV
############################################################

setwd(
  "C:/Users/andre/Documents/ABI/Draft paper/Third step/Output"
)


write.csv(
  table2_recruitment,
  "Table2_ABI_FP_FN_recruitment_diagnostic.csv",
  row.names = FALSE
)


############################################################
# 13. LATEX TABLE
#
# False positives first.
# Horizontal line after final FP.
############################################################

library(knitr)
library(kableExtra)


############################################################
# Number of FP rows
############################################################

n_FP_table <- sum(
  table2_recruitment$ErrorType ==
    "False positive"
)


############################################################
# Remove ErrorType from displayed table
#
# The horizontal rule separates FP from FN.
############################################################

table2_latex <- table2_recruitment %>%
  select(
    -ErrorType
  )


############################################################
# Generate LaTeX
############################################################

latex_table2 <- kable(
  
  table2_latex,
  
  format = "latex",
  
  booktabs = TRUE,
  
  escape = FALSE,
  
  caption = paste0(
    "Recruitment diagnostic for false-positive and false-negative ",
    "$\\mathrm{ABI}_{\\mathrm{ref}}$ classifications."
  ),
  
  col.names = c(
    
    "Region",
    
    "Stock",
    
    "$\\mathrm{ABI}_{\\mathrm{ref}}$",
    
    "$\\mathrm{SSB}/B_{\\mathrm{ref}}$",
    
    "$F/F_{\\mathrm{ref}}$",
    
    "Recent $R$ / median $R$",
    
    "$R$ percentile",
    
    "$R$ signal"
  ),
  
  align = c(
    "l",
    "l",
    "c",
    "c",
    "c",
    "c",
    "c",
    "c"
  ),
  
  linesep = ""
) %>%
  
  
  ##########################################################
# Slightly reduce size
##########################################################

kable_styling(
  font_size = 8
) %>%
  
  
  ##########################################################
# Horizontal line between FP and FN
##########################################################

row_spec(
  n_FP_table,
  extra_latex_after = "\\midrule"
)


############################################################
# Print LaTeX code in console
############################################################

cat(
  latex_table2
)



############################################################
# SM FIGURE
# Recruitment diagnostic for ABI false positives and
# false negatives using 3-year and 5-year windows
############################################################


############################################################
# 1. Prepare data
############################################################

plot_rec <- abi_error_recruitment_check %>%
  
  select(
    error_type,
    area,
    stock_label,
    R_percentile_3yr,
    R_percentile_5yr
  ) %>%
  
  pivot_longer(
    
    cols = c(
      R_percentile_3yr,
      R_percentile_5yr
    ),
    
    names_to = "window",
    
    values_to = "historical_percentile"
  ) %>%
  
  mutate(
    
    window = recode(
      window,
      R_percentile_3yr = "3-year",
      R_percentile_5yr = "5-year"
    ),
    
    window = factor(
      window,
      levels = c(
        "3-year",
        "5-year"
      )
    ),
    
    error_type = factor(
      
      error_type,
      
      levels = c(
        "False positive",
        "False negative"
      ),
      
      labels = c(
        "False positives (FP)",
        "False negatives (FN)"
      )
    ),
    
    area = factor(
      
      area,
      
      levels = c(
        "MED",
        "NEA",
        "US"
      )
    )
  )


############################################################
# 2. Order stocks within the figure
############################################################

stock_order <- plot_rec %>%
  
  distinct(
    error_type,
    area,
    stock_label
  ) %>%
  
  arrange(
    error_type,
    area,
    stock_label
  ) %>%
  
  pull(
    stock_label
  ) %>%
  
  unique()


plot_rec <- plot_rec %>%
  
  mutate(
    
    stock_label = factor(
      stock_label,
      levels = rev(stock_order)
    )
  )


############################################################
# 3. Thresholds
#
# FP -> unusually LOW recruitment: < P20
# FN -> unusually HIGH recruitment: > P80
############################################################

thresholds <- tibble(
  
  error_type = factor(
    
    c(
      "False positives (FP)",
      "False negatives (FN)"
    ),
    
    levels = c(
      "False positives (FP)",
      "False negatives (FN)"
    )
  ),
  
  threshold = c(
    0.20,
    0.80
  )
)


############################################################
# 4. Plot
############################################################

g_recruitment_SM <- ggplot(
  
  plot_rec,
  
  aes(
    x = historical_percentile,
    y = stock_label
  )
  
) +
  
  
  ##########################################################
# Connect 3-year and 5-year results for each stock
##########################################################

geom_line(
  
  aes(
    group = stock_label
  ),
  
  linewidth = 0.5,
  
  colour = "grey55"
) +
  
  
  ##########################################################
# 3-year and 5-year points
##########################################################

geom_point(
  
  aes(
    shape = window
  ),
  
  size = 3
) +
  
  
  ##########################################################
# P20 for FP and P80 for FN
##########################################################

geom_vline(
  
  data = thresholds,
  
  aes(
    xintercept = threshold
  ),
  
  linetype = "dashed",
  
  linewidth = 0.6,
  
  colour = "black"
) +
  
  
  ##########################################################
# Facets:
# rows = FP / FN
# columns = MED / NEA / US
##########################################################

facet_grid(
  
  rows = vars(error_type),
  
  cols = vars(area),
  
  scales = "free_y",
  
  space = "free_y"
) +
  
  
  ##########################################################
# X axis
##########################################################

scale_x_continuous(
  
  limits = c(
    0,
    1
  ),
  
  breaks = seq(
    0,
    1,
    by = 0.2
  ),
  
  expand = c(
    0.02,
    0.02
  )
) +
  
  
  ##########################################################
# Shapes
##########################################################

scale_shape_manual(
  
  values = c(
    "3-year" = 16,
    "5-year" = 17
  )
) +
  
  
  ##########################################################
# Labels
##########################################################

xlab(
  "Historical percentile of recent recruitment"
) +
  
  ylab(
    "Stock"
  ) +
  
  labs(
    shape = "Recruitment window"
  ) +
  
  
  ##########################################################
# Theme
##########################################################

theme_bw() +
  
  theme(
    
    text = element_text(
      size = 12
    ),
    
    strip.text = element_text(
      size = 11
    ),
    
    axis.text = element_text(
      size = 10
    ),
    
    legend.position = "right",
    
    panel.grid.minor = element_blank()
  )


############################################################
# 5. Show plot
############################################################

g_recruitment_SM


############################################################
# 6. Save Supplementary Figure
############################################################

setwd(
  "C:/Users/andre/Documents/ABI/Draft paper/Third step/Figure"
)


ggsave(
  
  filename = "SM_ABI_FP_FN_recruitment_3yr_5yr.png",
  
  plot = g_recruitment_SM,
  
  width = 11,
  
  height = 8,
  
  units = "in",
  
  dpi = 600
)


# Optional PDF
#
# ggsave(
#   filename = "SM_ABI_FP_FN_recruitment_3yr_5yr.pdf",
#   plot = g_recruitment_SM,
#   width = 11,
#   height = 8,
#   units = "in"
# )


############################################################
# Return to Output directory for tables
############################################################

setwd(
  "C:/Users/andre/Documents/ABI/Draft paper/Third step/Output"
)



############################################################
# TABLE 3
# ROC–Youden optimal threshold for ABIref
############################################################


############################################################
# FUNCTION TO CALCULATE TSS AT A GIVEN THRESHOLD
############################################################

calc_tss_at_threshold <- function(
    df,
    threshold
) {
  
  pred_status <- ifelse(
    df$ABImsy >= threshold,
    "Healthy",
    "Unhealthy"
  )
  
  
  TP <- sum(
    pred_status == "Healthy" &
      df$true_status == "Healthy",
    na.rm = TRUE
  )
  
  
  TN <- sum(
    pred_status == "Unhealthy" &
      df$true_status == "Unhealthy",
    na.rm = TRUE
  )
  
  
  FP <- sum(
    pred_status == "Healthy" &
      df$true_status == "Unhealthy",
    na.rm = TRUE
  )
  
  
  FN <- sum(
    pred_status == "Unhealthy" &
      df$true_status == "Healthy",
    na.rm = TRUE
  )
  
  
  sensitivity <- ifelse(
    (TP + FN) > 0,
    TP / (TP + FN),
    NA_real_
  )
  
  
  specificity <- ifelse(
    (TN + FP) > 0,
    TN / (TN + FP),
    NA_real_
  )
  
  
  TSS <- sensitivity + specificity - 1
  
  
  return(
    TSS
  )
}


############################################################
# FUNCTION TO CALCULATE TABLE 3 PER AREA
############################################################

calc_table3 <- function(df) {
  
  r <- pROC::roc(
    
    response = df$true_status,
    
    predictor = df$ABImsy,
    
    levels = c(
      "Unhealthy",
      "Healthy"
    ),
    
    quiet = TRUE
  )
  
  
  best <- pROC::coords(
    
    r,
    
    x = "best",
    
    best.method = "youden",
    
    ret = c(
      "threshold",
      "sensitivity",
      "specificity"
    ),
    
    transpose = FALSE
  )
  
  
  # Robust extraction
  
  thr_opt <- as.numeric(
    best[["threshold"]][1]
  )
  
  sens_opt <- as.numeric(
    best[["sensitivity"]][1]
  )
  
  spec_opt <- as.numeric(
    best[["specificity"]][1]
  )
  
  
  TSS_opt <- sens_opt + spec_opt - 1
  
  TSS_1 <- calc_tss_at_threshold(
    df,
    threshold = 1
  )
  
  
  delta_TSS <- TSS_opt - TSS_1
  
  
  thr_near_1 <- ifelse(
    abs(thr_opt - 1) <= 0.10,
    "Yes",
    "No"
  )
  
  
  tibble(
    
    Optimal_threshold = round(
      thr_opt,
      3
    ),
    
    TSSoptimal = round(
      TSS_opt,
      3
    ),
    
    TSS1.0 = round(
      TSS_1,
      3
    ),
    
    Delta_TSS = round(
      delta_TSS,
      3
    ),
    
    `Threshold_approx_1?` =
      thr_near_1
  )
}


############################################################
# BUILD TABLE 3
############################################################


# Check n

roc_dat %>%
  count(
    area,
    true_status
  )

# Should be 65


(table3 <- roc_dat %>%
    
    group_by(
      area
    ) %>%
    
    group_modify(
      ~ calc_table3(.x)
    ) %>%
    
    ungroup()
)


############################################################
# SAVE CSV
############################################################

write.csv(
  table3,
  "Table3_ROC_Youden_updated.csv",
  row.names = FALSE
)


############################################################
# CONVERT TABLE 3 TO LATEX
############################################################

(latex_table3 <- knitr::kable(
  
  table3,
  
  format = "latex",
  
  booktabs = TRUE,
  
  escape = FALSE,
  
  align = "lccccc",
  
  caption =
    "Comparison between the theoretical threshold (\\(\\mathrm{ABI}_{\\mathrm{ref}} = 1\\)) and the ROC--Youden optimal threshold for classifying stock status across regions. TSS = True Skill Score."
))


############################################################
# TABLE 4
# AUC and reference-threshold TSS by indicator and region
############################################################

table4_from_fig5 <- skill %>%
  
  mutate(
    
    area = as.character(
      area
    ),
    
    indicator = as.character(
      indicator
    ),
    
    
    indicator = case_when(
      
      indicator == "ABI[ref]" ~
        "ABIref",
      
      indicator == "SSB/B[ref]" ~
        "SSB/Bref",
      
      indicator == "SSB/B[lim]" ~
        "SSB/Blim",
      
      indicator == "F/F[Ref]" ~
        "F/Fref",
      
      TRUE ~
        indicator
    ),
    
    
    AUC = round(
      AUC,
      3
    ),
    
    
    TSS = round(
      TSS,
      3
    )
  )


############################################################
# AUC block
############################################################

table4_auc <- table4_from_fig5 %>%
  
  select(
    area,
    indicator,
    AUC
  ) %>%
  
  pivot_wider(
    
    names_from = indicator,
    
    values_from = AUC
  ) %>%
  
  mutate(
    Metric = "AUC"
  ) %>%
  
  select(
    
    Metric,
    
    Region = area,
    
    ABIref,
    
    `SSB/Bref`,
    
    `F/Fref`
  )


############################################################
# TSS block
############################################################

table4_tss <- table4_from_fig5 %>%
  
  select(
    area,
    indicator,
    TSS
  ) %>%
  
  pivot_wider(
    
    names_from = indicator,
    
    values_from = TSS
  ) %>%
  
  mutate(
    Metric = "TSS"
  ) %>%
  
  select(
    
    Metric,
    
    Region = area,
    
    ABIref,
    
    `SSB/Bref`,
    
    `F/Fref`
  )


############################################################
# Combine AUC + TSS
############################################################

table4 <- bind_rows(
  table4_auc,
  table4_tss
)


print(
  table4
)


############################################################
# Save CSV
############################################################

write.csv(
  
  table4,
  
  "C:/Users/andre/Documents/ABI/Draft paper/Third step/Output/Table4_indicator_skill_wide.csv",
  
  row.names = FALSE
)


############################################################
# LATEX TABLE
############################################################

latex_table4 <- kable(
  
  table4,
  
  format = "latex",
  
  booktabs = TRUE,
  
  escape = FALSE,
  
  align = c(
    "c",
    "l",
    "c",
    "c",
    "c"
  ),
  
  
  col.names = c(
    
    "Metric",
    
    "Region",
    
    "$\\mathrm{ABI}_{\\mathrm{ref}}$",
    
    "$\\mathrm{SSB}/B_{\\mathrm{ref}}$",
    
    "$F/F_{\\mathrm{ref}}$"
  ),
  
  
  caption = paste0(
    
    "Discrimination and classification skill of ",
    
    "$\\mathrm{ABI}_{\\mathrm{ref}}$, ",
    
    "$\\mathrm{SSB}/B_{\\mathrm{ref}}$, and ",
    
    "$F/F_{\\mathrm{ref}}$ across regions. ",
    
    "AUC = Area Under the Receiver--Operating Characteristic Curve; ",
    
    "TSS = True Skill Score. ",
    
    "AUC is threshold-independent, whereas TSS was calculated at the ",
    
    "theoretical reference threshold of 1 for each indicator."
  ),
  
  
  linesep = ""
  
) %>%
  
  
  ##########################################################
# Merge repeated AUC and TSS cells
##########################################################

collapse_rows(
  
  columns = 1,
  
  valign = "middle",
  
  latex_hline = "major"
) %>%
  
  
  ##########################################################
# Slightly reduce table size
##########################################################

kable_styling(
  font_size = 9
)


############################################################
# Print LaTeX Table 4
############################################################

latex_table4










