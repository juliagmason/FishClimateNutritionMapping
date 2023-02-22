# Compile data from googlesheet
# 3/23/22
# JGM 

#making datasets here so we can tabulate countries instead of just showing maps 

# compile lists of priority countries based on different metrics/datasets. For each one, make sure it either has a clean country column (matches google sheets codes sheet) OR an iso3 column with territories removed or specified

library (tidyverse)
library (googlesheets4)


# id url code for our Priority Mapping google sheet
# https://docs.google.com/spreadsheets/d/1H_tri9xS3Ypl6ncrU9gGpseJmia_JVykOsXc950zEp8/edit#gid=742482966
fspn_id <- "1H_tri9xS3Ypl6ncrU9gGpseJmia_JVykOsXc950zEp8"

# country codes compiled from our aggregation in the google sheet. 
# iso3_terr has codes for territories; this means there are repeats in the iso3 column
codes <- read_sheet(ss = fspn_id, sheet = "Country_codes") 
# provide access as prompted


# remove territories and clean
codes_sov <- codes %>%
  mutate (
    # fix Kiribati iso code so it doesn't treat as a territory; use phx group because line group has no data
    iso3_terr = ifelse (iso3_terr == "KIR-PHX", "KIR", iso3_terr)
  ) %>%
  filter (! grepl ("-", iso3_terr), !is.na(iso3), !iso3 %in% c("AFRICA", "ASIA", "OCEANIA")) %>%
  select (-iso3_terr)


###################################################################
# Climate vulnerability metrics ----

# climate vulnerable fisheries ----
# climate vulnerable defined as projected to lose 50% or more of their catch by 2100, using Gaines projections

load ("Data/gaines_projections_rcp60_85.Rdata")

# called "gaines". For each country, it has the difference in projected future catch/biomass in 2090-2100 relative to 2012-2021 under RCP 6.0 and RCP 8.5. Catch_diff_BAU is the projected difference under BAU management and catch_diff_Full is the projected difference under fully adaptive management (MEY and transboundary mgmt). Catch_pdiff_BAU is the percent difference in projected catch under BAU management. Catch_adapt_gains is what you gain by implementing adaptive management (catch_diff_Full - catch_diff_BAU). Catch_adapt_percent_gains is the percent gain by implementing adaptive management (catch_diff_Full - catch_diff_BAU / catch_diff_BAU)

## **** this is at the territory level, and does not distinguish iso3. **** have to figure out how to get rid of territories

# unique(gaines$country[which (!gaines$country %in% codes_sov$Country)])
# 
# t <- gaines %>% ungroup() %>% filter (!country %in% codes_sov$Country, rcp == "RCP 6.0") %>% select (iso3, country)

# clean to match country names
# use phx group for kiribati to match AMC
# east timor is timor leste. get rid of oecussi. 
# also have to change some country codes
gaines_clean <- gaines %>%
  ungroup() %>%
  mutate (
    country = case_when (
      country == "East Timor" ~ "Timor Leste",
      country ==  "Ivory Coast" ~ "Cote d’Ivoire",
      grepl("publique du Congo", country) ~ "Republique du Congo", 
      country == "Phoenix Group" ~ "Kiribati",
      TRUE ~ country),
    iso3 = case_when (
      country == "Bermuda" ~ "BMU",
      country == "French Guiana" ~ "GUF",
      country == "French Polynesia" ~ "PYF",
      TRUE ~ iso3
    )
  ) %>%
  filter (country %in% codes_sov$Country)

# pivot wider to show both scenarios
gaines_scen <- gaines_clean %>%
  ungroup() %>%
  select (iso3, rcp, catch_pdiff_BAU, catch_adapt_percent_gains) %>%
  pivot_wider (names_from = rcp, 
               values_from = c(catch_pdiff_BAU, catch_adapt_percent_gains)) %>%
  # probably a cleaner way of doing this. may want to use different symbol to grab scenario
  rename (catch_pdiff_BAU_60 = "catch_pdiff_BAU_RCP 6.0",
          catch_pdiff_BAU_85 = "catch_pdiff_BAU_RCP 8.5",
          adapt_perc_benefit_60 = "catch_adapt_percent_gains_RCP 6.0",
          adapt_perc_benefit_85 = "catch_adapt_percent_gains_RCP 8.5")

# terrestrial climate vulnerability indices----

# GAIN index -- staple crops
GAIN <- read_sheet(ss = fspn_id, sheet = "GAIN Food sector vulnerability") %>%
  select (-Country)

# Michelle Tigchelaar food systems paper--includes broader crops
tig_cluster <- read_sheet (ss = fspn_id, sheet = "Tigchelaar climate risk clusters")


###################################################################
# Food/nutrition vulnerability ----

# blue food dependent ----

### protein dependence on fish from Selig et al. 2019

#this doesn't have iso3, so have to join by country
protein_dep  <- read_sheet (ss = fspn_id, sheet = "Protein dependence on oceans") %>%
  rename (prot_dep = "Nutritional dependence" )%>% 
  # clean country names
  mutate (
    Country = gsub ("-", " ", Country),
    Country = case_when (
      Country ==  "Cote D'Ivoire" ~ "Cote d’Ivoire",
      Country == "The Bahamas" ~ "Bahamas",
      Country == "Gambia, The" ~ "Gambia",
      Country == "Great Britain" ~ "United Kingdom",
      Country == "Congo" ~ "Republique du Congo",
      Country == "Federated States of Micronesia" ~ "Micronesia",
      Country == "The Former Yugoslav Republic of Macedonia" ~ "North Macedonia",
      Country == "Brunei Darussalam" ~ "Brunei",
      Country == "Slovakia" ~ "Slovak Republic",
      Country == "Swaziland" ~ "Eswatini",
      TRUE ~ Country)
  ) %>%
  left_join (codes_sov, by = "Country")


### micronutrient dependence on fish:
# From Chris Golden, proprietary data [JM has this csv, but was asked not to share, so I put rankings in the google doc. can share for internal EDF usage]

# 7 columns. Iso3c (3 letter country code), nutrient, total terrestrial (amt available from land), total_aquatic (amt available from aquatic), total (sum of total terrestrial and total aquatic), prop_terrestrial, and prop_aquatic. 
# I only care about iso3c, nutrient, and prop_aquatic, and want to rename the country column to match mic_def
# [JE only] setwd("~/Documents/ACTIVE Research/EDF/Projects/Climate Resilient Food System/R")
mic_dep <- read_csv ("Data/DO_NOT_SHARE_seafood_nutrient_supply_GND_012021.csv") %>%
  select (iso3c, nutrient, prop_aquatic) %>%
  rename (iso3 = iso3c)


# define countries as blue foods dependent if they fit the micronutrient dependence criteria from Golden et al. 2016 AND are in the top countries in terms of protein dependence
mic_dep_vuln <- mic_dep %>%
  mutate (nutrient = gsub (" ", "_", nutrient)) %>%
  pivot_wider (
    names_from = nutrient,
    values_from = prop_aquatic) %>%
  
  filter (Vitamin_A > 0.1 | Zinc > 0.1 | Iron > 0.05)

prot_dep_vuln <- protein_dep %>%
  # take top 10%
  slice_max (prot_dep, prop = 0.1)


blue <- codes_sov %>%
  filter (iso3 %in% mic_dep_vuln$iso3 | iso3 %in% prot_dep_vuln$iso3) %>%
  select (iso3)



# micronutrient deficiency ----

# ranked micronutrient dependency from Golden
mic_def_golden <- read_sheet (ss = fspn_id, sheet = "Micronutrient deficiency") %>% select (iso3, `overall rank`) %>%
  rename (mic_def_golden = `overall rank`)


# alternate micronutrient deficiency metric, PIMII from beal et al 2018
# not sure what the cutoff is
mic_def_beal <- read_sheet (ss = fspn_id, sheet = "Beal micronutrient deficiency") %>%
  select (-Country) %>%
  rename (PIMII_Beal = PIMII)

# Prevalence of undernourishment ----
pou <- read_sheet (ss = fspn_id, sheet = "Prevalence of undernourishment FSD") %>%
  select (-TimePeriod)

# Food insecurity experience scale ----
fies <- read_sheet(ss = fspn_id, sheet = "Food insecurity FSD") %>%
  select (-Country)

# Global hunger index ----
# Compare with GHI, which includes PoU and under 5 wasting, stunting, and mortality ----
ghi <- read_sheet(ss = fspn_id, sheet = "Global Hunger Index") %>%
  rename (iso3 = iso3_sov) %>%
  select (-country) %>%
  # add severity scale? maybe don't need this
  mutate (GHI_severity = 
            case_when (GHI <= 9.9 ~ "Low",
                       between (GHI, 10, 19.9) ~ "Moderate",
                       between (GHI, 20, 34.9) ~ "Serious",
                       between (GHI, 35, 49.9) ~ "Alarming",
                       GHI >= 50 ~ "Extremely alarming")
  )

###################################################################
# combine and save ----


crfs_mapping_ds <- codes_sov %>%
  full_join (gaines_scen, by = "iso3") %>%
  full_join (GAIN, by = "iso3") %>%
  full_join (tig_cluster, by = "iso3") %>%
  full_join (pou, by = "iso3") %>%
  full_join (fies, by = "iso3") %>%
  full_join (ghi, by = "iso3") %>%
  full_join (mic_def_golden, by = "iso3") %>%
  full_join (mic_def_beal, by = "iso3") %>%
  mutate (blue_dep = ifelse (iso3 %in% blue$iso3, 1, 0))

saveRDS (crfs_mapping_ds, file = "Data/crfs_mapping_ds.Rds")
