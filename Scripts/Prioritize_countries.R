## Determine priority countries
# 5 26 2021
# JGM


## update 3/18/22. making datasets here so we can tabulate countries instead of just showing maps ----
# compile lists of priority countries based on different metrics/datasets. For each one, make sure it either has a clean country column (matches google sheets codes sheet) OR an iso3 column with territories removed or specified

library (tidyverse)
library (googlesheets4)

# function for copying R output tables into word/excel----
#https://stackoverflow.com/questions/24704344/copy-an-r-data-frame-to-an-excel-spreadsheet
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}


# id url code for our Priority Mapping google sheet
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

save (crfs_mapping_ds, file = "Data/crfs_mapping_ds.Rds")












## ds of aquatic food systems countries ----
aquatic_categories <- gaines %>%
  filter (country %in% AMC$Country) %>%
  select (country, iso3, rcp, catch_pdiff_BAU) %>%
  mutate (
    
    # define climate vulnerable as losing 50% or more of catch
    clim_vuln = ifelse (
    catch_pdiff_BAU < -0.5, 1, 0), 
    blue_1 = ifelse (iso3 %in% blue$iso3, 1, 0), 
    mic_def_1 = ifelse (iso3 %in% mic_def$iso3, 1, 0), 
    
    # color categories
      Category = 
        case_when (clim_vuln == 1 & iso3 %in% mic_def$iso3 & !iso3 %in% blue$iso3  ~ "Clim vuln & micro def",
                   clim_vuln == 0 & iso3 %in% mic_def$iso3 ~ "Micronutrient deficient",
                   is.na (clim_vuln) & iso3 %in% mic_def$iso3 ~ "Micronutrient deficient",
                   clim_vuln == 1 & !iso3 %in% mic_def$iso3 ~ "Climate vulnerable",
                   clim_vuln ==1 & iso3 %in% mic_def$iso3 & iso3 %in% blue$iso3 ~ "Blue foods dependent",
                   is.na (clim_vuln) & !iso3 %in% mic_def$iso3 ~ "None/no data",
                   TRUE ~ "None/no data")
  )

# sanity check...pivot wider? where are the islands?
# doesn't work, not all the same size

aquatic_categories %>% filter (rcp == "RCP 6.0") %>% View() 

aquatic_categories %>%
  mutate (sum_col = clim_vuln + blue_1 + mic_def_1) %>%
  filter (sum_col == 3, rcp == "RCP 6.0")

saveRDS(aquatic_categories, file = "Data/aquatic_priorities_climvuln_micdef_bluedep.Rds")


# start with climate vulnerable, then micronutrient deficient, then blue foods dependent

# tabulate "purple" dependent on blue foods countries in rcp 6.0
purp_aquatic <- aquatic_categories %>%
  filter (rcp == "RCP 6.0", Category == "Blue foods dependent") %>%
  pull(country)

write.excel (purp_aquatic)



### set cutoff point. This will match food insecurity cutoff and map labels. Should be 0.25, 0.5, 0.1
cutoff_pt <- 0.25

GAIN_top <- GAIN %>%
  slice_max (GAIN_food_vuln, prop = cutoff_pt) # 48 in top 25%, 19 in 10%, 96 in top 50%

 

# tig_vuln <- tig_cluster %>%
#   filter (cluster %in% c(3, 4, 5)) # 46 countries in 3 & 5; 60 with category 4

# EDF interested in 3,4,5; maybe 2

# join climate vulnerability
clim_vuln <- GAIN %>%
  full_join (tig_cluster, by = "iso3") %>%
  # make indicator column
  mutate (top_vuln = ifelse (iso3 %in% GAIN_top$iso3 | cluster %in% c(3, 5, 4, 2), 1, 0)) %>%
  select (-Country) # names are different from other data; join to codes later


pou_top <- pou %>%
  slice_max (POU, prop = cutoff_pt)  # 42 in top 25%

pou <- pou %>% 
  # make indicator column
  mutate (top_pou = ifelse(
    iso3 %in% pou_top$iso3, 1, 0
  ))



## ds of terrestrial food systems countries ----

# have to take the territories out of the country codes
codes_sov <-  codes %>% filter (! grepl ("-", iso3_terr))

terrestrial_categories <- pou %>%
  #join datasets together
  full_join (clim_vuln, by = "iso3") %>% 
  full_join (ghi, by = "iso3") %>%
  # replace NAs with 0? not sure if this is the right move. too many forms of NAs, will only show as gray/NA if there are no data from any of these sources
  replace_na(list(top_pou = 0, top_vuln = 0)) %>%
  # make category column by summing indicators
  #rowwise %>%
  mutate ( 
    Risk_POU = as.factor (case_when (
      top_vuln == 1 & top_pou == 0 ~ "Climate vulnerable",
      top_vuln == 0 & top_pou == 1 ~ "Undernourished",
      top_vuln == 1 & top_pou == 1 ~ "Both",
      TRUE ~ "Neither")),
    Risk_GHI = as.factor (
      case_when (
        top_vuln == 1 & GHI < 20 ~ "Climate vulnerable",
        top_vuln == 1 & is.na(GHI) ~ "Climate vulnerable",
        top_vuln == 0 & GHI > 20 ~ "Undernourished",
        top_vuln == 1 & GHI > 20 ~ "Both",
        TRUE ~ "Neither")
      )
  ) %>%
  left_join (codes_sov, by = "iso3") %>%
  select (-c(TimePeriod, iso3_terr))


saveRDS(terrestrial_categories, file = "Data/terrestrial_priorities_climvuln_pou.Rds")

# tabulate "red" terrestrial priorities
red_terrestrial <- terrestrial_categories %>%
  filter (Risk == "Both") %>%
  arrange (Country) %>%
  pull(Country)

write.excel (red_terrestrial)


red_terrestrial_GHI <- terrestrial_categories %>%
  filter (Risk_GHI == "Both") %>% 
  arrange (Country) %>%
  pull (Country)

write.excel (red_terrestrial_GHI)
# with alarming and extremely alarming, have 6 countries, all of which are in the pou vulnerable category

#########################################################################################################
# Post Feb 2022 VT retreat: prioritizing where in the world the highest vulnerability to climate change is coincidental with the highest food and/or nutrition deficiencies ----
#########################################################################################################

# Climate vulnerability ----

# GAIN index: threats to cereal crops
GAIN <- read_sheet(ss = fspn_id, sheet = "GAIN Food sector vulnerability") 


# GCRI: climate threats to people and economy. not food system specific. 
GCRI <- read_sheet(ss = fspn_id, sheet = "Global climate risk index (FSD)")

# what countries not shared?
GCRI$iso3[which (!GCRI$iso3 %in% GAIN$iso3)] # "XKX" "SSD" "PRI" "TWN" "WLD"
GAIN$iso3[which (!GAIN$iso3 %in% GCRI$iso3)] # "SOM" "SYR" "GNQ" "PLW" "NRU" "CUB" "STP" "TKM" "PRK" "AND" "LIE" "MCO" "SMR"

# top scores from each?
GAIN_top <-  GAIN %>%
  slice_max (GAIN_food_vuln, n = 50) # 48 in top 25%

GCRI_top  <-  GCRI %>%
  slice_max (GCRI, n = 50)

clim_vuln_join <- GAIN_top %>%
  full_join (GCRI_top, by = "iso3") # 82 countries with top 50 from each
View (clim_vuln_join)

# Tigchelaar categories--would want clusters 3 and 5 for high vulnerability. this captures crops beyond just cereals. 
tig_vuln <- read_sheet (ss = fspn_id, sheet = "Tigchelaar climate risk clusters") %>%
  filter (cluster %in% c(3, 5))# 46 countries

tig_vuln$iso3[which (!tig_vuln$iso3 %in% clim_vuln_join$iso3)] # 22 countries, " "BOL" "GHA" "GTM" "HND" "IDN" "MNG" "NAM" "NIC" "PHL" "TJK" "ZAF" "ZWE" "DJI" "DZA" "GEO" "HTI" "IRQ" "JOR""LBN" "SLV" "SWZ" "VEN"




# Food/nutrition deficiencies ----
beal <- read_sheet (ss = fspn_id, sheet = "Beal micronutrient deficiency")
beal_top <- beal %>%
  slice_max (PIMII, n = 50)

climvuln_microdef <- beal_top$Country[which (beal_top$iso3 %in% clim_vuln_join$iso3)] # 30 countries


food_insecure_top <- read_sheet (ss = fspn_id, sheet = "Food insecurity FSD") %>%
  slice_max (FIES, n = 50) # 30 in top 25%

ghi_top <- read_sheet (ss = fspn_id, sheet = "Global Hunger Index") %>%
  rename (iso3 = iso3_sov) %>%
  slice_max (GHI, n = 50)


# look at combinations ---
climvuln_insecure <- food_insecure_top$Country[which (food_insecure_top$iso3 %in% clim_vuln_join$iso3)] # 32

climvuln_insecure[which (climvuln_insecure %in% climvuln_microdef)] # 16 countries, most in Afr with Afghanistan and Cambodia



#########################################################################################################
# older blue foods stuff looking at benefits of adaptive management
# Gaines adaptive management data----

# google sheet with correct country codes

gaines_gs <- read_sheet(ss = fspn_id, sheet = "Gains from adaptive management (Gaines)") # this is rcp 8.5

gaines_country <- gaines_gs %>%
  select (country, iso3)

# aggregated gaines data, 6.0 and 8.5
gaines <- read_csv ("Data/Gaines1_total_catch_change_b1.csv") %>%
  # filter out the confusing ones. using phx group for kir
  filter (!country %in% c("Kiribati", "Serrana Bank", "Quita Sueño Bank")) %>%
  # calculate percent change
  mutate (catch_adapt_percent_gains = (catch_diff_Full - catch_diff_BAU) / catch_diff_BAU,
          catch_adapt_gains = catch_diff_Full - catch_diff_BAU) %>%


# save for later use
save(gaines, file = "Data/gaines_projections_rcp60_85.Rdata")


# Cheung data----
# fish_dep_df
load("Data/MCP_nutrition_compiled.RData")

## pull top priority countries by different metrics ----


# micronutrient/protein dependent "Blue" countries using golden et al and selig methodology--52 countries, 50 with climate data
# made this in Compile_data.R
blue_countries <- readRDS("Data/ocean_dependent_countries.RDS")


# rank by AMC rank
blue_countries_ranks <- blue_countries %>%
  left_join (AMC_full, by = "iso3") %>% 
  arrange (fish_dep_rank)

# of the 50 blue countries, which ones lose the most under BAU, percent change, gaines data?
gaines %>%
  filter (iso3 %in% blue_countries$iso3) %>%
  filter (rcp == "RCP 6.0", catch_pdiff_BAU < -0.5) %>%
  arrange (catch_pdiff_BAU) %>%
  select (country, catch_pdiff_BAU, catch_adapt_gains, fish_dep_rank) %>% View()

gaines %>%
  filter (iso3 %in% blue_countries$iso3) %>%
  filter (rcp == "RCP 8.5", catch_pdiff_BAU < -0.5) %>%
  arrange (catch_pdiff_BAU) %>%
  select (country, catch_pdiff_BAU, catch_adapt_gains, fish_dep_rank) %>% View()


# CLIMATE VULNERABLE ----
#willow's idea, which of the top 100 micronutrient deficient will lose 50% of their catch? 
lose50_60_12 <- gaines %>%
  filter (mic_def_rank < 100, iso3 %in% blue_countries$iso3) %>%
  filter (rcp == "RCP 6.0", catch_pdiff_BAU < -0.5) %>%
  arrange (catch_pdiff_BAU) %>%
  ungroup() %>%
  select (country, catch_pdiff_BAU, catch_diff_BAU, catch_adapt_gains) 

lose50_85_18 <- gaines %>%
  filter (mic_def_rank < 100, iso3 %in% blue_countries$iso3) %>%
  filter (rcp == "RCP 8.5", catch_pdiff_BAU < -0.5) %>%
  arrange (catch_pdiff_BAU) %>%
  ungroup() %>%
  select (country, catch_pdiff_BAU, catch_diff_BAU, catch_adapt_gains) 

save (lose50_60_12, file = "Data/Priority_loss_rcp60_12.RData")
save (lose50_85_18, file = "Data/Priority_loss_rcp85_18.RData")

# kate asked for absolute catch loss as well as percent
gaines %>%
  filter (mic_def_rank < 100, iso3 %in% blue_countries$iso3) %>%
  filter (rcp == "RCP 8.5") %>%
  arrange (catch_diff_BAU) %>%
  select (country, catch_pdiff_BAU, catch_diff_BAU, catch_adapt_gains, fish_dep_rank) %>% View()

gaines %>%
  filter (mic_def_rank < 100, iso3 %in% blue_countries$iso3) %>%
  filter (rcp == "RCP 6.0") %>%
  arrange (catch_diff_BAU) %>%
  select (country, catch_pdiff_BAU, catch_diff_BAU, catch_adapt_gains, fish_dep_rank) %>% View()


# what about cheung data
fish_dep_df %>%
  left_join (AMC_full, by = "iso3") %>%
  filter (iso3 %in% blue_countries$iso3, mic_def_rank < 100) %>% 
  filter (fish_loss < -50) %>%
  arrange (fish_loss) %>% 
  left_join (filter(gaines, rcp == "RCP 8.5"), by = "iso3") %>%
  select (country, fish_loss, catch_adapt_gains) %>% View()

# ADAPT ----
#of blue and mic def, which have most to gain from adaptive?
gain_60 <- gaines %>%
  filter (mic_def_rank < 100, iso3 %in% blue_countries$iso3) %>%
  filter (rcp == "RCP 6.0") %>%
  arrange (desc(catch_adapt_gains)) %>%
  ungroup() %>%
  select (country, catch_pdiff_BAU, catch_diff_BAU, catch_adapt_gains, catch_adapt_percent_gains) 

write.excel (gain_60)
save (gain_60, file = "Data/Priority_adapt_rcp60.RData")

gain_85 <- gaines %>%
  filter (mic_def_rank < 100, iso3 %in% blue_countries$iso3) %>%
  filter (rcp == "RCP 8.5") %>%
  arrange (desc(catch_adapt_gains)) %>%
  ungroup() %>%
  select (country, catch_pdiff_BAU, catch_diff_BAU, catch_adapt_gains) 

write.excel(gain_80)
save (gain_85, file = "Data/Priority_adapt_rcp85.RData")


# HUNGER RATES ----
#use GHI of alarming or extremely alarming, where 35 is alarming

load("Data/MCP_nutrition_compiled.RData")
  
hungry_85_14 <- fish_dep_df %>%
    left_join (AMC_full, by = "iso3") %>%
    filter (iso3 %in% blue_countries$iso3, GHI > 20) %>% 
    arrange (desc(GHI)) %>% 
  #filter (fish_loss < -50) %>% pull (Country)
    left_join (filter(gaines, rcp == "RCP 8.5"), by = "iso3") %>%
    select (Country, catch_pdiff_BAU, catch_diff_BAU, catch_adapt_gains) 


hungry_60_14 <- fish_dep_df %>%
  left_join (AMC_full, by = "iso3") %>%
  filter (iso3 %in% blue_countries$iso3, GHI > 20) %>% 
  arrange (desc(GHI)) %>% 
  #filter (fish_loss < -50) %>% pull (Country)
  left_join (filter(gaines, rcp == "RCP 6.0"), by = "iso3") %>%
  select (Country, catch_pdiff_BAU, catch_diff_BAU, catch_adapt_gains) 

save (hungry_60_14, file = "Data/Priority_hunger_rcp60_14.RData")
save (hungry_85_14, file = "Data/Priority_hunger_rcp85_14.RData")
write.excel(hungry_60_14)
write.excel(hungry_85_14)
  
  # try GHHI for micronutrient deficiency
  # our wolrd in data: Global Hidden Hunger Index scores in pre-school (aged under-5) children (GHI-PD) over the period 1999-2009. Hidden
  # Hunger Index (HHI-PD) for preschool-age children is calculated as the average of three deficiency prevalence
  # estimates: preschool children affected by stunting, anemia due to iron deficiency, and vitamin-A deficiency. The HHI-PD
  # score ranged between the best and worst possible scores of 0 and 100, respectively. Applying arbitrary cut-offs, HHI-PD
  # scores between 0 and 19.9 were considered mild, 20-34.9 as moderate, 35-44.9 as severe, and 45-100 as alarmingly
  # high.
  # 
  
  ghhi <- read_csv ("Data/global-hidden-hunger-index-in-pre-school-children.csv")
  
  ghhi %>%
    rename (iso3 = Code,
            ghhi =  "Hidden Hunger Index in pre-school children (Muthayya et al. (2013))") %>%
    left_join (AMC_full, by = "iso3") %>%
    filter (iso3 %in% blue_countries$iso3) %>% 
    filter (ghhi > 35) %>% View()


# of the blue countries, which loses the most absolutely?

gaines %>%
  filter (iso3 %in% blue_countries$iso3) %>%
  filter (rcp == "RCP 8.5") %>%
  arrange (catch_diff_BAU) %>%
  select (country, catch_diff_BAU, catch_adapt_gains, fish_dep_rank) %>% View()

#  who has the most to gain from adaptive management?
# 100 mic_Def_Rank for gaines8
# gaines6 %>% 
#  filter (fish_dep_rank <= 75, mic_def_rank <= 75) %>%
#   #filter (Category == "Both") %>%
#   arrange (desc(catch_adapt_gains)) %>%
#   select (country, catch_adapt_gains) %>%
#   head (20)

gaines %>%
  filter (iso3 %in% blue_countries$iso3) %>%
  filter (rcp == "RCP 6.0") %>%
  arrange (desc(catch_adapt_gains)) %>%
  select (country, catch_diff_BAU, catch_adapt_gains, fish_dep_rank) %>% View()

gaines8 %>% 
  filter (fish_dep_rank <= 75, mic_def_rank <= 100) %>%
  #filter (Category == "Both") %>%
  arrange (desc (catch_adapt_gains)) %>%
  select (country, catch_adapt_gains)

# who is most vulnerable to climate change, percent loss (Cheung), BAU?

gaines6 %>% 
  filter (fish_dep_rank <= 50, fish_loss_rank <= 50, mic_def_rank <= 50) %>%
  arrange (fish_dep_rank) %>%
  pull (country, catch_adapt_gains)

gaines6 %>% 
  arrange (catch_loss_BAU_rank) %>%
  select (country, catch_adapt_gains, catch_loss_BAU_rank)


# percent loss under BAU, Gaines for rcp 8.5
gaines8 %>% 
  filter (fish_dep_rank <= 75, mic_def_rank <= 75) %>%
  arrange (catch_pdiff_BAU) %>%
  select (country, catch_pdiff_BAU, catch_adapt_gains) %>% head (20)

gaines6 %>% 
  filter (fish_dep_rank <= 50, mic_def_rank <= 50) %>%
  arrange (catch_pdiff_BAU) %>%
  select (country, catch_pdiff_BAU, catch_adapt_gains) %>% View()

# percent loss under BAU, cheung data for rcp 8.5
load ("Data/MCP_nutrition_compiled.RData")

fish_dep_df %>% 
  left_join (gaines8, by = "iso3") %>%
  filter (mic_def_rank <= 75, fish_dep_rank <= 75) %>%
  arrange (fish_loss) %>%
  select (Country, fish_loss, catch_adapt_gains) %>% head (30)

# absolute loss under BAU, Gaines for both scenarios

gaines8 %>% 
  filter (Category == "Both") %>%
  #filter (fish_dep_rank <= 50, fish_loss_rank <= 50, mic_def_rank <= 50) %>%
  arrange (catch_diff_BAU) %>%
  select (country, catch_diff_BAU, catch_adapt_gains) %>% head(20)

x <- gaines6 %>% 
  filter (Category == "Both") %>%
  #filter (fish_dep_rank <= 75, mic_def_rank <= 75) %>%
  arrange (catch_diff_BAU) %>%
  select (country, catch_diff_BAU, catch_adapt_gains) %>% head(10)

tabout(x)

# https://stackoverflow.com/questions/37940271/export-r-output-to-excel
tabout <- function(output){
  print(output)
  capture.output(output, file = "clipboard", append = FALSE, 
                 type = "output", split = FALSE)
  lines <- readClipboard()
  for(i in 1 : 5) {lines <- gsub("  ", " ", lines, fixed=TRUE)}
  lines <- gsub(" ", "\t", lines, fixed=TRUE)
  writeClipboard(lines)
}




# what if we just looked at top 50 blue/red instead? using categories defined in world_map_figures
gaines8 %>%
  filter (Category == "Both") %>% View



gaines8_pdiff_loss <- gaines1_pdiff %>%
  filter (Category == "Both", rcp == "RCP 8.5") %>%
  #filter (fish_dep_rank <=50, mic_def_rank <=50) %>%
  
  arrange (catch_pdiff_BAU) %>%
  head (20)

gaines8_pdiff_loss %>%
  select (country, catch_pdiff_BAU)

gaines8 %>%
  filter (iso3 %in% gaines8_pdiff_loss$iso3) %>%
  select (country, catch_adapt_gains)
