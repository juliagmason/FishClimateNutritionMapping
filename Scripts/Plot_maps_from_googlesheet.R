# Plot maps from combined google sheet
# JGM
# 2/18/2021


# setup ----
rm(list=ls())
library (tidyverse)
library (googlesheets4)


# https://sarahhamid.net/blog/maps-in-r/
library (rnaturalearth)
# install.packages("rnaturalearthdata")
library (rnaturalearthdata)
library (sf)
# install.packages("cli")
library(cli)

# for labeling islands
library (ggrepel)

# make world map
world <- ne_countries(scale = "medium", returnclass = "sf") 


# pull data from google sheet ----
# id url code for our Priority Mapping google sheet
fspn_id <- "1H_tri9xS3Ypl6ncrU9gGpseJmia_JVykOsXc950zEp8"

# country codes compiled from our aggregation in the google sheet. 
# iso3_terr has codes for territories; this means there are repeats in the iso3 column

codes <- read_sheet(ss = fspn_id, sheet = "Country_codes") 


# Read in "All metrics combined" sheet and put in alphabetical order by country
AMC <- read_sheet (ss = fspn_id, sheet = "All metrics combined") %>%
  arrange (Country)


# Clean All metrics combined dataset ----

# manually add country codes; filter out howland and virgin islands (not in AMC)
codes_AMC <- codes %>% 
  filter (!iso3_terr %in% c("USA-VI", "USA-HUN")) %>%
  select (-Country)

AMC <- cbind (AMC, codes_AMC)

# clean: get rid of NA iso3[this gets rid of "Islands in the Mozambique Channel" and "Sahara,South"]
# also remove kiribati line group, creating problems and has no data
# rename wordy column headers to work in R
AMC <- AMC %>%
  filter (!is.na (iso3), !iso3_terr == "KIR-LG") %>%
  mutate (
    # fix Kiribati iso code so it doesn't treat as a territory
    iso3_terr = ifelse (iso3_terr == "KIR-PHX", "KIR", iso3_terr)
  ) %>%
  rename (mic_def_rank = "Micronutrient deficiency Ranks (1= most deficient)" ,
          fish_dep_rank = "*MERGED DEPENDENCE RANK* (where tied in column V (e.g., Maldives and North Korea), higher rank assigned to the country for which 1) the SUM of ranks was higher; 2) had rank for both metrics)",
          fish_loss_rank = "Fishery Climate Vulnerability Rank 2100 (1=most vulnerable)"
  ) 

# Categorize countries ----

AMC_cat  <-  AMC %>%
  mutate (
    
    Category = as.factor(case_when (
      fish_dep_rank <= 100 & mic_def_rank <=100 ~ "Both",
      fish_dep_rank <=100 & is.na(mic_def_rank) ~ "Dependent on blue foods",
      fish_dep_rank <=100 & mic_def_rank > 100 ~ "Dependent on blue foods",
      mic_def_rank <=100 & is.na (fish_dep_rank ) ~  "Micronutrient deficient",
      mic_def_rank <= 100 & fish_dep_rank > 100 ~ "Micronutrient deficient",
      is.na (mic_def_rank) & is.na (fish_dep_rank) ~ "No data",
      
      TRUE ~ "Neither"))
    
    ) %>%
  select (Country, iso3, iso3_terr, fish_dep_rank, fish_loss_rank, mic_def_rank, Category) 

# set order of levels
AMC_cat$Category <- factor (AMC_cat$Category, levels = c("Dependent on blue foods", "Micronutrient deficient", "Both", "Neither", "No data"))

# If we filter at rank <=50 for both, get 19 countries in the "Both" category: Angola, Bangladesh, Benin, Cambodia, Grenada, Guinea, Ivory Coast, Mozambique, Nigerial NK, Ppns, Kiribas, RdCongo , Sato Tome, Sierra Leone, Solomons, Sri Lanka, Thailand, Togo.

# rank <= 75, get 34 countries: als BHS, BLZ, BFA, CMR, DMA, IDN, LAO, MGD, MDA, MMR, NAM, FRA-MAF, SUR, TLS, ZMB (70, approx top quarter, get 32)
# rank 100, get 62 countries.


####################################################################################
# alternate blue food dependent metric ----
# Trying to do something more medically/scientifically defensible than just ranking countries. From Golden et al. 2016: a country is nutritionally vulnerable if derived more than 10% of vit a or zinc or > 5% iron from fish. We combined this with the protein dependence data from Selig et al. 2019 and rated a country as a "blue" country if it was nutritionally vulnerable according to Golden OR in the top 10% of protein-dependent countries from Selig et al. 

### This is what we showed in ppts

### micronutrient dependence on fish:
# From Chris Golden, proprietary data [JM has this csv, but was asked not to share, so I put rankings in the google doc. can share for internal EDF usage]

# 7 columns. Iso3c (3 letter country code), nutrient, total terrestrial (amt available from land), total_aquatic (amt available from aquatic), total (sum of total terrestrial and total aquatic), prop_terrestrial, and prop_aquatic. 
# I only care about iso3c, nutrient, and prop_aquatic, and want to rename the country column to match mic_def
mic_dep <- read_csv ("Data/DO_NOT_SHARE_seafood_nutrient_supply_GND_012021.csv") %>%
  select (iso3c, nutrient, prop_aquatic) %>%
  rename (iso3 = iso3c)

mic_dep_vuln <- mic_dep %>%
  mutate (nutrient = gsub (" ", "_", nutrient)) %>%
  pivot_wider (
    names_from = nutrient,
    values_from = prop_aquatic) %>%
  
  filter (Vitamin_A > 0.1 | Zinc > 0.1 | Iron > 0.05)

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
  # take top 10%
  slice_max (prot_dep, prop = 0.1) %>%
  left_join (codes, by = "Country")

blue <- AMC %>%
  filter (iso3 %in% mic_dep_vuln$iso3 | iso3 %in% protein_dep$iso3) %>%
  select (iso3)
  

AMC_cat  <-  AMC %>%

 # add color column based on Willow's red/blue/purple
  mutate (

    Category = as.factor(case_when (
      iso3 %in% blue$iso3 & mic_def_rank <=100 ~ "Both",
      iso3 %in% blue$iso3 & is.na(mic_def_rank) ~ "Dependent on blue foods",
      iso3 %in% blue$iso3 &  mic_def_rank >100 ~  "Dependent on blue foods",
      mic_def_rank <=100 & is.na (fish_dep_rank ) ~  "Micronutrient deficient",
      mic_def_rank <= 100 & fish_dep_rank > 100 ~ "Micronutrient deficient",
      is.na (mic_def_rank) & is.na (fish_dep_rank) ~ "No data",

      TRUE ~ "Neither"))
    
  ) %>%
  select (Country, iso3, iso3_terr, fish_dep_rank, fish_loss_rank, mic_def_rank, Category) 

# set order of levels
AMC_cat$Category <- factor (AMC_cat$Category, levels = c("Dependent on blue foods", "Micronutrient deficient", "Both", "Neither", "No data"))

# join to world map ----
# to plot on world map, have to restrict to countries, not territories

AMC_countries <- AMC_cat %>%
  filter (! grepl ("-", iso3_terr)) %>%
  #rename to match map iso3 column
  rename ("iso_a3" = iso3)


world_AMC <- merge (world, AMC_countries, all = TRUE) %>%
  replace_na (list (Category = "No data")) 

# create subset of islands for separate labels
# https://dadascience.design/post/r-low-budget-high-res-mapping-with-r-for-not-for-profit-print/
islands <- world_AMC %>%
  filter (!Category %in% c("Neither", "No data", "NA"),
          subregion %in% c ("Caribbean", "Polynesia", "Melanesia", "Micronesia"))

# centroids df for labels
# https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
# https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data

sf::sf_use_s2(FALSE)
islands_centroids <- cbind(islands, st_coordinates(st_centroid(islands))) # named X and Y
islands_centroids <- islands_centroids %>%
  filter (!is.na (Category)) %>%
  #something weird with kiribati
  mutate (X = ifelse (iso_a3 == "KIR", -168, X))


# plot maps ----

png ("Figures/Map_country_categories.png", width = 14, height = 6, units = "in", res = 300)
ggplot (data = world_AMC) +
  geom_sf (aes (fill = Category), lwd = .25, col = "black") +
  scale_fill_manual (values = c( "blue","red","purple", "gray70", "white")) +
  scale_color_manual (values = c( "blue","red","purple")) +
  theme_bw() +
  labs (fill = "", x = "", y = "") +
  ggtitle ("Priority country categories") +
  
  guides (color = "none") +
  theme (plot.title = element_text (hjust = 0.5, size = 16),
         legend.text = element_text (size = 12))
dev.off()

png ("Figures/Map_country_categories_islandlabels.png", width = 14, height = 6, units = "in", res = 300)
ggplot (data = world_AMC) +
  geom_sf (aes (fill = Category), lwd = .25, col = "black") +
  scale_fill_manual (values = c( "blue","red","purple", "gray70", "white")) +
  scale_color_manual (values = c( "blue","red","purple")) +
  theme_bw() +
  labs (fill = "", x = "", y = "") +
  ggtitle ("Priority country categories") +
  geom_label_repel (data = fortify (islands_centroids),
                    aes (label = name, x = X, y = Y, 
                         color = Category), 
                    size = 3, label.padding = 0.10
  ) +
  guides (color = FALSE) +
  theme (plot.title = element_text (hjust = 0.5, size = 16),
         legend.text = element_text (size = 12))
dev.off()