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
library (viridis) # for colorscale
# install.packages("cli")
library(cli)

# for labeling islands
library (ggrepel)

# make world map
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  # rename country code column to match our data
  rename (iso3 = iso_a3)


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
# also remove Kiribati line group, creating problems and has no data
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
# [JE only] setwd("~/Documents/ACTIVE Research/EDF/Projects/Climate Resilient Food System/R")
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
  filter (! grepl ("-", iso3_terr))

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
  mutate (X = ifelse (iso3 == "KIR", -168, X))


# Plot combination of blue food dependency and micronutrient deficiencies ----
# while (!is.null(dev.list())) # code to break dev.off null device = 1
# color guide: http://sape.inf.usi.ch/quick-reference/ggplot2/colour # ("goldenrod1", "darkorange1", "firebrick4")

png ("Figures/Map_country_categories.png", width = 14, height = 6, units = "in", res = 300)
ggplot (data = world_AMC) +
  geom_sf (aes (fill = Category), lwd = .25, col = "black") +
  scale_fill_manual (values = c("blue","red","purple", "gray70", "white")) +
  scale_color_manual (values = c("blue","red","purple")) +
  theme_bw() +
  labs (fill = "", x = "", y = "") +
  ggtitle ("Priority country categories") +
  guides (color = "none") +
  theme (plot.title = element_text (hjust = 0.5, size = 16),
         legend.text = element_text (size = 12))
dev.off()

## Same plot but label countries with category "both"??
both <- world_AMC %>%
  filter (Category == "Both")

both_centroids <- cbind(both, st_coordinates(st_centroid(both)))

png ("Figures/Map_country_categories_both.png", width = 14, height = 6, units = "in", res = 300)
ggplot (data = world_AMC) +
  geom_sf (aes (fill = Category), lwd = .25, col = "black") +
  scale_fill_manual (values = c("blue","hotpink1","purple", "gray70", "white")) +
  scale_color_manual (values = c("blue","hotpink1","purple")) +
  theme_bw() +
  labs (fill = "", x = "", y = "") +
  ggtitle ("Blue food dependent and micronutrient deficient") +
  geom_label_repel (data = fortify (both_centroids),
                    aes (label = name, x = X, y = Y), 
                    color = "purple",
                    size = 2.5, label.padding = 0.10) +
  guides (color = FALSE) +
  theme (plot.title = element_text (hjust = 0.5, size = 16),
         legend.text = element_text (size = 12)) 
dev.off()

png ("Figures/Map_country_categories_islandlabels.png", width = 14, height = 6, units = "in", res = 300)
ggplot (data = world_AMC) +
  geom_sf (aes (fill = Category), lwd = .25, col = "black") +
  scale_fill_manual (values = c("blue","red","purple", "gray70", "white")) +
  scale_color_manual (values = c("blue","red","purple")) +
  theme_bw() +
  labs (fill = "", x = "", y = "") +
  ggtitle ("Blue food dependent and micronutrient deficient") +
  geom_label_repel (data = fortify (islands_centroids),
                    aes (label = name, x = X, y = Y, 
                         color = Category), 
                    size = 3, label.padding = 0.10
  ) +
  guides (color = FALSE) +
  theme (plot.title = element_text (hjust = 0.5, size = 16),
         legend.text = element_text (size = 12)) 
dev.off()


#########################################################################################################################
## Plot combinations of climate vulnerability and food insecurity ----

dir.create ("Figures/Maps_compare")

# food insecure and GAIN climate vulnerable or Tigchelaar vulnerable?

# do we need to have the venn diagram map or can we just have summed risk indicators
food_insecure <- read_sheet (ss = fspn_id, sheet = "Food insecurity FSD") 

food_insecure_top <- food_insecure %>%
  slice_max (FIES, prop = 0.25)  # 30 in top 25%

food_insecure <- food_insecure %>% 
  # make indicator column
  mutate (top_insecure = ifelse(
    iso3 %in% food_insecure_top$iso3, 1, 0
  ))


GAIN <- read_sheet(ss = fspn_id, sheet = "GAIN Food sector vulnerability") 

GAIN_top <- GAIN %>%
  slice_max (GAIN_food_vuln, prop = 0.25) # 48 in top 25%

tig_cluster <- read_sheet (ss = fspn_id, sheet = "Tigchelaar climate risk clusters") 

tig_vuln <- tig_cluster %>%
  filter (cluster %in% c(3, 5)) # 46 countries

# join climate vulnerability
clim_vuln <- GAIN %>%
  full_join (tig_cluster, by = "iso3") %>%
  # make indicator column
  mutate (top_vuln = ifelse (iso3 %in% GAIN_top$iso3 | cluster %in% c(3, 5), 1, 0)) %>%
  select (iso3, top_vuln)

insecure_food_vuln <- food_insecure %>%
  #remove duplicated country column
  select (-Country) %>%
  #join datasets together
  full_join (clim_vuln, by = "iso3") %>% 
  # replace NAs with 0? not sure if this is the right move. too many forms of NAs, will only show as gray/NA if there are no data from any of these sources
  replace_na(list(top_insecure = 0, top_vuln = 0)) %>%
  # make category column by summing indicators
  #rowwise %>%
  mutate ( 
    Risk = as.factor (case_when (
      top_vuln == 1 & top_insecure == 0 ~ "Climate vulnerable",
      top_vuln == 0 & top_insecure == 1 ~ "Food insecure",
      top_vuln == 1 & top_insecure == 1 ~ "Both",
      TRUE ~ "Neither")
    )
  )

insecure_food_vuln$Risk <- factor (insecure_food_vuln$Risk, levels = c("Climate vulnerable", "Food insecure", "Both", "Neither", "No data"))

world_insecure_food_vuln <- merge (world, insecure_food_vuln, all = TRUE) %>%
  replace_na (list (Risk = "No data")) 

# create subset of islands for separate labels
# https://dadascience.design/post/r-low-budget-high-res-mapping-with-r-for-not-for-profit-print/
islands <- world_insecure_food_vuln %>%
  filter (Risk %in% c("Climate vulnerable", "Food insecure", "Both"),
          subregion %in% c ("Caribbean", "Polynesia", "Melanesia", "Micronesia"))

# centroids df for labels
# https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
# https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data

sf::sf_use_s2(FALSE)
islands_centroids <- cbind(islands, st_coordinates(st_centroid(islands))) # named X and Y
islands_centroids <- islands_centroids %>%
  #something weird with kiribati
  mutate (X = ifelse (iso3 == "KIR", -168, X))


# plot 
png ("Figures/Maps_compare/Food_insecure_climate_vulnerable.png", width = 14, height = 6, units = "in", res = 300)
ggplot (data = world_insecure_food_vuln) +
  geom_sf (aes (fill = as.factor(Risk)), lwd = .25, col = "black") +
  scale_fill_manual (values = c( "goldenrod1","red","darkorange2", "gray70", "white")) +
  scale_color_manual (values = c( "goldenrod1","red","darkorange2")) +
  theme_bw() +
  labs (fill = "", x = "", y = "") +
  ggtitle ("Food insecurity and climate vulnerability") +
  geom_label_repel (data = fortify (islands_centroids),
                    aes (label = name, x = X, y = Y, 
                         color = Risk), 
                    size = 2.5, label.padding = 0.05,
                    max.overlaps = 50) +
  guides (color = "none") +
  theme (plot.title = element_text (hjust = 0.5, size = 16),
         legend.text = element_text (size = 12))
dev.off()

# plot with BOTH labelled
both_food <- world_insecure_food_vuln %>%
  filter (Risk %in% c("Both"))

sf::sf_use_s2(FALSE)
both_food_centroids <- cbind(both_food, st_coordinates(st_centroid(both_food))) # named X and Y

png ("Figures/Maps_compare/Food_insecure_climate_vulnerable_both.png", width = 14, height = 6, units = "in", res = 300)
ggplot (data = world_insecure_food_vuln) +
  geom_sf (aes (fill = as.factor(Risk)), lwd = .25, col = "black") +
  scale_fill_manual (values = c( "goldenrod1","hotpink1","red", "gray70", "white")) +
  scale_color_manual (values = c( "goldenrod1","hotpink1","red")) +
  theme_bw() +
  labs (fill = "", x = "", y = "") +
  ggtitle ("Food insecurity and climate vulnerability") +
  geom_label_repel (data = fortify (both_food_centroids),
                    aes (label = name, x = X, y = Y), 
                    color = "red",
                    size = 2.5, label.padding = 0.10) +
  guides (color = "none") +
  theme (plot.title = element_text (hjust = 0.5, size = 16),
         legend.text = element_text (size = 12))
dev.off()


#########################################################################################################################
## Plot combinations of climate vulnerability and POU ----




#############################################################################################################################
# Plot single variable maps to compare metrics ----
#############################################################################################################################
dir.create ("Figures/Maps_singlevar")

# Climate vulnerability: GAIN index ----
GAIN_index <- read_sheet(ss = fspn_id, sheet = "GAIN Food sector vulnerability") 

  
world_GAIN <- merge (world, GAIN_index, all = TRUE)

islands <- world_GAIN %>%
  filter (subregion %in% c ("Caribbean", "Polynesia", "Melanesia", "Micronesia"))

# centroids df for labels
# https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
# https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data

sf::sf_use_s2(FALSE)
islands_centroids <- cbind(islands, st_coordinates(st_centroid(islands))) # named X and Y
islands_centroids <- islands_centroids %>%
  filter (!is.na (GAIN_food_vuln)) %>%
  #something weird with kiribati
  mutate (X = ifelse (iso3 == "KIR", -168, X))


png ("Figures/Maps_singlevar/Map_GAIN_food_vuln_islandlabels.png", width = 14, height = 6, units = "in", res = 300)
ggplot (data = world_GAIN) +
  geom_sf (aes (fill = GAIN_food_vuln, color = GAIN_food_vuln), lwd = .25, col = "black") +
  scale_fill_viridis(direction = -1) +
  scale_color_viridis(direction = -1) +
  theme_bw() +
  labs (fill = "", x = "", y = "") +
  ggtitle ("GAIN food vulnerability") +
  geom_label_repel (data = fortify (islands_centroids),
                    aes (label = name, x = X, y = Y, 
                         color = GAIN_food_vuln), 
                    size = 2.5, label.padding = 0.05,
                    max.overlaps = 50
  ) +
  guides (color = "none") +
  theme (plot.title = element_text (hjust = 0.5, size = 16),
         legend.text = element_text (size = 12))
dev.off()

# Climate vulnerability: Global climate risk index----

GCRI <- read_sheet(ss = fspn_id, sheet = "Global climate risk index (FSD)") 

world_GCRI <- merge (world, GCRI, all = TRUE)

islands <- world_GCRI %>%
  filter (subregion %in% c ("Caribbean", "Polynesia", "Melanesia", "Micronesia")) 

sf::sf_use_s2(FALSE)
islands_centroids <- cbind(islands, st_coordinates(st_centroid(islands))) # named X and Y
islands_centroids <- islands_centroids %>%
  filter (!is.na (GCRI)) %>%
  #something weird with kiribati
  mutate (X = ifelse (iso3 == "KIR", -168, X))

png ("Figures/Maps_singlevar/Map_GCRI_islandlabels.png", width = 14, height = 6, units = "in", res = 300)
ggplot (data = world_GCRI) +
  geom_sf (aes (fill = GCRI), lwd = .25, col = "black") +
  scale_fill_viridis(direction = -1) +
  scale_color_viridis(direction = -1) +
  theme_bw() +
  labs (fill = "", x = "", y = "") +
  ggtitle ("Global Climate Risk Index") +
  geom_label_repel (data = fortify (islands_centroids),
                    aes (label = name, x = X, y = Y, 
                         color = GCRI), 
                    size = 2.5, label.padding = 0.05,
                    max.overlaps = 50
  ) +
  guides (color = "none") +
  theme (plot.title = element_text (hjust = 0.5, size = 16),
         legend.text = element_text (size = 12))
dev.off()

# micronutrient deficient, Beal data----
beal <- read_sheet(ss = fspn_id, sheet = "Beal micronutrient deficiency") 
world_microdef <- merge (world, beal, all = TRUE)

islands <- world_microdef %>%
  filter (subregion %in% c ("Caribbean", "Polynesia", "Melanesia", "Micronesia")) 
sf::sf_use_s2(FALSE)
islands_centroids <- cbind(islands, st_coordinates(st_centroid(islands))) # named X and Y
islands_centroids <- islands_centroids %>%
  filter (!is.na (PIMII)) %>%
  #something weird with kiribati
  mutate (X = ifelse (iso3 == "KIR", -168, X))

png ("Figures/Maps_singlevar/Map_Microdef_Beal_islandlabels.png", width = 14, height = 6, units = "in", res = 300)
ggplot (data = world_microdef) +
  geom_sf (aes (fill = PIMII), lwd = .25, col = "black") +
  scale_fill_viridis(direction = -1) +
  scale_color_viridis(direction = -1) +
  theme_bw() +
  labs (fill = "", x = "", y = "") +
  ggtitle ("Micronutrient deficiency prevalence, Beal et al.") +
  geom_label_repel (data = fortify (islands_centroids),
                    aes (label = name, x = X, y = Y, 
                         color = PIMII), 
                    size = 2.5, label.padding = 0.05,
                    max.overlaps = 50
  ) +
  guides (color = "none") +
  theme (plot.title = element_text (hjust = 0.5, size = 16),
         legend.text = element_text (size = 12))
dev.off()

# micronutrient deficient, Golden data ----
# proprietary, do not share
#pro: includes omega 3, more recent?
mic_def_golden <- read_csv ("Data/2017_sevs_country_average.csv")

mic_def_mn_golden <- mic_def_golden %>%
  group_by (iso3) %>%
  summarize (mn_def = mean (sev_base))

world_microdef_golden <- merge (world, mic_def_mn_golden, all = TRUE)

islands <- world_microdef_golden %>%
  filter (subregion %in% c ("Caribbean", "Polynesia", "Melanesia", "Micronesia")) 
sf::sf_use_s2(FALSE)
islands_centroids <- cbind(islands, st_coordinates(st_centroid(islands))) # named X and Y
islands_centroids <- islands_centroids %>%
  filter (!is.na (mn_def)) %>%
  #something weird with kiribati
  mutate (X = ifelse (iso3 == "KIR", -168, X))

png ("Figures/Maps_singlevar/Map_Microdef_Golden_islandlabels.png", width = 14, height = 6, units = "in", res = 300)
ggplot (data = world_microdef_golden) +
  geom_sf (aes (fill = mn_def), lwd = .25, col = "black") +
  scale_fill_viridis(direction = -1) +
  scale_color_viridis(direction = -1) +
  theme_bw() +
  labs (fill = "", x = "", y = "") +
  ggtitle ("Micronutrient deficiency prevalence, Golden et al.") +
  geom_label_repel (data = fortify (islands_centroids),
                    aes (label = name, x = X, y = Y, 
                         color = mn_def), 
                    size = 2.5, label.padding = 0.05,
                    max.overlaps = 50
  ) +
  guides (color = "none") +
  theme (plot.title = element_text (hjust = 0.5, size = 16),
         legend.text = element_text (size = 12))
dev.off()
  

# food insecurity, FSD ----
food_insecure <- read_sheet(ss = fspn_id, sheet = "Food insecurity FSD") 

world_insec <- merge (world, food_insecure, all = TRUE)

islands <-world_insec %>%
  filter (subregion %in% c ("Caribbean", "Polynesia", "Melanesia", "Micronesia")) 
sf::sf_use_s2(FALSE)
islands_centroids <- cbind(islands, st_coordinates(st_centroid(islands))) # named X and Y
islands_centroids <- islands_centroids %>%
  filter (!is.na (FIES)) %>%
  #something weird with kiribati
  mutate (X = ifelse (iso3 == "KIR", -168, X))

png ("Figures/Maps_singlevar/Map_foodinsecure_islandlabels.png", width = 14, height = 6, units = "in", res = 300)
ggplot (data = world_insec) +
  geom_sf (aes (fill = FIES), lwd = .25, col = "black") +
  scale_fill_viridis(direction = -1) +
  scale_color_viridis(direction = -1) +
  theme_bw() +
  labs (fill = "", x = "", y = "") +
  ggtitle ("Food insecurity, FSD") +
  geom_label_repel (data = fortify (islands_centroids),
                    aes (label = name, x = X, y = Y, 
                         color = FIES), 
                    size = 2.5, label.padding = 0.05,
                    max.overlaps = 50
  ) +
  guides (color = "none") +
  theme (plot.title = element_text (hjust = 0.5, size = 16),
         legend.text = element_text (size = 12))
dev.off()

# global hunger index ----
ghi <- read_sheet(ss = fspn_id, sheet = "Global Hunger Index") %>%
  rename (iso3 = iso3_sov)

world_ghi <- merge (world, ghi, all = TRUE)

islands <-world_ghi %>%
  filter (subregion %in% c ("Caribbean", "Polynesia", "Melanesia", "Micronesia")) 
sf::sf_use_s2(FALSE)
islands_centroids <- cbind(islands, st_coordinates(st_centroid(islands))) # named X and Y
islands_centroids <- islands_centroids %>%
  filter (!is.na (GHI)) %>%
  #something weird with kiribati
  mutate (X = ifelse (iso3 == "KIR", -168, X))

png ("Figures/Maps_singlevar/Map_GHI_islandlabels.png", width = 14, height = 6, units = "in", res = 300)
ggplot (data = world_ghi) +
  geom_sf (aes (fill = GHI), lwd = .25, col = "black") +
  scale_fill_viridis(direction = -1) +
  scale_color_viridis(direction = -1) +
  theme_bw() +
  labs (fill = "", x = "", y = "") +
  ggtitle ("Global Hunger Index") +
  geom_label_repel (data = fortify (islands_centroids),
                    aes (label = name, x = X, y = Y, 
                         color = GHI), 
                    size = 2.5, label.padding = 0.05,
                    max.overlaps = 50
  ) +
  guides (color = "none") +
  theme (plot.title = element_text (hjust = 0.5, size = 16),
         legend.text = element_text (size = 12))
dev.off()

