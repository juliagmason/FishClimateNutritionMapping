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
  arrange (Country) %>%
  # AMC country row has extra notes; use country column from codes
  select (-Country)


# Clean All metrics combined dataset ----

# manually add country codes; filter out howland and virgin islands (not in AMC)
codes_AMC <- codes %>% 
  filter (!iso3_terr %in% c("USA-VI", "USA-HUN")) 

AMC <- cbind (AMC, codes_AMC)

# clean: get rid of NA iso3[this gets rid of "Islands in the Mozambique Channel" and "Sahara,South"]
# also remove Kiribati line group, creating problems and has no data
# Remove the territories here; this is creating too much confusion
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
  ) %>%
  filter (! grepl ("-", iso3_terr))



### MAP 1: Blue foods system ### ----
# Categorize blue foods dependency ----

# AMC_cat  <-  AMC %>%
#   mutate (
#     
#     Category = as.factor(case_when (
#       fish_dep_rank <= 100 & mic_def_rank <=100 ~ "Both",
#       fish_dep_rank <=100 & is.na(mic_def_rank) ~ "Dependent on blue foods",
#       fish_dep_rank <=100 & mic_def_rank > 100 ~ "Dependent on blue foods",
#       mic_def_rank <=100 & is.na (fish_dep_rank ) ~  "Micronutrient deficient",
#       mic_def_rank <= 100 & fish_dep_rank > 100 ~ "Micronutrient deficient",
#       is.na (mic_def_rank) & is.na (fish_dep_rank) ~ "No data",
#       
#       TRUE ~ "Neither"))
#     
#     ) %>%
#   select (Country, iso3, fish_dep_rank, fish_loss_rank, mic_def_rank, Category) 
# 
# # set order of levels
# AMC_cat$Category <- factor (AMC_cat$Category, levels = c("Dependent on blue foods", "Micronutrient deficient", "Both", "Neither", "No data"))

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


### protein dependence on fish from Selig et al. 2019

# define protein dependent countries as those in the top 10% of Selig's nutritional dependence index
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

# define countries as blue foods dependent if they fit the micronutrient dependence criteria from Golden et al. 2016 AND are in the top countries in terms of protein dependence
blue <- AMC %>%
  filter (iso3 %in% mic_dep_vuln$iso3 | iso3 %in% protein_dep$iso3) %>%
  select (iso3)

# define countries as micronutrient deficient if they are in the top 100 ranked micronutrient deficiency
mic_def <- AMC %>%
  filter (mic_def_rank <= 100)
  

# set up to plot as blue/red/purple----
AMC_cat  <-  AMC %>%

 # add color column based on Willow's red/blue/purple
  mutate (

    Category = as.factor(case_when (
      iso3 %in% blue$iso3 & mic_def_rank <=100 ~ "Both",
      iso3 %in% blue$iso3 & is.na(mic_def_rank) ~ "Dependent on blue foods",
      iso3 %in% blue$iso3 &  mic_def_rank >100 ~  "Dependent on blue foods",
      mic_def_rank <= 100 & !iso3 %in% blue$iso3 ~ "Micronutrient deficient",
      is.na (mic_def_rank) & is.na (fish_dep_rank) ~ "No data",

      TRUE ~ "Neither"))
    
  ) %>%
  select (Country, iso3, fish_dep_rank, fish_loss_rank, mic_def_rank, Category) 

# set order of levels
AMC_cat$Category <- factor (AMC_cat$Category, levels = c("Dependent on blue foods", "Micronutrient deficient", "Both", "Neither", "No data"))

# join to world map 

world_AMC <- merge (world, AMC_cat, all = TRUE) %>%
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


# Plot blue countries only ----
png ("Figures/Map_bluefoods_dep.png", width = 14, height = 6, units = "in", res = 300)
world_AMC %>%
  # just make a new column, don't have a good way of subsetting
  mutate (Blue = ifelse (iso3 %in% blue$iso3, "blue", "not")) %>%
  ggplot() +
  geom_sf (aes (fill = Blue), lwd = .25, col = "black") +
  scale_fill_manual (values = c( "blue", "white")) +
  theme_bw() +
  labs (fill = "", x = "", y = "") +
  ggtitle ("Countries dependent on blue foods \n Micronutrient dependent (Golden) and top 10% of protein dependence (Selig)") +
  theme (plot.title = element_text (hjust = 0.5, size = 12),
         legend.position = "none")
dev.off()

# Plot micronutrient deficient countries only----
png ("Figures/Map_micronutrient_def.png", width = 14, height = 6, units = "in", res = 300)
world_AMC %>%
  # just make a new column
  mutate (Micro_def = ifelse (mic_def_rank <=100 , "def", "not")) %>%
  ggplot() +
  geom_sf (aes (fill = Micro_def), lwd = .25, col = "black") +
  scale_fill_manual (values = c( "hotpink1", "white")) +
  theme_bw() +
  labs (fill = "", x = "", y = "") +
  ggtitle ("Top 100 micronutrient deficient countries \n Golden data: Calcium, Iron, Omega-3, Vitamin A, Vitamin B12, Zinc") +
  theme (plot.title = element_text (hjust = 0.5, size = 12),
         legend.position = "none")
dev.off()

# Plot combination of blue food dependency and micronutrient deficiencies ----
# while (!is.null(dev.list())) # code to break dev.off null device = 1
# color guide: http://sape.inf.usi.ch/quick-reference/ggplot2/colour # ("goldenrod1", "darkorange1", "firebrick4")

png ("Figures/Map_bluefoods_dep_micronutrient_def.png", width = 14, height = 6, units = "in", res = 300)
ggplot (data = world_AMC) +
  geom_sf (aes (fill = Category), lwd = .25, col = "black") +
  scale_fill_manual (values = c("blue","hotpink1","purple", "gray70", "white")) +
  scale_color_manual (values = c("blue","hotpink1","purple")) +
  theme_bw() +
  labs (fill = "", x = "", y = "") +
  ggtitle ("Blue food dependent and micronutrient deficient") +
  guides (color = "none") +
  theme (plot.title = element_text (hjust = 0.5, size = 16),
         legend.text = element_text (size = 12))
dev.off()

## Same plot but label countries with category "both"
both <- world_AMC %>%
  filter (Category == "Both")

both_centroids <- cbind(both, st_coordinates(st_centroid(both)))

png ("Figures/Map_bluefoods_dep_micronutrient_def_bothlabel.png", width = 14, height = 6, units = "in", res = 300)
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
                    size = 2.5, label.padding = 0.05, max.overlaps = 50) +
  guides (color = "none") +
  theme (plot.title = element_text (hjust = 0.5, size = 16),
         legend.text = element_text (size = 12)) 
dev.off()

# label small islands
png ("Figures/Map_bluefoods_dep_micronutrient_def_islandlabel.png", width = 14, height = 6, units = "in", res = 300)
ggplot (data = world_AMC) +
  geom_sf (aes (fill = Category), lwd = .25, col = "black") +
  scale_fill_manual (values = c("blue","hotpink1","purple", "gray70", "white")) +
  scale_color_manual (values = c("blue","hotpink1","purple")) +
  theme_bw() +
  labs (fill = "", x = "", y = "") +
  ggtitle ("Blue food dependent and micronutrient deficient") +
  geom_label_repel (data = fortify (islands_centroids),
                    aes (label = name, x = X, y = Y, 
                         color = Category), 
                    size = 2.5, label.padding = 0.05, max.overlaps = 50
  ) +
  guides (color = "none") +
  theme (plot.title = element_text (hjust = 0.5, size = 16),
         legend.text = element_text (size = 12)) 
dev.off()


#########################################################################################
# MAP 1A: triple threat: blue food dependent, micronutrient deficient, and climate vulnerable ----

# climate vulnerable defined as projected to lose 50% or more of their catch by 2100, using Gaines projections

load ("Data/gaines_projections_rcp60_85.Rdata")
# called "gaines". For each country, it has the difference in projected future catch/biomass in 2090-2100 relative to 2012-2021 under RCP 6.0 and RCP 8.5. Catch_diff_BAU is the projected difference under BAU management and catch_diff_Full is the projected difference under fully adaptive management (MEY and transboundary mgmt). Catch_pdiff_BAU is the percent difference in projected catch under BAU management. Catch_adapt_gains is what you gain by implementing adaptive management (catch_diff_Full - catch_diff_BAU). Catch_adapt_percent_gains is the percent gain by implementing adaptive management (catch_diff_Full - catch_diff_BAU / catch_diff_BAU)

## **** this is at the territory level, and does not distinguish iso3. **** have to figure out how to get rid of territories

#unique(gaines$country[which (!gaines$country %in% AMC$Country)])

gaines <- gaines %>%
   mutate (
    country = case_when (
      country ==  "Ivory Coast" ~ "Cote d’Ivoire",
      grepl("publique du Congo", country) ~ "Republique du Congo", 
      TRUE ~ country)
   ) %>%
  filter (country %in% AMC$Country)


# In previous graphs, we defined climate vulnerable countries as those that would lose >50% of their catch under BAU management, that is, catch_pdiff_BAU < -0.5

# merge to world data to plot just vulnerable countries
# might be easier to split into rcp and then merge--have to deal with NA countries
world_gaines_60 <- merge (world, filter(gaines, rcp == "RCP 6.0"), all = TRUE) %>%
  replace_na(list (rcp = "RCP 6.0"))
world_gaines_85 <- merge (world, filter(gaines, rcp == "RCP 8.5"), all = TRUE) %>%
  replace_na(list (rcp = "RCP 8.5"))

world_gaines <- rbind (world_gaines_60, world_gaines_85) %>%
  mutate (clim_vuln = ifelse (
    catch_pdiff_BAU < -0.5, 1, 0
  ),
  # Categorize with blue foods first, then micronutrient deficient, then climate vulnerable
  Category = case_when (
    # blue foods dependent only
     iso3 %in% blue$iso3 & !iso3 %in% mic_def$iso3 ~ "Dependent on blue foods",
    # micronutrient deficient only
    !iso3 %in% blue$iso3 & iso3 %in% mic_def$iso3 ~ "Micronutrient deficient",
    # both blue foods and micronutrient deficient, but not climate vuln
    clim_vuln %in% c(0, NA) & iso3 %in% blue$iso3 & iso3 %in% mic_def$iso3  ~ "Blue dep & micro def",
    # # both AND climate vulnerable
    clim_vuln == 1 & iso3 %in% blue$iso3 & iso3 %in% mic_def$iso3 ~ "Climate vulnerable",
    is.na(clim_vuln)& !iso3 %in% blue$iso3 & !iso3 %in% mic_def$iso3 ~ "No data",
    TRUE ~ "None"
  )) 



# set order of labels
world_gaines$Category <- factor (world_gaines$Category, levels = c("Dependent on blue foods", "Micronutrient deficient", "Blue dep & micro def", "Climate vulnerable", "None", "No data"))

triple <- world_gaines %>%
  filter (Category == "Climate vulnerable")

triple_centroids <- cbind(triple, st_coordinates(st_centroid(triple)))

png ("Figures/Blue_dep_mic_def_clim_vuln_triple_threat_label.png", width = 14, height = 6, units = "in", res = 300)
ggplot (data = world_gaines) +
  geom_sf (aes (fill = Category), lwd = .25, col = "black") +
  scale_fill_manual (values = c("blue","hotpink1","purple", "red", "gray80", "white")) +
  #scale_color_manual (values = c("blue","hotpink1","purple")) +
  facet_wrap (~rcp) +
  theme_bw() +
  labs (fill = "", x = "", y = "") +
  ggtitle (("Blue food dependent, micronutrient deficient, and climate vulnerable \n Golden/Selig blue foods dependence, top 100 micronutrient deficient, projected to lose > 50% of catch by 2100 under BAU mgmt,   \n Red is a subset of purple")) +
  geom_label_repel (data = fortify (triple_centroids),
                    aes (label = name, x = X, y = Y), 
                    color = "red",
                    size = 2.5, label.padding = 0.05, max.overlaps = 50) +
  guides (color = "none") 
  # theme (plot.title = element_text (hjust = 0.5, size = 16),
  #        legend.text = element_text (size = 12)) 
dev.off()

## MAP 1B: first climate vulnerable, then micronutrient def, then blue ----

# climate vulnerable fisheries----
world_gaines <- rbind (world_gaines_60, world_gaines_85) %>%
  mutate (clim_vuln = ifelse (
    catch_pdiff_BAU < -0.5, 1, 0
  ))

# create subset of islands for separate labels
# https://dadascience.design/post/r-low-budget-high-res-mapping-with-r-for-not-for-profit-print/
islands <- world_gaines %>%
  filter (clim_vuln ==1,
          subregion %in% c ("Caribbean", "Polynesia", "Melanesia", "Micronesia"))

# centroids df for labels
# https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
# https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data

sf::sf_use_s2(FALSE)
islands_centroids <- cbind(islands, st_coordinates(st_centroid(islands))) # named X and Y
islands_centroids <- islands_centroids %>%
  #something weird with kiribati
  mutate (X = ifelse (iso3 == "KIR", -168, X))


png ("Figures/Climate_vulnerable_fisheries_gaines.png", width = 14, height = 6, units = "in", res = 300)

ggplot(world_gaines) +
  geom_sf (aes (fill = as.factor(clim_vuln)), lwd = .25, col = "black") +
  scale_fill_manual (values = c("white", "goldenrod1")) +
  facet_wrap (~rcp) +
  theme_bw() +
  labs (fill = "", x = "", y = "") +
  ggtitle ("Climate vulnerable fisheries \n Projected to lose > 50% of catch by 2100 under BAU mgmt") +
  guides (fill = "none") +
  geom_label_repel (data = fortify (islands_centroids),
                    aes (label = name, x = X, y = Y), 
                    col = "goldenrod1", 
                    size = 2.5, label.padding = 0.05,
                    max.overlaps = 50
  ) +
  theme (plot.title = element_text (hjust = 0.5, size = 16),
         legend.text = element_text (size = 12)
  ) +
  labs(caption = "Gray indicates no data")

dev.off()

# now layer on micronutrient deficiency----
# just pull the ranking so I don't have to merge again
mic_def <- AMC %>%
  filter (mic_def_rank <= 100)

fish_clim_micdef <- world_gaines %>%
  mutate (Category = 
            case_when (clim_vuln == 1 & iso3 %in% mic_def$iso3 ~ "Both",
                       clim_vuln == 0 & iso3 %in% mic_def$iso3 ~ "Micronutrient deficient",
                       is.na (clim_vuln) & iso3 %in% mic_def$iso3 ~ "Micronutrient deficient",
                       clim_vuln == 1 & !iso3 %in% mic_def$iso3 ~ "Climate vulnerable",
                       TRUE ~ "Neither/no data")
  ) 


islands <- fish_clim_micdef %>%
  filter (Category != "Neither/no data",
          subregion %in% c ("Caribbean", "Polynesia", "Melanesia", "Micronesia"))

sf::sf_use_s2(FALSE)
islands_centroids <- cbind(islands, st_coordinates(st_centroid(islands))) 
islands_centroids <- islands_centroids %>%
  #something weird with kiribati
  mutate (X = ifelse (iso3 == "KIR", -168, X))
  
png ("Figures/Climate_vulnerable_fisheries_micronutrient_deficient.png", width = 14, height = 6, units = "in", res = 300)
ggplot (fish_clim_micdef) +
  geom_sf (aes (fill = as.factor(Category)), lwd = .25, col = "black") +
  scale_fill_manual (values = c( "red", "goldenrod1","hotpink1", "white")) +
  scale_color_manual (values = c( "red", "goldenrod1","hotpink1")) +
  facet_wrap (~rcp) +
  theme_bw() +
  labs (fill = "", x = "", y = "") +
  guides (color = "none") +
  ggtitle ("Climate vulnerable fisheries and micronutrient deficient \n Projected to lose > 50% of catch by 2100 under BAU mgmt, top 100 micronutrient deficient") +
  geom_label_repel (data = fortify (islands_centroids),
                    aes (label = name, x = X, y = Y, 
                         color = Category), 
                    size = 2.5, label.padding = 0.05,
                    max.overlaps = 50
  ) 

dev.off()

# now layer on blue food dependent ----

# one way that might be easiest to visualize is to take the subset of the red countries that are ALSO blue foods dependent
fish_clim_micdef_blue_subset <- world_gaines %>%
  mutate (Category = 
            case_when (clim_vuln == 1 & iso3 %in% mic_def$iso3 & !iso3 %in% blue$iso3  ~ "Clim vuln & micro def",
                       clim_vuln == 0 & iso3 %in% mic_def$iso3 ~ "Micronutrient deficient",
                       is.na (clim_vuln) & iso3 %in% mic_def$iso3 ~ "Micronutrient deficient",
                       clim_vuln == 1 & !iso3 %in% mic_def$iso3 ~ "Climate vulnerable",
                       clim_vuln ==1 & iso3 %in% mic_def$iso3 & iso3 %in% blue$iso3 ~ "Blue foods dependent",
                       is.na (clim_vuln) & !iso3 %in% mic_def$iso3 ~ "None/no data",
                       TRUE ~ "None/no data")
  ) 


# set order of levels
fish_clim_micdef_blue_subset$Category <- factor (fish_clim_micdef_blue_subset$Category, levels = c("Climate vulnerable", "Micronutrient deficient", "Clim vuln & micro def", "Blue foods dependent", "None/No data"))

islands <- fish_clim_micdef_blue_subset %>%
  filter (Category != "None/No data",
          subregion %in% c ("Caribbean", "Polynesia", "Melanesia", "Micronesia"))

sf::sf_use_s2(FALSE)
islands_centroids <- cbind(islands, st_coordinates(st_centroid(islands))) 
islands_centroids <- islands_centroids %>%
  #something weird with kiribati
  mutate (X = ifelse (iso3 == "KIR", -168, X))

png ("Figures/Climate_vulnerable_fisheries_micronutrient_deficient_bluefoods_dep_subset_islandlabels.png", width = 14, height = 6, units = "in", res = 300)
ggplot (fish_clim_micdef_blue_subset) +
  geom_sf (aes (fill = as.factor(Category)), lwd = .25, col = "black") +
  scale_fill_manual (values = c( "goldenrod1","hotpink1","red", "blue", "white")) +
  scale_color_manual (values = c( "goldenrod1","hotpink1", "red", "blue")) +
  facet_wrap (~rcp) +
  theme_bw() +
  labs (fill = "", x = "", y = "") +
  guides (color = "none") +
  ggtitle ("Climate vulnerable fisheries, micronutrient deficient, and blue foods dependent \n Projected to lose > 50% of catch by 2100 under BAU mgmt, top 100 micronutrient deficient, Golden/Selig blue foods dependence \n Blue is a subset of red") +
  geom_label_repel (data = fortify (islands_centroids),
                    aes (label = name, x = X, y = Y, 
                         color = Category), 
                    size = 2.5, label.padding = 0.05,
                    max.overlaps = 50
  ) 

dev.off()

# label "both" countries
both <- fish_clim_micdef_blue_subset %>%
  filter (Category == "Blue foods dependent")

both_centroids <- cbind(both, st_coordinates(st_centroid(both)))

png ("Figures/Climate_vulnerable_fisheries_micronutrient_deficient_bluefoods_dep_subset_bluelabels.png", width = 14, height = 6, units = "in", res = 300)
ggplot (fish_clim_micdef_blue_subset) +
  geom_sf (aes (fill = as.factor(Category)), lwd = .25, col = "black") +
  scale_fill_manual (values = c( "goldenrod1","hotpink1","red", "blue", "white")) +

  facet_wrap (~rcp) +
  theme_bw() +
  labs (fill = "", x = "", y = "") +
  guides (color = "none") +
  ggtitle ("Climate vulnerable fisheries, micronutrient deficient, and blue foods dependent \n Projected to lose > 50% of catch by 2100 under BAU mgmt, top 100 micronutrient deficient, Golden/Selig blue foods dependence \n Blue is a subset of red") +
  geom_label_repel (data = fortify (both_centroids),
                    aes (label = name, x = X, y = Y), 
                         color = "blue", 
                    size = 2.5, label.padding = 0.05,
                    max.overlaps = 50
  ) 

dev.off()
#########################################################################################################################

### MAP 2: Terrestrial food system climate vulnerability and food insecurity ----


# climate vulnerability ----

# GAIN index -- staple crops
GAIN <- read_sheet(ss = fspn_id, sheet = "GAIN Food sector vulnerability") 

# define most vulnerable by quantile? 10%, 25%, 50%

### set cutoff point. This will match food insecurity cutoff and map labels. Should be 0.25, 0.5, 0.1
cutoff_pt <- 0.5

GAIN_top <- GAIN %>%
  slice_max (GAIN_food_vuln, prop = cutoff_pt) # 48 in top 25%, 19 in 10%, 96 in top 50%

# Michelle Tigchelaar food systems paper--includes broader crops
tig_cluster <- read_sheet (ss = fspn_id, sheet = "Tigchelaar climate risk clusters") 

# tig_vuln <- tig_cluster %>%
#   filter (cluster %in% c(3, 4, 5)) # 46 countries in 3 & 5; 60 with category 4

# EDF interested in 3,4,5; maybe 2

# join climate vulnerability
clim_vuln <- GAIN %>%
  full_join (tig_cluster, by = "iso3") %>%
  # make indicator column
  mutate (top_vuln = ifelse (iso3 %in% GAIN_top$iso3 | cluster %in% c(3, 5, 4, 2), 1, 0)) %>%
  select (iso3, top_vuln) # 72 with GAIN top 25% and tig 3 and 5; 83 with 4

#################################################################
# food insecurity ----

# One option: Food Insecurity Experience (subjective, from Food Systems Dashboard)
food_insecure <- read_sheet (ss = fspn_id, sheet = "Food insecurity FSD") 

food_insecure_top <- food_insecure %>%
  slice_max (FIES, prop = 0.25)  # 30 in top 25%

food_insecure <- food_insecure %>% 
  # make indicator column
  mutate (top_insecure = ifelse(
    iso3 %in% food_insecure_top$iso3, 1, 0
  ))

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


### Prevalence of undernourishment ----
pou <- read_sheet (ss = fspn_id, sheet = "Prevalence of undernourishment FSD")

pou_top <- pou %>%
  slice_max (POU, prop = cutoff_pt)  # 42 in top 25%

pou <- pou %>% 
  # make indicator column
  mutate (top_pou = ifelse(
    iso3 %in% pou_top$iso3, 1, 0
  ))

pou_food_vuln <- pou %>%
  #join datasets together
  full_join (clim_vuln, by = "iso3") %>% 
  # replace NAs with 0? not sure if this is the right move. too many forms of NAs, will only show as gray/NA if there are no data from any of these sources
  replace_na(list(top_pou = 0, top_vuln = 0)) %>%
  # make category column by summing indicators
  #rowwise %>%
  mutate ( 
    Risk = as.factor (case_when (
      top_vuln == 1 & top_pou == 0 ~ "Climate vulnerable",
      top_vuln == 0 & top_pou == 1 ~ "Undernourished",
      top_vuln == 1 & top_pou == 1 ~ "Both",
      TRUE ~ "Neither")
    )
  )

pou_food_vuln$Risk <- factor (pou_food_vuln$Risk, levels = c("Climate vulnerable", "Undernourished", "Both", "Neither", "No data"))

world_pou_food_vuln <- merge (world, pou_food_vuln, all = TRUE) %>%
  replace_na (list (Risk = "No data")) 

# create subset of islands for separate labels
# https://dadascience.design/post/r-low-budget-high-res-mapping-with-r-for-not-for-profit-print/
islands <- world_pou_food_vuln %>%
  filter (Risk %in% c("Climate vulnerable", "Undernourished", "Both"),
          subregion %in% c ("Caribbean", "Polynesia", "Melanesia", "Micronesia"))

# centroids df for labels
# https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
# https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data

sf::sf_use_s2(FALSE)
islands_centroids <- cbind(islands, st_coordinates(st_centroid(islands))) # named X and Y
islands_centroids <- islands_centroids %>%
  #something weird with kiribati
  mutate (X = ifelse (iso3 == "KIR", -168, X))

# plot climate vulnerable only----

# set filenames based on cutoff point

png (paste0("Figures/Climate_vulnerable_", cutoff_pt * 100, "perc.png"), width = 14, height = 6, units = "in", res = 300)

ggplot(world_pou_food_vuln) +
  geom_sf (aes (fill = as.factor(top_vuln)), lwd = .25, col = "black") +
  scale_fill_manual (values = c(  "white", "goldenrod1")) +
  
  theme_bw() +
  labs (fill = "", x = "", y = "") +
  ggtitle (
  paste0(
  "Climate vulnerable terrestrial food systems \n Top ", cutoff_pt * 100, "% GAIN Index and Tigchelaar clusters 2,3,4,5")) +
  guides (fill = "none") +
  theme (plot.title = element_text (hjust = 0.5, size = 16),
         legend.text = element_text (size = 12)
  )

dev.off()

# plot pou only ----
png (paste0("Figures/POU_", cutoff_pt * 100, "perc.png"), width = 14, height = 6, units = "in", res = 300)

ggplot(world_pou_food_vuln) +
  geom_sf (aes (fill = as.factor(top_pou)), lwd = .25, col = "black") +
  scale_fill_manual (values = c("white", "hotpink1")) +
  
  theme_bw() +
  labs (fill = "", x = "", y = "") +
  ggtitle (paste0("Top ", cutoff_pt *100, "% Prevalence of Undernourishment")) +
  guides (fill = "none") +
  theme (plot.title = element_text (hjust = 0.5, size = 16),
         legend.text = element_text (size = 12)
  )

dev.off()



# plot combined with BOTH labelled----
both_food <- world_pou_food_vuln %>%
  filter (Risk %in% c("Both"))

sf::sf_use_s2(FALSE)
both_food_centroids <- cbind(both_food, st_coordinates(st_centroid(both_food))) # named X and Y

png (paste0("Figures/Food_pou_climate_vulnerable_both_", cutoff_pt * 100, "perc.png"), width = 14, height = 6, units = "in", res = 300)
ggplot (data = world_pou_food_vuln) +
  geom_sf (aes (fill = as.factor(Risk)), lwd = .25, col = "black") +
  scale_fill_manual (values = c( "goldenrod1","hotpink1","red", "gray70", "white")) +
  scale_color_manual (values = c( "goldenrod1","hotpink1","red")) +
  theme_bw() +
  labs (fill = "", x = "", y = "") +
  ggtitle (paste0("Prevalance of undernourishment & climate vulnerability, top ", cutoff_pt * 100, "% \n GAIN index and Tigchelaar med-high risk clusters")) +
  geom_label_repel (data = fortify (both_food_centroids),
                    aes (label = name, x = X, y = Y), 
                    color = "red",
                    size = 2.5, label.padding = 0.05,
                    max.overlaps = 50) +
  guides (color = "none") +
  theme (plot.title = element_text (hjust = 0.5, size = 16),
         legend.text = element_text (size = 12))
dev.off()



#############################################################################################################################
# Plot single variable maps to compare metrics ----
#############################################################################################################################
dir.create ("Figures/Maps_singlevar")

# Climate vulnerability: GAIN index ----
GAIN <- read_sheet(ss = fspn_id, sheet = "GAIN Food sector vulnerability") 

# plot 50%, 10%, 25% top?
# https://stackoverflow.com/questions/69815942/using-cut-and-quantile-to-bucket-continuous-columns-in-r
world_GAIN <- merge (world, GAIN, all = TRUE) %>%
  # create categorical variable to show 50, 25, 10% cutoffs
  mutate (quantile_cat = cut (GAIN_food_vuln, 
                              breaks = quantile (GAIN_food_vuln, probs = c(0, 0.5, 0.75, 0.9, 1), na.rm = TRUE),
                              labels = c ("Low", "Top 50%", "Top 25%", "Top 10%")
                              )
  )

islands <- world_GAIN %>%
  filter (subregion %in% c ("Caribbean", "Polynesia", "Melanesia", "Micronesia"))

# centroids df for labels
# https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
# https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data

sf::sf_use_s2(FALSE)
islands_centroids <- cbind(islands, st_coordinates(st_centroid(islands))) # named X and Y
islands_centroids <- islands_centroids %>%
  filter (!is.na (GAIN_food_vuln), !is.na (quantile_cat), quantile_cat != "Low") %>%
  #something weird with kiribati
  mutate (X = ifelse (iso3 == "KIR", -168, X))


png ("Figures/Maps_singlevar/Map_GAIN_food_vuln_islandlabels_cutoffquantiles.png", width = 14, height = 6, units = "in", res = 300)
ggplot (data = world_GAIN) +
  geom_sf (aes (fill = quantile_cat, color = quantile_cat), lwd = .25, col = "black") +
  scale_fill_manual(values = c("white", "goldenrod1", "goldenrod3", "goldenrod4")) +
  scale_color_manual(values = c("goldenrod1", "goldenrod3", "goldenrod4")) +
  theme_bw() +
  labs (fill = "", x = "", y = "") +
  ggtitle ("GAIN Index food system vulnerability") +
  geom_label_repel (data = fortify (islands_centroids),
                    aes (label = name, x = X, y = Y, 
                         color = quantile_cat), 
                    size = 2.5, label.padding = 0.05,
                    max.overlaps = 50
  ) +
  guides (color = "none") +
  theme (plot.title = element_text (hjust = 0.5, size = 16),
         legend.text = element_text (size = 12))
dev.off()

# Climate vulnerability: Tigchelaar clusters ----
tig_cluster <- read_sheet (ss = fspn_id, sheet = "Tigchelaar climate risk clusters") 
world_tig <- merge (world, tig_cluster, all = TRUE)

# set order of levels in order of EDF priority
tig_cluster$cluster_longname <- factor (tig_cluster$cluster_longname, levels = c("High vulnerability; dependent on domestic production; high hazards", "Import-dependent; high vulnerability", "Medium-high vulnerability; dependent on domestic production; low hazards", "Low-medium vulnerability; big producer; high hazards", "Low vulnerability; medium exposure to global markets", "Import dependent, low vulnerability"))

islands <- world_tig %>%
  filter (subregion %in% c ("Caribbean", "Polynesia", "Melanesia", "Micronesia"))
sf::sf_use_s2(FALSE)
islands_centroids <- cbind(islands, st_coordinates(st_centroid(islands))) # named X and Y
islands_centroids <- islands_centroids %>%
  filter (cluster %in% c(2,3,4,5)) %>%
  #something weird with kiribati
  mutate (X = ifelse (iso3 == "KIR", -168, X))

png ("Figures/Maps_singlevar/Map_Tigchelaar_clusters_islandlabels.png", width = 14, height = 6, units = "in", res = 300)
ggplot (data = world_tig) +
  geom_sf (aes (fill = cluster_longname), lwd = .25, col = "black") +
  # not respecting levels; set breaks manually
  scale_fill_manual(breaks = c ("High vulnerability; dependent on domestic production; high hazards", "Import-dependent; high vulnerability", "Medium-high vulnerability; dependent on domestic production; low hazards", "Low-medium vulnerability; big producer; high hazards", "Low vulnerability; medium exposure to global markets", "Import dependent, low vulnerability"), values = c("goldenrod4", "darkorange3", "goldenrod3", "goldenrod1", "cadetblue4", "cadetblue")) +
  scale_color_manual(c("Import dependent, high vulnerability", "Low-medium vulnerability; big producer; high hazards"), 
                     values = c("darkorange3", "goldenrod1")) +
  theme_bw() +
  labs (fill = "", x = "", y = "") +
  ggtitle ("Tigchelaar in prep food system risk clusters \n yellow/brown are EDF priority clusters") +
  geom_label_repel (data = fortify (islands_centroids),
                    aes (label = name, x = X, y = Y, 
                         color = cluster_longname), 
                    size = 2.5, label.padding = 0.05,
                    max.overlaps = 50
  ) +
  guides (color = "none") +
  theme (plot.title = element_text (hjust = 0.5, size = 16),
         legend.text = element_text (size = 10))
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

# prevalence of undernourishment, POU, FSD----
pou <- read_sheet (ss = fspn_id, sheet = "Prevalence of undernourishment FSD")

# plot 50%, 10%, 25% top?
# https://stackoverflow.com/questions/69815942/using-cut-and-quantile-to-bucket-continuous-columns-in-r
world_pou <- merge (world, pou, all = TRUE) %>%
  # create categorical variable to show 50, 25, 10% cutoffs
  mutate (quantile_cat = cut (POU, 
                              breaks = quantile (POU, probs = c(0, 0.5, 0.75, 0.9, 1), na.rm = TRUE),
                              labels = c ("Low", "Top 50%", "Top 25%", "Top 10%")
  )
  )

islands <- world_pou %>%
  filter (subregion %in% c ("Caribbean", "Polynesia", "Melanesia", "Micronesia"))

# centroids df for labels
# https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
# https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data

sf::sf_use_s2(FALSE)
islands_centroids <- cbind(islands, st_coordinates(st_centroid(islands))) # named X and Y
islands_centroids <- islands_centroids %>%
  filter (!is.na (POU), !is.na (quantile_cat), quantile_cat != "Low") %>%
  #something weird with kiribati
  mutate (X = ifelse (iso3 == "KIR", -168, X))


png ("Figures/Maps_singlevar/Map_POU_islandlabels_cutoffquantiles.png", width = 14, height = 6, units = "in", res = 300)
ggplot (data = world_pou) +
  geom_sf (aes (fill = quantile_cat, color = quantile_cat), lwd = .25, col = "black") +
  scale_fill_manual(values = c("white", "hotpink", "hotpink3", "hotpink4")) +
  scale_color_manual(values = c("hotpink1", "hotpink3", "hotpink4")) +
  theme_bw() +
  labs (fill = "", x = "", y = "") +
  ggtitle ("Prevalence of Undernourishment") +
  geom_label_repel (data = fortify (islands_centroids),
                    aes (label = name, x = X, y = Y, 
                         color = quantile_cat), 
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

