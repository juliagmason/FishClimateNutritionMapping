# Board food committee metrics 
# 5/23/23
# JGM

# see also: https://ourworldindata.org/hunger-and-undernourishment

# drawing from plot_maps_from_googlesheet but don't want to clutter

library (tidyverse)
library (rnaturalearth)
library (rnaturalearthdata)
library (googlesheets4)

# make world map
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  # rename country code column to match our data
  rename (iso3 = iso_a3)


# pull data from google sheet ----
# id url code for our Priority Mapping google sheet
#https://docs.google.com/spreadsheets/d/1H_tri9xS3Ypl6ncrU9gGpseJmia_JVykOsXc950zEp8/edit#gid=742482966
fspn_id <- "1H_tri9xS3Ypl6ncrU9gGpseJmia_JVykOsXc950zEp8"

# country codes compiled from our aggregation in the google sheet. 
# iso3_terr has codes for territories; this means there are repeats in the iso3 column

codes <- read_sheet(ss = fspn_id, sheet = "Country_codes") 


# Prevalence of Undernourishment ----

#The prevalence of undernourishment (PoU) is an estimate of the proportion of the population whose habitual food consumption is insufficient to provide the dietary energy levels that are required to maintain a normal active and healthy life. It is expressed as a percentage. This indicator will measure progress towards SDG Target 2.1.
pou <- read_sheet (ss = fspn_id, sheet = "Prevalence of undernourishment FSD")

world_pou <- merge (world, pou, all = TRUE) 



ggplot(world_pou) +
  geom_sf (aes (fill = POU), lwd = .25) +
  scale_fill_viridis_c(option = "magma", direction = -1)+
  theme_classic() +
  ggtitle ("Prevalance of Undernourishment") +
  labs (fill = "% undernourished", x = "", y = "") +
  theme (plot.title = element_text (hjust = 0.5, size = 16),
         legend.text = element_text (size = 12)
  )


# GAIN vulnerability index ----
GAIN <- read_sheet(ss = fspn_id, sheet = "GAIN Food sector vulnerability") 
world_gain <- merge (world, GAIN, all = TRUE)

ggplot(world_gain) +
  geom_sf (aes (fill = GAIN_food_vuln), lwd = .25, col = "gray20") +
  scale_fill_viridis_c(option = "magma", direction = -1)+
  theme_classic() +
  ggtitle ("GAIN index of staple crop climate vulnerability") +
  labs (fill = "Index", x = "", y = "") +
  theme (plot.title = element_text (hjust = 0.5, size = 16),
         legend.text = element_text (size = 12)
  )


# GAIN water index ----
# downloaded 5/24/23 https://gain.nd.edu/our-work/country-index/methodology/
gain_water <- read.csv("Data/GAIN_water_vulnerability.csv") %>%
  select (ISO3, Name, X2020) %>%
  rename (iso3 = ISO3,
          gain_water = X2020)

world_gain_water <- merge (world, gain_water, all = TRUE)

ggplot(world_gain_water) +
  geom_sf (aes (fill = gain_water), lwd = .25, col = "gray20") +
  scale_fill_viridis_c(option = "magma", direction = -1)+
  theme_classic() +
  ggtitle ("GAIN index of water sector climate vulnerability") +
  labs (fill = "Index", x = "", y = "") +
  theme (plot.title = element_text (hjust = 0.5, size = 16),
         legend.text = element_text (size = 12)
  )


# Loss of fish resources ----
#https://github.com/cfree14/aquacast/blob/master/code/ms_figures/FigS17_eezs_analyzed.R

# for first pass, take the more vulnerable, plot in country land
mcp <- read_sheet (ss = fspn_id, sheet = "Maximum catch potential (Cheung)")

mcp_85 <- mcp %>%
  select (Country, "RCP 8.5 2100 avg") %>%
  rename (fish_loss = "RCP 8.5 2100 avg")  %>%
  mutate (
    # remove any parentheses that split EEZs into multiple regions. will take an average or a max
    Country = gsub ("\\(.*", "", Country),
    Country = gsub ("\\\n", "", Country),
    Country = gsub ("-", " ", Country),
    # remove trailing spaces
    Country = trimws(Country),
    Country = case_when (
      Country == "Andaman and Nicobar Islands" ~ "Andaman and Nicobar",
      Country == "Ascension Island" ~ "Ascension",
      Country == "British Virgin Island" ~ "British Virgin Islands",
      Country == "Brunei Darussalam" ~ "Brunei",
      Country == "Cabo Verde" ~ "Cape Verde",
      Country == "Cocos" ~ "Cocos Islands",
      Country == "Comoros" ~ "Comoro Islands",
      Country == "Congo" ~ "Republique du Congo",
      Country == "Côte d’Ivoire" ~ "Cote d’Ivoire",
      Country == "Faroe Islands" ~ "Faeroe Islands",
      Country == "French Guyana" ~ "French Guiana",
      Country == "Îles Glorieuses" ~ "Glorioso Islands",
      Country == "Marshall Island" ~ "Marshall Islands",
      Country == "Pitcairn Islands" ~ "Pitcairn",
      Country == "Prince Edward Island" ~ "Prince Edward Islands",
      Country == "Viet Nam" ~ "Vietnam",
      Country == "United States Virgin Islands" ~ "Virgin Islands of the United States",
      Country == "United States of America" ~ "United States",
      Country == "United Republic of Tanzania" ~ "Tanzania",
      Country == "Taiwan Province of China" ~ "Taiwan",
      Country == "Republic of Korea" ~ "South Korea",
      Country == "Democratic People’s Republic of Korea" ~ "North Korea",
      Country == "Réunion" ~ "Reunion",
      Country == "Russian Federation" ~ "Russia",
      Country == "Saint Barthélemy" ~ "Saint Barthelemy",
      Country == "Sint Eustatius and Saba" ~ "Sint Eustasius",
      Country == "St Paul and Amsterdam Islands" ~ "Amsterdam Island and Saint Paul Island",
      Country == "Trindade and Martim Vaz Island" ~ "Trindade",
      Country == "Tristan da Cunha Island" ~ "Tristan da Cunha",
      Country == "Tromelin Island" ~ "Ile Tromelin",
      Country == "Curaçao" ~ "Curacao",
      Country == "Wallis and Futuna Island" ~ "Wallis and Futuna",
      TRUE ~ Country
      
    )
  ) %>%
  # take most vulnerable for repeated countries
  group_by (Country) %>%
  summarize (fish_loss = min (fish_loss)) %>%
  left_join (codes, by = "Country")

world_mcp_85  <- merge (world, mcp_85, all = TRUE)  %>%
  mutate (fish_loss_cut = cut(fish_loss, breaks = c (-100, -75, -50, -25, 0, 25, 50, 75, 800),
                              labels = c ("< -75%", "-50%", "-25%", "0%", "25%", "50%", "75%", ">100%")
                              )
          ) 

# grabbing centroid code from plot_maps_from_googlesheet
# can't figure out how to fix color scale for subset of islands yet. 
library (ggrepel)

islands <- world_mcp_85 %>%
  filter (subregion %in% c ("Caribbean", "Polynesia", "Melanesia", "Micronesia"))

sf::sf_use_s2(FALSE)
islands_centroids <- cbind(islands, st_coordinates(st_centroid(islands))) # named X and Y
islands_centroids <- islands_centroids %>%
  #filter (fish_loss_cut %in% c("< -75%", "-50%", "-25%")) %>%
  #something weird with kiribati
  mutate (X = ifelse (iso3 == "KIR", -168, X))

world_mcp_85 %>%
ggplot() +
  geom_sf (aes (fill = fish_loss_cut), lwd = .15, col = "grey20") +
  scale_fill_viridis_d(option = "magma")+
  geom_label_repel (data = fortify (islands_centroids),
                    aes (label = name, x = X, y = Y, 
                         color = fish_loss_cut), 
                    size = 2.5, label.padding = 0.05, max.overlaps = 50
  ) +
  scale_color_viridis_d(option = "magma")+
  
  theme_classic() +
  ggtitle (label = "Projected % change in fish catch potential by 2100") +
  labs (fill = "% change", x = "", y = "") +
  theme (plot.title = element_text (hjust = 0.5, size = 20),
         legend.text = element_text (size = 14),
         legend.title = element_text (size = 16)
  )

# dependence on aquatic foods ----
aquatic_food_dep <- read.csv("Data/DO_NOT_SHARE_seafood_nutrient_supply_GND_012021.csv") %>%
  rename (iso3 = iso3c)

prot_dep_golden <- aquatic_food_dep %>%
  filter (nutrient == "Protein")

world_aquatic_prot_dep <- merge (world, prot_dep_golden, all = TRUE)

world_aquatic_prot_dep %>%
  ggplot() +
  geom_sf (aes (fill = prop_aquatic), lwd = .15, col = "grey20") +
  scale_fill_viridis_c(option = "magma", direction = -1)+
  
  theme_classic() +
  ggtitle ("% of protein supply derived from aquatic foods") +
  labs (fill = "", x = "", y = "") +
  theme (plot.title = element_text (hjust = 0.5, size = 16),
         legend.text = element_text (size = 12)
  )

omg_dep_golden <- aquatic_food_dep %>%
  filter (nutrient == "Omega-3 fatty acids")

world_aquatic_omg_dep <- merge (world, omg_dep_golden, all = TRUE)

# grabbing centroid code from plot_maps_from_googlesheet
library (ggrepel)

islands <- world_aquatic_omg_dep %>%
  filter (subregion %in% c ("Caribbean", "Polynesia", "Melanesia", "Micronesia"))

sf::sf_use_s2(FALSE)
islands_centroids <- cbind(islands, st_coordinates(st_centroid(islands))) # named X and Y
islands_centroids <- islands_centroids %>%
  filter (!is.na(prop_aquatic)) %>%
  #something weird with kiribati
  mutate (X = ifelse (iso3 == "KIR", -168, X))

world_aquatic_omg_dep %>%
  ggplot() +
  geom_sf (aes (fill = prop_aquatic), lwd = .15, col = "grey20") +
  scale_fill_viridis_c(option = "magma", direction = -1)+
  
  theme_classic() +
  ggtitle ("% of Omega 3 supply derived from aquatic foods") +
  geom_label_repel (data = fortify (islands_centroids),
                    aes (label = name, x = X, y = Y, 
                         color = prop_aquatic), 
                    size = 2.5, label.padding = 0.05, max.overlaps = 50
  ) +
  scale_color_viridis_c(option = "magma", direction = -1)+
  labs (fill = "", x = "", y = "") +
  guides (color = "none") +
  theme (plot.title = element_text (hjust = 0.5, size = 16),
         legend.text = element_text (size = 12)
  )
