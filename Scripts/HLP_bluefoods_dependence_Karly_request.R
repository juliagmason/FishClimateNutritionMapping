# Blue foods dependence of HLP countries
# 2/14/23

# request from Karly Kelso to pull blue foods dependence for HLP countries; she provided excel sheet

hlp <- read_csv ("Data/HLP_countries.csv")

### micronutrient dependence on fish:
# From Chris Golden, proprietary data [JM has this csv, but was asked not to share, so I put rankings in the google doc. can share for internal EDF usage]

# 7 columns. Iso3c (3 letter country code), nutrient, total terrestrial (amt available from land), total_aquatic (amt available from aquatic), total (sum of total terrestrial and total aquatic), prop_terrestrial, and prop_aquatic. 
# I only care about iso3c, nutrient, and prop_aquatic, and want to rename the country column to match mic_def
# [JE only] setwd("~/Documents/ACTIVE Research/EDF/Projects/Climate Resilient Food System/R")
mic_dep <- read_csv ("Data/DO_NOT_SHARE_seafood_nutrient_supply_GND_012021.csv") %>%
  select (iso3c, nutrient, prop_aquatic) %>%
  rename (iso3 = iso3c)

# choose one nutrient?
mic_dep %>%
  group_by (nutrient) %>%
  summarise (mean = mean (prop_aquatic, na.rm = TRUE))

ca <- mic_dep %>%
  filter (nutrient == "Calcium")

ca_rank <- ca %>%
  mutate (rank = rank (-prop_aquatic))

mic_dep_rank <- mic_dep %>%
  group_by (nutrient) %>%
  mutate (rank = rank (-prop_aquatic))

mic_dep_rank %>%
  filter (iso3 %in% hlp$iso_3) %>% View()

# mean prop first, then rank
rank_mean_nutrient <- mic_dep %>%
  group_by (iso3) %>%
  summarise (mean_prop = mean (prop_aquatic, na.rm = TRUE)) %>%
  mutate (rank = rank (-mean_prop))
#roughly works with overall rank

# method we used for google sheet; this is exactly the same as google sheet
mic_dep_rank_sum  <- mic_dep_rank %>%
  group_by (iso3) %>%
  summarise (rank_sum = sum (rank)) %>%
  mutate (Micronutrient_dependence = rank (rank_sum))
  
# just use overall rank from google sheet
mic_prop_aquatic_rank <- read_sheet(ss = fspn_id, sheet = "Aquatic nutrient contribution") %>%
  select (Country, `Overall rank`) %>%
  rename (Micronutrient_dependence = `Overall rank`)

# protein Selig from google sheets or Selig SI
# United States, Great Britain
protein_selig <- read_csv ("Data/Selig_SI_ocean_dependence_rankings.csv") %>%
  mutate (Country = case_when (
    Country == "Great Britain" ~ "United Kingdom",
    Country == "United States" ~ "United States of America",
    TRUE ~ Country
  )) %>%
  select (Country, `Nutri rank`) %>%
  rename (Protein_dependence = `Nutri rank`)

t <- hlp %>%
  left_join (protein_selig, by = "Country") %>%
  left_join (mic_prop_aquatic_rank, by = "Country")

write.csv (t, file = "Data/HLP_countries_dependence.csv", row.names = FALSE)

h <- hlp %>%
  rename (iso3 = iso_3) %>%
  left_join (mic_dep, by = "iso3")
