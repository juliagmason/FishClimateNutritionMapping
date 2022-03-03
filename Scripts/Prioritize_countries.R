## Determine priority countries
# 5 26 2021
# JGM


# compile lists of priority countries based on different metrics/datasets

library (tidyverse)
library (googlesheets4)

# function for copying R output tables into word/excel----
#https://stackoverflow.com/questions/24704344/copy-an-r-data-frame-to-an-excel-spreadsheet
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}


# All metrics combined google sheet

# id url code for our Priority Mapping google sheet
fspn_id <- "1H_tri9xS3Ypl6ncrU9gGpseJmia_JVykOsXc950zEp8"

# country codes compiled from our aggregation in the google sheet. 
# iso3_terr has codes for territories; this means there are repeats in the iso3 column
codes <- read_sheet(ss = fspn_id, sheet = "Country_codes") 

AMC <- read_sheet (ss = fspn_id, sheet = "All metrics combined") %>%
  arrange (Country)

# manually add country codes
codes_AMC <- codes$iso3[-which(codes$iso3 %in% c("USA-VI", "USA-HUN"))]
AMC$iso3 <- codes_AMC

# get rid of NA iso3 bc creating confusion when I left_join
AMC <- AMC %>%
  filter (!is.na (iso3))


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
