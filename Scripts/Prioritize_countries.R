## Determine priority countries
# 5 26 2021
# JGM

# updated 3/23/22; pull from crfs_mapping_ds.Rds; made in Compile_data_from_googlesheet.R

library (tidyverse)

# function for copying R output tables into word/excel----
#https://stackoverflow.com/questions/24704344/copy-an-r-data-frame-to-an-excel-spreadsheet
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

crfs_mapping_ds <- readRDS ("Data/crfs_mapping_ds.Rds")

# set cutoff point for continuous data and grab vulnerable countries ----
cutoff_pt <- 0.25

GAIN_top <- crfs_mapping_ds %>%
  select (iso3, GAIN_food_vuln) %>%
  slice_max (GAIN_food_vuln, prop = cutoff_pt)

pou_top <- crfs_mapping_ds %>%
  select (iso3, POU) %>%
  slice_max (POU, prop = cutoff_pt)

# create columns based on criteria from March 2022 meetings ----

crfs_priorities <- crfs_mapping_ds %>%
  mutate (
    # climate vulnerable fisheries: if lose > 50% catch under rcp 6.0
    aquatic_clim_vuln = ifelse (catch_pdiff_BAU_85 < -0.5, 1, 0),
    # terrestrial climate vulnerability: GAIN cutoff and priority tigchelaar clusters
    terrestrial_clim_vuln = ifelse (iso3 %in% GAIN_top$iso3 | cluster %in% c(3, 5, 4, 2), 1, 0), 
    undernourished = ifelse (iso3 %in% pou_top$iso3, 1, 0),
    # using top 100 micronutrient deficient from golden
    micronutrient_def = ifelse (mic_def_golden <= 100, 1, 0)
  ) 
    

# grab lists of countries that meet criteria ----
t_yellow <- crfs_priorities %>%
  filter (terrestrial_clim_vuln == 1) %>%
  pull (Country)

write.excel(t_yellow)
    
pou_pink <- crfs_priorities %>%
  filter (undernourished == 1) %>%
  pull (Country)

write.excel(pou_pink)

t_both_red <- crfs_priorities %>%
  filter (terrestrial_clim_vuln == 1 & undernourished == 1) %>%
  pull (Country)


write.excel (t_both_red)


a_climvuln_yellow <- crfs_priorities %>%
  filter (aquatic_clim_vuln == 1) %>%
  pull (Country)

write.excel (a_climvuln_yellow)

a_micdef <- crfs_priorities %>%
  filter (micronutrient_def == 1) %>%
  pull (Country)

write.excel(a_micdef)

a_both <- crfs_priorities %>%
  filter (aquatic_clim_vuln == 1 & micronutrient_def == 1) %>%
  pull (Country) 

write.excel(a_both)

a_triple <- crfs_priorities %>%
  filter (aquatic_clim_vuln == 1 & micronutrient_def ==1 & blue_dep == 1) %>%
  pull (Country)

write.excel (a_triple)

b <- crfs_priorities %>%
  filter (blue_dep == 1) %>%
  pull (Country)

write.excel (b)
