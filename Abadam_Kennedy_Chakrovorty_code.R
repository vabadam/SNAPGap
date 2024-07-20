#######################################################################################
# Viz Title: Tracking the LRFS SNAP Gap: SNAP Hot spots relative to farmers market 
#            in the Southeast Region  
# Group Members: Vidalina Abadam (George Mason University)
#                Jennifer Kennedy (North Carolina A&T State University) 
#                Sanchita Chakrovorty (University of Georgia) 
#
# Outcome 2: Create more and better markets and increase market awareness and access
# Indicator 1: Trends in U.S. FMPP Grants with SNAP/EBT-Component
# Indicator 2: Share of SNAP-accepting FM to total FM in 2018, by State
# Indicator 3: Map overlay SNAP Hot spots and FMs location 
#
# Code developed by Vidalina Abadam
# Vidalina Abadam.(July 2024). LRFS SNAP Gap, (Version 1). USDA AMS Data Warehouse.
#######################################################################################


#######################################################################################
# Indicator 1: Trends in U.S. FMPP Grants with SNAP/EBT-Component
# Data Source: LAMP Navigator's Public LAMP Dataset, 2006-2023
# https://www.ams.usda.gov/sites/default/files/media/LAMPDatasetandDataDictionary.xlsx
#######################################################################################
library(stringr, quietly = TRUE) 
library(dplyr, quietly = TRUE) 

setwd("C:/AMS Viz Challenge/Southeast_SNAPGAP/Data/LAMP")

# Read public LAMP dataset
lamp1 <- read_excel("LAMPDatasetandDataDictionary.xlsx", 
                 sheet = "LAMP Dataset 2006 to 2023")

# Assign SNAP and EBT identifier; avoid double-count; limit to FMPP grants
lamp2 <- lamp1 %>%
  mutate(SNAP = ifelse(str_detect(`Project Abstract/Goal`, "(?i)SNAP"), 1, 0)) %>%
  mutate(EBT = ifelse(str_detect(`Project Abstract/Goal`, "(?i)EBT"), 1, 0))
table(lamp2$SNAP)
table(lamp2$EBT)
  
# Combine SNAP and EBT identifier to avoid double-count
lamp3 <- lamp2 %>%
  mutate(EBT.SNAP = ifelse(lamp2$SNAP==1 | lamp2$EBT==1, 1, 0)) 
table(lamp3$EBT.SNAP)

# Create summary table for viz only for FMPP grants (since FM focus) with SNAP|EBT component
ind1_sum1 <- lamp3 %>%
  filter(`Grant Type`== "FMPP") %>%
  filter(EBT.SNAP == 1) %>%
  select(Year, `Award Amount`, `Project Count`) %>% 
  group_by(Year) %>% 
  summarise(across(`Award Amount`:`Project Count`, sum))

# Adjust for inflation;calculate real value in 2006 dollars
inf <- read_excel("Inflation_Indexed_2006.xlsx", 
                    sheet = "Inflation")

ind1_sum2 <- left_join(ind1_sum1, inf, 
                             by=c('Year'='Year')) %>% 
  mutate(AwardAmount_Real2006Dollars = `Award Amount` * Index_2006_100)

# Export indicator #1 summary table
write.csv(ind1_sum2, "C:/AMS Viz Challenge/Southeast_SNAPGAP/Output/indicator1_summary_table.csv")

#########################################################################################################
# Indicator 2: Share of SNAP-accepting FM to total FM in 2018, by State
# Data Source: USDA Food Environment Atlas (Last updated 9/10/2020)
# https://www.ers.usda.gov/data-products/food-environment-atlas/data-access-and-documentation-downloads/
#########################################################################################################

setwd("C:/AMS Viz Challenge/Southeast_SNAPGAP/Data/Atlas")

# Read LOCAL dataset
local1 <- read_excel("FoodEnvironmentAtlas.xls", 
                    sheet = "LOCAL") 

# Get the latest year (2018) for SNAP-accepting FM; remove 2 obs with County==NA 
ind2 <- local1 %>% 
  select(State, County, FMRKT18, FMRKT_SNAP18) %>% 
  filter(County!="NA" & County!= "Wade Hampton") %>% 
  group_by(State) %>% 
  summarise(across(FMRKT18:FMRKT_SNAP18, sum)) %>% 
  adorn_totals("row") %>%
  mutate (percent.snap = 100*(FMRKT_SNAP18/FMRKT18)) 

# Rename to "All+DC"
ind2[52, "State"] <- "All+DC"

# Sort percent.snap by descending order
ind2 <- ind2[order(ind2$percent.snap, decreasing = TRUE),]  

# Export indicator #2 summary table
write.csv(ind2, "C:/AMS Viz Challenge/Southeast_SNAPGAP/Output/indicator2_summary_table.csv")


########################################################################################
# Indicator 3: Overlay of SNAP Hotspots and Location of FMs
# Reference: https://allison-bauman.quarto.pub/usda-ams-datametrics/localfoodsales.html
########################################################################################

###########################################################################################################
# 3A Get FM point data from Data Warehouse
# Data Source: Data Warehouse (https://allison-bauman.quarto.pub/usda-ams-datametrics/localfoodsales.html)
###########################################################################################################

##############################################
# 3A.1 State and county data
##############################################
library(tidyverse, quietly = TRUE)
library(janitor, quietly = TRUE)
library(sf, quietly = TRUE)
library(sp, quietly = TRUE)
library(magrittr, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(tidyr, quietly = TRUE)

# Get county and state fips, state name, county name 
county <- tidycensus::fips_codes %>% 
  unite("fips", 
        c(state_code, county_code), 
        sep = "", remove = FALSE) %>% 
  rename(county_name = county) %>% 
  select(fips, county_name, state_name)

# Add 02010 Aleutian Islands Census Area, Alaska
county <- county %>% 
  add_row(fips = "02010", 
          county_name = "Aleutian Islands Census Area", 
          state_name = "Alaska", 
          .after = 67)

state <- tidycensus::fips_codes %>% 
  select(state_code, state_name) %>% 
  rename(fips = state_code) %>% distinct()

# Merge so we have county and state data in one data frame
county_state <- bind_rows(county, state)

# Manually add US as fips "00"
county_state <- county_state %>% 
  add_row(fips = "00", 
          county_name = NA, 
          state_name = "US")

# Import county spatial data frame
county_sf <- tigris::counties(progress_bar = FALSE) %>% 
  clean_names()

############################################################
# 3A.2 Extract farmers markets; Add SNAP/EBT identifier
############################################################
library(readxl, quietly = TRUE)
library(sf, quietly = TRUE)
library(lubridate, quietly = TRUE)
library(purrr, quietly = TRUE)
library(stringr, quietly = TRUE) 

# Import and bind data in one data frame, add file name to indicate type
file_list <- fs::dir_ls(path = "C:/AMS Viz Challenge/Southeast_SNAPGAP/Data/FAME/localfoodsales/AMS_directory")

df <- file_list %>% 
  map(read_xlsx) %>%
  bind_rows(.id = "variable_name") %>% 
  mutate(
    variable_name = str_remove(variable_name, 
                               "C:/AMS Viz Challenge/Southeast_SNAPGAP/Data/FAME/localfoodsales/AMS_directory/"), 
    variable_name = str_remove(variable_name, ".xlsx"))

# Drop obs. not updated since 2020. Keep SNAP-related variables.
df <- df %>% 
  mutate(
    update_time = as_date(update_time)) %>% filter(
      update_time>"2020-08-30", 
      !is.na(location_x), !is.na(location_y)) %>% 
  select(
    variable_name, listing_id, variable_name, 
    listing_name, location_address, location_x, location_y, 
    FNAP, FNAP_1, FNAP_2, FNAP_3, FNAP_888, FNAP_4, FNAP_5,
    FNAP_3_desc, FNAP_other_desc) 

# listing_id 308158 has a comma at the end of location_y, remove this
df <- df %>% 
  mutate(location_y = str_remove(location_y, ","))

# Rename x and y, lat and long and make numeric
df <- df %>% 
  rename(lat = location_y,
         long = location_x) %>% 
  mutate(lat = as.numeric(lat), 
         long = as.numeric(long))

sum(is.na(df$lat))
sum(is.na(df$long))

## Get FIPS codes from lat/long data using the spatial county data from the Tigris package, defined above as county_sf
# make data frame into a spatial data frame and keep original lat/long variables
df_sf <- df %>% 
  st_as_sf(coords = c("long", "lat"),
           crs = st_crs(county_sf), 
           remove = FALSE)

# intersect our spatial point-level data with the tigris county spatial data frame 
intersected <- st_intersects(df_sf, county_sf)

# get the fips code for each entry
# Cambridge Farmers' Market, listing id 309678 did not have a fips, added manually
df_sf <- df_sf %>%
  mutate(
    intersection = as.integer(intersected), 
    fips = county_sf$geoid[intersection], 
    fips = case_when(listing_id=="309678" ~ "16087", 
                     TRUE ~ fips))
rm(intersected)

# Turn back into a regular data frame
df <- as_tibble(df_sf) %>%
  select(!c(listing_id, geometry, intersection))

# Group by variable name and join with county data so we can add a 0 value for those counties for each variable
# group by variable name and nest
nested_df <- df %>% 
  group_by(variable_name) %>% 
  nest()

# Join nested data frame by fips for each variable and unnest and add a count
nested_df <- nested_df %>% 
  mutate(data = map(data , ~ full_join(., county, by = "fips"))) %>% 
  unnest(cols = data) %>% 
  mutate(value = 
           case_when(is.na(listing_name) & is.na(location_address) ~ 0, 
                     TRUE ~1)) 
sum(is.na(nested_df$lat))
sum(is.na(nested_df$long))
  
# Convert snap variables from char to numeric, then replace NA with 0
nested_df <- nested_df %>% 
  mutate_at(c(7:12), as.numeric)  

nested_df[, 7:12][is.na(nested_df[, 7:12])] <- 0

# Generate value_allsnap and value_ebtsnap to create subset
# Note: FNAP_2 = accept EBT; validate with table  
nested_df$value_allsnap <- rep(0, nrow(nested_df))
nested_df$value_allsnap [nested_df$FNAP_1==1 | 
                         nested_df$FNAP_2==1 | 
                         nested_df$FNAP_3==1 | 
                         nested_df$FNAP_888==1 | 
                         nested_df$FNAP_4==1 | 
                         nested_df$FNAP_5==1 ] <- 1

nested_df$value_ebtsnap <- rep(0, nrow(nested_df))
nested_df$value_ebtsnap [nested_df$FNAP_2==1 ] <- 1

# Quick table check 
table(nested_df$FNAP)
#table(nested_df$FNAP_1)
table(nested_df$FNAP_2)
#table(nested_df$FNAP_3)
#table(nested_df$FNAP_4)
#table(nested_df$FNAP_5)
#table(nested_df$FNAP_888)

# Note: Only 841 obs out of 17,906 are snap; out of 841 total SNAP, 711 (85%) accept EBT payments 
ct <- nested_df %>%
  tabyl(value_allsnap, value_ebtsnap)
ct

# Validate: checkct should be empty
checkct <- nested_df  %>%
  filter(value_allsnap==0 & value_ebtsnap==1)

# Only keep operations with complete coordinates for mapping
nested_df_operations <- nested_df   %>%
  filter(!is.na(long))

# Only select farmers markets in Southeast States
nested_df_fm.south <- nested_df_operations  %>%
  filter(state_name %in%  c("Florida", 
                            "Georgia", 
                            "North Carolina", 
                            "South Carolina", 
                            "Virginia")) %>%
  filter(variable_name=="farmersmarket")

# Check for duplicated values
nested_df_fm.south %>% 
  group_by(variable_name, location_address, long, lat) %>% 
  tally %>% filter(n > 1) %>% 
  select(location_address)

# Remove duplicated values; fm.south will be merged with county df with LISA hotspot info
fm.south  <- nested_df_fm.south   %>% 
  distinct(variable_name, location_address, long, lat, .keep_all = TRUE)

# Export
write.csv(fm.south, "C:/AMS Viz Challenge/Southeast_SNAPGAP/Output/fm.south.csv")

########################################################################
# 3B Identify SNAP Hotspots
# Data source: USDA Food Environment Atlas (Last updated: 9/10/2020)
#     https://www.ers.usda.gov/data-products/food-environment-atlas/
# Variable used for hotspot analysis is SNAPSPTH17 from sheet "STORES".
# SNAPSPTH17 = SNAP-authorized stores/1,000 pop, 2017
# Note: This is the same as the data from the Data Warehouse Dropbox: 
#       FAME/foodaccess/FoodEnvironmentAtlas
########################################################################
setwd("C:/AMS Viz Challenge/Southeast_SNAPGAP/Data/Atlas")

sto1 <- read_excel("FoodEnvironmentAtlas.xls", sheet = "STORES")
sto2 <- sto1 %>% 
  select(FIPS, State, County, 
         SNAPS12,
         SNAPS17,
         PCH_SNAPS_12_17,
         SNAPSPTH12,
         SNAPSPTH17,
         PCH_SNAPSPTH_12_17) %>%
  filter(State %in%  c("FL", "GA", "NC", "SC", "VA"))
sto2$Category <- 'Stores'

write.csv(sto2, "C:/AMS Viz Challenge/Southeast_SNAPGAP/Output/atlas.stores.csv")



######################################################################################################
# Note: To convert into spatial object, 
# atlas_stores.csv was loaded in QGIS (https://qgis.org/)
# and was joined with Census county shapefile: cb_2020_us_county_500k.shp
# The resulting spatial object is "South_Cty2020_AtlasStr.shp" 
# Hotspot analysis reference: 
#   https://spatialanalysis.github.io/handsonspatialdata/local-spatial-autocorrelation-1.html
######################################################################################################
install.packages("spatmap")
install.packages("rgeoda")
install.packages("geodaData")

library(sf)
library(tmap)
library(rgeoda)
library(geodaData)
library(RColorBrewer)

# Read the shapefile 
setwd("C:/AMS Viz Challenge/Southeast_SNAPGAP/Data/QGIS")

chi.poly <- readOGR(dsn=".",layer = "South_Cty2020_AtlasStr")
class(chi.poly)
class(chi.poly@data)

# Convert chi.poly from sp to sf so the succeeding functions will work
chi.poly.sf <- as(chi.poly, "sf")

# Map SNAPSPTH17
tm_shape(chi.poly) +
  tm_fill("SNAPSPTH17", style = "jenks", n = 6) +
  tm_borders() +
  tm_layout(legend.outside = TRUE, legend.outside.position = "left")

# Set the color palette for hotspot analysis/lisa map 
match_palette <- function(patterns, classifications, colors){
  classes_present <- base::unique(patterns)
  mat <- matrix(c(classifications,colors), ncol = 2)
  logi <- classifications %in% classes_present
  pre_col <- matrix(mat[logi], ncol = 2)
  pal <- pre_col[,2]
  return(pal)
}

lisa_map <- function(df, lisa, alpha = .05) {
  clusters <- lisa_clusters(lisa,cutoff = alpha)
  labels <- lisa_labels(lisa)
  pvalue <- lisa_pvalues(lisa)
  colors <- lisa_colors(lisa)
  lisa_patterns <- labels[clusters+1]
  
  pal <- match_palette(lisa_patterns,labels,colors)
  labels <- labels[labels %in% lisa_patterns]
  
  df["lisa_clusters"] <- clusters
  tm_shape(df) +
    tm_fill("lisa_clusters",labels = labels, palette = pal,style = "cat")
}

significance_map <- function(df, lisa, permutations = 999, alpha = .05) {
  pvalue <- lisa_pvalues(lisa)
  target_p <- 1 / (1 + permutations)
  potential_brks <- c(.00001, .0001, .001, .01)
  brks <- potential_brks[which(potential_brks > target_p & potential_brks < alpha)]
  brks2 <- c(target_p, brks, alpha)
  labels <- c(as.character(brks2), "Not Significant")
  brks3 <- c(0, brks2, 1)
  
  cuts <- cut(pvalue, breaks = brks3,labels = labels)
  df["sig"] <- cuts
  
  pal <- rev(brewer.pal(length(labels), "Greens"))
  pal[length(pal)] <- "#D3D3D3"
  
  tm_shape(df) +
    tm_fill("sig", palette = pal)
}

# Create LISA map using weights 
w <- queen_weights(chi.poly.sf)
lisa <- local_moran(w, chi.poly.sf['SNAPSPTH17'])
lisa_map(chi.poly.sf, lisa)

# Create the significance map
significance_map(chi.poly.sf, lisa) 

# Permutations for LISA map
lisa <- local_moran(w, chi.poly.sf['SNAPSPTH17'], permutations = 99999)
lisa_map(chi.poly.sf, lisa) +
  tm_borders() +
  tm_layout(title = "Local Moran Cluster Map of No. of SNAP-authorized stores/1,000 pop., 2017", 
            legend.outside = TRUE)

# Permutations for significance map
significance_map(chi.poly.sf, lisa, permutations = 99999) +
  tm_borders() +
  tm_layout(title = "Local Moran Significance Map of SNAP-authorized stores/1,000 pop., 2017", legend.outside = TRUE)

# Combine LISA map and significance map for hot/cold spots
#tmap_mode("view")
#tmap_mode("plot")
lisa_map(chi.poly.sf, lisa, alpha = .05) +
  tm_borders() +
  tm_layout(title = "Statistically Significant Local Moran Cluster Map of SNAP-authorized stores/1,000 pop., 2018", 
            legend.outside = TRUE)

# Extract the LISA values the FM point data can be overlaid in QGIS
lisa
class(lisa) 
str(lisa)

# Get the values of the local Moran:
lms <- lisa_values(gda_lisa = lisa)
lms.df <- as.data.frame(lms)

# Get the pseudo-p values of significance of local Moran computation
pvals <- lisa_pvalues(lisa)
pvals.df <- as.data.frame(pvals)

# Get the cluster indicators of local Moran computation:
cats <- lisa_clusters(lisa, cutoff = 0.05)
cats.df <- as.data.frame(cats)

# The predefined values of the indicators of LISA cluster are:
# 0 Not significant
# 1 High-High
# 2 Low-Low
# 3 High-Low
# 4 Low-High
# 5 Undefined
# 6 Isolated

# Bind LISA values with dataframe
AtlasStr_data <- chi.poly@data 
hotspot.store <- cbind(AtlasStr_data, cats.df) 

# Add variable for lisa categories
hotspot.store <- hotspot.store %>%
  rename(cluster = cats) %>%
  mutate (cluster_desc = case_when (
      cluster == 0 ~ "Not significant", 
      cluster == 1 ~ "SNAP Hot spot: HH", 
      cluster == 2 ~ "SNAP Cold spot: LL", 
      cluster == 3 ~ "Outlier: HL", 
      cluster == 4 ~ "Outlier: LH"
    ))

# Export and load to QGIS to overlay FM with hotspots
write.csv(hotspot.store, "C:/AMS Viz Challenge/Southeast_SNAPGAP/Output/hotspot.store.csv")

write.csv(sto2, "C:/AMS Viz Challenge/Southeast_SNAPGAP/Output/atlas.stores.csv")

###################################################
# 3C Join hotspot and FM to create summary tables 
###################################################

# Join the datasets
fm.hotspot.join <- left_join(fm.south, hotspot.store, 
                      by=c('fips'='GEOID'))
write.csv(fm.hotspot.join, "C:/AMS Viz Challenge/Southeast_SNAPGAP/Output/fm.hotspot.join.csv")

# Create summary table for viz
ind3_sum1 <- fm.hotspot.join %>%
  group_by(cluster, cluster_desc) %>% 
  summarise(across(starts_with('value'), sum)) 

# Clean summary table and calculate percentage of fm_snap in fm_total
ind3_sum2 <- ind3_sum1  %>%
  select (cluster, cluster_desc, value, value_ebtsnap) %>%
  rename(
    fm_total = value,  
    fm_snap = value_ebtsnap) %>%
  adorn_totals("row") %>%
  mutate (percent.fm_snap = 100*(fm_snap/fm_total))

# Export indicator #3 summary table
write.csv(ind3_sum2, "C:/AMS Viz Challenge/Southeast_SNAPGAP/Output/indicator3_summary_table.csv")

##########################################
############# END OF CODE ################
##########################################