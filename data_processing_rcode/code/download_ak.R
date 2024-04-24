# This function can be used to download the most recent survey data files for the AK regions and 
# put them in the data_raw folder.
# The ai_strata.csv, ebs_strata.csv, and goa_strata.csv do not change year to year, and should be retained 
# between updates (do not delete these files)

# Resources --------------------------------------------------------------------

# - https://www.fisheries.noaa.gov/alaska/science-data/groundfish-assessment-program-bottom-trawl-surveys

# Install libraries ------------------------------------------------------------

PKG <- c(
  # Mapping
  "akgfmaps", # devtools::install_github("sean-rohan-noaa/akgfmaps", build_vignettes = TRUE)
  "sf",
  "janitor",
  "readr", 
  "rmarkdown", 
  "tidyr", 
  "dplyr",
  "googledrive",
  "here",
  "magrittr",
  "stringr", 
  "readxl",
  "RODBC", 
  "googledrive",
  "RCurl" # for ftp connection
)

for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p, verbose = FALSE)
    require(p,character.only = TRUE)}
}

# Knowns -----------------------------------------------------------------------

data_source <- "gd" # gd = google drive; oracle; api


### Download data from Oracle --------------------------------------------------
# mostly recording this for posterity, in case I need to use it again for google drive loads! - EHM
if (data_source == "oracle") {
  # Log into Oracle
  
  if (file.exists("Z:/Projects/ConnectToOracle.R")) {
    # This has a specific username and password because I DONT want people to have access to this!
    source("Z:/Projects/ConnectToOracle.R")
  } else {
    # For those without a ConnectToOracle file
    library(rstudioapi)
    library(RODBC)
    channel <- odbcConnect(dsn = "AFSC", 
                           uid = rstudioapi::showPrompt(title = "Username", 
                                                        message = "Oracle Username", default = ""), 
                           pwd = rstudioapi::askForPassword("Enter Password"),
                           believeNRows = FALSE)
  }
  
  # Pull data from GAP_PRODUCTS
  
  locations <- c(
    # "GAP_PRODUCTS.METADATA_COLUMN", # metadata
    # "GAP_PRODUCTS.METADATA_TABLE", # metaddata
    "GAP_PRODUCTS.FOSS_CATCH",
    "GAP_PRODUCTS.FOSS_HAUL",
    "GAP_PRODUCTS.FOSS_SPECIES"#,
    # "GAP_PRODUCTS.FOSS_SURVEY_SPECIES"
  )
  
  print(Sys.Date())
  
  error_loading <- c()
  for (i in 1:length(locations)){
    print(locations[i])
    
    a <- RODBC::sqlQuery(channel, paste0("SELECT * FROM ", locations[i], "; "))
    
    if (is.null(nrow(a))) { # if (sum(grepl(pattern = "SQLExecDirect ", x = a))>1) {
      error_loading <- c(error_loading, locations[i])
    } else {
      write.csv(x = a, 
                here::here("data_processing_rcode/data",
                           paste0(tolower(gsub(pattern = '.', 
                                               replacement = "_", 
                                               x = locations[i], 
                                               fixed = TRUE)),
                                  ".csv")))
    }
    remove(a)
  }
  error_loading
}

### Download data from google drive folder -------------------------------------
googledrive::drive_deauth()
googledrive::drive_auth()
2

if (data_source == "gd") { # if you are loading these files from google drive
  
  dir_googledrive <- "https://drive.google.com/drive/folders/1NcDCxolMf-drd01vy0_NIhqD1lf3r_Ud" # downloaded 4/12/2024
  
  # see what files are in this google drive
  temp <- googledrive::drive_ls(path = googledrive::as_id(dir_googledrive))
  # download each of the 3 files in the folder (catch, haul, and species)
  for (i in 1:nrow(temp)) {
    print(temp$name[i]) # for seeing progress
    googledrive::drive_download(
      file = temp$id[i], 
      path = here::here("data_processing_rcode/data/", temp$name[i]), 
      overwrite = TRUE)    
  }
}

### Download data from FOSS API ------------------------------------------------

# !!!! IN DEV !!! #TOLEDO

# if (data_source == "api") {
# ## New Data download function for getting data from the Fisheries one-stop-shop (FOSS): 
# #the API is broken so instead will need to go to FOSS() and manually download the data for each survey and save to local folder. 
# 
# # install.packages(c("httr", "jsonlite"))
# library(httr)
# library(jsonlite)
# library(dplyr)
# 
# # link to the API
# api_link <- "https://apps-st.fisheries.noaa.gov/ods/foss/afsc_groundfish_survey/"
# 
# 
# #EBS
# res <- httr::GET(
#   url = paste0(api_link, '?q={"srvy":"EBS"}'))
# data <- jsonlite::fromJSON(base::rawToChar(res$content))
# 
# as_tibble(data$items) %>% 
#   mutate_if(is.character, type.convert, as.is = TRUE) %>%
#   head(3) %>%
#   dplyr::mutate(across(where(is.numeric), round, 3)) %>%
#   dplyr::select(year, srvy, stratum, species_code, cpue_kgkm2) %>%
#   flextable::flextable() %>%
#   flextable::fit_to_width(max_width = 6) %>% 
#   flextable::theme_zebra() %>%
#   flextable::colformat_num(x = ., j = c("year", "species_code"), big.mark = "") 
# 
# }

# Load data --------------------------------------------------------------------

catch <- readr::read_csv(file = here::here("data_processing_rcode/data/gap_products_foss_catch.csv"))[,-1] # remove "row number" column
haul <- readr::read_csv(file = here::here("data_processing_rcode/data/gap_products_foss_haul.csv"))[,-1] # remove "row number" column
species <- readr::read_csv(file = here::here("data_processing_rcode/data/gap_products_foss_species.csv"))[,-1] # remove "row number" column

# Wrangle data -----------------------------------------------------------------

# come up with full combination of what species should be listed for what hauls/surveys
# for zero-filled data, all species caught in a survey need to have zero or non-zero row entries for a haul
comb <- dplyr::full_join(
  x = dplyr::left_join(catch, haul, by = "HAULJOIN") %>%
    dplyr::select(SURVEY_DEFINITION_ID, SPECIES_CODE) %>%
    dplyr::distinct(),
  y = haul %>%
    dplyr::select(SURVEY_DEFINITION_ID, HAULJOIN) %>%
    dplyr::distinct(), 
  by = "SURVEY_DEFINITION_ID", 
  relationship = "many-to-many"
)

# Join data to make a full zero-filled CPUE dataset
dat <- comb %>% 
  # add species data to unique species by survey table
  dplyr::left_join(species, "SPECIES_CODE") %>% 
  # add catch data
  dplyr::full_join(catch, c("SPECIES_CODE", "HAULJOIN")) %>% 
  # add haul data
  dplyr::full_join(haul) %>% # , c("SURVEY_DEFINITION_ID", "HAULJOIN")
  # modify zero-filled rows
  dplyr::mutate(
    CPUE_KGKM2 = ifelse(is.null(CPUE_KGKM2), 0, CPUE_KGKM2),
    CPUE_KGHA = CPUE_KGKM2/100, # Hectares
    CPUE_NOKM2 = ifelse(is.null(CPUE_NOKM2), 0, CPUE_NOKM2),
    CPUE_NOHA = CPUE_NOKM2/100, # Hectares
    COUNT = ifelse(is.null(COUNT), 0, COUNT),
    WEIGHT_KG = ifelse(is.null(WEIGHT_KG), 0, WEIGHT_KG), 
    region = dplyr::case_when( 
      # I would suggest using the SURVEY_NAME or SURVEY column instead of the names you have. 
      # They're fine, but a little simple. Ive recreated them here so things will hopefully mesh with your code - EHM 4/2024 # TOLEDO
      SURVEY_DEFINITION_ID == 78 ~ "Bering Sea Slope Survey", 
      SURVEY_DEFINITION_ID == 47 ~ "Gulf of Alaska", 
      SURVEY_DEFINITION_ID == 52 ~ "Aleutian Islands", 
      SURVEY_DEFINITION_ID == 98 ~ "Eastern Bering Sea", 
      SURVEY_DEFINITION_ID == 143 ~ "Northern Bering Sea"
    )) 

# data changes specific to DisMAP
dat1 <- dat %>% 
  dplyr::filter(!(SURVEY_DEFINITION_ID == 98 & STRATUM %in% c(90, 82))) %>% # remove stratum in EBS that are missing from 1982-1987. However, I would reconsider doing this, you loose out on so much data!  - EHM 4/2024 # TOLEDO
  dplyr::filter(SURVEY_DEFINITION_ID != 78) %>% # remove bering sea slope survey 
  dplyr::select(region, 
                year = YEAR, 
                Cruise = CRUISE, 
                Haul = HAUL, 
                lat = LATITUDE_DD_START, 
                lon = LONGITUDE_DD_START, 
                stratum = STRATUM, 
                vesselID = VESSEL_ID, # you don't need this anymore, right? I can get it for you if you do, but this seems like a hold over from an earlier time- EHM 4/2024 # TOLEDO
                # stratumarea = ----, # you don't need this anymore, right? I can get it for you if you do, but this seems like a hold over from an earlier time- EHM 4/2024 # TOLEDO
                depth = DEPTH_M, 
                spp = SCIENTIFIC_NAME, # since you have worms codes here, I don't think you need these anymore? we should just be able to bind to your pre-approved species list?  - EHM 4/2024 # TOLEDO
                common = COMMON_NAME, # since you have worms codes here, I don't think you need these anymore? we should just be able to bind to your pre-approved species list?  - EHM 4/2024 # TOLEDO
                wtcpue = CPUE_KGHA, # I would consider moving everthing from HA to KM2 - EHM 4/2024 # TOLEDO
                worms = WORMS# I've added this column
  ) %>% 
  # remove rows that are eggs
  dplyr::filter(spp != "" &
                  # remove all spp that contain the word "egg"
                  !grepl("egg", spp),
                !grepl("Polychaete tubes", spp)) %>%   
  # if you choose to keep the common name column - EHM 4/2024 # TOLEDO
  dplyr::mutate(
    stratumarea = NA, # remvoed above because the new data tables dont provide this, but I can get it for you if you need it. - EHM 4/2024 # TOLEDO
    # Create a unique haulid
    haulid = paste(formatC(vesselID, width=3, flag=0), Cruise, formatC(Haul, width=3, flag=0), sep='-'), 
    lon = ifelse(lon > 0, lon - 360, lon), 
    # change -9999 wtcpue to NA
    # wtcpue = ifelse(wtcpue == "-9999", NA, wtcpue),  # hopefully not in there anymore?? please let me know if you find any of these hanging around!  - EHM 4/2024 # TOLEDO
    
    # adjust spp names
    # add species names for two rockfish complexes
    spp = ifelse(grepl("Rougheye and Blackspotted Rockfish Unid.", common), "Sebastes melanostictus and S. aleutianus", spp),
    spp = ifelse(grepl("Dusky and Dark Rockfishes Unid.", common), "Sebastes variabilis and S. ciliatus", spp), 
    # catch A. stomias and A. evermanii (as of 2018 both spp appear as "valid" so not sure why they are being changed)
    spp = ifelse(grepl("Atheresthes", spp), "Atheresthes stomias and A. evermanni", spp), 
    # catch L. polystryxa (valid in 2018), and L. bilineata (valid in 2018)
    spp = ifelse(grepl("Lepidopsetta", spp), "Lepidopsetta sp.", spp),
    # catch M. jaok (valid in 2018), M. niger (valid in 2018), M. polyacanthocephalus (valid in 2018), M. quadricornis (valid in 2018), M. verrucosus (changed to scorpius), M. scorpioides (valid in 2018), M. scorpius (valid in 2018) (M. scorpius is in the data set but not on the list so it is excluded from the change)
    #spp = ifelse(grepl("Myoxocephalus", spp ) & !grepl("scorpius", spp), "Myoxocephalus sp.", spp),
    # catch B. maculata (valid in 2018), abyssicola (valid in 2018), aleutica (valid in 2018), interrupta (valid in 2018), lindbergi (valid in 2018), mariposa (valid in 2018), minispinosa (valid in 2018), parmifera (valid in 2018), smirnovi (valid in 2018), cf parmifera (Orretal), spinosissima (valid in 2018), taranetzi (valid in 2018), trachura (valid in 2018), violacea (valid in 2018)
    # B. panthera is not on the list of spp to change
    spp = ifelse(grepl("Bathyraja", spp), 'Bathyraja sp.', spp),
    # catch S. melanostictus and S. aleutianus (blackspotted & rougheye), combined into one complex
    spp = ifelse(grepl("Sebastes melanostictus", spp)|grepl("Sebastes aleutianus", spp), "Sebastes melanostictus and S. aleutianus", spp),
    # catch S. variabilis and S. ciliatus (dusky + dark rockfish), combined into one complex
    spp = ifelse(grepl("Sebastes variabilis", spp)|grepl("Sebastes ciliatus", spp), "Sebastes variabilis and S. ciliatus", spp), 
    spp = ifelse(grepl("Hippoglossoides", spp), "Hippoglossoides elassodon and H. robustus", spp)
  ) %>% 
  # dplyr::select(region, haulid, year, lat, lon, stratum, stratumarea, depth, spp, wtcpue) %>% # Should be redundant
  readr::type_convert(col_types = cols(
    lat = col_double(),
    lon = col_double(),
    year = col_integer(),
    wtcpue = col_double(),
    spp = col_character(),
    depth = col_integer(),
    haulid = col_character()
  )) %>% 
  dplyr::group_by(region, haulid, stratum, stratumarea, year, lat, lon, depth, spp) %>% 
  dplyr::summarise(wtcpue = sum(wtcpue, na.rm = TRUE)) %>% 
  # Calculate a corrected longitude for Aleutians (all in western hemisphere coordinates)
  dplyr::ungroup()

# Not sure how to incorporate this code, if it is suggesting that a region needs to be removed- EHM 4/2024 # TOLEDO

# for GOA in 2001 missed 27 strata and will be removed, stratum 50 is
# missing from 3 years but will be kept, 410, 420, 430, 440, 450 are missing 
#from 3 years but will be kept, 510 and higher are missing from 7 or more years
# of data and will be removed
# test <- goa %>%
#   filter(year != 2001) %>% 
#   select(stratum, year) %>% 
#   distinct() %>% 
#   group_by(stratum) %>% 
#   summarise(count = n())%>%
#   filter(count >= 11)  

write.csv(x = dat, here::here("data_processing_rcode/data/afsc.csv")) # you can call it anything you like, but I figured that this would be easy to find and rename

# OLD SCRIPT -------------------------------------------------------------------
# 
# ## old script to download from AFSC website. Those data files are outdated and no longer being updated. Instead use the above code/process to get the correct AK survey data
#  # from FOSS.... 
# 
# # # Check [Alaskan website](https://www.fisheries.noaa.gov/alaska/commercial-fishing/alaska-groundfish-bottom-trawl-survey-data) for any new data and add it to the list, files to watch are ai2014-2018, ebs2017-2018, and goa2015-2017.  Did the names changes?  Are there more recent files?.  
# # # This is the old website: https://archive.fisheries.noaa.gov/afsc/RACE/groundfish/survey_data/data.htm and it is not being updated with new data.
# # library(tibble)
# # download_ak <- function(){
# #   # define the destination folder
# #   for (i in seq(file_list)){
# #     # define the destination file path
# #     file <- paste("data_raw", ak_files$survey[i], sep = "/")
# #     # define the source url
# #     url <- paste("https://apps-afsc.fisheries.noaa.gov/RACE/groundfish/survey_data/downloads", ak_files$survey[i], sep = "/")
# #     # download the file from the url to the destination file path
# #     download.file(url,file)
# #     # unzip the new file - this will overwrite an existing file of the same name
# #     unzip(file, exdir = "data_raw")
# #     # delete the downloaded zip file
# #     file.remove(file)
# #   }
# # }
# # 
# # ## Check names on website and make sure to update which files to download based on any name changes with the addition
# #   # of a new year of data
# # 
# # ak_files <- tibble(survey = c("ai1983_2000.zip", 
# #                               "ai2002_2012.zip", 
# #                               "ai2014_2018.zip", 
# #                               
# #                               "ebs1982_1984.zip", 
# #                               "ebs1985_1989.zip", 
# #                               "ebs1990_1994.zip", 
# #                               "ebs1995_1999.zip", 
# #                               "ebs2000_2004.zip", 
# #                               "ebs2005_2008.zip", 
# #                               "ebs2009_2012.zip", 
# #                               "ebs2013_2016.zip", 
# #                               "ebs2017_2019.zip", 
# #                               
# #                               "goa1984_1987.zip", 
# #                               "goa1990_1999.zip", 
# #                               "goa2001_2005.zip", 
# #                               "goa2007_2013.zip", 
# #                               "goa2015_2019.zip"))
# # 
# # file_list <- ak_files$survey
# # download_ak()


