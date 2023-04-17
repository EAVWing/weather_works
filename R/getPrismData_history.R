library(prism)
library(dplyr)
library(lubridate)
library(DomoR)
library(tidyr)
library(raster)
library(tigris)
library(sf)
library(stringr)
library(rvest)

# Update calendar

fetch_update_calendar <-
  function(url = "http://www.prism.oregonstate.edu/calendar/list.php") {
    updates <- url %>%
      read_html() %>%
      html_nodes(xpath = '//*[@id="calendar"]/table') %>%
      html_table()
    updates <- updates[[1]]
    
    updatesClean <- updates %>%
      mutate(
        date = as_date(updates$Date, format = '%d %B %Y'),
        PPT_date_updated = ymd(str_sub(PPT, 1, 10)),
        PPT_update_number = as.integer(str_sub(PPT,-2,-2)),
        TMIN_date_updated = ymd(str_sub(TMIN, 1, 10)),
        TMIN_update_number = as.integer(str_sub(TMIN,-2,-2)),
        TMAX_date_updated = ymd(str_sub(TMAX, 1, 10)),
        TMAX_update_number = as.integer(str_sub(TMAX,-2,-2)),
        TMEAN_date_updated = ymd(str_sub(TMEAN, 1, 10)),
        TMEAN_update_number = as.integer(str_sub(TMEAN,-2,-2))
      ) %>%
      dplyr::select(-PPT,-TMIN,-TMAX,-TMEAN)
    
    updatesClean
  }

updatesClean <-  fetch_update_calendar()

# SETUP the county shape file
state_shapes <- states(cb = TRUE)

state_list <- state_shapes %>%
  as.data.frame() %>%
  dplyr::select(STATEFP, NAME) %>%
  rename(state_name = NAME) %>%
  unique()

county_shapes <- counties(cb = TRUE)

# List of all counties
county_list <- county_shapes %>%
  as.data.frame() %>%
  dplyr::select(STATEFP, COUNTYFP, NAME) %>%
  dplyr::mutate(COUNTYFP = paste0(STATEFP, COUNTYFP)) %>%
  unique() %>%
  rename(county_name = NAME) %>%
  merge(state_list) %>%
  filter(!(STATEFP %in% c('02', '15', '60', '66', '69', '72', '78'))) %>%
  dplyr::select(COUNTYFP, county_name) %>%
  unique()

county_shapes <- county_shapes %>%
  mutate(COUNTYFP = paste0(STATEFP, COUNTYFP)) %>%
  filter(COUNTYFP %in% county_list$COUNTYFP)

the_crs <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 "

county_CRS <- st_transform(county_shapes, crs = the_crs) %>% dplyr::select(COUNTYFP)

county_sp <- as(county_CRS, "Spatial")

# Domo init
load("domoCustomer")
load("domoAccessToken")
DomoR::init(domoCustomer, domoAccessToken)


# PULL prism files
if (file.exists("C:/Users/sewing/Documents/PrismTemp")){
  options(prism.path = "C:/Users/sewing/Documents/PrismTemp")} else {
    options(prism.path = "C:/Users/steph/PrismTemp")
  }

start_date <- date("2018/01/01") #max(stableMean$Date)

end_date <- date("2018/06/30") #Sys.Date() - 1

get_prism_dailys(type = "ppt",
                 minDate = start_date,
                 maxDate = end_date,
                 keepZip = F)

check_corrupt(type = "ppt", minDate = start_date, maxDate = end_date)

get_prism_dailys(type = "tmean",
                 minDate = start_date,
                 maxDate = end_date,
                 keepZip = F)

check_corrupt(type = "tmean", minDate = start_date, maxDate = end_date)

get_prism_dailys(type = "tmax",
                 minDate = start_date,
                 maxDate = end_date,
                 keepZip = F)

check_corrupt(type = "tmax", minDate = start_date, maxDate = end_date)

get_prism_dailys(type = "tmin",
                 minDate = start_date,
                 maxDate = end_date,
                 keepZip = F)

check_corrupt(type = "tmin", minDate = start_date, maxDate = end_date)

precip_files_in_domo <- DomoR::fetch("b805a69c-81f3-474e-b0da-7ba88096b4ce")

precip_stable_in_domo <- filter(precip_files_in_domo, status == "stable")

first_precip_stable_date <- min(precip_stable_in_domo$date)
last_precip_stable_date <- max(precip_stable_in_domo$date)

stable_precip_check <- tibble(date = seq(first_precip_stable_date, last_precip_stable_date, by = "day")) %>%
  merge(precip_stable_in_domo, all.x = T) %>%
  filter(is.na(status))

precip_dates_to_pull <- seq(start_date, end_date, by = "days") #stable_precip_check$date

precip_e_p_files_in_domo <- filter(precip_files_in_domo, status != "stable")

first_precip_e_p_date <- last_precip_stable_date + 1
last_precip_e_p_date <- date(max(updatesClean$date))

e_p_precip_check_1 <- tibble(date = seq(first_precip_e_p_date, last_precip_e_p_date, by = "days")) %>%
  merge(precip_e_p_files_in_domo, all.x = T) %>%
  filter(is.na(status))

precip_dates_to_pull <- append(precip_dates_to_pull, e_p_precip_check_1$date)

e_p_precip_check_2 <- tibble(date = seq(first_precip_e_p_date, last_precip_e_p_date, by = "day")) %>%
  merge(precip_e_p_files_in_domo, all.x = T) %>%
  merge(updatesClean, all.x = T) %>%
  filter(!is.na(status)) %>%
  filter(PPT_date_updated >= updated)

precip_dates_to_pull <- append(precip_dates_to_pull, e_p_precip_check_2$date)

# get_prism_dailys(type = "ppt",
#                  dates = precip_dates_to_pull,
#                  keepZip = F)

prismFileList <- ls_prism_data(absPath = TRUE) %>%
  mutate(Date = ymd(str_sub(files, start = -12, end = -5))) %>%
  filter(Date %in% precip_dates_to_pull)

stablePpt <- prismFileList[which(grepl("_ppt_stable_4kmD", prismFileList$files)), ]

provisionalPpt <- prismFileList[which(grepl("_ppt_provisional_4kmD", prismFileList$files)), ]

earlyPpt <- prismFileList[which(grepl("_ppt_early_4kmD", prismFileList$files)), ]

# Convert Rasters
# precipitation
if(length(earlyPpt$Date > 0)){
  early_ppt_stack <- stack(earlyPpt$abs_path)
  early_stack_projected <- projectRaster(early_ppt_stack, crs = the_crs, method = "ngb")
  county_cropped <- crop(county_sp, early_stack_projected)
  county_seq <- tibble(COUNTYFP = county_cropped@data$COUNTYFP, ID = seq(1:length(county_cropped@data$COUNTYFP)))
  precip <- raster::extract(early_stack_projected, county_cropped, df = T)
  names(precip) <- append("ID", as.character(earlyPpt$Date))
  precip_early <- merge(precip, county_seq, all = T) %>%
    dplyr::select(-ID) %>%
    pivot_longer(cols = -COUNTYFP, names_to = 'date', values_to = 'precip') %>%
    mutate(status = "early") %>%
    group_by(COUNTYFP, date, status) %>%
    summarise(min = min(precip),
              sum = sum(precip),
              max = max(precip),
              mean = round(mean(precip), 2),
              median = median(precip),
              count = n()) %>%
    ungroup()
} else {
  precip_early <- tibble(COUNTYFP = numeric(),
                         date = date(),
                         status = character(),
                         sum = numeric(),
                         min = numeric(),
                         max = numeric(),
                         mean = numeric(),
                         median = numeric(),
                         count = numeric())
}

if(length(provisionalPpt$Date > 0)){
  provisional_ppt_stack <- stack(provisionalPpt$abs_path)
  provisional_stack_projected <- projectRaster(provisional_ppt_stack, crs = the_crs, method = "ngb")
  county_cropped <- crop(county_sp, provisional_stack_projected)
  county_seq <- tibble(COUNTYFP = county_cropped@data$COUNTYFP, ID = seq(1:length(county_cropped@data$COUNTYFP)))
  precip <- raster::extract(provisional_stack_projected, county_cropped, df = T)
  names(precip) <- append("ID", as.character(provisionalPpt$Date))
  precip_provisional <- merge(precip, county_seq, all = T) %>%
    dplyr::select(-ID) %>%
    pivot_longer(cols = -COUNTYFP, names_to = 'date', values_to = 'precip') %>%
    mutate(status = "provisional") %>%
    group_by(COUNTYFP, date, status) %>%
    summarise(min = min(precip),
              sum = sum(precip),
              max = max(precip),
              mean = round(mean(precip), 2),
              median = median(precip),
              count = n()) %>%
    ungroup()
} else {
  precip_provisional <- tibble(COUNTYFP = numeric(),
                               date = date(),
                               status = character(),
                               sum = numeric(),
                               min = numeric(),
                               max = numeric(),
                               mean = numeric(),
                               median = numeric(),
                               count = numeric())
}

if(length(stablePpt$Date > 0)){
  stable_ppt_stack <- stack(stablePpt$abs_path)
  stable_stack_projected <- projectRaster(stable_ppt_stack, crs = the_crs, method = "ngb")
  county_cropped <- crop(county_sp, stable_stack_projected)
  county_seq <- tibble(COUNTYFP = county_cropped@data$COUNTYFP, ID = seq(1:length(county_cropped@data$COUNTYFP)))
  precip <- raster::extract(stable_stack_projected, county_cropped, df = T)
  names(precip) <- append("ID", as.character(stablePpt$Date))
  precip_stable <- merge(precip, county_seq, all = T) %>%
    dplyr::select(-ID) %>%
    pivot_longer(cols = -COUNTYFP, names_to = 'date', values_to = 'precip') %>%
    mutate(status = "stable") %>%
    group_by(COUNTYFP, date, status) %>%
    summarise(min = min(precip),
              sum = sum(precip),
              max = max(precip),
              mean = round(mean(precip), 2),
              median = median(precip),
              count = n()) %>%
    ungroup()
} else {
  precip_stable <- tibble(COUNTYFP = numeric(),
                          date = date(),
                          status = character(),
                          sum = numeric(),
                          min = numeric(),
                          max = numeric(),
                          mean = numeric(),
                          median = numeric(),
                          count = numeric())
}

precip_upload <- rbind(precip_early, precip_provisional) %>%
  rbind(precip_stable)

try(replace_ds("953809a4-47bb-43fc-8897-f837c78d8b7d", precip_upload))

# Min temp
min_files_in_domo <- fetch("af1db7db-833f-4959-8bcb-0081f1cf7001")

min_stable_in_domo <- filter(min_files_in_domo, status == "stable")

first_min_stable_date <- min(min_stable_in_domo$date)
last_min_stable_date <- max(min_stable_in_domo$date)

stable_min_check <- tibble(date = seq(first_min_stable_date, last_min_stable_date, by = "day")) %>%
  merge(min_stable_in_domo, all.x = T) %>%
  filter(is.na(status))

min_dates_to_pull <- seq(start_date, end_date, by = "days") #stable_min_check$date

min_e_p_files_in_domo <- filter(min_files_in_domo, status != "stable")

first_min_e_p_date <- last_min_stable_date + 1
last_min_e_p_date <- date(max(updatesClean$date))

e_p_min_check_1 <- tibble(date = seq(first_min_e_p_date, last_min_e_p_date, by = "day")) %>%
  merge(min_e_p_files_in_domo, all.x = T) %>%
  filter(is.na(status))

min_dates_to_pull <- append(min_dates_to_pull, e_p_min_check_1$date)

e_p_min_check_2 <- tibble(date = seq(first_min_e_p_date, last_min_e_p_date, by = "day")) %>%
  merge(min_e_p_files_in_domo, all.x = T) %>%
  merge(updatesClean, all.x = T) %>%
  filter(!is.na(status)) %>%
  filter(TMIN_date_updated >= updated)

min_dates_to_pull <- append(min_dates_to_pull, e_p_min_check_2$date)

# get_prism_dailys(type = "tmin",
#                  dates = min_dates_to_pull,
#                  keepZip = F)

prismFileList <- ls_prism_data(absPath = TRUE) %>%
  mutate(Date = ymd(str_sub(files, start = -12, end = -5))) %>%
  filter(Date %in% min_dates_to_pull)

stableMin <- prismFileList[which(grepl("_tmin_stable_4kmD", prismFileList$files)), ]

provisionalMin <- prismFileList[which(grepl("_tmin_provisional_4kmD", prismFileList$files)), ]

earlyMin <- prismFileList[which(grepl("_tmin_early_4kmD", prismFileList$files)), ]

if(length(earlyMin$Date > 0)){
  early_Min_stack <- stack(earlyMin$abs_path)
  early_stack_projected <- projectRaster(early_Min_stack, crs = the_crs, method = "ngb")
  county_cropped <- crop(county_sp, early_stack_projected)
  county_seq <- tibble(COUNTYFP = county_cropped@data$COUNTYFP, ID = seq(1:length(county_cropped@data$COUNTYFP)))
  tmin <- raster::extract(early_stack_projected, county_cropped, df = T)
  names(tmin) <- append("ID", as.character(earlyMin$Date))
  tmin_early <- merge(tmin, county_seq, all = T) %>%
    dplyr::select(-ID) %>%
    pivot_longer(cols = -COUNTYFP, names_to = 'date', values_to = 'tmin') %>%
    mutate(status = "early") %>%
    group_by(COUNTYFP, date, status) %>%
    summarise(min = min(tmin),
              sum = sum(tmin),
              max = max(tmin),
              mean = round(mean(tmin), 2),
              median = median(tmin),
              count = n()) %>%
    ungroup()
} else {
  tmin_early <- tibble(
    COUNTYFP = numeric(),
    date = date(),
    status = character(),
    min = numeric(),
    max = numeric(),
    mean = numeric(),
    median = numeric(),
    count = numeric()
  )
}

if(length(provisionalMin$Date > 0)){
  provisional_Min_stack <- stack(provisionalMin$abs_path)
  provisional_stack_projected <- projectRaster(provisional_Min_stack, crs = the_crs, method = "ngb")
  county_cropped <- crop(county_sp, provisional_stack_projected)
  county_seq <- tibble(COUNTYFP = county_cropped@data$COUNTYFP, ID = seq(1:length(county_cropped@data$COUNTYFP)))
  tmin <- raster::extract(provisional_stack_projected, county_cropped, df = T)
  names(tmin) <- append("ID", as.character(provisionalMin$Date))
  tmin_provisional <- merge(tmin, county_seq, all = T) %>%
    dplyr::select(-ID) %>%
    pivot_longer(cols = -COUNTYFP, names_to = 'date', values_to = 'tmin') %>%
    mutate(status = "provisional") %>%
    group_by(COUNTYFP, date, status) %>%
    summarise(min = min(tmin),
              sum = sum(tmin),
              max = max(tmin),
              mean = round(mean(tmin), 2),
              median = median(tmin),
              count = n()) %>%
    ungroup()
} else {
  tmin_provisional <- tibble(
    COUNTYFP = numeric(),
    date = date(),
    status = character(),
    min = numeric(),
    max = numeric(),
    mean = numeric(),
    median = numeric(),
    count = numeric()
  )
}

if(length(stableMin$Date > 0)){
  stable_Min_stack <- stack(stableMin$abs_path)
  stable_stack_projected <- projectRaster(stable_Min_stack, crs = the_crs, method = "ngb")
  county_cropped <- crop(county_sp, stable_stack_projected)
  county_seq <- tibble(COUNTYFP = county_cropped@data$COUNTYFP, ID = seq(1:length(county_cropped@data$COUNTYFP)))
  tmin <- raster::extract(stable_stack_projected, county_cropped, df = T)
  names(tmin) <- append("ID", as.character(stableMin$Date))
  tmin_stable <- merge(tmin, county_seq, all = T) %>%
    dplyr::select(-ID) %>%
    pivot_longer(cols = -COUNTYFP, names_to = 'date', values_to = 'tmin') %>%
    mutate(status = "stable") %>%
    group_by(COUNTYFP, date, status) %>%
    summarise(min = min(tmin),
              sum = sum(tmin),
              max = max(tmin),
              mean = round(mean(tmin), 2),
              median = median(tmin),
              count = n()) %>%
    ungroup()
} else {
  tmin_stable <- tibble(
    COUNTYFP = numeric(),
    date = date(),
    status = character(),
    min = numeric(),
    max = numeric(),
    mean = numeric(),
    median = numeric(),
    count = numeric()
  )
}

tmin_upload <- rbind(tmin_early,
                     tmin_provisional) %>%
  rbind(tmin_stable)

try(replace_ds("dbd27025-c33c-47d1-becd-07ceadd43029", tmin_upload))

# Max temp
max_files_in_domo <- fetch("e79285f4-5fb8-4dc0-ad51-821fbd4c9b77")

max_stable_in_domo <- filter(max_files_in_domo, status == "stable")

first_max_stable_date <- min(max_stable_in_domo$date)
last_max_stable_date <- max(max_stable_in_domo$date)

stable_max_check <- tibble(date = seq(first_max_stable_date, last_max_stable_date, by = "day")) %>%
  merge(max_stable_in_domo, all.x = T) %>%
  filter(is.na(status))

max_dates_to_pull <- seq(start_date, end_date, by = "days") #stable_max_check$date

max_e_p_files_in_domo <- filter(max_files_in_domo, status != "stable")

first_max_e_p_date <- last_max_stable_date + 1
last_max_e_p_date <- date(max(updatesClean$date))

e_p_max_check_1 <- tibble(date = seq(first_max_e_p_date, last_max_e_p_date, by = "day")) %>%
  merge(max_e_p_files_in_domo, all.x = T) %>%
  filter(is.na(status))

max_dates_to_pull <- append(max_dates_to_pull, e_p_max_check_1$date)

e_p_max_check_2 <- tibble(date = seq(first_max_e_p_date, last_max_e_p_date, by = "day")) %>%
  merge(max_e_p_files_in_domo, all.x = T) %>%
  merge(updatesClean, all.x = T) %>%
  filter(!is.na(status)) %>%
  filter(TMAX_date_updated >= updated)

max_dates_to_pull <- append(max_dates_to_pull, e_p_max_check_2$date)

# get_prism_dailys(type = "tmax",
#                  dates = max_dates_to_pull,
#                  keepZip = F)

prismFileList <- ls_prism_data(absPath = TRUE) %>%
  mutate(Date = ymd(str_sub(files, start = -12, end = -5))) %>%
  filter(Date %in% max_dates_to_pull)

stableMax <- prismFileList[which(grepl("_tmax_stable_4kmD", prismFileList$files)), ]

provisionalMax <- prismFileList[which(grepl("_tmax_provisional_4kmD", prismFileList$files)), ]

earlyMax <- prismFileList[which(grepl("_tmax_early_4kmD", prismFileList$files)), ]

if(length(earlyMax$Date > 0)){
  early_Max_stack <- stack(earlyMax$abs_path)
  early_stack_projected <- projectRaster(early_Max_stack, crs = the_crs, method = "ngb")
  county_cropped <- crop(county_sp, early_stack_projected)
  county_seq <- tibble(COUNTYFP = county_cropped@data$COUNTYFP, ID = seq(1:length(county_cropped@data$COUNTYFP)))
  tmax <- raster::extract(early_stack_projected, county_cropped, df = T)
  names(tmax) <- append("ID", as.character(earlyMax$Date))
  tmax_early <- merge(tmax, county_seq, all = T) %>%
    dplyr::select(-ID) %>%
    pivot_longer(cols = -COUNTYFP, names_to = 'date', values_to = 'tmax') %>%
    mutate(status = "early") %>%
    group_by(COUNTYFP, date, status) %>%
    summarise(min = min(tmax),
              sum = sum(tmax),
              max = max(tmax),
              mean = round(mean(tmax), 2),
              median = median(tmax),
              count = n()) %>%
    ungroup()
} else {
  tmax_early <- tibble(
    COUNTYFP = numeric(),
    date = date(),
    status = character(),
    min = numeric(),
    max = numeric(),
    mean = numeric(),
    median = numeric(),
    count = numeric()
  )
}

if(length(provisionalMax$Date > 0)){
  provisional_Max_stack <- stack(provisionalMax$abs_path)
  provisional_stack_projected <- projectRaster(provisional_Max_stack, crs = the_crs, method = "ngb")
  county_cropped <- crop(county_sp, provisional_stack_projected)
  county_seq <- tibble(COUNTYFP = county_cropped@data$COUNTYFP, ID = seq(1:length(county_cropped@data$COUNTYFP)))
  tmax <- raster::extract(provisional_stack_projected, county_cropped, df = T)
  names(tmax) <- append("ID", as.character(provisionalMax$Date))
  tmax_provisional <- merge(tmax, county_seq, all = T) %>%
    dplyr::select(-ID) %>%
    pivot_longer(cols = -COUNTYFP, names_to = 'date', values_to = 'tmax') %>%
    mutate(status = "provisional") %>%
    group_by(COUNTYFP, date, status) %>%
    summarise(min = min(tmax),
              sum = sum(tmax),
              max = max(tmax),
              mean = round(mean(tmax), 2),
              median = median(tmax),
              count = n()) %>%
    ungroup()
} else {
  tmax_provisional <- tibble(
    COUNTYFP = numeric(),
    date = date(),
    status = character(),
    min = numeric(),
    max = numeric(),
    mean = numeric(),
    median = numeric(),
    count = numeric()
  )
}

if(length(stableMax$Date > 0)){
  stable_Max_stack <- stack(stableMax$abs_path)
  stable_stack_projected <- projectRaster(stable_Max_stack, crs = the_crs, method = "ngb")
  county_cropped <- crop(county_sp, stable_stack_projected)
  county_seq <- tibble(COUNTYFP = county_cropped@data$COUNTYFP, ID = seq(1:length(county_cropped@data$COUNTYFP)))
  tmax <- raster::extract(stable_stack_projected, county_cropped, df = T)
  names(tmax) <- append("ID", as.character(stableMax$Date))
  tmax_stable <- merge(tmax, county_seq, all = T) %>%
    dplyr::select(-ID) %>%
    pivot_longer(cols = -COUNTYFP, names_to = 'date', values_to = 'tmax') %>%
    mutate(status = "stable") %>%
    group_by(COUNTYFP, date, status) %>%
    summarise(min = min(tmax),
              sum = sum(tmax),
              max = max(tmax),
              mean = round(mean(tmax), 2),
              median = median(tmax),
              count = n()) %>%
    ungroup()
} else {
  tmax_stable <- tibble(
    COUNTYFP = numeric(),
    date = date(),
    status = character(),
    min = numeric(),
    max = numeric(),
    mean = numeric(),
    median = numeric(),
    count = numeric()
  )
}

tmax_upload <- rbind(tmax_early,
                     tmax_provisional) %>%
  rbind(tmax_stable)

try(replace_ds("853f36ae-84d9-486a-8439-ecdd4b381b7e", tmax_upload))

# Mean temp
mean_files_in_domo <- fetch("f3a7c104-55e4-4ce7-8746-4a1fcd0f8e86")

mean_stable_in_domo <- filter(mean_files_in_domo, status == "stable")

first_mean_stable_date <- min(mean_stable_in_domo$date)
last_mean_stable_date <- max(mean_stable_in_domo$date)

stable_mean_check <- tibble(date = seq(first_mean_stable_date, last_mean_stable_date, by = "day")) %>%
  merge(mean_stable_in_domo, all.x = T) %>%
  filter(is.na(status))

mean_dates_to_pull <- seq(start_date, end_date, by = "days") #stable_mean_check$date

mean_e_p_files_in_domo <- filter(mean_files_in_domo, status != "stable")

first_mean_e_p_date <- last_mean_stable_date + 1
last_mean_e_p_date <- date(max(updatesClean$date))

e_p_mean_check_1 <- tibble(date = seq(first_mean_e_p_date, last_mean_e_p_date, by = "day")) %>%
  merge(mean_e_p_files_in_domo, all.x = T) %>%
  filter(is.na(status))

mean_dates_to_pull <- append(mean_dates_to_pull, e_p_mean_check_1$date)

e_p_mean_check_2 <- tibble(date = seq(first_mean_e_p_date, last_mean_e_p_date, by = "day")) %>%
  merge(mean_e_p_files_in_domo, all.x = T) %>%
  merge(updatesClean, all.x = T) %>%
  filter(!is.na(status)) %>%
  filter(TMEAN_date_updated >= updated)

mean_dates_to_pull <- append(mean_dates_to_pull, e_p_mean_check_2$date)

# get_prism_dailys(type = "tmean",
#                  dates = mean_dates_to_pull,
#                  keepZip = F)

prismFileList <- ls_prism_data(absPath = TRUE) %>%
  mutate(Date = ymd(str_sub(files, start = -12, end = -5))) %>%
  filter(Date %in% mean_dates_to_pull)

stableMean <- prismFileList[which(grepl("_tmean_stable_4kmD", prismFileList$files)), ]

provisionalMean <- prismFileList[which(grepl("_tmean_provisional_4kmD", prismFileList$files)), ]

earlyMean <- prismFileList[which(grepl("_tmean_early_4kmD", prismFileList$files)), ]

if(length(earlyMean$Date > 0)){
  early_Mean_stack <- stack(earlyMean$abs_path)
  early_stack_projected <- projectRaster(early_Mean_stack, crs = the_crs, method = "ngb")
  county_cropped <- crop(county_sp, early_stack_projected)
  county_seq <- tibble(COUNTYFP = county_cropped@data$COUNTYFP, ID = seq(1:length(county_cropped@data$COUNTYFP)))
  tmean <- raster::extract(early_stack_projected, county_cropped, df = T)
  names(tmean) <- append("ID", as.character(earlyMean$Date))
  tmean_early <- merge(tmean, county_seq, all = T) %>%
    dplyr::select(-ID) %>%
    pivot_longer(cols = -COUNTYFP, names_to = 'date', values_to = 'tmean') %>%
    mutate(status = "early") %>%
    group_by(COUNTYFP, date, status) %>%
    summarise(min = min(tmean),
              sum = sum(tmean),
              max = max(tmean),
              mean = round(mean(tmean), 2),
              median = median(tmean),
              count = n()) %>%
    ungroup()
} else{
  tmean_early <-
    tibble(
      COUNTYFP = numeric(),
      date = date(),
      status = character(),
      min = numeric(),
      max = numeric(),
      mean = numeric(),
      median = numeric(),
      count = numeric())
}

if(length(provisionalMean$Date > 0)){
  provisional_Mean_stack <- stack(provisionalMean$abs_path)
  provisional_stack_projected <- projectRaster(provisional_Mean_stack, crs = the_crs, method = "ngb")
  county_cropped <- crop(county_sp, provisional_stack_projected)
  county_seq <- tibble(COUNTYFP = county_cropped@data$COUNTYFP, ID = seq(1:length(county_cropped@data$COUNTYFP)))
  tmean <- raster::extract(provisional_stack_projected, county_cropped, df = T)
  names(tmean) <- append("ID", as.character(provisionalMean$Date))
  tmean_provisional <- merge(tmean, county_seq, all = T) %>%
    dplyr::select(-ID) %>%
    pivot_longer(cols = -COUNTYFP, names_to = 'date', values_to = 'tmean') %>%
    mutate(status = "provisional") %>%
    group_by(COUNTYFP, date, status) %>%
    summarise(min = min(tmean),
              sum = sum(tmean),
              max = max(tmean),
              mean = round(mean(tmean), 2),
              median = median(tmean),
              count = n()) %>%
    ungroup()
} else {
  tmean_provisional <-
    tibble(
      COUNTYFP = numeric(),
      date = date(),
      status = character(),
      min = numeric(),
      max = numeric(),
      mean = numeric(),
      median = numeric(),
      count = numeric())
}

if(length(stableMean$Date > 0)){
  stable_Mean_stack <- stack(stableMean$abs_path)
  stable_stack_projected <- projectRaster(stable_Mean_stack, crs = the_crs, method = "ngb")
  county_cropped <- crop(county_sp, stable_stack_projected)
  county_seq <- tibble(COUNTYFP = county_cropped@data$COUNTYFP, ID = seq(1:length(county_cropped@data$COUNTYFP)))
  tmean <- raster::extract(stable_stack_projected, county_cropped, df = T)
  names(tmean) <- append("ID", as.character(stableMean$Date))
  tmean_stable <- merge(tmean, county_seq, all = T) %>%
    dplyr::select(-ID) %>%
    pivot_longer(cols = -COUNTYFP, names_to = 'date', values_to = 'tmean') %>%
    mutate(status = "stable") %>%
    group_by(COUNTYFP, date, status) %>%
    summarise(min = min(tmean),
              sum = sum(tmean),
              max = max(tmean),
              mean = round(mean(tmean), 2),
              median = median(tmean),
              count = n()) %>%
    ungroup()
} else {
  tmean_stable <-
    tibble(
      COUNTYFP = numeric(),
      date = date(),
      status = character(),
      min = numeric(),
      max = numeric(),
      mean = numeric(),
      median = numeric(),
      count = numeric())
}

tmean_upload <- rbind(tmean_early,
                      tmean_provisional) %>%
  rbind(tmean_stable)

try(replace_ds("a2cc9a65-d1cc-4134-9615-1980a03c53a4", tmean_upload))

# for (i in 1:length(prismFileList$files)){
#   unlink(paste(str_split(prismFileList$abs_path[i], "/")[[1]][1:5], collapse = '/'), recursive = T)
# }
