#install.packages("dplyr", dependencies = TRUE)
#install.packages("fst")
#install.packages("sas7bdat")
#install.packages("ggplot2")
#install.packages("sf")

require(dplyr)
require(tidyr)
require(sf)
require(lubridate)
require(stringr)
require(tidyverse)

########################################################
# 1. Clean covariate data
########################################################

#read in outage data:
setwd("~/outagesxasthma/data/cc-confounders")
cov_dta <- fst::read.fst("cc_confounders-2.fst")
cov_dta <-cov_dta %>%
  dplyr::select("dps_id",
                "mean_temp",
                "avg_daily_pm",
                "avg_daily_no2",
                "avg_daily_o3",
                "max_ws",
                "max_temp",
                "mean_rh",
                "sum_precip",
                "Date") %>% 
  dplyr::mutate(DayDate = lubridate::ymd(Date, tz = "America/New_York"), date = lubridate::ymd(Date, tz = "America/New_York") ) 


### read in localities
setwd("~/outagesxasthma/data/locality")
locality <- st_read("e_locality.shp") %>%
  mutate(dps_id = PRIME_DPS_) %>%
  select(dps_id) %>%
  sf::st_transform(sparcs_sf, crs = 4326) %>%
  st_make_valid()

cov_dta2 <- right_join(locality, cov_dta, by = "dps_id")

########################################################
# 2. clean and prep outages data
########################################################

#unique(outage_dta$bin)
setwd("~/outagesxasthma/data/nycha-outages")
outage_dta <- st_read("data_elec_build.shp") %>%
  filter(otg_typ == "electric") %>%
  mutate(start_date = as.POSIXct(rprt_dt, format = "%m/%d/%y\n\n%I:%M%p", tz = "America/New_York"),
         end_date = as.POSIXct(end_dat, format = "%m/%d/%y %I:%M %p", tz = "America/New_York"),
         duration = difftime(end_date, start_date, units = "hours"),
         duration = as.numeric(duration))# %>%
 # mutate(end_date = if_else(duration > 24, start_date + hours(24), end_date))

od <-  outage_dta %>% filter(!start_date > "2023-01-01 00:00:00 EST") %>%
  filter(duration > 0)%>%
  mutate(date_range = map2(start_date, end_date, seq, by = "hours")) %>%
  unnest(date_range) %>%
  mutate(date = as.Date(date_range, tz = "America/New_York")) %>%
  as.data.frame() %>%
  select(bin, planned, date, tds, num_bld) %>%
  group_by(bin, date, tds, num_bld) %>%
  summarize(exposed = 1,
            planned = paste(unique(planned), collapse = ", ")) %>%
  ungroup( )

full_data <- complete(od, bin = unique(od$bin), date = seq(min(od$date), max(od$date), by = "day"))


outage_exposure <- full_join(full_data, od) %>%
  mutate(exposed = if_else(is.na(exposed),0, exposed)) 



########################################################
# 3. prep sparcs data
########################################################
setwd("~/mold-busters/data/sparcs-data")

sparcs_2019 <- read.csv("ed1722nyc_ariel_updates_2019.csv") %>%
  drop_na(F1E.output.bin)%>%
  mutate(ADMIT_DT = as.Date(ADMIT_DT, format = "%d%b%Y" , tz = "America/New_York" ))

sparcs_2020 <- read.csv("ed1722nyc_ariel_updates_2020.csv") %>%
  drop_na(F1E.output.bin)%>%
  mutate(ADMIT_DT = as.Date(ADMIT_DT, format = "%d%b%Y" , tz = "America/New_York" ))

sparcs_2021 <- read.csv("ed1722nyc_ariel_updates_2021.csv") %>%
  drop_na(F1E.output.bin)%>%
  mutate(ADMIT_DT = as.Date(ADMIT_DT, format = "%d%b%Y" , tz = "America/New_York" ))

sparcs_2022 <- read.csv("ed1722nyc_ariel_updates_2022.csv") %>%
  drop_na(F1E.output.bin)%>%
  mutate(ADMIT_DT = as.Date(ADMIT_DT, format = "%d%b%Y", tz = "America/New_York" ))

sparcs <- rbind(sparcs_2019,
                sparcs_2020,
                sparcs_2021,
                sparcs_2022) %>%
  mutate(F1E.latitude = gsub('"', '', F1E.latitude))

sparcs_sf <- sf::st_as_sf(sparcs, coords = c("F1E.longitude", "F1E.latitude"), crs = 4326) 
#per the documentation, the coordinates are wgs


# 0c Add unique id variable ### need to see what I need to do about repeated visits

sparcs_sf<- sparcs_sf %>% mutate(id = 1:n()) %>%
  mutate(bin = as.numeric(F1E.output.bin))


########################################################
# 6. Create case and control dates
########################################################

# 0b Convert case date to posixct
sparcs_sf$CaseDate = parse_date_time(sparcs_sf$ADMIT_DT, "ymd", tz = "America/New_York")

####*************************************************
#### 1: Define Functions to Create Control Hours ####
####*************************************************

# Here I edit functions developed by Sebastian Rowland

# 1a Define function to create potentially matching datehours
# Note: our data is in Eastern Time (ET)
#       as such, we are matching on the socially-recognized time that the case would have experienced
#       In particular, due to daylight savings time, certain hours do not 'exist' in ET
#       This will lead to some NA's in the dataset 
#       While converting to UTC would avoid these NA's, 
#       the cases experienced time according to ET 
#       and their time-varying factors would follow ET, not UTC 
#       so if we match on UTC we would not be matching on hour of the day 

make_control_hr <- function(hours1, BeforeAfter, WK){ 
  VarName <- paste0(BeforeAfter, '_', stringr::str_trunc(WK, 1, 'left', ''))    
  hours1 %>% mutate(!!VarName := CaseDate + as.period(7 * WK, 'day'))  
}


# 1d Use function to create bidirectionally symmetric datehours 
hours1 <-  sparcs_sf
hours1 <- hours1 %>% 
  mutate(CaseDate = parse_date_time(ADMIT_DT, 'ymd', tz = 'America/New_York')) 
hours1 <- make_control_hr(hours1, 'Before', -4)
hours1 <- make_control_hr(hours1, 'Before', -3)
hours1 <- make_control_hr(hours1, 'Before', -2)
hours1 <- make_control_hr(hours1, 'Before', -1)
hours1 <- make_control_hr(hours1, 'CaseDay', 0)
hours1 <- make_control_hr(hours1, 'After', 1)
hours1 <- make_control_hr(hours1, 'After', 2)
hours1 <- make_control_hr(hours1, 'After', 3)
hours1 <- make_control_hr(hours1, 'After', 4)

# 1e Put in long format by HourName
hours2 <- hours1 %>% 
  gather('DayName', 'DayDate', contains('CaseDay_'),
         contains('Before_'), contains('After_') ) 

# 1f Stratify by month of event 
hours3 <- hours2 %>% filter(month(CaseDate) == month(DayDate))


# 1g attach these dates to their correct data

hours3 <- hours3 %>%
  select( id, bin, DayName, DayDate, UNIQUE_PERSONAL_ID, Race_Ethn, GENDER_CD,
          AGE,ADMIT_DT) %>%
  mutate(date = format(DayDate, "%Y-%m-%d", tz = "America/New_York")) %>%
  mutate(date = as.Date(date, tz = "America/New_York")) %>%
  mutate(bin = bin)%>%
  group_by(id, bin, DayName, DayDate, UNIQUE_PERSONAL_ID, Race_Ethn, GENDER_CD,
           AGE,ADMIT_DT) %>%
  slice(1)

########################################################
# 4. Now link localities to their outage data
########################################################

outage_exposure$bin <- as.numeric(outage_exposure$bin)


sparcs_outages <- dplyr::left_join(hours3,outage_exposure, multiple = "all", by = c("bin", "date")) %>% 
  drop_na(bin)%>%
  drop_na(exposed)%>%
  select( date, exposed, bin, id, planned, DayName,UNIQUE_PERSONAL_ID, Race_Ethn, GENDER_CD,
          AGE) %>%
  group_by(date, exposed, bin, id, planned, DayName,UNIQUE_PERSONAL_ID, Race_Ethn, GENDER_CD,
           AGE) %>%
  slice(1) %>%
  ungroup()


### use bin to link


sparcs_outages<- st_join(sparcs_outages,cov_dta2) %>%
  filter(date.x == date.y)



### pare down the columns:

sparcs_build_final <- sparcs_outages %>%
  select(id, 
         bin,
         date.x,
         exposed,
         DayName, 
         AGE, 
         DayDate, 
         mean_temp,
         avg_daily_pm,
         avg_daily_no2,
         avg_daily_o3,
         max_temp,
         max_ws, 
         mean_rh, 
         sum_precip, 
         planned, 
         Race_Ethn, 
         GENDER_CD,
         UNIQUE_PERSONAL_ID) %>%
  distinct() %>%
  drop_na(mean_temp) %>%
  as.data.frame() 

#### there is not electricity or elevator data until December 2019 - adjusting dataset accordingly:
sparcs_build_final_v2 <- sparcs_build_final%>%
  mutate(year_month = format(date.x, "%Y-%m")) %>%
  filter(year_month >= "2019-12")%>%
  group_by(id) %>% mutate(n = n()) %>%
  as.data.frame() %>%
  select(-geometry)


setwd("~/outagesxasthma/data")
#fst::write_fst(sparcs_build_final_v2, "sparcs_outages_buildings.fst")
fst::write_fst(sparcs_build_final_v2, "sparcs_outages_buildings_full.fst")



