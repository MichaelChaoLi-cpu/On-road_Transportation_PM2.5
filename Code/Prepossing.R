library(readr)
library(tidyverse)
library("dplyr")
library(haven)
library(readxl)
library(foreign)
library(zoo)

setwd(".\\Data\\")

# Function for Washing Land Cover Data
Wash_Land_Cover <- function(data, time) {
  # data is the dataset, and time is year number, e.g., 2010
  data <- data %>%
    mutate(year = time) %>%
    rename(
      Unknown = VALUE_0,
      Open_Water = VALUE_11,
      Perenial_Ice = VALUE_12,
      Developed_Open_Space = VALUE_21,
      Developed_Low_Intensity = VALUE_22,
      Developed_Medium_Intensity = VALUE_23,
      Developed_High_Intensity = VALUE_24,
      Barren_Land = VALUE_31,
      Deciduous_Forest = VALUE_41,
      Evergreen_Forest = VALUE_42,
      Mixed_Forest = VALUE_43,
      Shrub = VALUE_52,
      Grassland = VALUE_71,
      Pasture = VALUE_81,
      Cultivated_Crops = VALUE_82,
      Woody_Wetlands = VALUE_90,
      Emergent_Herbaceous_Wetlands = VALUE_95
    ) %>%
    mutate(
      TotalArea = Open_Water + Perenial_Ice + Developed_Open_Space + 
        Developed_Low_Intensity + Developed_Medium_Intensity + 
        Developed_High_Intensity + Barren_Land + Deciduous_Forest + 
        Evergreen_Forest + Mixed_Forest + Shrub + Grassland + Pasture + 
        Cultivated_Crops + Woody_Wetlands + Emergent_Herbaceous_Wetlands
    ) %>%
    mutate(
      Open_Water_perc = Open_Water / TotalArea * 100,
      Perenial_Ice_perc = Perenial_Ice / TotalArea * 100,
      Developed_Open_Space_perc = Developed_Open_Space / TotalArea * 100,
      Developed_Low_Intensity_perc = Developed_Low_Intensity / TotalArea * 100,
      Developed_Medium_Intensity_perc = Developed_Medium_Intensity / TotalArea * 100,
      Developed_High_Intensity_perc = Developed_High_Intensity / TotalArea * 100,
      Barren_Land_perc = Barren_Land / TotalArea * 100,
      Deciduous_Forest_perc = Deciduous_Forest / TotalArea * 100,
      Evergreen_Forest_perc = Evergreen_Forest / TotalArea * 100,
      Mixed_Forest_perc = Mixed_Forest / TotalArea * 100,
      Shrub_perc = Shrub / TotalArea * 100,
      Grassland_perc = Grassland / TotalArea * 100,
      Pasture_perc = Pasture / TotalArea * 100,
      Cultivated_Crops_perc = Cultivated_Crops / TotalArea * 100,
      Woody_Wetlands_perc = Woody_Wetlands / TotalArea * 100,
      Emergent_Herbaceous_Wetlands_perc = Emergent_Herbaceous_Wetlands / TotalArea * 100,
      Green_rate = Deciduous_Forest_perc + Evergreen_Forest_perc + Mixed_Forest_perc + 
        Shrub_perc + Grassland_perc + Pasture_perc + Cultivated_Crops_perc ,
      Blue_rate = Open_Water_perc +
        Woody_Wetlands_perc + Emergent_Herbaceous_Wetlands_perc,
      Grey_rate = Developed_Open_Space_perc + Developed_Low_Intensity_perc +
        Developed_Medium_Intensity_perc + Developed_High_Intensity_perc,
      Other_rate = Barren_Land_perc + Perenial_Ice_perc
    ) %>%
    select(
      -Unknown, -Open_Water, -Perenial_Ice, -Developed_Open_Space,
      -Developed_Low_Intensity, -Developed_Medium_Intensity,
      -Developed_High_Intensity, -Barren_Land, -Deciduous_Forest,
      -Evergreen_Forest, -Mixed_Forest, -Shrub, -Grassland, -Pasture, 
      -Cultivated_Crops, -Woody_Wetlands, -Emergent_Herbaceous_Wetlands
    )
  data$GEOID <- data$GEOID %>% as.numeric()
  return(data)
}

# land cover https://www.mrlc.gov/data
# original data
LC_01 <- read.dbf(file = 'LandCover\\LC_2001.dbf', as.is = T) 
LC_01 <- Wash_Land_Cover(LC_01, 2001)

LC_04 <- read.dbf(file = 'LandCover\\LC_2004.dbf', as.is = T) 
LC_04 <- Wash_Land_Cover(LC_04, 2004)

LC_06 <- read.dbf(file = 'LandCover\\LC_2006.dbf', as.is = T) 
LC_06 <- Wash_Land_Cover(LC_06, 2006)

LC_08 <- read.dbf(file = 'LandCover\\LC_2008.dbf', as.is = T) 
LC_08 <- Wash_Land_Cover(LC_08, 2008)

LC_11 <- read.dbf(file = 'LandCover\\LC_2011.dbf', as.is = T) 
LC_11 <- Wash_Land_Cover(LC_11, 2011)

LC_13 <- read.dbf(file = 'LandCover\\LC_2013.dbf', as.is = T) 
LC_13 <- Wash_Land_Cover(LC_13, 2013)

LC_16 <- read.dbf(file = 'LandCover\\LC_2016.dbf', as.is = T) 
LC_16 <- Wash_Land_Cover(LC_01, 2016)
# original data

# generate data
LC_02 <- LC_01 %>%
  select(GEOID) %>%
  mutate(year = 2002)
LC_03 <- LC_01 %>%
  select(GEOID) %>%
  mutate(year = 2003)
LC_05 <- LC_01 %>%
  select(GEOID) %>%
  mutate(year = 2005)
LC_07 <- LC_01 %>%
  select(GEOID) %>%
  mutate(year = 2007)
LC_09 <- LC_01 %>%
  select(GEOID) %>%
  mutate(year = 2009)
LC_10 <- LC_01 %>%
  select(GEOID) %>%
  mutate(year = 2010)
LC_12 <- LC_01 %>%
  select(GEOID) %>%
  mutate(year = 2012)
LC_14 <- LC_01 %>%
  select(GEOID) %>%
  mutate(year = 2014)
LC_15 <- LC_01 %>%
  select(GEOID) %>%
  mutate(year = 2015)
  
LC_data <- plyr::rbind.fill(LC_01, LC_02, LC_03, LC_04, LC_05, LC_06, LC_07, 
                 LC_08, LC_09, LC_10, LC_11, LC_12, LC_13, LC_14,
                 LC_15, LC_16) %>%
  arrange(GEOID, year)


LC_data <- LC_data %>% 
  group_by(GEOID) %>% 
  mutate(
    Open_Water_perc = na.approx(Open_Water_perc, maxgap = Inf, rule = 2),
    Perenial_Ice_perc = na.approx(Perenial_Ice_perc, maxgap = Inf, rule = 2),
    Developed_Open_Space_perc = na.approx(Developed_Open_Space_perc, maxgap = Inf, rule = 2),
    Developed_Low_Intensity_perc = na.approx(Developed_Low_Intensity_perc, maxgap = Inf, rule = 2),
    Developed_Medium_Intensity_perc = na.approx(Developed_Medium_Intensity_perc, maxgap = Inf, rule = 2),
    Developed_High_Intensity_perc = na.approx(Developed_High_Intensity_perc, maxgap = Inf, rule = 2),
    Barren_Land_perc = na.approx(Barren_Land_perc, maxgap = Inf, rule = 2),
    Deciduous_Forest_perc = na.approx(Deciduous_Forest_perc, maxgap = Inf, rule = 2),
    Evergreen_Forest_perc = na.approx(Evergreen_Forest_perc, maxgap = Inf, rule = 2),
    Mixed_Forest_perc = na.approx(Mixed_Forest_perc, maxgap = Inf, rule = 2),
    Shrub_perc = na.approx(Shrub_perc, maxgap = Inf, rule = 2),
    Grassland_perc = na.approx(Grassland_perc, maxgap = Inf, rule = 2),
    Pasture_perc = na.approx(Pasture_perc, maxgap = Inf, rule = 2),
    Cultivated_Crops_perc = na.approx(Cultivated_Crops_perc, maxgap = Inf, rule = 2),
    Woody_Wetlands_perc = na.approx(Woody_Wetlands_perc, maxgap = Inf, rule = 2),
    Emergent_Herbaceous_Wetlands_perc = na.approx(Emergent_Herbaceous_Wetlands_perc, maxgap = Inf, rule = 2),
    Green_rate = Deciduous_Forest_perc + Evergreen_Forest_perc + Mixed_Forest_perc + 
      Shrub_perc + Grassland_perc + Pasture_perc + Cultivated_Crops_perc ,
    Blue_rate = Open_Water_perc +
      Woody_Wetlands_perc + Emergent_Herbaceous_Wetlands_perc,
    Grey_rate = Developed_Open_Space_perc + Developed_Low_Intensity_perc +
      Developed_Medium_Intensity_perc + Developed_High_Intensity_perc,
    Other_rate = Barren_Land_perc + Perenial_Ice_perc
         )
rm(LC_01)
rm(LC_02)
rm(LC_03)
rm(LC_04)
rm(LC_05)
rm(LC_06)
rm(LC_07)
rm(LC_08)
rm(LC_09)
rm(LC_10)
rm(LC_11)
rm(LC_12)
rm(LC_13)
rm(LC_14)
rm(LC_15)
rm(LC_16)
# Land Cover https://www.mrlc.gov/data

# CO2 on road https://daac.ornl.gov/CMS/guides/CMS_CO2_Relationships.html
CO2_01 <- read.dbf(file = 'CO2\\CO2_onroad_2001.dbf', as.is = T) %>%
  mutate(year = 2001) %>%
  select(GEOID, MEAN, year, SUM) %>%
  rename(
    co2_mean = MEAN,
    co2_sum = SUM
  )
CO2_02 <- read.dbf(file = 'CO2\\CO2_onroad_2002.dbf', as.is = T) %>%
  mutate(year = 2002) %>%
  select(GEOID, MEAN, year, SUM) %>%
  rename(
    co2_mean = MEAN,
    co2_sum = SUM
  )
CO2_03 <- read.dbf(file = 'CO2\\CO2_onroad_2003.dbf', as.is = T) %>%
  mutate(year = 2003) %>%
  select(GEOID, MEAN, year, SUM) %>%
  rename(
    co2_mean = MEAN,
    co2_sum = SUM
  )
CO2_04 <- read.dbf(file = 'CO2\\CO2_onroad_2004.dbf', as.is = T) %>%
  mutate(year = 2004) %>%
  select(GEOID, MEAN, year, SUM) %>%
  rename(
    co2_mean = MEAN,
    co2_sum = SUM
  )
CO2_05 <- read.dbf(file = 'CO2\\CO2_onroad_2005.dbf', as.is = T) %>%
  mutate(year = 2005) %>%
  select(GEOID, MEAN, year, SUM) %>%
  rename(
    co2_mean = MEAN,
    co2_sum = SUM
  )
CO2_06 <- read.dbf(file = 'CO2\\CO2_onroad_2006.dbf', as.is = T) %>%
  mutate(year = 2006) %>%
  select(GEOID, MEAN, year, SUM) %>%
  rename(
    co2_mean = MEAN,
    co2_sum = SUM
  )
CO2_07 <- read.dbf(file = 'CO2\\CO2_onroad_2007.dbf', as.is = T) %>%
  mutate(year = 2007) %>%
  select(GEOID, MEAN, year, SUM) %>%
  rename(
    co2_mean = MEAN,
    co2_sum = SUM
  )
CO2_08 <- read.dbf(file = 'CO2\\CO2_onroad_2008.dbf', as.is = T) %>%
  mutate(year = 2008) %>%
  select(GEOID, MEAN, year, SUM) %>%
  rename(
    co2_mean = MEAN,
    co2_sum = SUM
  )
CO2_09 <- read.dbf(file = 'CO2\\CO2_onroad_2009.dbf', as.is = T) %>%
  mutate(year = 2009) %>%
  select(GEOID, MEAN, year, SUM) %>%
  rename(
    co2_mean = MEAN,
    co2_sum = SUM
  )
CO2_10 <- read.dbf(file = 'CO2\\CO2_onroad_2010.dbf', as.is = T) %>%
  mutate(year = 2010) %>%
  select(GEOID, MEAN, year, SUM) %>%
  rename(
    co2_mean = MEAN,
    co2_sum = SUM
  )
CO2_11 <- read.dbf(file = 'CO2\\CO2_onroad_2011.dbf', as.is = T) %>%
  mutate(year = 2011) %>%
  select(GEOID, MEAN, year, SUM) %>%
  rename(
    co2_mean = MEAN,
    co2_sum = SUM
  )
CO2_12 <- read.dbf(file = 'CO2\\CO2_onroad_2012.dbf', as.is = T) %>%
  mutate(year = 2012) %>%
  select(GEOID, MEAN, year, SUM) %>%
  rename(
    co2_mean = MEAN,
    co2_sum = SUM
  )
CO2_13 <- read.dbf(file = 'CO2\\CO2_onroad_2013.dbf', as.is = T) %>%
  mutate(year = 2013) %>%
  select(GEOID, MEAN, year, SUM) %>%
  rename(
    co2_mean = MEAN,
    co2_sum = SUM
  )
CO2_14 <- read.dbf(file = 'CO2\\CO2_onroad_2014.dbf', as.is = T) %>%
  mutate(year = 2014) %>%
  select(GEOID, MEAN, year, SUM) %>%
  rename(
    co2_mean = MEAN,
    co2_sum = SUM
  )
CO2_15 <- read.dbf(file = 'CO2\\CO2_onroad_2015.dbf', as.is = T) %>%
  mutate(year = 2015) %>%
  select(GEOID, MEAN, year, SUM) %>%
  rename(
    co2_mean = MEAN,
    co2_sum = SUM
  )
CO2_16 <- read.dbf(file = 'CO2\\CO2_onroad_2016.dbf', as.is = T) %>%
  mutate(year = 2016) %>%
  select(GEOID, MEAN, year, SUM) %>%
  rename(
    co2_mean = MEAN,
    co2_sum = SUM
  )
co2_data <- plyr::rbind.fill(CO2_01, CO2_02, CO2_03, CO2_04, CO2_05, CO2_06, CO2_07, 
                            CO2_08, CO2_09, CO2_10, CO2_11, CO2_12, CO2_13, CO2_14,
                            CO2_15, CO2_16) 
rm(CO2_01)
rm(CO2_02)
rm(CO2_03)
rm(CO2_04)
rm(CO2_05)
rm(CO2_06)
rm(CO2_07)
rm(CO2_08)
rm(CO2_09)
rm(CO2_10)
rm(CO2_11)
rm(CO2_12)
rm(CO2_13)
rm(CO2_14)
rm(CO2_15)
rm(CO2_16)
co2_data$GEOID <- co2_data$GEOID %>% as.numeric()
co2_data <- co2_data %>%
  mutate(co2_mean = co2_mean / 1000000,
         co2_sum = co2_sum / 1000000)
# unit of co2 is million tons
# CO2 on road https://daac.ornl.gov/CMS/guides/CMS_CO2_Relationships.html


# pm25 https://www.cdc.gov/nceh/tracking/topics/AirQuality.htm
pm25 <- read.csv('Daily_PM2.5_Concentrations_All_County__2001-2016.csv') #Not included in repo
pm25_01 <- pm25 %>%
  mutate(GEOID = statefips * 1000 + countyfips) %>%
  select(year, GEOID, PM25_mean_pred) %>%
  filter(year == 2001) 
pm25_01 <- pm25_01 %>%
  aggregate(by = list(pm25_01$GEOID, pm25_01$year), FUN = "mean", na.rm = T)

pm25_02 <- pm25 %>%
  mutate(GEOID = statefips * 1000 + countyfips) %>%
  select(year, GEOID, PM25_mean_pred) %>%
  filter(year == 2002) 
pm25_02 <- pm25_02 %>%
  aggregate(by = list(pm25_02$GEOID, pm25_02$year), FUN = "mean", na.rm = T)

pm25_03 <- pm25 %>%
  mutate(GEOID = statefips * 1000 + countyfips) %>%
  select(year, GEOID, PM25_mean_pred) %>%
  filter(year == 2003) 
pm25_03 <- pm25_03 %>%
  aggregate(by = list(pm25_03$GEOID, pm25_03$year), FUN = "mean", na.rm = T)

pm25_04 <- pm25 %>%
  mutate(GEOID = statefips * 1000 + countyfips) %>%
  select(year, GEOID, PM25_mean_pred) %>%
  filter(year == 2004) 
pm25_04 <- pm25_04 %>%
  aggregate(by = list(pm25_04$GEOID, pm25_04$year), FUN = "mean", na.rm = T)

pm25_05 <- pm25 %>%
  mutate(GEOID = statefips * 1000 + countyfips) %>%
  select(year, GEOID, PM25_mean_pred) %>%
  filter(year == 2005) 
pm25_05 <- pm25_05 %>%
  aggregate(by = list(pm25_05$GEOID, pm25_05$year), FUN = "mean", na.rm = T)

pm25_06 <- pm25 %>%
  mutate(GEOID = statefips * 1000 + countyfips) %>%
  select(year, GEOID, PM25_mean_pred) %>%
  filter(year == 2006) 
pm25_06 <- pm25_06 %>%
  aggregate(by = list(pm25_06$GEOID, pm25_06$year), FUN = "mean", na.rm = T)

pm25_07 <- pm25 %>%
  mutate(GEOID = statefips * 1000 + countyfips) %>%
  select(year, GEOID, PM25_mean_pred) %>%
  filter(year == 2007) 
pm25_07 <- pm25_07 %>%
  aggregate(by = list(pm25_07$GEOID, pm25_07$year), FUN = "mean", na.rm = T)

pm25_08 <- pm25 %>%
  mutate(GEOID = statefips * 1000 + countyfips) %>%
  select(year, GEOID, PM25_mean_pred) %>%
  filter(year == 2008) 
pm25_08 <- pm25_08 %>%
  aggregate(by = list(pm25_08$GEOID, pm25_08$year), FUN = "mean", na.rm = T)

pm25_09 <- pm25 %>%
  mutate(GEOID = statefips * 1000 + countyfips) %>%
  select(year, GEOID, PM25_mean_pred) %>%
  filter(year == 2009) 
pm25_09 <- pm25_09 %>%
  aggregate(by = list(pm25_09$GEOID, pm25_09$year), FUN = "mean", na.rm = T)

pm25_10 <- pm25 %>%
  mutate(GEOID = statefips * 1000 + countyfips) %>%
  select(year, GEOID, PM25_mean_pred) %>%
  filter(year == 2010) 
pm25_10 <- pm25_10 %>%
  aggregate(by = list(pm25_10$GEOID, pm25_10$year), FUN = "mean", na.rm = T)

pm25_11 <- pm25 %>%
  mutate(GEOID = statefips * 1000 + countyfips) %>%
  select(year, GEOID, PM25_mean_pred) %>%
  filter(year == 2011) 
pm25_11 <- pm25_11 %>%
  aggregate(by = list(pm25_11$GEOID, pm25_11$year), FUN = "mean", na.rm = T)

pm25_12 <- pm25 %>%
  mutate(GEOID = statefips * 1000 + countyfips) %>%
  select(year, GEOID, PM25_mean_pred) %>%
  filter(year == 2012) 
pm25_12 <- pm25_12 %>%
  aggregate(by = list(pm25_12$GEOID, pm25_12$year), FUN = "mean", na.rm = T)

pm25_13 <- pm25 %>%
  mutate(GEOID = statefips * 1000 + countyfips) %>%
  select(year, GEOID, PM25_mean_pred) %>%
  filter(year == 2013) 
pm25_13 <- pm25_13 %>%
  aggregate(by = list(pm25_13$GEOID, pm25_13$year), FUN = "mean", na.rm = T)

pm25_14 <- pm25 %>%
  mutate(GEOID = statefips * 1000 + countyfips) %>%
  select(year, GEOID, PM25_mean_pred) %>%
  filter(year == 2014) 
pm25_14 <- pm25_14 %>%
  aggregate(by = list(pm25_14$GEOID, pm25_14$year), FUN = "mean", na.rm = T)

pm25_15 <- pm25 %>%
  mutate(GEOID = statefips * 1000 + countyfips) %>%
  select(year, GEOID, PM25_mean_pred) %>%
  filter(year == 2015) 
pm25_15 <- pm25_15 %>%
  aggregate(by = list(pm25_15$GEOID, pm25_15$year), FUN = "mean", na.rm = T)

pm25_16 <- pm25 %>%
  mutate(GEOID = statefips * 1000 + countyfips) %>%
  select(year, GEOID, PM25_mean_pred) %>%
  filter(year == 2016) 
pm25_16 <- pm25_16 %>%
  aggregate(by = list(pm25_16$GEOID, pm25_16$year), FUN = "mean", na.rm = T)

rm(pm25)
pm25_data <- rbind(pm25_01, pm25_02, pm25_03, pm25_04, pm25_05, 
                   pm25_06, pm25_07, pm25_08, pm25_09, pm25_10, 
                   pm25_11, pm25_12, pm25_13, pm25_14, pm25_15, 
                   pm25_16)
rm(pm25_01)
rm(pm25_02)
rm(pm25_03)
rm(pm25_04)
rm(pm25_05)
rm(pm25_06)
rm(pm25_07)
rm(pm25_08)
rm(pm25_09)
rm(pm25_10)
rm(pm25_11)
rm(pm25_12)
rm(pm25_13)
rm(pm25_14)
rm(pm25_15)
rm(pm25_16)
pm25_data <- pm25_data %>%
  rename(pm25 = PM25_mean_pred)
#### pm25 https://www.cdc.gov/nceh/tracking/topics/AirQuality.htm

#weather
weather <- read.csv('temp_seasonal_county.csv') %>%
  rename(GEOID = fips)


#weather

#population in every year
POP_01 <- read.dbf(file = 'Pop\\pop_2001.dbf', as.is = T) %>% mutate(year = 2001)
POP_02 <- read.dbf(file = 'Pop\\pop_2002.dbf', as.is = T) %>% mutate(year = 2002)
POP_03 <- read.dbf(file = 'Pop\\pop_2003.dbf', as.is = T) %>% mutate(year = 2003)
POP_04 <- read.dbf(file = 'Pop\\pop_2004.dbf', as.is = T) %>% mutate(year = 2004)
POP_05 <- read.dbf(file = 'Pop\\pop_2005.dbf', as.is = T) %>% mutate(year = 2005)
POP_06 <- read.dbf(file = 'Pop\\pop_2006.dbf', as.is = T) %>% mutate(year = 2006)
POP_07 <- read.dbf(file = 'Pop\\pop_2007.dbf', as.is = T) %>% mutate(year = 2007)
POP_08 <- read.dbf(file = 'Pop\\pop_2008.dbf', as.is = T) %>% mutate(year = 2008)
POP_09 <- read.dbf(file = 'Pop\\pop_2009.dbf', as.is = T) %>% mutate(year = 2009)
POP_10 <- read.dbf(file = 'Pop\\pop_2010.dbf', as.is = T) %>% mutate(year = 2010)
POP_11 <- read.dbf(file = 'Pop\\pop_2011.dbf', as.is = T) %>% mutate(year = 2011)
POP_12 <- read.dbf(file = 'Pop\\pop_2012.dbf', as.is = T) %>% mutate(year = 2012)
POP_13 <- read.dbf(file = 'Pop\\pop_2013.dbf', as.is = T) %>% mutate(year = 2013)
POP_14 <- read.dbf(file = 'Pop\\pop_2014.dbf', as.is = T) %>% mutate(year = 2014)
POP_15 <- read.dbf(file = 'Pop\\pop_2015.dbf', as.is = T) %>% mutate(year = 2015)
POP_16 <- read.dbf(file = 'Pop\\pop_2016.dbf', as.is = T) %>% mutate(year = 2016)

pop_data <- rbind(POP_01, POP_02, POP_03, POP_04, 
                  POP_05, POP_06, POP_07, POP_08, 
                  POP_09, POP_10, POP_11, POP_12, 
                  POP_13, POP_14, POP_15, POP_16)
rm(POP_01)
rm(POP_02)
rm(POP_03)
rm(POP_04)
rm(POP_05)
rm(POP_06)
rm(POP_07)
rm(POP_08)
rm(POP_09)
rm(POP_10)
rm(POP_11)
rm(POP_12)
rm(POP_13)
rm(POP_14)
rm(POP_15)
rm(POP_16)
pop_data$GEOID <- pop_data$GEOID %>% as.integer()
pop_data <- pop_data %>%
  select(GEOID, year, MEAN, SUM) %>%
  mutate(
    pop_density = MEAN * 100 / 1000,
    pop_total = SUM
  ) %>%
  select(-MEAN, -SUM)
#population in every year

# merge data
data_set <- left_join(co2_data, LC_data)
rm(co2_data)
rm(LC_data)
data_set <- inner_join(data_set, pm25_data)
rm(pm25_data)
data_set <- inner_join(data_set, weather)
rm(weather)
data_set <- inner_join(data_set, pop_data)
rm(pop_data)

data_set %>% write.csv(file = 'dataset.csv')


