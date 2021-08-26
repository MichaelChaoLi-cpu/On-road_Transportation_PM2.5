library(plm)
library(tidyverse)
library(spdep)
library(rgdal)
library(tigris)
library(rgeos)
library(stargazer)
library(rsq)
library(splm)
library(GWmodel)
library(gwpr)
library(spdep)
library(gwpr)
library(sandwich)
library(stargazer)

options(tigris_use_cache = TRUE)

setwd(".\\Data\\")

data_set <- read.csv(file = 'dataset.csv')
summ <- data_set %>%
  dplyr::select(pm25, co2_mean , Developed_Open_Space_perc , Developed_Low_Intensity_perc ,
           Developed_Medium_Intensity_perc , Developed_High_Intensity_perc ,
           Open_Water_perc , Woody_Wetlands_perc , Emergent_Herbaceous_Wetlands_perc , 
           Deciduous_Forest_perc , Evergreen_Forest_perc , Mixed_Forest_perc , 
           Shrub_perc , Grassland_perc , Pasture_perc , Cultivated_Crops_perc ,
           pop_density ,
           summer_tmmx , winter_tmmx , summer_rmax , winter_rmax)
stargazer(summ, title = "Table XXX: Data Statistic Summary",  type = "text", 
          no.space = T,
          covariate.labels = c("PM2.5",
            "CO2 Emission on Road(Million Tons/km2)",
                               "Open Space Developed Area (%)",
                               "Low Intensity Developed Area (%)", "Medium Intensity Developed Area (%)",
                               'High Intensity Developed Area (%)', 'Open Water (%)',
                               'Woody Wetlands (%)', 'Emergent Herbaceous Wetlands (%)',
                               'Deciduous Forest (%)', 'Evergreen Forest (%)',
                               'Mixed Forest (%)', 'Shrub (%)', 'Grassland (%)',
                               'Pasture (%)', 'Cultivated Crops (%)',
                               'Population Density (Thousand Capita/km2)',
                               'Mean Of Daily Temperature In Summer', 
                               'Mean Of Daily Temperature In Winter', 
                               'Mean Of Relative Humidity In Summer', 
                               'Mean Of Relative Humidity In Winter'
          ),
          iqr = F, out = "RegressionResult\\summary_table.html"
) 


# panel regression
pdata <- pdata.frame(data_set, index = c('GEOID', 'year'))
reg.form.pm = pm25 ~ co2_mean + Developed_Open_Space_perc + Developed_Low_Intensity_perc +
  Developed_Medium_Intensity_perc + Developed_High_Intensity_perc +
  Open_Water_perc + Woody_Wetlands_perc + Emergent_Herbaceous_Wetlands_perc + 
  Deciduous_Forest_perc + Evergreen_Forest_perc + Mixed_Forest_perc + 
  Shrub_perc + Grassland_perc + Pasture_perc + Cultivated_Crops_perc +
  pop_density +
  summer_tmmx + winter_tmmx + summer_rmax + winter_rmax

random.pm <- plm(reg.form.pm, data = pdata, model = 'random')
summary(random.pm)

fix.pm <- plm(reg.form.pm, data = pdata, model = 'within')
summary(fix.pm)

pooling.pm <- plm(reg.form.pm, data = pdata, model = 'pooling')
summary(pooling.pm)


#test
plmtest(pooling.pm)
pFtest(fix.pm, pooling.pm)
phtest(fix.pm, random.pm)
plmtest(pooling.pm, type = c("bp"))
#### accoding to the test, fem is better

stargazer(fix.pm,
          title = "Table R1: Panel Test",  type = "text", 
          no.space = T,
          covariate.labels = c("CO2 Emission on Road(Million Tons/km2)",
                               "Open Space Developed Area (%)",
                               "Low Intensity Developed Area (%)", "Medium Intensity Developed Area (%)",
                               'High Intensity Developed Area (%)', 'Open Water (%)',
                               'Woody Wetlands (%)', 'Emergent Herbaceous Wetlands (%)',
                               'Deciduous Forest (%)', 'Evergreen Forest (%)',
                               'Mixed Forest (%)', 'Shrub (%)', 'Grassland (%)',
                               'Pasture (%)', 'Cultivated Crops (%)',
                               'Population Density (Thousand Capita/km2)',
                               'Mean Of Daily Temperature In Summer', 
                               'Mean Of Daily Temperature In Winter', 
                               'Mean Of Relative Humidity In Summer', 
                               'Mean Of Relative Humidity In Winter'
          ),
          dep.var.labels = c("PM2.5"),
          column.labels = c("FEM"),
          iqr = F, out = "05_RegressionResult\\Panel_regression.html") 
# panel regression

data_set <- read.csv(file = 'dataset.csv')
data_set$GEOID <- data_set$GEOID %>% as.factor() 
data_set <- data_set %>%
  dplyr::select(-X, -co2_sum, -TotalArea, Group.1, Group.2, pop_total)

#extracted the spatial information
setwd(".\\Data\\Boundary\\")
shape_usa_county <- readOGR(dsn = ".", layer = "tl_2016_us_county")
shape_usa_county@data$GEOID <- shape_usa_county@data$GEOID %>%
  as.integer()
shape_usa_county@data$GEOID <- shape_usa_county@data$GEOID %>%
  as.factor()
shape_usa_county@data <- shape_usa_county@data %>%
  dplyr::select(GEOID)
shape_usa_with_data <- data_set %>%
  dplyr::select(GEOID) %>% unique()
shape_usa_county <- geo_join(shape_usa_county, shape_usa_with_data, 'GEOID', 'GEOID', how = 'inner')
shape_usa_county@data$year <- shape_usa_county@data$year %>% as.numeric()
rm(shape_usa_with_data)
queen.nb = poly2nb(shape_usa_county, row.names = shape_usa_county$GEOID)
W = nb2mat(queen.nb)
setwd(".\\Data\\")

#test spatial
listW = mat2listw(W, style = "W")
slag.test <- slmtest(reg.form, data = data_set, index = c('GEOID', 'year'), 
                     model = 'within', listw = listW, test="rlme")
slag.test
serr.test <- slmtest(reg.form, data = data_set, index = c('GEOID', 'year'), 
                     listw = listW, model = 'within', test="rlml")
serr.test
#test sparial

#SPAM
spam <- spml(reg.form, data = data_set, index = c('GEOID', 'year'), 
             listw = mat2listw(W), model = 'within',
                    spatial.error = 'none', lag = T)
summary(spam)
impacts_spam <- summary(spdep::impacts(spam, listw = mat2listw(W, style = "W"), 
                                       time = 16, R = 500), zstats = T, short = T)
impacts_spam

#SPEM
spem <- spml(reg.form, data = data_set, index = c('GEOID', 'year'), listw = W, model = 'within',
                   spatial.error = 'kkp', lag = F)
spem
summary(spem)

#SPDM
spdm <- spml(reg.form, data = data_set, index = c('GEOID', 'year'), listw = W, model = 'within',
                    spatial.error = 'kkp', lag = T)
summary(spdm)
impacts_spdm <- summary(spdep::impacts(spdm, listw = mat2listw(W, style = "W"),
                                       time = 16, R = 500), zstats = T, short = T)
impacts_spdm

#sphtest
random.spdm <- spml(reg.form, data = data_set, index = c('GEOID', 'year'), listw = W, model = 'random',
                           spatial.error = 'kkp', lag = T)
sphtest(random.spdm, spdm) #note "spdm" is based on FEM

#r2 estimation
ss_tot <- sum((data_set$pm25 - mean(data_set$pm25))^2)
SSres.spdm <- sum(spdm$residuals ^ 2)
R2.spdm <- 1 - SSres.spdm/ss_tot

SSres.spam <- sum(spamslag$residuals ^ 2)
#in the model object of SPEM and FEM, R2 had been included 


R2.spam <- 1 - SSres.spam/ss_tot

