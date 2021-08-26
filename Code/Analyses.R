library(tidyverse)
library(spdep)
library(rgdal)
library(tigris)
library(rgeos)
library(rsq)
library(splm)
library(GWmodel)
library(gwpr)
library(spdep)
library(gwpr)
library(sandwich)
library(stargazer)

setwd(".\\Data\\")
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
rm(shape_usa_with_data)
queen.nb = poly2nb(shape_usa_county, row.names = shape_usa_county$GEOID)
W = nb2mat(queen.nb)
setwd(".\\Data\\")

#SPDM
#----------------------------------- Read the Equation IV in the Manuscript
spdm <- spml(reg.form, data = data_set, index = c('GEOID', 'year'), listw = W, model = 'within',
             spatial.error = 'kkp', lag = T)
summary(spdm)
impacts_spdm <- summary(spdep::impacts(spdm, listw = mat2listw(W, style = "W"),
                                       time = 16, R = 500), zstats = T, short = T)
impacts_spdm

#--------output durbin model of PM2.5
co1 <- impacts_spdm$res$direct
co2 <- impacts_spdm$res$indirect
co3 <- impacts_spdm$res$total

se1 <- impacts_spdm$semat[,1] %>% as.vector()
se2 <- impacts_spdm$semat[,2] %>% as.vector()
se3 <- impacts_spdm$semat[,3] %>% as.vector()

# 1000000^2 million tons to g
# co3[1] is the coefficient of on-road transportation CO2 emission
# 404g is CO2 emission per mile average passegar car uasage
# 1.609344 mile to km
# 10^9 to billion km
pm25_bikm <- 1000000^2 / co3[1] / 404 * 1.609344 / 10^9
pm25_bikm_low <- 1000000^2 / (co3[1] + se3[1] * 1.96) / 404 * 1.609344 / 10^9
pm25_bikm_up <- 1000000^2 / (co3[1] - se3[1] * 1.96) / 404 * 1.609344 / 10^9


#create spatail panel dataset
shape_usa_county <- st_as_sf(shape_usa_county) %>% dplyr::select(GEOID)
spatial_panel <- left_join(data_set, shape_usa_county)
spatial_panel <- as_Spatial(spatial_panel$geometry)
rm(shape_usa_county)

spatial_panel <- spatial_panel %>%
  subset(year > 2002) 
# the reason we have drop two years data is the limitation of the size of matrix in R

#----------------------------------- Read the Equation VIII in the Manuscript
Dmat <- st.dist(dp.locat = coordinates(spatial_panel), obs.tv = spatial_panel@data$year)

#----------------------------------- Read the Equation VII in the Manuscript
bw.gt <- bw.gtwr(reg.form, data = spatial_panel, obs.tv = spatial_panel@data$year,
                 adaptive = T, approach = "CV", kernel = "bisquare",
                 theta = 0, longlat = F, st.dMat = Dmat)

#----------------------------------- Read the Equation V and VI in the Manuscript
Model_gtwr <- gtwr(reg.form, data = spatial_panel, regression.points = spatial_panel,
                   reg.tv = spatial_panel@data$year,
                   obs.tv = spatial_panel@data$year, st.bw = bw.gt,
                   kernel = "bisquare", adaptive = T, st.dMat = Dmat)

summary(Model_gtwr$SDF@data$co2_mean)

#----------------------------------- Read the Equation IX in the Manuscript
Model_gtwr$SDF@data$on_road_share <- Model_gtwr$SDF@data$co2_mean * Model_gtwr$lm$model$co2_mean /
  Model_gtwr$lm$model$pm25 * 100

#----------------------------------- Read the Equation X in the Manuscript
Model_gtwr$SDF@data <- Model_gtwr$SDF@data %>%
  mutate(on_road_share = ifelse(on_road_share > 100, 100, 
                                ifelse(on_road_share < 0, 0, on_road_share)))

mort = raster("mort_tiff.tif")
Model_gtwr$SDF@data$Mortality <- extract(mort, Model_gtwr$SDF, method="simple", fun = sum)
Model_gtwr$SDF@data$GEOID <- ab_t@data$GEOID
Model_gtwr$SDF@data$year <- ab_t@data$year
predict_mortality <- Model_gtwr$SDF@data %>%
  dplyr::select(GEOID, year, Mortality)
predict_mortality$pm25 <- Model_gtwr$lm$model$pm25
predict_mortality_wider <- predict_mortality %>%
  pivot_wider(names_from = year, values_from = pm25)
predict_mortality_wider$mor_2010 <- predict_mortality_wider$Mortality
predict_mortality_wider$mor_2011 <- predict_mortality_wider$mor_2010 *
  (1 + 0.012 * (predict_mortality_wider$`2011` - predict_mortality_wider$`2010`))
predict_mortality_wider$mor_2012 <- predict_mortality_wider$mor_2011 *
  (1 + 0.012 * (predict_mortality_wider$`2012` - predict_mortality_wider$`2011`))
predict_mortality_wider$mor_2013 <- predict_mortality_wider$mor_2012 *
  (1 + 0.012 * (predict_mortality_wider$`2013` - predict_mortality_wider$`2012`))
predict_mortality_wider$mor_2014 <- predict_mortality_wider$mor_2013 *
  (1 + 0.012 * (predict_mortality_wider$`2014` - predict_mortality_wider$`2013`))
predict_mortality_wider$mor_2015 <- predict_mortality_wider$mor_2014 *
  (1 + 0.012 * (predict_mortality_wider$`2015` - predict_mortality_wider$`2014`))
predict_mortality_wider$mor_2016 <- predict_mortality_wider$mor_2015 *
  (1 + 0.012 * (predict_mortality_wider$`2016` - predict_mortality_wider$`2015`))
predict_mortality_wider$mor_2009 <- predict_mortality_wider$mor_2010 /
  (1 + 0.012 * (predict_mortality_wider$`2010` - predict_mortality_wider$`2009`))
predict_mortality_wider$mor_2008 <- predict_mortality_wider$mor_2009 /
  (1 + 0.012 * (predict_mortality_wider$`2009` - predict_mortality_wider$`2008`))
predict_mortality_wider$mor_2007 <- predict_mortality_wider$mor_2008 /
  (1 + 0.012 * (predict_mortality_wider$`2008` - predict_mortality_wider$`2007`))
predict_mortality_wider$mor_2006 <- predict_mortality_wider$mor_2007 /
  (1 + 0.012 * (predict_mortality_wider$`2007` - predict_mortality_wider$`2006`))
predict_mortality_wider$mor_2005 <- predict_mortality_wider$mor_2006 /
  (1 + 0.012 * (predict_mortality_wider$`2006` - predict_mortality_wider$`2005`))
predict_mortality_wider$mor_2004 <- predict_mortality_wider$mor_2005 /
  (1 + 0.012 * (predict_mortality_wider$`2005` - predict_mortality_wider$`2004`))
predict_mortality_wider$mor_2003 <- predict_mortality_wider$mor_2004 /
  (1 + 0.012 * (predict_mortality_wider$`2004` - predict_mortality_wider$`2003`))
predict_mortality_wider <- predict_mortality_wider %>%
  dplyr::select(GEOID, mor_2010:mor_2003)
colnames(predict_mortality_wider) <- c("GEOID",    "2010", "2011", "2012",
                                       "2013", "2014", "2015", "2016", "2009", "2008",
                                       "2007", "2006", "2005", "2004", "2003")
predict_mortality <- predict_mortality_wider %>%
  pivot_longer(names_to = "year", values_to = "mortality_predict", cols = `2010`:`2003`)
rm(predict_mortality_wider)
predict_mortality$year <- predict_mortality$year %>% as.numeric()
Model_gtwr$SDF@data <- left_join(Model_gtwr$SDF@data, predict_mortality, 
                                 by = c("GEOID","year"))
Model_gtwr$SDF@data$Mortality <- Model_gtwr$SDF@data$mortality_predict
Model_gtwr$SDF@data$Mortality_on_road <- Model_gtwr$SDF@data$Mortality * 
  Model_gtwr$SDF@data$on_road_share / 100

Model_gtwr$SDF@data$Mortality_on_road <- Model_gtwr$SDF@data$Mortality_on_road %>% 
  as.numeric()
sum(Model_gtwr$SDF@data$Mortality)
sum(Model_gtwr$SDF@data$Mortality_on_road)
summary(Model_gtwr$SDF@data$Mortality_on_road)

#R2 of GTWR
gtwr.res <- Model_gtwr$SDF@data$Intercept + 
  Model_gtwr$SDF@data$co2_mean * Model_gtwr$lm$model$co2_mean +
  Model_gtwr$SDF@data$Developed_Open_Space_perc * Model_gtwr$lm$model$Developed_Open_Space_perc +
  Model_gtwr$SDF@data$Developed_Low_Intensity_perc * Model_gtwr$lm$model$Developed_Low_Intensity_perc +
  Model_gtwr$SDF@data$Developed_Medium_Intensity_perc * Model_gtwr$lm$model$Developed_Medium_Intensity_perc +
  Model_gtwr$SDF@data$Developed_High_Intensity_perc * Model_gtwr$lm$model$Developed_High_Intensity_perc +
  Model_gtwr$SDF@data$Open_Water_perc * Model_gtwr$lm$model$Open_Water_perc +
  Model_gtwr$SDF@data$Woody_Wetlands_perc * Model_gtwr$lm$model$Woody_Wetlands_perc +  
  Model_gtwr$SDF@data$Emergent_Herbaceous_Wetlands_perc * Model_gtwr$lm$model$Emergent_Herbaceous_Wetlands_perc +  
  Model_gtwr$SDF@data$Deciduous_Forest_perc * Model_gtwr$lm$model$Deciduous_Forest_perc +
  Model_gtwr$SDF@data$Evergreen_Forest_perc * Model_gtwr$lm$model$Evergreen_Forest_perc +
  Model_gtwr$SDF@data$Mixed_Forest_perc * Model_gtwr$lm$model$Mixed_Forest_perc +
  Model_gtwr$SDF@data$Shrub_perc * Model_gtwr$lm$model$Shrub_perc +
  Model_gtwr$SDF@data$Grassland_perc * Model_gtwr$lm$model$Grassland_perc +
  Model_gtwr$SDF@data$Pasture_perc * Model_gtwr$lm$model$Pasture_perc +
  Model_gtwr$SDF@data$Cultivated_Crops_perc * Model_gtwr$lm$model$Cultivated_Crops_perc +
  Model_gtwr$SDF@data$pop_density * Model_gtwr$lm$model$pop_density +  
  Model_gtwr$SDF@data$summer_tmmx * Model_gtwr$lm$model$summer_tmmx +
  Model_gtwr$SDF@data$winter_tmmx * Model_gtwr$lm$model$winter_tmmx +
  Model_gtwr$SDF@data$summer_rmax * Model_gtwr$lm$model$summer_rmax +
  Model_gtwr$SDF@data$winter_rmax * Model_gtwr$lm$model$winter_rmax -  
  Model_gtwr$lm$model$pm25  

SStot.gtwr <- sum((Model_gtwr$lm$model$pm25 - mean(Model_gtwr$lm$model$pm25))^2)  
SSres.gtwr <- sum(gtwr.res^2)
R2.gtwr <- 1 - SSres.gtwr/SStot.gtwr