library(ggplot2)
library("readxl")
library(tidyverse)
library(tidyverse)
library(dplyr)
library(tmap)
library(rgdal)
library(tigris)
library(trend)
library(tmaptools)
library(gridExtra)
library("viridis") 
library(magick)
library(stargazer)
library(plotrix)

setwd(".\\Data\\")

#figure 1
data_set <- read.csv(file = 'dataset.csv')
year_data <- data_set %>%
  dplyr::select(-GEOID) %>%
  aggregate(by = list(data_set$year), 
            FUN = "mean", na.rm = T) %>%
  dplyr::select(year, co2_mean, pm25) %>%
  mutate(co2_mean = co2_mean * 100) %>%
  pivot_longer(cols = co2_mean:pm25, names_to = 'item', values_to = 'count') %>%
  rename(Value = count,
         Year = year)
figure_1 <- ggplot(data = year_data, aes(x = Year, y = Value, linetype = item , colour = item, shape = item )) +
  geom_line(stat = "identity", position=position_dodge(), size = 1) +
  geom_point(size = 4) +
  theme(axis.text.x = element_text(color = 'black', size = 24),
        axis.text.y = element_text(color = 'black', size = 24),
        title = element_text(color = 'black', size = 26),
        legend.text = element_text(color = 'black', size = 24),
        legend.direction = "horizontal",
        legend.position = "bottom")+
  scale_y_continuous(name = 'Means of the Counties',
                     breaks = c(10, 20, 30, 40),
                     labels = c('10', '20', '30', '40')) +
  scale_linetype_discrete(guide = F) +
  scale_colour_discrete(name = "Items:", breaks = c("co2_mean","pm25"),
                        labels = c(expression(paste(CO[2]," Emission ","(10,000t/",km^{2},")")), 
                                   expression(paste(PM[2.5]," (",mu,"g/",m^{3},")")) )) +
  scale_shape_discrete(guide = F) +
  theme_bw() + theme(legend.position = "bottom")

figure_1

tiff('Figure//01_trend.jpg', units = "in", width = 8, height = 6, res = 300, compression = 'lzw')
figure_1
dev.off()


#figure 2
spdm_table <- read_excel("05_RegressionResult/random_sem.xlsx", sheet = 1) %>% 
  as.tibble() %>%
  rename('Coefficients' = 'Estimate') %>%
  filter(Dep == 'PM2.5')
spdm_table$item <- factor(spdm_table$item, levels = unique(spdm_table$item))

figure_2 <- ggplot(data = spdm_table, aes(x = item, y = Coefficients)) +
  geom_bar(stat = "identity", position=position_dodge(), fill = 'orange') +
  geom_errorbar(aes(ymin = Coefficients - Std.Error * 1.96, ymax = Coefficients + Std.Error * 1.96),
                position=position_dodge(1), width = .2) +
  geom_text(aes(label = label), color="blue",
            position=position_dodge(1), size = 7) + 
  scale_fill_brewer(palette="Paired") +
  scale_x_discrete(name = '', 
                   labels = c(
                     expression("CO2 Emission on Road\n(Million Tons/km2)"),
                     "Open Space\nDeveloped Area (%)",
                     "Low Intensity\nDeveloped Area (%)", "Medium Intensity\nDeveloped Area (%)",
                     'High Intensity\nDeveloped Area (%)', 'Open Water (%)',
                     'Woody Wetlands (%)', 'Emergent Herbaceous\nWetlands (%)',
                     'Deciduous Forest (%)', 'Evergreen Forest (%)',
                     'Mixed Forest (%)', 'Shrub (%)', 'Grassland (%)',
                     'Pasture (%)', 'Cultivated Crops (%)',
                     "Population Density\n(1000/km2)",
                     "Mean Of Daily Temperature\nIn Summer (K Degree)", 
                     "Mean Of Daily Temperature\nIn Winter (K Degree)", 
                     'Mean Of Relative Humidity\nIn Summer (%)', 
                     'Mean Of Relative Humidity\nIn Winter (%)'
                              )) +
  scale_y_continuous(name = expression(paste("Marginal Effects on ", PM[2.5], " Concentration")),
                   breaks = c(-2, -1, -0.5, 0, 0.5, 1, 2, 4),
                   labels = c('-2', '-1', '-0.5', '0', '0.5', '1', '2', '4')) +
  labs(caption = "Note: *** p < 1%, ** p < 5%, * p < 10%") +
  theme(axis.text.x = element_text(color = 'black', size = 14, angle = 90, vjust = 0.5),
        axis.text.y = element_text(color = 'black', size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        title = element_text(color = 'black', size = 22),
        legend.direction = "horizontal",
        legend.position = "bottom") 
figure_2

tiff('Figure//02_coef.jpg', units="in", width=16, height=12, res=300, compression = 'lzw')
figure_2
dev.off()

#figure 3
setwd(".\\Data\\Boundary\\")
shape_usa_county <- readOGR(dsn = ".", layer = "tl_2016_us_county")
shape_usa_county@data <- shape_usa_county@data %>%
  dplyr::select(GEOID)
shape_usa_county@data$GEOID <- shape_usa_county@data$GEOID %>%
  as.integer()

Model_gtwr$SDF$contri <- Model_gtwr$SDF$co2_mean * Model_gtwr$lm$model$co2_mean / Model_gtwr$lm$model$pm25 * 100
Model_gtwr$SDF$GEOID <- ab_t@data$GEOID
coeff_GTWR <- Model_gtwr$SDF@data
coeff_GTWR_mean <- coeff_GTWR %>%
  aggregate(by = list(coeff_GTWR$GEOID), FUN = "mean")
shape_coeff_mean <- geo_join(shape_usa_county, coeff_GTWR_mean, 'GEOID', 'GEOID', how = 'inner')

# tm set
title_size = .0001
legend_title_size = 1
margin = 0
tmap_mode('plot')
bound <- readOGR(dsn = ".", layer = "tl_2016_us_BEA")
# tm set
prj <- get_projection(bound,  guess.longlat = FALSE)
setwd(".\\Data\\")

brk_1 = c(0, 5, 10, 15, 20, 25, 30 , 35, 40)
labels_brk_1 = c('0', '', '10', '', '20', '', '30', '', '40')
a_4 <-
  tm_shape(shape_coeff_mean) +
  tm_polygons(col = 'contri', pal = viridis(9), auto.palette.mapping = FALSE,
              title = "Average Contribution of CO2 Emission\non Road to Countys' PM2.5 Concentration (%):",
              border.alpha = 0, legend.is.portrait = F,  style = 'cont',
              breaks = brk_1, labels = labels_brk_1) +
  tm_shape(bound) +
  tm_polygons(lwd = 1, alpha = .1) +
  tm_text("Name", remove.overlap = T, size = 0.6, col = 'white') +
  tm_grid(projection = prj, alpha = .25)+ 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("left", "bottom"),
    main.title.position = c("center", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) 
a_4 %>%
  tmap_save("04_Figure\\AverageContribution.png", ,width = 210, height = 120, units = 'mm', dpi = 1000)

#figure 4
brk_1 = c(0, 1, 2, 4, 8, 16, 32 , 64, 100, 500)
labels_brk_1 = c('0', '1', '2', '4', '8', '16', '32' , '64', '100', '500+')
a_5 <-
  tm_shape(Model_gtwr$SDF %>%
             subset(time_stamp == 2010)) +
  tm_polygons(col = 'Mortality_trans', pal = "Reds", auto.palette.mapping = FALSE,
              title = "Mortality Caused by PM2.5 from On-road Transportation",
              border.alpha = 0, legend.is.portrait = F,  style = 'cont',
              breaks = brk_1, labels = labels_brk_1) +
  tm_shape(bound) +
  tm_polygons(lwd = 2, alpha = .1) +
  tm_text("Name", remove.overlap = T) +
  tm_grid(projection = prj, alpha = .25)+ 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("left", "bottom"),
    main.title.position = c("center", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  )
a_5 %>%
  tmap_save("04_Figure\\Morta_2010.png", ,width = 210, height = 120, units = 'mm', dpi = 1000)

#figure s1
a_s1 <-
  tm_shape(shape_coeff_mean) +
  tm_polygons(col = 'co2_mean', pal = 'Reds', auto.palette.mapping = FALSE,
              title = "Average Effects of CO2 Emission\non Road on Local PM2.5:",
              border.alpha = 0, legend.is.portrait = F,  style = 'cont') +
  tm_shape(bound) +
  tm_polygons(lwd = 2, alpha = .1) +
  tm_text("Name", remove.overlap = T, size = 0.7) +
  tm_grid(projection = prj, alpha = .25)+ 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("left", "bottom"),
    main.title.position = c("center", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) 
a_s1 %>%
  tmap_save("04_Figure\\CoefficientsCO2PM25.png", ,width = 210, height = 120, units = 'mm', dpi = 1000)

#figure s2
brk_1 = c(0, 5, 10, 15, 20, 25, 30 , 35, 40)
labels_brk_1 = c('0', '', '10', '', '20', '', '30', '', '40')
a_3_1 <-
  tm_shape(Model_gtwr$SDF %>%
             subset(time_stamp == 2003)) +
  tm_polygons(col = 'contri', pal = viridis(9), auto.palette.mapping = FALSE,
              title = "Contribution of On-road Transportation\nto Local PM2.5 in 2003(%):",
              border.alpha = 0, legend.is.portrait = F,  style = 'cont',
              breaks = brk_1, labels = labels_brk_1) +
  tm_shape(bound) +
  tm_polygons(lwd = 1, alpha = .1) +
  tm_text("Name", remove.overlap = T, size = 0.5, col = 'white') +
  tm_grid(projection = prj, alpha = .25)+ 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("left", "bottom"),
    main.title.position = c("center", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) +
  tm_credits("2003", size = 2.5,
             position = c("RIGHT", "BOTTOM"))

a_3_2 <-
  tm_shape(Model_gtwr$SDF %>%
             subset(time_stamp == 2004)) +
  tm_polygons(col = 'contri', pal = viridis(9), auto.palette.mapping = FALSE,
              title = "Contribution of On-road Transportation\nto Local PM2.5 in 2004(%):",
              border.alpha = 0, legend.is.portrait = F,  style = 'cont',
              breaks = brk_1, labels = labels_brk_1) +
  tm_shape(bound) +
  tm_polygons(lwd = 1, alpha = .1) +
  tm_text("Name", remove.overlap = T, size = 0.5, col = 'white') +
  tm_grid(projection = prj, alpha = .25)+ 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("left", "bottom"),
    main.title.position = c("center", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) +
  tm_credits("2004", size = 2.5,
             position = c("RIGHT", "BOTTOM"))

a_3_3 <-
  tm_shape(Model_gtwr$SDF %>%
             subset(time_stamp == 2005)) +
  tm_polygons(col = 'contri', pal = viridis(9), auto.palette.mapping = FALSE,
              title = "Contribution of On-road Transportation\nto Local PM2.5 in 2005(%):",
              border.alpha = 0, legend.is.portrait = F,  style = 'cont',
              breaks = brk_1, labels = labels_brk_1) +
  tm_shape(bound) +
  tm_polygons(lwd = 1, alpha = .1) +
  tm_text("Name", remove.overlap = T, size = 0.5, col = 'white') +
  tm_grid(projection = prj, alpha = .25)+ 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("left", "bottom"),
    main.title.position = c("center", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  ) +
  tm_credits("2005", size = 2.5,
             position = c("RIGHT", "BOTTOM"))

a_3_4 <-
  tm_shape(Model_gtwr$SDF %>%
             subset(time_stamp == 2006)) +
  tm_polygons(col = 'contri', pal = viridis(9), auto.palette.mapping = FALSE,
              title = "Contribution of On-road Transportation\nto Local PM2.5 in 2006(%):",
              border.alpha = 0, legend.is.portrait = F,  style = 'cont',
              breaks = brk_1, labels = labels_brk_1) +
  tm_shape(bound) +
  tm_polygons(lwd = 1, alpha = .1) +
  tm_text("Name", remove.overlap = T, size = 0.5, col = 'white') +
  tm_grid(projection = prj, alpha = .25)+ 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("left", "bottom"),
    main.title.position = c("center", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  )+
  tm_credits("2006", size = 2.5,
             position = c("RIGHT", "BOTTOM"))

a_3_5 <-
  tm_shape(Model_gtwr$SDF %>%
             subset(time_stamp == 2007)) +
  tm_polygons(col = 'contri', pal = viridis(9), auto.palette.mapping = FALSE,
              title = "Contribution of On-road Transportation\nto Local PM2.5 in 2007(%):",
              border.alpha = 0, legend.is.portrait = F,  style = 'cont',
              breaks = brk_1, labels = labels_brk_1) +
  tm_shape(bound) +
  tm_polygons(lwd = 1, alpha = .1) +
  tm_text("Name", remove.overlap = T, size = 0.5, col = 'white') +
  tm_grid(projection = prj, alpha = .25)+ 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("left", "bottom"),
    main.title.position = c("center", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  )+
  tm_credits("2007", size = 2.5,
             position = c("RIGHT", "BOTTOM"))

a_3_6 <-
  tm_shape(Model_gtwr$SDF %>%
             subset(time_stamp == 2008)) +
  tm_polygons(col = 'contri', pal = viridis(9), auto.palette.mapping = FALSE,
              title = "Contribution of On-road Transportation\nto Local PM2.5 in 2008(%):",
              border.alpha = 0, legend.is.portrait = F,  style = 'cont',
              breaks = brk_1, labels = labels_brk_1) +
  tm_shape(bound) +
  tm_polygons(lwd = 1, alpha = .1) +
  tm_text("Name", remove.overlap = T, size = 0.5, col = 'white') +
  tm_grid(projection = prj, alpha = .25)+ 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("left", "bottom"),
    main.title.position = c("center", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  )+
  tm_credits("2008", size = 2.5,
             position = c("RIGHT", "BOTTOM"))

a_3_7 <-
  tm_shape(Model_gtwr$SDF %>%
             subset(time_stamp == 2009)) +
  tm_polygons(col = 'contri', pal = viridis(9), auto.palette.mapping = FALSE,
              title = "Contribution of On-road Transportation\nto Local PM2.5 in 2009(%):",
              border.alpha = 0, legend.is.portrait = F,  style = 'cont',
              breaks = brk_1, labels = labels_brk_1) +
  tm_shape(bound) +
  tm_polygons(lwd = 1, alpha = .1) +
  tm_text("Name", remove.overlap = T, size = 0.5, col = 'white') +
  tm_grid(projection = prj, alpha = .25)+ 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("left", "bottom"),
    main.title.position = c("center", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  )+
  tm_credits("2009", size = 2.5,
             position = c("RIGHT", "BOTTOM"))

a_3_8 <-
  tm_shape(Model_gtwr$SDF %>%
             subset(time_stamp == 2010)) +
  tm_polygons(col = 'contri', pal = viridis(9), auto.palette.mapping = FALSE,
              title = "Contribution of On-road Transportation\nto Local PM2.5 in 2010(%):",
              border.alpha = 0, legend.is.portrait = F,  style = 'cont',
              breaks = brk_1, labels = labels_brk_1) +
  tm_shape(bound) +
  tm_polygons(lwd = 1, alpha = .1) +
  tm_text("Name", remove.overlap = T, size = 0.5, col = 'white') +
  tm_grid(projection = prj, alpha = .25)+ 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("left", "bottom"),
    main.title.position = c("center", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  )+
  tm_credits("2010", size = 2.5,
             position = c("RIGHT", "BOTTOM"))

a_3_9 <-
  tm_shape(Model_gtwr$SDF %>%
             subset(time_stamp == 2011)) +
  tm_polygons(col = 'contri', pal = viridis(9), auto.palette.mapping = FALSE,
              title = "Contribution of On-road Transportation\nto Local PM2.5 in 2011(%):",
              border.alpha = 0, legend.is.portrait = F,  style = 'cont',
              breaks = brk_1, labels = labels_brk_1) +
  tm_shape(bound) +
  tm_polygons(lwd = 1, alpha = .1) +
  tm_text("Name", remove.overlap = T, size = 0.5, col = 'white') +
  tm_grid(projection = prj, alpha = .25)+ 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("left", "bottom"),
    main.title.position = c("center", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  )+
  tm_credits("2011", size = 2.5,
             position = c("RIGHT", "BOTTOM"))

a_3_10 <-
  tm_shape(Model_gtwr$SDF %>%
             subset(time_stamp == 2012)) +
  tm_polygons(col = 'contri', pal = viridis(9), auto.palette.mapping = FALSE,
              title = "Contribution of On-road Transportation\nto Local PM2.5 in 2012(%):",
              border.alpha = 0, legend.is.portrait = F,  style = 'cont',
              breaks = brk_1, labels = labels_brk_1) +
  tm_shape(bound) +
  tm_polygons(lwd = 1, alpha = .1) +
  tm_text("Name", remove.overlap = T, size = 0.5, col = 'white') +
  tm_grid(projection = prj, alpha = .25)+ 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("left", "bottom"),
    main.title.position = c("center", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  )+
  tm_credits("2012", size = 2.5,
             position = c("RIGHT", "BOTTOM"))

a_3_11 <-
  tm_shape(Model_gtwr$SDF %>%
             subset(time_stamp == 2013)) +
  tm_polygons(col = 'contri', pal = viridis(9), auto.palette.mapping = FALSE,
              title = "Contribution of On-road Transportation\nto Local PM2.5 in 2013(%):",
              border.alpha = 0, legend.is.portrait = F,  style = 'cont',
              breaks = brk_1, labels = labels_brk_1) +
  tm_shape(bound) +
  tm_polygons(lwd = 1, alpha = .1) +
  tm_text("Name", remove.overlap = T, size = 0.5, col = 'white') +
  tm_grid(projection = prj, alpha = .25)+ 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("left", "bottom"),
    main.title.position = c("center", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  )+
  tm_credits("2013", size = 2.5,
             position = c("RIGHT", "BOTTOM"))

a_3_12 <-
  tm_shape(Model_gtwr$SDF %>%
             subset(time_stamp == 2014)) +
  tm_polygons(col = 'contri', pal = viridis(9), auto.palette.mapping = FALSE,
              title = "Contribution of On-road Transportation\nto Local PM2.5 in 2014(%):",
              border.alpha = 0, legend.is.portrait = F,  style = 'cont',
              breaks = brk_1, labels = labels_brk_1) +
  tm_shape(bound) +
  tm_polygons(lwd = 1, alpha = .1) +
  tm_text("Name", remove.overlap = T, size = 0.5, col = 'white') +
  tm_grid(projection = prj, alpha = .25)+ 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("left", "bottom"),
    main.title.position = c("center", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  )+
  tm_credits("2014", size = 2.5,
             position = c("RIGHT", "BOTTOM"))

a_3_13 <-
  tm_shape(Model_gtwr$SDF %>%
             subset(time_stamp == 2015)) +
  tm_polygons(col = 'contri', pal = viridis(9), auto.palette.mapping = FALSE,
              title = "Contribution of On-road Transportation\nto Local PM2.5 in 2015(%):",
              border.alpha = 0, legend.is.portrait = F,  style = 'cont',
              breaks = brk_1, labels = labels_brk_1) +
  tm_shape(bound) +
  tm_polygons(lwd = 1, alpha = .1) +
  tm_text("Name", remove.overlap = T, size = 0.5, col = 'white') +
  tm_grid(projection = prj, alpha = .25)+ 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("left", "bottom"),
    main.title.position = c("center", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  )+
  tm_credits("2015", size = 2.5,
             position = c("RIGHT", "BOTTOM"))

a_3_14 <-
  tm_shape(Model_gtwr$SDF %>%
             subset(time_stamp == 2016)) +
  tm_polygons(col = 'contri', pal = viridis(9), auto.palette.mapping = FALSE,
              title = "Contribution of On-road Transportation\nto Local PM2.5 in 2016(%):",
              border.alpha = 0, legend.is.portrait = F,  style = 'cont',
              breaks = brk_1, labels = labels_brk_1) +
  tm_shape(bound) +
  tm_polygons(lwd = 1, alpha = .1) +
  tm_text("Name", remove.overlap = T, size = 0.5, col = 'white') +
  tm_grid(projection = prj, alpha = .25)+ 
  tm_layout(
    inner.margins = c(margin, margin, margin, margin),
    title.size = title_size, 
    legend.position = c("left", "bottom"),
    main.title.position = c("center", "bottom"),
    legend.title.size = legend_title_size,
    legend.text.size = legend_title_size * 0.75
  )+
  tm_credits("2016", size = 2.5,
             position = c("RIGHT", "BOTTOM"))

a_3_1 %>%
  tmap_save("04_Figure\\contri\\ContriCO2PM25_2_01.png", ,width = 210, height = 120, units = 'mm', dpi = 1000)
a_3_2 %>%
  tmap_save("04_Figure\\contri\\ContriCO2PM25_2_02.png", ,width = 210, height = 120, units = 'mm', dpi = 1000)
a_3_3 %>%
  tmap_save("04_Figure\\contri\\ContriCO2PM25_2_03.png", ,width = 210, height = 120, units = 'mm', dpi = 1000)
a_3_4 %>%
  tmap_save("04_Figure\\contri\\ContriCO2PM25_2_04.png", ,width = 210, height = 120, units = 'mm', dpi = 1000)
a_3_5 %>%
  tmap_save("04_Figure\\contri\\ContriCO2PM25_2_05.png", ,width = 210, height = 120, units = 'mm', dpi = 1000)
a_3_6 %>%
  tmap_save("04_Figure\\contri\\ContriCO2PM25_2_06.png", ,width = 210, height = 120, units = 'mm', dpi = 1000)
a_3_7 %>%
  tmap_save("04_Figure\\contri\\ContriCO2PM25_2_07.png", ,width = 210, height = 120, units = 'mm', dpi = 1000)
a_3_8 %>%
  tmap_save("04_Figure\\contri\\ContriCO2PM25_2_08.png", ,width = 210, height = 120, units = 'mm', dpi = 1000)
a_3_9 %>%
  tmap_save("04_Figure\\contri\\ContriCO2PM25_2_09.png", ,width = 210, height = 120, units = 'mm', dpi = 1000)
a_3_10 %>%
  tmap_save("04_Figure\\contri\\ContriCO2PM25_2_10.png", ,width = 210, height = 120, units = 'mm', dpi = 1000)
a_3_11 %>%
  tmap_save("04_Figure\\contri\\ContriCO2PM25_2_11.png", ,width = 210, height = 120, units = 'mm', dpi = 1000)
a_3_12 %>%
  tmap_save("04_Figure\\contri\\ContriCO2PM25_2_12.png", ,width = 210, height = 120, units = 'mm', dpi = 1000)
a_3_13 %>%
  tmap_save("04_Figure\\contri\\ContriCO2PM25_2_13.png", ,width = 210, height = 120, units = 'mm', dpi = 1000)
a_3_14 %>%
  tmap_save("04_Figure\\contri\\ContriCO2PM25_2_14.png", ,width = 210, height = 120, units = 'mm', dpi = 1000)
