# Contribution of On-Road Transportation to PM2.5  
<img src="https://img.shields.io/badge/Study%20Status-Results%20Available-yellow.svg" alt="Study Status: Results Available">   
  
This is the data repository for public available code and data to reproduce analyses of the relationship between on-road transportation and PM2.5 concentration done by Chao Li and Shunsuke Managi.
  
## Summary Results
  
![](/Figure/time_ser_low.gif)
  
Figure: Annually contributions of on-road transportation to PM2.5 in each county from 2003 to based on estimation of the GTWR model.  
6.17 billion kilometres (km) per km2 on-road transportation increase is associated with a 1-μg⁄m3  PM2.5 concentration increase in a specific county in the contiguous United States (CONUS), using the SPDM.  
On-road transportation marginally contributes to PM2.5, only 1.09% in the CONUS, based on the estimation of the GTWR.   
Approximately 3,605 premature deaths are attributed to PM2.5 originating from on-road transportation, accounting for 6.59% in 2010, and about 50,223 premature deaths ascribe to PM2.5 taking 6.49% from 2003 to 2016.   
  
## Code
[Prepossing.R](/Code/Prepossing.R) includes the code to extract all necessary data and prepocess data for statistical analyses.  
[Basic_Model_Selection.R](/Code/Basic_Model_Selection.R) includes the code to select the best basic model.  
[Analyses.R](/Code/Analyses.R) includes the code to implement the spatial panel Durbin model (SPDM) and geographical and temporal weighted model (GTWR).  
[Figure.R](/Code/Figure.R) includes the code to generate figures in Main Text and Supplementary Materials.  
  
## Data
**Daily_PM2.5_Concentrations_All_County__2001-2016.csv** The data are obtained from the Centers for Disease Control and Prevention, as estimated with the downscaler model by the Environmental Protection Agency (EPA) (more details about this dataset: <https://www.cdc.gov/nceh/tracking/topics/AirQuality.htm>). The data include the estimated daily PM2.5 concentration in each county from 2001 to 2016. We calculate the mean annual PM2.5 concentration in each county, and the unit is microgram per cubic metre (μg⁄m3).  
**Note:** This dataset is large. We, therefore, recommend to download from the website.  
**CO2_onroad_2001.dbf - CO2_onroad_2016.dbf** We obtain the annual on-road CO2 emissions at a 1-km resolution from 2001 to 2016 from the National Aeronautics and Space Administration (more details about this dataset: <https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1735>) and extract the tables from the rasters by ArcGIS pro 2.0. The county boundaries in the CONUS are acquired from the U.S. Census Bureau, created in 2016. The on-road county-level CO2 emissions are expressed in units of million tons per km2. Because the areas of the different counties vary dramatically, the total CO2 emissions in each county cannot be compared.   
**pop_2001.dbf - pop_2016.dbf** The population density data are the raster data provided by the University of Southampton from 2001 to 2016 with a 100-m resolution and estimated via unconstrained top-down methods (details about WorldPop: <https://www.worldpop.org/project/categories?id=18>) and also extracted by ArcGIS pro 2.0. The unit of the population density is thousand people per km2.  
**temp_seasonal_county.csv** Meteorological variables are also the grid data, such as the temperature and relative humidity in winter and summer, from 2001 to 2016 with a 4-km grid size in each county (more details about meteorological variables: <http://www.climatologylab.org/gridmet.html>)  
**mort_tiff.tif** The basic data for this process is 2010 data extracted from previous research, which is approximately 54,730 in the CONUS in 2010, 54,905 in the whole U.S. (Lelieveld, J., Evans, J. S., Fnais, M., Giannadaki, D. & Pozzer, A. The contribution of outdoor air pollution sources to premature mortality on a global scale. Nature 525, 367-371, doi:10.1038/nature15371 (2015))  
**tl_2016_us_county.shp** The boundary data can be obtained from the U.S. Census Bureau: <https://www2.census.gov/geo/tiger/GENZ2016/shp/>  

We thank Jos Lelieveld, Andrea Pozzer, and other authors for providing the Raster data of PM2.5-related premature deaths.  
We also thank all abovementioned data providers for making their data public and for enabling this research to be possible.  
  
## Cite our Article  
Li, C., Managi, S., 2021. Contribution of on-road transportation to PM2.5. Scientific Reports 11.
  
## Contact Us:
- Email: Prof. Shunsuke Managi <managi@doc.kyushu-u.ac.jp>  
- Email: Chao Li <chaoli0394@gmail.com>  
  
## Term of Use:
Authors/funders retain copyright (where applicable) of code on this Github repo. 
This GitHub repo and its contents herein, including data, link to data source, and analysis code that are intended solely for reproducing the results in the manuscript "Contribution of On-Road Transportation to PM2.5" The analyses rely upon publicly available data from multiple sources, that are often updated without advance notice. We hereby disclaim any and all representations and warranties with respect to the site, including accuracy, fitness for use, and merchantability. By using this site, its content, information, and software you agree to assume all risks associated with your use or transfer of information and/or software. You agree to hold the authors harmless from any claims relating to the use of this site.
