#here, I am trying to make a map of ATL, with annual PM2.5 and CDC values by FIPS

library(dplyr)
library(lubridate)
library(ggplot2)
library(sf)
library(RColorBrewer)
library(raster)
library(mapview)
library(data.table)
library(geosphere)

setwd("C:/Users/pegeorg/OneDrive - Emory University/Papers and Projects/Fellowship Project/DB/")

rm(list=ls())

######   load the shapefile of the US counties and database to use  ------------------------------------
GA_censustracts <- st_read("../../DATABASES RAW/Shapefiles/tl_2019_13_tract/tl_2019_13_tract.shp")
counties = st_read('../../DATABASES RAW/Shapefiles/tl_2016_us_county/tl_2016_us_county.shp')



#####################                  2019                ############################################# 

#SEDAC data - convert to dataframe first for analysis (takes long to load FYI)
df1 = raster('../../DATABASES RAW/SEDAC/Annual PM25 global/sdei-global-annual-gwr-pm2-5-modis-misr-seawifs-aod-v4-gl-03-2019-geotiff/sdei-global-annual-gwr-pm2-5-modis-misr-seawifs-aod-v4-gl-03-2019.tif')


##### filter datababases to make them smaller  -------------------------------------------------------
counties = counties %>% filter(STATEFP == 13)    #13 = Georgia

#Georgia lon and lat boundaries   #N 35.05  S 30.20  W -85.83  E -80.66
df2 = as.data.frame(rasterToPoints(df1))
PM_2019_Georgia = df2 %>% filter(x > -85.83 & x < -80.66 & y > 30.2 & y < 35.05)
PM_2019_Georgia_short = PM_2019_Georgia[seq(1, nrow(PM_2019_Georgia), 77),]

##project to SpatialPointsDataframe (for use with mapview to check dataframe)
PM_2019_Georgia_short = PM_2019_Georgia[seq(1, nrow(PM_2019_Georgia), 77),]
coordinates(PM_2019_Georgia_short) = c('x', 'y')
proj4string(PM_2019_Georgia_short) = CRS("+proj=longlat +datum=WGS84")
mapview(PM_2019_Georgia_short)

#calculate average PM2.5 by census tract
PM_2019_Georgia_sf <- st_as_sf(PM_2019_Georgia, coords = c("x", "y"), crs = st_crs(GA_censustracts))
PM_censustracts <- st_join(GA_censustracts, PM_2019_Georgia_sf)

PM_censustracts_dt <- as.data.table(PM_censustracts)
PM_censustracts_avg <- PM_censustracts_dt[, .(avg_PM25 = mean(sdei.global.annual.gwr.pm2.5.modis.misr.seawifs.aod.v4.gl.03.2019, na.rm = TRUE)), by = GEOID]


# Define hospital locations
hospitals <- data.frame(
  name = c("Egleston", "Scottish Rite", "Hughes Spalding"),
  x = c(-84.3, -84.35, -84.4),
  y = c(33.8, 33.9, 33.75)
)

# Convert hospitals to sf object
hospitals_sf <- st_as_sf(hospitals, coords = c("x", "y"), crs = st_crs(counties))

# Calculate buffer around Egleston (30 miles)
Egleston_buffer <- st_buffer(hospitals_sf[hospitals_sf$name == "Egleston", ], dist = units::set_units(30, "miles"))

# Filter counties
counties_filtered <- st_intersection(counties, Egleston_buffer)

# Merge PM_censustracts_avg with GA_censustracts
PM_censustracts_avg_merged <- merge(PM_censustracts_avg, GA_censustracts, by = "GEOID")

# Convert PM_censustracts_avg_merged back to an sf object
PM_censustracts_avg_sf <- st_as_sf(PM_censustracts_avg_merged, crs = st_crs(GA_censustracts))

str(PM_censustracts_avg_sf)

# Filter PM_censustracts_avg_sf within 30 miles of Egleston
PM_censustracts_avg_filtered <- st_intersection(PM_censustracts_avg_sf, Egleston_buffer)

#map of atlanta
ggplot() +
  # Background map of Georgia
  geom_sf(data = counties, fill = "gray90", color = "gray80", size = 0.1, alpha = 0) +
  # Choropleth map of PM2.5 levels within 30 miles of Egleston
  geom_sf(data = PM_censustracts_avg_filtered, aes(fill = avg_PM25), color = "transparent", size = 0.1) +
  # Outline of Atlanta metro area counties
  geom_sf(data = counties_filtered, fill = NA, size = 0.2, color = "black") +
  # Points for hospitals, with different shapes and colors
  geom_sf(data = hospitals_sf, aes(color = name, shape = name), size = 6, stroke = 1, color = "black") +
  # Color scale for PM2.5 levels
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  # Manual color and shape scale for hospitals
  scale_color_manual(values = c("Egleston" = "red", "Scottish Rite" = "blue", "Hughes Spalding" = "green")) +
  scale_shape_manual(values = c("Egleston" = 16, "Scottish Rite" = 17, "Hughes Spalding" = 18)) +
  # Plot theme and legend settings
  theme_minimal() +
  theme(legend.position = "right") +
  labs(title = "Annual 2019 PM2.5 Levels by Census Tract",
       subtitle = "Within 30 miles of CHOA",
       fill = "Avg PM2.5",
       color = "Hospitals",
       shape = "Hospitals") +
  coord_sf(xlim = c(-84.8, -83.7), ylim = c(33.4, 34.3))  # Zoom in on the top half of Georgia



# Calculate buffer around Egleston (30 miles)
Egleston_buffer <- st_buffer(hospitals_sf[hospitals_sf$name == "Egleston", ], dist = units::set_units(300, "miles"))

# Filter counties
counties_filtered <- st_intersection(counties, Egleston_buffer)

# Merge PM_censustracts_avg with GA_censustracts
PM_censustracts_avg_merged <- merge(PM_censustracts_avg, GA_censustracts, by = "GEOID")

# Convert PM_censustracts_avg_merged back to an sf object
PM_censustracts_avg_sf <- st_as_sf(PM_censustracts_avg_merged, crs = st_crs(GA_censustracts))

str(PM_censustracts_avg_sf)

# Filter PM_censustracts_avg_sf within 30 miles of Egleston
PM_censustracts_avg_filtered <- st_intersection(PM_censustracts_avg_sf, Egleston_buffer)


#map of georgia
ggplot() +
  # Background map of Georgia
  geom_sf(data = counties, fill = "gray90", color = "gray80", size = 0.1, alpha = 0) +
  # Choropleth map of PM2.5 levels within 30 miles of Egleston
  geom_sf(data = PM_censustracts_avg_filtered, aes(fill = avg_PM25), color = "transparent", size = 0.1) +
  # Outline of Atlanta metro area counties
  geom_sf(data = counties_filtered, fill = NA, size = 0.2, color = "black") +
  # Points for hospitals, with different shapes and colors
  geom_sf(data = hospitals_sf, aes(color = name, shape = name), size = 3) +
  # Color scale for PM2.5 levels
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  # Manual color and shape scale for hospitals
  scale_color_manual(values = c("Egleston" = "red", "Scottish Rite" = "blue", "Hughes Spalding" = "green")) +
  scale_shape_manual(values = c("Egleston" = 16, "Scottish Rite" = 17, "Hughes Spalding" = 18)) +
  # Plot theme and legend settings
  theme_minimal() +
  theme(legend.position = "right") +
  labs(title = "PM2.5 Levels by Census Tract",
       subtitle = "Georgia",
       fill = "Avg PM2.5",
       color = "Hospitals",
       shape = "Hospitals") +
  coord_sf(xlim = c(-85.5, -81), ylim = c(30, 35))  # Zoom in on the top half of Georgia



## density plot, of Atlanta only  
Egleston <- c(-84.3, 33.8)

PM_2019_ATL <- PM_2019_Georgia %>%
  filter(distVincentySphere(cbind(x, y), Egleston) <= 30 * 1609.34)

library(ggplot2)

ggplot(PM_2019_ATL, aes(x = sdei.global.annual.gwr.pm2.5.modis.misr.seawifs.aod.v4.gl.03.2019)) +
  geom_density(fill = "blue", alpha = 0.6) +
  labs(
    title = "Density Plot of Annual PM2.5 values in Atlanta Metropolitan Area",
    x = "PM2.5",
    y = "Density"
  ) +
  theme_minimal()



# now, for SVI data      


CDC_GA_2018 = read.csv('../../DATABASES RAW/CDC SVI Georgia/Georgia_2018.csv')

#4 summary themes are:
           # SES: RPL_THEME1
           # Household comp + diability: RPL_THEME2
           # Minority Status/Language: RPL_THEME3
           # Housing type and transport: RPL_THEME4
           # overall RPL_THEMES
# flags = tracts in the top 10% (ie at 90th %ile) are given value of 1 to indicate high vulnerability 


CDC_GA_2018_short = CDC_GA_2018 %>% dplyr::select(FIPS, RPL_THEME1, RPL_THEME2, RPL_THEME3, RPL_THEME4, RPL_THEMES)

str(CDC_GA_2018_short)


# Merge GA_censustracts with CDC_GA_2018_short using FIPS/GEOID
GA_censustracts_CDC <- GA_censustracts %>%
  left_join(CDC_GA_2018_short %>% mutate(FIPS = as.character(FIPS)), by = c("GEOID" = "FIPS"))


str(GA_censustracts_CDC)

# Convert hospitals to sf object
hospitals_sf <- st_as_sf(hospitals, coords = c("x", "y"), crs = st_crs(counties))

# Calculate buffer around Egleston (30 miles)
Egleston_buffer <- st_buffer(hospitals_sf[hospitals_sf$name == "Egleston", ], dist = units::set_units(30, "miles"))

# Filter counties
counties_filtered <- st_intersection(counties, Egleston_buffer)


# Filter GA_censustracts_CDC_joined within 30 miles of Egleston
GA_censustracts_CDC_filtered <- st_intersection(GA_censustracts_CDC, Egleston_buffer)




ggplot() +
  # Background map of Georgia
  geom_sf(data = counties, fill = "gray90", color = "gray80", size = 0.1, alpha = 0) +
  # Choropleth map of RPL_THEMES levels within 30 miles of Egleston
  geom_sf(data = GA_censustracts_CDC_filtered, aes(fill = RPL_THEMES), color = "transparent", size = 0.1) +
  # Outline of Atlanta metro area counties
  geom_sf(data = counties_filtered, fill = NA, size = 0.2, color = "black") +
  # Points for hospitals, with different shapes and colors
  geom_sf(data = hospitals_sf, aes(color = name, shape = name), size = 6, stroke = 1, color = "black") +
  # Color scale for RPL_THEMES levels
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  # Manual color and shape scale for hospitals
  scale_color_manual(values = c("Egleston" = "white", "Scottish Rite" = "white", "Hughes Spalding" = "white")) +
  scale_shape_manual(values = c("Egleston" = 16, "Scottish Rite" = 17, "Hughes Spalding" = 18)) +
  # Plot theme and legend settings
  theme_minimal() +
  theme(legend.position = "right") +
  labs(title = "Social Vulnerability Index (SVI) Levels 
by Census Tract",
       subtitle = "Overall",
       fill = "",
       color = "Hospitals",
       shape = "Hospitals") +
  coord_sf(xlim = c(-84.8, -83.7), ylim = c(33.4, 34.3))  # Zoom in on the top half of Georgia



ggplot() +
  # Background map of Georgia
  geom_sf(data = counties, fill = "gray90", color = "gray80", size = 0.1, alpha = 0) +
  # Choropleth map of RPL_THEMES levels within 30 miles of Egleston
  geom_sf(data = GA_censustracts_CDC_filtered, aes(fill = RPL_THEME1), color = "transparent", size = 0.1) +
  # Outline of Atlanta metro area counties
  geom_sf(data = counties_filtered, fill = NA, size = 0.2, color = "black") +
  # Points for hospitals, with different shapes and colors
  geom_sf(data = hospitals_sf, aes(color = name, shape = name), size = 6, stroke = 1, color = "black") +
  # Color scale for RPL_THEMES levels
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  # Manual color and shape scale for hospitals
  scale_color_manual(values = c("Egleston" = "white", "Scottish Rite" = "white", "Hughes Spalding" = "white")) +
  scale_shape_manual(values = c("Egleston" = 16, "Scottish Rite" = 17, "Hughes Spalding" = 18)) +
  # Plot theme and legend settings
  theme_minimal() +
  theme(legend.position = "right") +
  labs(title = "Social Vulnerability Index (SVI) Levels 
by Census Tract",
       subtitle = "Socioeconomic Status Theme",
       fill = "",
       color = "Hospitals",
       shape = "Hospitals") +
  coord_sf(xlim = c(-84.8, -83.7), ylim = c(33.4, 34.3))   # Zoom in on the top half of Georgia


ggplot() +
  # Background map of Georgia
  geom_sf(data = counties, fill = "gray90", color = "gray80", size = 0.1, alpha = 0) +
  # Choropleth map of RPL_THEMES levels within 30 miles of Egleston
  geom_sf(data = GA_censustracts_CDC_filtered, aes(fill = RPL_THEME3), color = "transparent", size = 0.1) +
  # Outline of Atlanta metro area counties
  geom_sf(data = counties_filtered, fill = NA, size = 0.2, color = "black") +
  # Points for hospitals, with different shapes and colors
  geom_sf(data = hospitals_sf, aes(color = name, shape = name), size = 6, stroke = 1, color = "black") +
  # Color scale for RPL_THEMES levels
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  # Manual color and shape scale for hospitals
  scale_color_manual(values = c("Egleston" = "white", "Scottish Rite" = "white", "Hughes Spalding" = "white")) +
  scale_shape_manual(values = c("Egleston" = 16, "Scottish Rite" = 17, "Hughes Spalding" = 18)) +
  # Plot theme and legend settings
  theme_minimal() +
  theme(legend.position = "right") +
  labs(title = "Social Vulnerability Index (SVI) Levels 
by Census Tract",
       subtitle = "Minority Status/Language Theme",
       fill = "High ",
       color = "Hospitals",
       shape = "Hospitals") +
  coord_sf(xlim = c(-84.8, -83.7), ylim = c(33.4, 34.3))   # Zoom in on the top half of Georgia


ggplot() +
  # Background map of Georgia
  geom_sf(data = counties, fill = "gray90", color = "gray80", size = 0.1, alpha = 0) +
  # Choropleth map of RPL_THEMES levels within 30 miles of Egleston
  geom_sf(data = GA_censustracts_CDC_filtered, aes(fill = RPL_THEME4), color = "transparent", size = 0.1) +
  # Outline of Atlanta metro area counties
  geom_sf(data = counties_filtered, fill = NA, size = 0.2, color = "black") +
  # Points for hospitals, with different shapes and colors
  geom_sf(data = hospitals_sf, aes(color = name, shape = name), size = 6, stroke = 1, color = "black") +
  # Color scale for RPL_THEMES levels
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  # Manual color and shape scale for hospitals
  scale_color_manual(values = c("Egleston" = "white", "Scottish Rite" = "white", "Hughes Spalding" = "white")) +
  scale_shape_manual(values = c("Egleston" = 16, "Scottish Rite" = 17, "Hughes Spalding" = 18)) +
  # Plot theme and legend settings
  theme_minimal() +
  theme(legend.position = "right") +
  labs(title = "Social Vulnerability Index (SVI) Levels 
by Census Tract",
       subtitle = "Housing Type/Transportation Theme",
       fill = "High ",
       color = "Hospitals",
       shape = "Hospitals") +
  coord_sf(xlim = c(-84.8, -83.7), ylim = c(33.4, 34.3))   # Zoom in on the top half of Georgia



