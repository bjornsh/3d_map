#---------------------------------------------------------------------------------------------------
# Syfte
#---------------------------------------------------------------------------------------------------

# Skapa en fungerande generell workflow för visualisering av 3d kartor 
# med hjälp av R rayshader paketet




#---------------------------------------------------------------------------------------------------
# clean start
#---------------------------------------------------------------------------------------------------
rm(list = ls())
invisible(gc())


#---------------------------------------------------------------------------------------------------
# Options
#---------------------------------------------------------------------------------------------------

# # send output to RStudio Viewer rather than external X11 window
# options(rgl.useNULL = TRUE,
#         rgl.printRglwidget = TRUE)

# avoid scientific notation
options(scipen=999)



#---------------------------------------------------------------------------------------------------
# Libraries
#---------------------------------------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, sf, sp,
               jsonlite,
               devtools,
               rgl,
               mapview,
               stars,
               viridis)


# bara första gången
devtools::install_github("tylermorganwall/rayshader")
devtools::install_github("h-a-graham/rayvista", dependencies=TRUE)
# remotes::install_github("dmurdoch/rgl")


# alltid
library(rayvista)
library(rayshader)





##################################################################################
### Functions, paths
##################################################################################

`%notin%` <- Negate(`%in%`)

# ladda funktioner från Github
source_url("https://raw.githubusercontent.com/bjornsh/funktioner/main/func_filer.R")



dir.create(paste0(getwd(), "/shapefile"))
dir.create(paste0(getwd(), "/output"))
folder_shapefile = paste0(getwd(), "/shapefile")
folder_output = paste0(getwd(), "/output/")

folder_github = "https://github.com/bjornsh/gis_data/raw/main/" 





#---------------------------------------------------------------------------------------------------
# Fetch data
#---------------------------------------------------------------------------------------------------

### Kommun
url_shp = paste0(folder_github, "ak_riks.zip")
get_shapefile(url_shp)
kommun = st_read(paste0(folder_shapefile, "/", file_name, ".shp"),
              options = "ENCODING=WINDOWS-1252") %>% 
  filter(KOMMUNNAMN == "Uppsala")




# create directory to store data
dir.create(paste0(getwd(), "/data"))

#### SCB befolkningsdata för 1km2 rutor (4.6 MB)
download.file("https://www.scb.se/contentassets/790b7863da264730b626e4289dcb15a5/grid1km_totpop_20181231.zip",
              destfile= "data/grid.zip")

unzip("data/grid.zip", exdir="data", overwrite=TRUE)

filenames <- list.files(path="data",pattern="*shp")

scb = st_read(paste0("data/", filenames), 
              options = "ENCODING=WINDOWS-1252")

# 50% of grid diameter to be added to corner coordinates to create center coordinates 
dist_to_center = as.numeric(scb_rutstorlek(scb, 2)) / 2


# create grid center coordinates from grid ID (bottom left corner + grid diameter in x and y direction
scb = scb %>%
  as.data.frame() %>%
  dplyr::select(Ruta, Pop) %>%  
  mutate(x_center = as.numeric(substr(Ruta, 1, 6)) + dist_to_center,   
         y_center = as.numeric(substr(Ruta, 7, 13)) + dist_to_center)

xy = scb[,c("x_center", "y_center")]

spdf <- SpatialPointsDataFrame(coords = xy, data = scb) # create spatial points

spdf1 = st_as_sf(spdf) %>% # convert to sf object
  st_set_crs(3006) # SCB data comes in Sweref 99TM


test = spdf1 %>% 
  st_join(., kommun) %>% 
  filter(!is.na(KOMMUNNAMN))



scb_plot = test %>% 
  filter(Pop >= 1) %>%  
  ggplot() + 
  geom_point(aes(x = x_center, y = y_center, color = Pop)) + 
  scale_color_gradient(low = "grey",
                       high = "red") +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
#        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

scb_plot


# Close RGL window
rgl::rgl.close()

# Create 3d map
plot_gg(scb_plot, width = 3.5, multicore = TRUE, windowsize = c(800, 800), 
        zoom = 0.85, phi = 35, theta = 30, sunangle = 225, soliddepth = "auto")

# Sys.sleep(0.2)
# render_snapshot() # clear = TRUE


#---------------------------------------------------------------------------------------------------
# Store resultat
#---------------------------------------------------------------------------------------------------

r = rgl::rglwidget()
htmlwidgets::saveWidget(r, paste0(folder_output, "test_karta.html"))

