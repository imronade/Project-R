## Stasiun GAW Palu
## Created by Imron Ade
## April, 09 2020

# start to count execution time
start.time <- Sys.time()

# load libraries ----------------------------------------------------------

library(rgdal)
library(raster)
library(sp)
library(maptools)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(ncdf4)
library(tidyverse)

# import data -------------------------------------------------------------

# set folder data
alamat_file <- "D:/Kimia_Atmosfer/data_uv/"
setwd(alamat_file)

# load shp
shp <- readShapePoly(paste0(alamat_file,"shp_indonesia/"
                            ,"IDN_adm1.shp"))
shp2 <- readShapePoly(paste0(alamat_file,"LuarIndo/"
                             ,"luari_ndo_lgkp.shp"))
# Create the clipping polygon
CP <- as(extent(95, 141, -11.1, 6.1), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(shp2))

# Clip the map
out <- gIntersection(shp2, CP, byid=TRUE)

# bind shp
shp_ok <- bind(out,shp)

# Next the shapefile has to be converted to a dataframe for use in ggplot2
shapefile_df2 <- fortify(out)
shapefile_df <- fortify(shp)
shapefile_df3 <- fortify(shp_ok)

# load data
alamat_nc <- paste0(alamat_file, "data_nc/")
nc <- list.files(path = paste0(alamat_file, "data_nc/"),pattern = "*.nc")

# load bmkg logo
get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}
img <- get_png("logo_bmkg.png")

# export data -------------------------------------------------------------

# set color
warna <- c("#FFFFFF","#008C00","#00CE00", "#FFFF00", "#FFFF00", "#EFEF00",
           "#FFA700", "#EF9B00", "#FF0000", "#EF0000", "#CE0000", "#A11AF0")

# export to tiff

# set time and validation time
start_date <- as.POSIXct("2020-04-19 20:00:00")
tt <- seq(start_date, by = "hours", length = 73)
waktu <- paste(tt, "WITA")
validasi <- "Waktu Validasi: 2020-04-19 20:00 WITA"

i=1 
while (i <= 73) {
  print(paste0("dataku_",i))
  
  dataku <- raster(paste0(alamat_nc,nc[[i]]))

  # make UV index
  satuan <- function(x) { x * 40 }
  dataku <- calc(dataku,satuan)

  # crop to indonesia
  aod_indonesia <- crop(dataku, CP)
  
  #convert the raster to points for plotting
  map.p <- rasterToPoints(aod_indonesia)
  
  #Make the points a dataframe for ggplot
  df <- data.frame(map.p)
  
  #Make appropriate column headings
  colnames(df) <- c("Longitude", "Latitude", "UV")
  
  #Now make the map
  ggplot(data=df, aes(y=Latitude, x=Longitude)) +
    geom_raster(aes(fill=UV), interpolate = TRUE) +
    scale_y_continuous(breaks = c(-10,-5,0,5), labels = c("10S","5S","0","5N"),
                       expand = c(0,0)) + 
    scale_x_continuous(breaks =c(100,120,140),labels = c("100E","120E","140E"),
                       expand = c(0,0)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(y = "Lintang", x = "Bujur") +
    scale_fill_gradientn("Indeks UV",colours=warna,na.value = "transparent"
                         ,limits=c(0,11), breaks=seq(1, 10, by=1), oob = scales::squish) +
    geom_polygon(data = shapefile_df3, aes(x = long, y = lat, group = group), 
                 colour = "black", fill = "gray",alpha=0.25) + 
    labs(title = expression("Indeks Ultra Violet Sinar Matahari"),
         subtitle = paste(waktu[i],"                                                                                                                                                                                                                                 ",
                          "INDONESIA"),
         caption = paste(validasi,"                                                                                                                                                                                 ",
                         "Diolah oleh: Stasiun GAW Palu \n Sumber Data: ECMWF")) + 
    theme (legend.key.size = unit(1.5, "cm"),
           legend.key.width = unit(0.5,"cm"),
           plot.title = element_text(hjust = 0.5,size=14 ,face = "bold"),
           plot.subtitle = element_text(hjust = 0.5, size=10),
           plot.caption = element_text(hjust = 1, size=10)) +
    annotation_custom(img, xmin = 141.5, xmax = 145.5, ymin = -14, ymax = -10) +
    coord_cartesian(clip = "off") +
    theme(plot.margin = unit(c(1, 1, 3, 1), "lines"))
  
  # hjust=1, place on the right
  # hjust=0, place on the left
  # hjust=0.5, place on the center
  
  # save plot
  if (i < 10) {
    ggsave(paste0("UV_0",i,"_nc.tiff"), width  = 338.666666667, height = 170.920833333, 
           units = "mm", dpi = 100)
  }else{
    ggsave(paste0("UV_",i,"_nc.tiff"), width  = 338.666666667, height = 170.920833333, 
           units = "mm", dpi = 100)
  }
  
  i=i+1
}


# make animation ----------------------------------------------------------

# load the data
gambar <- list.files(pattern = "*_nc.tiff")

# first install ImageMagick (https://imagemagick.org/index.php)
# then call the following function (which calls "convert", part of ImageMagick I suppose)

# convert tiff to jpeg
library(jpeg)
library(tiff)

for (im in 1:length(gambar)) {
  img <- readTIFF(gambar[im], native=TRUE)
  if (im < 10) {
    writeJPEG(img, target = paste0("0",im,"_nc.jpeg"), quality = 1)
  } else {
    writeJPEG(img, target = paste0(im,"_nc.jpeg"), quality = 1)
  }
}

library(magick)
frames <- list.files(pattern = "*_nc.jpeg")
m <- image_read(frames)
m <- image_animate(m[11:23], fps = 0.5)
image_write(m, "UV_animation_nc_1day.gif")

# remove tiff files
sampah <- dir(pattern="_nc.tiff")
file.remove(sampah)

# finish 
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
