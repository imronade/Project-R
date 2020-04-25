## Created by Imron Ade
## April, 25 April 2020


# load libraries ----------------------------------------------------------

library(raster)
library(stringr)
library(lubridate)
library(tidyverse)


# preparation -------------------------------------------------------------

alamat_file <- "D:/pak_donaldi/PM10_DATAGLOBAL/"
setwd(alamat_file)

tahuns <- c(2014:2018)

# make units pm10 to microgram/m**3
satuan <- function(x) { x * 10**9 }

lon = c(102.43,103.79,103.7,103.64)  # Array of x coordinates
lat <- c(-1.46, -1.2, -1.65, -1.63)  # Array of y coordinates
nama.station <- c("blhd tebo", "blhd tanjung jabung timur",
                  "stamet sulthan thaha", "Staklim jambi")

# lon = c(102.375,103.75,103.75,103.625)  
# lat <- c(-1.5, -1.25, -1.625, -1.625)  
# points <- SpatialPoints(cbind(lat,lon)) # Build a spPoints object

# extract data ------------------------------------------------------------

i=1
while (i <= length(tahuns)) {
  
  tahun <- tahuns[i]
  setwd(paste0(alamat_file,tahun))
  files <- dir(pattern = ".nc")
  vals <- c()
  
  j=1
  while (j <= length(files)) {
    
    r <- brick(paste0(alamat_file,tahun,"/",files[j]))
    r <- calc(r,satuan)
    
    # Extract and tidy
    points_data <- r %>% 
      raster::extract(cbind(lon,lat), df=T) %>%
      gather(time, pm10,-ID) %>% 
      spread(key = ID,value =  pm10) %>%   # Can be skipped if you want a "long" table
      mutate(time = ymd(str_sub(names(r),2,11))) %>% 
      as_tibble()
    # points_data 
    colnames(points_data) <- c("tanggal",nama.station)
    
    nama_file <- str_remove(files[j], ".nc")
    print(nama_file)
    # save data to csv
    write.csv(points_data,paste0(nama_file,".csv"))
    j=j+1  
  }
  i=i+1
}
