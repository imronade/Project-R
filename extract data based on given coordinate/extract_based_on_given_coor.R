# load libraries
library(ncdf4)
library(raster)
library(sp)
library(readxl)

# load data
setwd("G:\\ekstrak_era5\\")

ncin <- nc_open("G:\\ekstrak_era5\\ERA5_prec_hourly_2019.nc")
ncin

r <- brick("ERA5_prec_hourly_2019.nc", varname = "tp")

koor <- read_excel("D:/Kantor/script/ekstrak_data_era5/koor.xlsx")
koor$Lat <- as.numeric(koor$Lat)
koor$Lon <- as.numeric(koor$Lon)

# lat13 <- -1.97157
# lon13 <- 120.97725

# process
i=1
vals <- c()
# vals_fin <- data.frame(Date=as.Date(character()),
#                   File=character(), 
#                   User=character(), 
#                   stringsAsFactors=FALSE) 
while (i <= dim(koor)[1]) {
  vals_i <- extract(r, matrix(c(koor$Lon[i], koor$Lat[i]), ncol = 2))
  vals <- cbind(vals,t(vals_i))
  i=i+1
}

vals_fin <- as.data.frame(vals, row.names = FALSE)
names(vals_fin) <- as.vector(koor$ID)

# save data to csv
write.csv(vals_fin, "G:\\ekstrak_era5\\era5_pos_hujan.csv")