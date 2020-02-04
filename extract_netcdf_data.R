## Created by ExHunter000
## January, 01 2020

# load library
library(sp)
library(raster)
library(ncdf4)

setwd("E:\\project\\paper\\Prediksi kenyamanan bola\\data\\data_papua\\")
dd=1
xt=matrix(as.numeric(as.matrix(raster("E:\\project\\paper\\Prediksi kenyamanan bola\\data\\data_papua\\temp_2010_2019_10_11_papua.nc",band=dd))),,1)

path="E:\\project\\paper\\Prediksi kenyamanan bola\\data\\data_papua\\temp_2010_2019_10_11_papua.nc"
ncin=nc_open(path)
ncin

time <- ncvar_get(ncin, "time")  
nt<- dim(time)

while(dd<=nt){
  x=raster("E:\\project\\paper\\Prediksi kenyamanan bola\\data\\data_papua\\temp_2010_2019_10_11_papua.nc",band=dd)
  xm=as.matrix(x)
  xn=matrix(as.numeric(xm),,1)
  xt=cbind(xt,xn)
  dd=dd+1
}
xt=xt[,2:ncol(xt)]

path="E:\\project\\paper\\Prediksi kenyamanan bola\\data\\data_papua\\temp_2010_2019_10_11_papua.nc"
ncin=nc_open(path)
ncin
lon <- ncvar_get(ncin,"longitude")
lat <- ncvar_get(ncin,"latitude")
lonlat <- as.matrix(expand.grid(lat,lon))
mintemp.df <- data.frame(cbind(lonlat))

xt <- cbind(mintemp.df,xt)

# save data to csv
write.csv(xt,"E:\\project\\paper\\Prediksi kenyamanan bola\\data\\data_papua\\temp_2010_2019_10_11_papua.csv")
