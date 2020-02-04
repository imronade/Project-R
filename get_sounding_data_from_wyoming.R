## Created by ExHunter000
## May, 05 2018

# load library
library(rvest)
library(readr)
library(stringr)
library(dplyr)

# set directory to save the data
alamat <- setwd("D:/tugasmaritim/tes3")
URL <- "http://weather.uwyo.edu/cgi-bin/sounding?region=seasia&TYPE=TEXT%3ALIST&YEAR=2008&MONTH=01&FROM=0100&TO=3112&STNM=97180"
pg <-read_html(URL)
n <- str_count(pg,"Ujung Pandang")
#polanomornyaganjilbuattabelnya

t=1
while(t<(n*2)){
html_nodes(pg, "pre")[[t]] %>% 
  html_text() -> dat

read_table(dat, skip=5, col_types="ddddddddddd",
           col_names=c("pres", "hght", "temp", "dwpt", "relh", "mixr",
                       "drct", "sknt", "thta", "thte", "thtv")) -> df

write.csv(df,row.names=FALSE,file = (paste(alamat,"/",t,".csv",sep="")))
t=t+2
}

while(t<=(n)){
  html_nodes(pg, "h2")[[t]] %>% 
    html_text() -> info
  write.csv(info,row.names=FALSE,file = (paste(alamat,"/","info",t,".csv",sep="")))
  t=t+1
  }

filenames <- list.files(alamat, pattern="*.csv", full.names=TRUE)
dd <- list.files(full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows
write.csv(dd,row.names=FALSE,file = (paste(alamat,"/","bulanan.csv",sep="")))

########################################################################################################
html_nodes(pg, "pre")[[58]] %>% 
  html_text() -> dat2

read_table(dat2, skip=5, col_types="ddddddddddd",
           col_names=c("pres", "hght", "temp", "dwpt", "relh", "mixr",
                       "drct", "sknt", "thta", "thte", "thtv")) -> df2

dplyr::glimpse(df)
dplyr::glimpse(df2)

# save data to csv file
write.csv(df,row.names=FALSE,file = ("D:/tugasmaritim/tes/uji.csv"))
