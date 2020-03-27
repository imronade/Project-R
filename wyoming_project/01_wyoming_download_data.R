## Created by ExHunter000
## May, 05 2018

# load libraries
library(rvest)
library(readr)
library(stringr)

#isi dengan path folder yang akan menyimpan hasil download
alamat <- setwd("D:/tugasmaritim/")

#copykan url yang akan kita download datanya
URL <- "http://weather.uwyo.edu/cgi-bin/sounding?region=seasia&TYPE=TEXT%3ALIST&YEAR=2012&MONTH=03&FROM=0100&TO=3112&STNM=96749"
pg <-read_html(URL)

#tulis nama kota yang akan kita download datanya
n <- str_count(pg,"Jakarta")

t=1
while(t<=n){
  html_nodes(pg, "h2")[[t]] %>%
    html_text() -> info

  html_nodes(pg, "pre")[[(t*2)-1]] %>%
    html_text() -> dat

  read_table(dat, skip=5, col_types="ddddddddddd",
             col_names=c("pres", "hght", "temp", "dwpt", "relh", "mixr",
                         "drct", "sknt", "thta", "thte", "thtv")) -> df
  fin <- data.frame(info,df)

  html_nodes(pg, "pre") -> formeta
  html_text(formeta[[t*2]]) %>%
    strsplit("\n") %>%
    unlist() %>%
    trimws() %>%       # hilangin spasi
    .[-1] %>%          # hilangin blank
    strsplit(": ") %>% # pisahin value dan variable
    lapply(function(x) setNames(as.list(x), c("measure", "value"))) %>% # pasangkan listnya
    dplyr::bind_rows() -> metadata # ubah ke data frame
  metadata

  write.csv(metadata,row.names = FALSE, file = (paste(alamat,"/",t,"_metadata",".csv", sep = "")))
  write.csv(fin,row.names=FALSE,file = (paste(alamat,"/",t,"_data",".csv",sep="")))
  t=t+1
}


filenames <- list.files(alamat, pattern="*_data.csv", full.names=TRUE)
dd <- list.files(full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows
dataset <- do.call("rbind",lapply(filenames,FUN=function(filenames){ read.csv(filenames)}))
write.csv(dataset,row.names=FALSE,file = (paste(alamat,"/","bulanan.csv",sep="")))

filenames_meta <- list.files(alamat, pattern="*_metadata.csv", full.names=TRUE)
dd_meta <- list.files(full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows
dataset_meta <- do.call("rbind",lapply(filenames_meta,FUN=function(filenames_meta){ read.csv(filenames_meta)}))
write.csv(dataset_meta,row.names=FALSE,file = (paste(alamat,"/","bulanan_metadata.csv",sep="")))
# save data to csv file
write.csv(df,row.names=FALSE,file = ("D:/tugasmaritim/tes/uji.csv"))