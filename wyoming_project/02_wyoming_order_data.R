## Created by ExHunter000
## March , 26 2020

# load libraries ----------------------------------------------------------

library(readxl)
library(tidyverse)
library(dplyr)
library(stringr)
library(tidyr)

# initial data -------------------------------------------------------------

# check and create folder
mainDir <- "D:/FOLDER_IMRON/project/data/"
subDir <- "output"

ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir, subDir)), FALSE)

alamat_simpan <- paste0(mainDir,subDir)

# set directory
alamat_file <- "D:/FOLDER_IMRON/project/data/"
setwd(alamat_file)

tempoDir <- "temp"
dir.create(file.path(mainDir, tempoDir))
tmpDir <- paste0(alamat_file,"temp/")

tahuns <- c(2015:2019)
# tahun <- 2015
bulan <- c(paste0(0,c(1:9)),c(10:12))

# Processing --------------------------------------------------------------

## import data

### copy data to temporary directory
# copy file to temporary folder
# get all file names (full path) with the pattern "bulanan_metadata.csv" within all folders in directory
files <- list.files(alamat_file, full.names = TRUE, recursive = TRUE, pattern = "bulanan_metadata.csv")

# extract the desired parts for the new filename with regex
# d+ stands for 1+x digits, .* for anything and $ for the end of line
# extract only the second capturing group (the parts within parenthesis) via \\2
filenames_new <- gsub("(.*/)(\\d+/temp\\d+.*$)", "\\2", files)
filenames_new <- gsub("/", "_", filenames_new)
filenames_new <- gsub("D:_FOLDER_IMRON_project_data__","",filenames_new)

#create the new filepaths
files_new <- paste0(tmpDir, filenames_new) 

#perform copy
file.copy(from = files, to = files_new)

# make list to file name
output_name <- str_remove_all(filenames_new, "[bulanan_metada.csv]")


## open data
# open .csv in folder
file.list <- list.files(path = tmpDir, pattern = "*.csv",full.names = TRUE)
df.list <- lapply(file.list, read.csv) 

## filter data
# get observation time and precipitable water data
for (i in 1:length(files_new)) {
  obs.tpw <- as.data.frame(df.list[i])
  df <- obs.tpw
  idx <- which(df$measure == "Station number")
  
  df1 <- df %>% 
    # Create Unique Identifier for each station
    dplyr::mutate(station_id = cut(1:nrow(df), 
                                   c(idx, nrow(df)),
                                   right = FALSE, 
                                   include.lowest = TRUE)) %>% 
    dplyr::filter(measure %in% c("Observation time", 
                                 "Precipitable water [mm] for entire sounding")) %>% 
    # Turn each value in measure to a new column
    tidyr::pivot_wider(names_from = "measure", values_from = "value", ) %>% 
    # Inelegant way of sorting by date and time
    dplyr::mutate(ot =  as.numeric(sub("\\/", ".", `Observation time`))) %>% 
    dplyr::arrange(ot) %>% 
    dplyr::select(-ot) %>% 
    tidyr::drop_na()
  
  # order the observation time
  df2 <- df1[order(df1$`Observation time`),]
  df2 <- as.data.frame(df2)
  
  # export data to csv
  write.csv(df2, paste0(alamat_simpan,"/",output_name[i],"_tpw.csv"))
}

# delete temp folder
system(paste0("rm -r ", tmpDir))

