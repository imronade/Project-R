## March, 02 2020

# load libraries ----------------------------------------------------------

library(readxl)
library(readr)


# load data ---------------------------------------------------------------

era5 <- read_csv("era5_pos_hujan_all.csv")

# prosess to convert daily ------------------------------------------------

# n = hour
n <- 24
i=1
daily_prec <- c()
while (i <= dim(era5)[2]-1) {
  seqs <- seq_along(era5[[i+1]])
  daily <- tapply(era5[[i+1]],rep(seqs,each=n)[seqs],FUN=sum)
  daily_prec <- cbind(daily_prec, daily)
  i=i+1
}

# change col names
daily_prec <- as.data.frame(daily_prec, row.names = FALSE)
names = colnames(era5[,2:24])
colnames(daily_prec) = names

# convert below 0.1 to 0 because it is TTU

daily_prec[daily_prec < 0.1] <- 0

# export data -------------------------------------------------------------

# save data to csv
write.csv(daily_prec, "D:/Kantor/script/ekstrak_data_era5/era5_daily_all.csv")

