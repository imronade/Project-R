## February,03 2020

# load library
# load data
library(readr)
data_R_papua <- read_csv("E:/project/paper/Prediksi kenyamanan bola/data/data_R_papua.csv")

data_R_papua <- unname(data_R_papua)

# initial condition with first colomn data
data_timeseries <- data_R_papua[2]

# row bind the data to timeseries
i <- 2
colomn <- dim(data_R_papua)

while (i <= colomn[2]) {
  data_timeseries <- rbind(data_timeseries,data_R_papua[i])
  i <- i+1
}

# check the dimension of the data
dim(data_timeseries)

# make variable time to help process next step
time <- rep(0:23, 655140)

# add time to data
test <- cbind(time, data_timeseeries)

# changing colomn names of the data
colnames(test) <- c("time", "data")

# begin to averageif (in excel) 
# in R average/ mean based on level of another variable
# load tidyverse to approch the summarizing the data
library(tidyverse)

# process mean by time
mean_by_time <- test %>% 
  group_by(time) %>% 
  summarize(averaged.baseontime = mean(data))


# add sd of data
by_data <- test %>% 
  group_by(time) %>% 
  summarize(
    averaged.baseontime = mean(data),
    sd.data = sd(data),
    n = n()
  )

by_data

# save the result to csv
write.csv(by_data,'E:\\project\\paper\\MyData.csv', row.names = FALSE)
