library(tidyverse)
library(lubridate)
library(Ckmeans.1d.dp)


setwd("C:/Users/WONGYUN/Documents/GitHub/hwaseongBusOperStat")

#### table of available data ####
avail_index <- data.frame(matrix(ncol = 4, nrow = 0))
folders <- list.files("./rawData")
for (i in 1:length(folders)) {
  lineNm <- folders[i]
  lineNo <- strsplit(lineNm,"_")[[1]][1]
  for (j in 1:length(list.files(paste0("./rawData/",lineNm)))) {
    fileNm <- list.files(paste0("./rawData/",lineNm))[j]
    date <- strsplit(fileNm,"_")[[1]][1]
    avail_index <- avail_index |>
      rbind(unname(c(lineNo,date,lineNm,fileNm)))
  }
  
  if (i == length(folders)) {
    names(avail_index) <- c("line", "date", "folder", "filename")
  }
}

#### OPEN DATA ####
for (i in 1:nrow(avail_index)) {
  this_row <- avail_index[i,]
  if (this_row$date != "2022-10-25") {next}
  this_data <- read.csv(paste0("./rawData/",this_row$folder,"/",this_row$filename))
  this_data$timestamp <- substr(this_data$querytime,10,15)
  this_data$hms <- lubridate::hms(format(strptime(this_data$timestamp, format = "%H%M%S"), format = "%H:%M:%S"))
  this_data["remainSeatCnt"][this_data["remainSeatCnt"] == -1] <- NA

  clustering <- Ckmeans.1d.dp::Ckmeans.1d.dp(as.numeric(filter(this_data,stationSeq==1)$timestamp),k=c(1:30))
  this_data[this_data$stationSeq==1,"dispatch"] <- clustering$cluster
  if ("dep_schedule" %in% ls()) {
  dep_schedule <- rbind(dep_schedule,cbind(line=this_row$line,aggregate(timestamp~plateNo+dispatch,FUN=min,this_data)))
  } else {
    dep_schedule <- cbind(line=this_row$line,aggregate(timestamp~plateNo+dispatch,FUN=min,this_data))
  }
  
  # for (bus in unique(this_data$plateNo)) {
  #  this_bus <- this_data |>
  #    filter(plateNo==bus)
  #  
  # }
  
  # timespace <- ggplot(data=this_data,mapping=aes(x=timestamp,y=stationSeq,colour=plateNo)) +
  #   labs(title=this_row$line, x = "시간", y = "정류장순번", color = "차량번호") +
  #   theme(plot.title = element_text(hjust = 0.5)) +
  #   scale_x_discrete(labels=NULL) +
  #   geom_point(size=2)
  #ggsave(paste0("./plots/timespace/",this_row$line,"_",this_row$date,".png"),timespace)  
}


