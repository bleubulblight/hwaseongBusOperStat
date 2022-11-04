library(tidyverse)
library(lubridate)
library(plotly)
library(Ckmeans.1d.dp)
`%notin%` <- Negate(`%in%`)


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

#####################################################
## FOR THIS WEEK's ANALYSIS ONLY ONE TABLE IS USED ##
#####################################################
interest <- c(10) #list of interested tables

#### CALL DATA ####
for (i in 1:nrow(avail_index)) {
  if (i %notin% interest) {next} #ignore non-interest data
  #### general import ####
  this_row <- avail_index[i,]
  this_data <- read.csv(paste0("./rawData/",this_row$folder,"/",this_row$filename))
  this_data$datetime <- as.POSIXct(strptime(this_data$querytime,format=paste0("%Y%m%d-%H%M%S")))
  this_data["remainSeatCnt"][this_data["remainSeatCnt"] == -1] <- NA
  
  #### dispatch categorisation ####
  #### clustering method ####
  clustering <-
    Ckmeans.1d.dp::Ckmeans.1d.dp(as.numeric(filter(this_data,stationSeq==1)$datetime),k=c(1:30))
  this_data[this_data$stationSeq==1,"dispatch"] <- clustering$cluster
  deptimes <- 
    arrange(aggregate(datetime~dispatch+plateNo,FUN=min,data=this_data),dispatch)
  #### manual ####
  for (j in 1:length(unique(this_data$plateNo))) {
    this_plate <- unique(this_data$plateNo)[j]
    dispatch_times <- filter(deptimes, plateNo == this_plate)
    for (dispatch in 1:nrow(dispatch_times)) {
      this_data[this_data$datetime > dispatch_times$datetime[dispatch] & this_data$plateNo==this_plate, ]$dispatch <-
        dispatch_times$dispatch[dispatch]
    }
  }
  
  ### time-space diagram ####
  timespace <- ggplot(data=this_data,mapping=aes(x=datetime,y=stationSeq,colour=factor(plateNo))) +
    geom_point(size=2) +
    labs(title=paste0(this_row$line," [AllDots]"), x = "시간", y = "정류장순번", color = "차량번호") +
    theme(plot.title = element_text(hjust = 0.5),legend.key=element_blank(),legend.title.align=0.5) +
    guides(colour=guide_legend(nrow=10))
  print(ggplotly(timespace))#print(timespace)
  #ggsave(paste0("./plots/timespace/",this_row$line,"_",this_row$date,".png"),timespace)
  
  
  #### arrival time by stn ####
  arr_times1 <- aggregate(datetime~dispatch+stationSeq,this_data,FUN=min)
  #### diagram ####
  arronly <- ggplot(data=arr_times1,mapping=aes(x=datetime,y=stationSeq,group_by=dispatch,colour=factor(dispatch))) +
    geom_step(size=1) +
    labs(title=paste0(this_row$line," [ArrOnly]"), x = "시간", y = "정류장순번", color = "편성") +
    theme(plot.title = element_text(hjust = 0.5),legend.key=element_blank(),legend.title.align=0.5) +
    guides(colour=guide_legend(nrow=10))
  print(ggplotly(arronly))#print(arronly)
  #ggsave(paste0("./plots/arronly/",this_row$line,"_",this_row$date,"_arronly.png"),arronly)
  
  
  arr_table <- reshape(arr_times1,idvar=c("dispatch"),timevar="stationSeq",direction="wide")
  #write.csv(arr_table,"./arr_times.csv",row.names=F)
  
  #### vacancies by stn ####
  arr_times2 <- this_data |>
    group_by(dispatch,stationSeq) |>
    slice(which.min(datetime)) |>
    select(c("dispatch","datetime","stationSeq","remainSeatCnt")) |>
    as.data.frame()
  #### diagram ####
  vacancies <- ggplot(data=arr_times2,mapping=aes(x=stationSeq,y=remainSeatCnt,group_by=dispatch,colour=factor(dispatch))) +
    geom_line(size=1) +
    labs(title=paste0(this_row$line," [Vacancies]"), x = "시간", y = "잔여좌석", color = "편성") +
    theme(plot.title = element_text(hjust = 0.5),legend.key=element_blank(),legend.title.align=0.5) +
    guides(colour=guide_legend(nrow=10))
  print(ggplotly(vacancies))#print(vacancies)
  #ggsave(paste0("./plots/vacancies/",this_row$line,"_",this_row$date,"_vacancies.png"),vacancies)
  
  seat_table <- reshape(arr_times2[,c("dispatch","stationSeq","remainSeatCnt")],idvar=c("dispatch"),timevar="stationSeq",direction="wide")
  #write.csv(seat_table,"./empty_seats.csv",row.names=F)
  
  #### crowdedness index ####
  
}


