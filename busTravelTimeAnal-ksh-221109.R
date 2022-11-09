# 0. library
library(ggplot2)
library(MASS)
#install.packages("reshape")
library(reshape)
library(lubridate)

##화성시 버스 대중교통 운영 정량적 평가지표 개발##

### 버스 평균 통행시간 분석

### 1002 버스 (화성시청~사당역) 10월 24 하루치 데이터


gtPath <- "C:/Users/sanghyeok/Desktop/sanghyeok_temp/git_cowork/hwaseongBusOperStat/"

arrTimeData <- read.csv(paste0(gtPath, "arr_times.csv"))

colnames(arrTimeData)


arrTimeData[,c(2:ncol(arrTimeData))] <- lapply(arrTimeData[,c(2:ncol(arrTimeData))], 
                                                     function(x) {as.POSIXct(x, format="%Y-%m-%d %H:%M:%S")})

length(arrTimeData)
for (i in c(2:(length(arrTimeData)-1))){
  print(paste0('Mean time from busStop',i-1,' to busStop',i,': ',mean(difftime(arrTimeData[,(i+1)], arrTimeData[,(i)]), na.rm=TRUE)))
}


# 1. 통행시간 누적 그래프 그려보기

  # 1.1. 행,열 방향 전환

    arrTimeData
    
    arrTimeData_t <- data.frame(t(arrTimeData[,-1])) # 행,열 방향 바꾸기
    colnames(arrTimeData_t) <- paste0('Dispatch',c(1:ncol(arrTimeData_t))) # Dispatch 번호를 열명으로
    # colnames(arrTimeData_t) <- paste0('Dispatch',c(1:ncol(arrTimeData_t)),
    #                                  '_',hour(arrTimeData[,'datetime.1']),':',minute(arrTimeData[,'datetime.1']))

    
    arrTimeData_t <- cbind('BusStop'=c(1:101),arrTimeData_t)# 정류장 인덱스 열 추가
    arrTimeData_t[,c(2:ncol(arrTimeData_t))] <- lapply(arrTimeData_t[,c(2:ncol(arrTimeData_t))], 
                                                   function(x) {as.POSIXct(x, format="%Y-%m-%d %H:%M:%S")} ) # 데이터형식 POSIXct로 변환
    
    
    ggplot(data=arrTimeData_t,aes(x=BusStop,y=Dispatch1))+geom_line()+geom_point()
  
  # 1.2. 출발 정류장으로부터의 소요시간 계산
    
    travelTimeFromStop1 <- arrTimeData_t

    for (j in c(2:ncol(travelTimeFromStop1))){
      startTime <- travelTimeFromStop1[1,j]
      travelTimeFromStop1[,j]
      #travelTimeFromStop1[,j] <- difftime(travelTimeFromStop1[,j],startTime, units="mins")
      travelTimeFromStop1[,j] <- as.numeric(travelTimeFromStop1[,j]-startTime, units="mins")
      
    }
    
    travelTimeFromStop1
    ggplot(data=travelTimeFromStop1,aes(x=BusStop,y=Dispatch1))+geom_line()+geom_point()
    
  # 1.3. 여러 Dispatch를 한번에 그리기위해 melt 함수 쓰기
    
    travelTimeFromStop1_melted <- melt(travelTimeFromStop1, id=c('BusStop')) 
    colnames(travelTimeFromStop1_melted) <- c("BusStop", "Dispatch", "CumTrvTime")
    
    ggplot(data=travelTimeFromStop1_melted,aes(x=BusStop,y=CumTrvTime,group=Dispatch,color=Dispatch))+
      geom_line()+
      geom_point()+
      ggtitle("Cumulated Travel Time from Fist Stop for Each Dispatch")
    
    
  # 1.4. 타켓 정류장 + 예상 도로 통행시간 따로 보기(상행) 
    #targetStop <- c(1,9,26,42,51,53,61,77,94,101)
    targetStop <- c(1,9,26,42,51)
    travelTimeFromStop1ToTgStop_melted <- travelTimeFromStop1_melted[travelTimeFromStop1_melted$BusStop %in% targetStop,]
    travelTimeFromStop1ToTgStop_melted$type <- 'bus'
    
    
    travelTimeFromStop1ToTgStop_melted <- rbind(travelTimeFromStop1ToTgStop_melted,
                                                data.frame(BusStop=c(1,9,26,42,51),Dispatch=c('car'),CumTrvTime=c(0,13,38,67,90),type=c('car')))
    
    ggplot(data=travelTimeFromStop1ToTgStop_melted,
           aes(x=BusStop,y=CumTrvTime,group=Dispatch,color=Dispatch,linetype=type))+
      geom_line()+
      geom_point()+
      ggtitle('상행')
      
    
  # 1.5. 타켓 정류장 + 예상 도로 통행시간 따로 보기(하행)
    travelTimeFromStop53 <- travelTimeFromStop1
  
    for (j in c(2:ncol(travelTimeFromStop53))){
      travelTimeFromStop53[,j] <- travelTimeFromStop1[,j] - travelTimeFromStop1[travelTimeFromStop1$BusStop==53,j] 
    }
    travelTimeFromStop53
    
    targetStop <- c(53,61,77,94,101)
    
    travelTimeFromStop53_melted <- melt(travelTimeFromStop53, id=c('BusStop')) 
    colnames(travelTimeFromStop53_melted) <- c("BusStop", "Dispatch", "CumTrvTime")
    
    travelTimeFromStop53ToTgStop_melted <- travelTimeFromStop53_melted[travelTimeFromStop53_melted$BusStop %in% targetStop,]
    travelTimeFromStop53ToTgStop_melted$type <- 'bus'
    
    travelTimeFromStop53ToTgStop_melted <- rbind(travelTimeFromStop53ToTgStop_melted,
                                                data.frame(BusStop=c(53,61,77,94,101),Dispatch=c('car'),CumTrvTime=c(0,28,61,115,126),type=c('car')))
    
    ggplot(data=travelTimeFromStop53ToTgStop_melted,
           aes(x=BusStop,y=CumTrvTime,group=Dispatch,color=Dispatch,linetype=type))+
      geom_line()+
      geom_point()+
      ggtitle('하행')

    
      
  

    #####################################################################################################################
arrTimeData[1,2]

arrTimeData_target <- arrTimeData[,c(1,1+1,9+1,26+1,42+1,51+1,53+1,61+1,77+1,94+1,101+1)]

















