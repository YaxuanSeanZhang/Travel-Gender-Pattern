################# 1. Preparing Packages and Global variables/functions#################------
library(dplyr)
library(lubridate)
library(gepaf)
library(stringr)
library(ggplot2)
library(mclust)
library(factoextra)
library(NbClust)

################# 2. read data#################------
##read activity-travel episode data
actr = read.csv('data/UMN_Equity_ucalitems.csv')
##read app built-in survey data
survey = read.csv('data/UMN_Equity_survey.csv')

################# 3. data formatting#################-----
##data type formatting
survey$user_id = as.character(survey$user_id)
survey$user_id = str_trim(tolower(survey$user_id))
survey$calendar_item_id = as.character(survey$calendar_item_id)
survey$calendar_item_start_time = as.POSIXct(survey$calendar_item_start_timestamp/1000, 
                                             origin="1970-01-01")
survey$calendar_item_start_time<-strptime(survey$calendar_item_start_time,
                                          format = "%Y-%m-%d %H:%M")
survey$calendar_time = as.POSIXct(survey$timestamp/1000, origin="1970-01-01")
survey$calendar_time<-strptime(survey$calendar_time,format = "%Y-%m-%d %H:%M")

actr$type = as.character(actr$type)
actr$subtype = as.character(actr$subtype)
actr$user_id = as.character(actr$user_id)
actr$user_id = str_trim(tolower(actr$user_id))

colnames(actr)[2]<-c("UserId")
colnames(actr)[3:4]<-c("starttime","endtime")
actr$starttime<-strptime(actr$starttime,format = "%m/%d/%Y %H:%M")
actr$endtime<-strptime(actr$endtime,format = "%m/%d/%Y %H:%M")
actr$startdate<-strptime(actr$starttime,format = "%Y-%m-%d")

#activity-travel type 
actr$subtype[actr$subtype=='OTHER'&actr$type=='TRIP']='OTHER_TRIP'
actr$subtype[actr$subtype=='OTHER'&actr$type=='ACTIVITY']='OTHER_ACTIVITY'
actr[actr$type=='OFF'| actr$type=='DATA COLLECTION STARTED'|
            actr$type=='INACC','type'] = 'DEVICE_OFF'


actr$duration = as.numeric(difftime(actr$endtime,actr$starttime,units="mins"))

actr = data.frame(actr %>% 
                    group_by(UserId,cal_item_id, starttime) %>%
                    slice_tail())
actr = actr[order(actr$UserId,actr$starttime,actr$endtime),]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       


################# 4. merge activity-travel data with survey responses#################--------
for (i in 1:11) {
  if(i<=3){
    actr = merge(actr, 
                 survey[survey$question_id==(i+10)|
                          survey$question_id==i,
                        c("user_id", "calendar_item_id",
                          "calendar_item_start_time", "response","calendar_time")], 
                 by.x=c("UserId", "cal_item_id", "starttime"), 
                 by.y=c("user_id","calendar_item_id","calendar_item_start_time"),
                 all.x = TRUE,all.y=FALSE)
  }else if (i == 4){
    actr = merge(actr, 
                 survey[survey$question_id==(i+10),
                        c("user_id", "calendar_item_id",
                          "calendar_item_start_time", "response","calendar_time")], 
                 by.x=c("UserId", "cal_item_id", "starttime"), 
                 by.y=c("user_id","calendar_item_id","calendar_item_start_time"),
                 all.x = TRUE,all.y=FALSE)
  }else{
    actr = merge(actr, 
                 survey[survey$question_id==(i+10)|
                          survey$question_id==i-1,
                        c("user_id", "calendar_item_id",
                          "calendar_item_start_time", "response","calendar_time")], 
                 by.x=c("UserId", "cal_item_id", "starttime"), 
                 by.y=c("user_id","calendar_item_id","calendar_item_start_time"),
                 all.x = TRUE,all.y=FALSE)
  }
  #keep the latest survey response
  actr = actr[order(actr$UserId,actr$starttime,actr$endtime,actr$calendar_time),]
  actr = data.frame(actr %>% 
                      group_by(UserId,cal_item_id, starttime) %>%
                      slice_tail())
  actr = actr[,-c(length(actr))]
  colnames(actr)[length(actr)] = paste0('Q',i)
  actr = actr[order(actr$UserId,actr$starttime,actr$endtime),]
}

actr = actr %>%
  mutate(across((ncol(actr)-10):ncol(actr), ~ifelse(is.na(.), 'MISSING', as.character(.))))

columns = c('UserId','cal_item_id', 'starttime','endtime','startdate','type','subtype',
            'duration','distance','Q1','Q2','Q3','Q4','Q5','Q6','Q7','Q8','Q9',
            'Q10','Q11')
actr = actr[,columns]

################# 5. temporal gap#################-----------
##add a missing episode
actr = actr[actr$duratio>0,]
n <- nrow(actr)
gap <- (actr$endtime[1:(n - 1)] < actr$starttime[2:n]) & (actr$UserId[1:(n - 1)] == actr$UserId[2:n])
table(gap) 

gap_df <- data.frame()
for (i in 1:length(gap)) {
  if(gap[i]){
    gap_row = actr[i,]
    gap_row$starttime = actr$endtime[i]
    gap_row$endtime = actr$starttime[i+1]
    gap_row$type = 'DEVICE_OFF'
    gap_row[,(ncol(gap_row)-10):ncol(gap_row)] = 'MISSING'
    gap_df = rbind(gap_df,gap_row)
  }
}

actr = rbind(actr,gap_df)
actr$duration = as.numeric(difftime(actr$endtime,actr$starttime,units="mins"))
actr = actr[order(actr$UserId,actr$starttime,actr$endtime),]
################# 6. valid data#################-----------
##summarize the survey response
tb1 = actr %>%
  group_by(UserId,startdate) %>%
  summarise(cnt = n(),valid = sum(Q1!='MISSING'), ratio = valid/cnt,
            duration_valid = sum(duration[Q1!='MISSING']))
tb1 = data.frame(tb1)
summary(tb1$ratio)
summary(tb1$duration_valid)

##define valid data (survey response ratio & valid duration)
tb1$ifvalid = 'invalid'
tb1$ifvalid[(tb1$ratio>=0.6&tb1$duration_valid>=960)] = 'valid'
table(tb1$ifvalid)

actr = merge(actr, tb1[,c('UserId','startdate','ifvalid')], 
             by.x=c("UserId", "startdate"), 
             by.y=c("UserId","startdate"),all.x = TRUE,all.y=FALSE)
actr = actr[order(actr$UserId,actr$starttime,actr$endtime),]

################# 7. gender study label#################---------
#7.1) label household task activity----
n <- nrow(actr)
actr$HH_SURVEY = 'MI'
actr$HH_SURVEY[1:n]<-ifelse(actr$type[1:n]=='ACTIVITY',
                            ifelse(actr$Q1[1:n]!='MISSING',
                                   ifelse(actr$Q1[1:n]=='2','N','Y'),'MI'),'MI')

#7.2) label household supporting trip----
n <- nrow(actr)
actr$previous[2:n]<-ifelse(actr$UserId[1:(n - 1)] == actr$UserId[2:n],actr$subtype[1:(n-1)],NA)
actr$following[n] = NA
actr$following[1:(n-1)]<-ifelse(actr$UserId[1:(n - 1)] == actr$UserId[2:n],actr$subtype[2:n],NA)

###label trip/trip leg
actr$type[1:(n-1)]<-ifelse(actr$UserId[1:(n - 1)] == actr$UserId[2:n],
                           ifelse(actr$type[1:(n-1)]=='TRIP',
                                  ifelse(actr$following[1:(n - 1)]%in%Trip,'LEG','TRIP'),
                                  actr$type[1:(n-1)]),actr$type[1:(n-1)])

###label household tag (based on destination)
actr$HH_SURVEY[1:(n-1)]<-ifelse(actr$UserId[1:(n - 1)] == actr$UserId[2:n],
                                ifelse(actr$type[1:(n-1)]=='TRIP',
                                       ifelse(actr$HH_SURVEY[2:n]=='Y','Y',
                                              ifelse(actr$HH_SURVEY[2:n]=='N','N','MI')),
                                       actr$HH_SURVEY[1:(n-1)]),actr$HH_SURVEY[1:(n-1)])

for (i in (n-1):1) {
  if(actr$UserId[i] == actr$UserId[i+1]&actr$type[i]=='LEG'){
    actr$HH_SURVEY[i] = actr$HH_SURVEY[i+1]
  }
}

#7.3) re-categorize sub-type----
n <- nrow(actr)
actr$subtype_SURVEY = actr$type
actr$subtype_SURVEY[actr$type=='LEG'] = 'TRIP'
actr$subtype_SURVEY[actr$type=='ACTIVITY'] = 'OUTHOME'
actr$subtype_SURVEY[actr$subtype=='HOME'] = 'HOME'
actr$subtype_SURVEY = paste0(actr$subtype_SURVEY,'.',actr$HH_SURVEY)
actr$subtype_SURVEY[actr$type=='DEVICE_OFF'] = 'MI'