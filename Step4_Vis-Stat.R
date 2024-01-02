########################## 1. gender analysis ##########################
#read data
per = read.csv("personday_regression_20230818_selcols.csv")
id = read.csv("userid_20230818_readonly.csv")

#merge & format data
per = merge(per,id,by.x='NEWID',by.y='NEWID',all.x = T,all.y = F)
per$USERID = str_trim(tolower(per$USERID))
clust = clust.plot.user
clust$UserId = gsub("\\#.*","",clust$UserId)
clust$USERID = substr(clust$UserId,0,9)
clust= merge(x=clust,y=per,by.x=c("USERID","date"),by.y = c('USERID','DATE'),
             all.x=F,all.y = F)
colnames(clust)[4] = 'group'

#reclassify gender
clust$GENDER = factor(clust$GENDER, 
                      levels = c( "Female",'Male','Nonbinary'), 
                      labels =   c("Cisgender Female",
                                   'Cisgender Male',
                                   'Nonbinary Gender'))

#gender visualization
tb1 = with(clust, table(GENDER, group))
tb1_groupsum = apply(tb1, 1, sum)

tb1 = as.data.frame(tb1)
#get percentage of each gender group
for(i in 1:6){
  tb1[(3*i-2):(3*i),'Freq'] = tb1[(3*i-2):(3*i),'Freq']/tb1_groupsum
}

#visualize
ggplot(tb1,aes(x=GENDER)) + 
  geom_bar(aes(x=GENDER, y = Freq,fill = group),position="dodge", 
           stat="identity",alpha = 1) +
  scale_fill_manual(values = c( "#E41A1C","#4DAF4A", "#984EA3",
                                "#FF7F00","#377EB8",  "#FFFF33"),
                    labels = c('1 - Nonbinary Gender weekday',
                               '2 - Cisgender Female weekday',
                               '3 - Cisgender Male weekday', 
                               '4 - Nonbinary Gender weekend',
                               '5 - Cisgender Female weekend',
                               '6 - Cisgender Male weekend'),
                    guide=guide_legend(ncol=2)) + 
  ylab('Percentage') + xlab('') +  
  scale_y_continuous(expand = c(0, 0),limits = c(0, 0.48)) + 
  ggplot2::theme(legend.position="bottom",legend.title = element_text(size = 30),
                 legend.text = element_text(size = 30),
                 axis.text = element_text(size = 30),
                 axis.title = element_text(size = 30))

########################## 2. chaid analysis ##########################
#data format
clust = clust%>% as.data.table()
clust[,AGE_R2:=fcase(
  AGE_R == '18-24',
  'Young Age',
  AGE_R == '25-34',
  'Middle Age',
  AGE_R == '35-44',
  'Middle Age',
  AGE_R == '45-54',
  'Middle Age',
  AGE_R == '55-64',
  'Sinor Age',
  AGE_R == '65+',
  'Sinor Age'
)]

clust[,EDU_R2:=fcase(
  EDU_R == 'Bachelor',
  'Bachelor+',
  EDU_R == 'Graduate',
  'Bachelor+',
  EDU_R == 'Other',
  'Bachelor+',
  EDU_R == 'High-School or Less',
  'Bachelor-'
)]

clust$AREA_R = as.character(clust$AREA_R)
clust[,AREA_R:= fifelse(AREA_R=='','Urban',AREA_R)]

clust$INC_R = as.character(clust$INC_R)
clust[,INC_R:= fifelse(INC_R=='Unknown','No',INC_R)]

#prepare for chaid
clust$RACE_R2 <- factor(clust$RACE_R2, levels = c('White','Non-White'))
clust$AGE_R2 <- factor(clust$AGE_R2,levels = c('Middle Age',"Young Age","Sinor Age"))
clust$EDU_R2 <- factor(clust$EDU_R2,levels = c("Bachelor+","Bachelor-"))
clust$INC_R <- factor(clust$INC_R,levels = c("No","Yes"))
clust$EMP_LOC_R3 <- factor(clust$EMP_LOC_R3,
                           levels = c("EMPLOYED OUT","EMPLOYED MIX",
                                      "EMPLOYED WFH","UNEMPLOYED/RETIRED"))

clust$group <- factor(
  clust$group,
  levels = c("4 - Regular Work (mostly without Household Tasks at Home)",
             "3 - Regular Work (mostly with Household Tasks at Home)",
             "2 - Mostly at Home (mostly without Household Tasks)",
             "1 - Mostly at Home (mostly with Household Tasks)",
             "5 - OutHome in the Evening",
             "6 - Mostly OutHome"))

clust$group <- factor(
  clust$group,
  levels = c("4 - Mostly At Home (mostly without Household Tasks)",
             "1 - Mostly At Home (mostly with Household Tasks)",
             "2 - Mostly At Home (Household Tasks in the morning)",
             "3 - Mostly At Home (Household Tasks at Night)",
             "6 - Mostly OutHome (mostly without Household Tasks)",
             "5 - Mostly OutHome (mostly with Household Tasks)"))

library(CHAID)

#chaid analysis
control = chaid_control(alpha2 = 0.01, alpha3 = -1, alpha4 = 0.01, 
                        minsplit = 3700, minbucket = 3700, minprob = 0.05,
                        stump = F, maxheight = -1)

chaidattrit1 <- chaid(group ~ GENDER + hh_type + EMP_LOC_R3 + AGE_R2 + RACE_R2+
                        EDU_R2 + student +INC_R, data=clust,control = control)
print(chaidattrit1)
plot(chaidattrit1)

chisq.test(clust$group, clust$GENDER)

