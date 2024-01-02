########################## 1. Preparing Packages and Global variables/functions ##########################

## checks for installed packages, install and load those needed
list.of.packages <- c("data.table",      # Table. https://cran.r-project.org/web/packages/data.table/data.table.pdf
                      "reshape2",        # Flexibility Reshape Data. https://cran.r-project.org/web/packages/reshape2/reshape2.pdf
                      "lubridate",       # Date and Time. https://cran.r-project.org/web/packages/lubridate/lubridate.pdf
                      'tictoc',          # Functions for timing. https://cran.r-project.org/web/packages/tictoc/tictoc.pdf
                      "utils",           # Utitlity Functions. https://cran.r-project.org/web/packages/R.utils/R.utils.pdf
                      "itertools",       # Iterator Tools. https://cran.r-project.org/web/packages/itertools/itertools.pdf
                      
                      "scales",          # Visualization. https://cran.r-project.org/web/packages/scales/scales.pdf
                      "gridExtra",       # Grid Graphics.https://cran.r-project.org/web/packages/gridExtra/gridExtra.pdf 
                      "ggpubr",          # Publication ready plots (ggplot2). https://cran.r-project.org/web/packages/ggpubr/ggpubr.pdf
                      "ggthemes",        # Extra Themes, Scales and Geoms (ggplot2). https://cran.r-project.org/web/packages/ggthemes/ggthemes.pdf
                      "ggdendro",        # Dendrograms and Tree Diagrams(ggplot2). https://cran.r-project.org/web/packages/ggdendro/ggdendro.pdf
                      "tidyverse",       # Common data representations and 'API' design. https://cran.r-project.org/web/packages/tidyverse/index.html
                      "RColorBrewer",    # RCololBrewer
                      
                      "zoo",              #S3 Infrastructure for Regular and Irregular Time Series (Z'sOrdered Observations)
                      "forcats",         # Categorical variables. https://cran.r-project.org/web/packages/forcats/forcats.pdf
                      "TraMineR",        # Sequence Analysis. https://cran.r-project.org/web/packages/TraMineR/TraMineR.pdf
                      "WeightedCluster", # Clustering of Weighted Data. https://cran.r-project.org/web/packages/WeightedCluster/WeightedCluster.pdf
                      "factoextra",      # Multivariate data analyses. https://cran.r-project.org/web/packages/factoextra/factoextra.pdf
                      
                      
                      "fda.usc",         # Functional Data Analysis. https://cran.r-project.org/web/packages/fda.usc/fda.usc.pdf
                      'fdANOVA',         # Analysis of Variance for Univariate and Multivariate Functional Data. https://cran.r-project.org/web/packages/fdANOVA/fdANOVA.pdf 
                      "refund",          # Regression with Functional Data. https://cran.r-project.org/web/packages/refund/refund.pdf
                      'fossil'          # Palaeoecological and Palaeogeographical Analysis. https://cran.r-project.org/web/packages/fossil/fossil.pdf
)

## Make sure that all pacakges in the list above have been installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, library, character.only = TRUE) # show library of each package



## simply the function for dplyr::select
select <- dplyr::select

## Function for sentence capitalization
capwords <- function(s, strict = FALSE) { 
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

library(dplyr)
library(TraMineR)
library(tibble)
library(tidyr)
library(factoextra)
library(WeightedCluster)
library(forcats)

########################## 2. Import data ##########################
actr$starttime_shifted = strptime(actr$starttime,format = "%Y-%m-%d %H:%M:%S") - (180*60)
actr$endtime_shifted = strptime(actr$endtime,format = "%Y-%m-%d %H:%M:%S") - (180*60)
actr$subtype_category = factor(actr$subtype_SURVEY, 
                               levels = c( "HOME.Y", "HOME.N","HOME.MI", 
                                           "OUTHOME.Y", "OUTHOME.N","OUTHOME.MI", 
                                           'TRIP.Y', 'TRIP.N','TRIP.MI','MI'),
                               labels =   c("HOME.Y", "HOME.N","HOME.MI",  
                                            "OUTHOME.Y","OUTHOME.N","OUTHOME.MI", 
                                            'TRIP.Y', 'TRIP.N', 'TRIP.MI','MI'))

myVars <- c("UserId", "starttime", "endtime", "starttime_shifted",
            "endtime_shifted",'startdate','subtype_category',
            "subtype", "type",'Q1','Q2','Q3','Q4','Q5','Q6','Q7',
            'Q8','Q9','Q10','Q11','ifvalid')
indata <- actr[myVars]

# sort by user_id and then start time of the episode
indata <- indata[
  order(indata[, 1], indata[,2]),
]

start_midnight = "0:00 AM"
end_midnight = "11:59 PM"

########################## 3. Create Activity Travel Sequences ##########################
timeInterval <- 300 # ## minutes interval

# For each Person-Day: Create a function to convert activity/trip episodes to time points strings
create_seq <- function(start, end, fsub){
  
  # for each individual, get the first date and last date of the records
  start_0 = ymd_hm(paste(date(min(start)), start_midnight),tz = "CST6CDT") - minutes(timeInterval/60)
  end_0 = ymd_hm(paste(date(max(end)), end_midnight),tz = "CST6CDT")
  
  # sequence of time points between start and end time
  timeseq <- seq(start_0, end_0, by = timeInterval)[-1]          
  
  # approx impute data between time points; if values is missing, remain left-continuous
  alltypes <- approx(start, fsub, xout = timeseq, method = "constant")
  
  df.curtype <- data.frame(curtime = timeseq, curtype = as.character(letters[alltypes$y])) %>%
    mutate(curdate = substr(curtime, 1, 10)) %>%
    group_by(curdate) %>%
    dplyr::summarize(typestring = gsub("NA", "z", paste(curtype, collapse = "")))
  
  return(df.curtype)
}

## For each user: Apply such function to create sequences
inseq <- indata %>%
  group_by(UserId) %>%
  do(create_seq(.$starttime_shifted, .$endtime_shifted, .$subtype_category)) %>% 
  ungroup()

# get the corresponding letter for each subtype
HHsubtypes = levels(indata$subtype_category)
HHsubtypelist <- vector(mode="list", length=length(HHsubtypes))
names(HHsubtypelist) <- HHsubtypes
for (i in 1:length(HHsubtypes)){
  HHsubtypelist[[i]] <- letters[i]
}

########################## 4. cleaning Data ################################################
#get valid person-day
inseq= data.frame(inseq)
inseq$curdate = strptime(inseq$curdate,format = "%Y-%m-%d")

inseq_label = indata %>%
  group_by(UserId,startdate) %>% summarise(ifvalid=head(ifvalid,1))
inseq_label = data.frame(inseq_label)

inseq_label$startdate = strptime(inseq_label$startdate,format = "%Y-%m-%d")

inseq = merge(inseq, inseq_label, by.x=c("UserId", "curdate"), 
              by.y=c("UserId","startdate"),all.x = TRUE,all.y=FALSE)
inseq = inseq[!is.na(inseq$ifvalid),]
inseq = inseq[inseq$ifvalid=='valid',]

# save the files
write.csv(inseq, "sequence_Daynamica_5min.csv", row.names=TRUE)

# number of characters for one day
stringLength = round(24 * 60 * 60 / timeInterval)

#create a list of days of week 
seven_days <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

# define a function to do data cleaning and add two columns (date, weekdays) & 
#before 10 am start & after 2 pm end & remove missing all day
#time1: 10 am, which means the data must start earlier than 10 am for a day 12 * (10-3)
#time2: 3 pm, which means the data must end later than 3 pm for a day 12 * (15-3)
#min_duration: 6 hours, which means the missing value is no longer than 8 hours for a day 8 *12
data_cleaning <- function(inseq, time1 = 84, time2 = 132, min_duration = 96){
  
  # deal with the first (last) day with NA data at the start (end)
  # if the data start and end within the valid range, replace z with homeletter 'HOME.Y'
  homeletter <- as.character(HHsubtypelist["HOME.Y"][[1]])
  for(i in 1:nrow(inseq)){
    cur.seq <- inseq$typestring[i]
    cur.vector <- strsplit(cur.seq, "")[[1]]
    cur.length <- length(cur.vector)
    
    # deal with string start
    if(cur.vector[1] == 'z' & length(cur.vector[cur.vector=='z'])<time1){
      for(j in 2:cur.length){
        cur.vector[j-1] <- homeletter
        if(cur.vector[j] != 'z'){break}
      }
      inseq$typestring[i] <- paste(cur.vector, collapse = "")
    } 
    
    # deal with string end
    if(cur.vector[cur.length] == 'z'& length(cur.vector[cur.vector=='z'])<time2){
      for(j in 2:cur.length){
        cur.vector[cur.length-j+2] <- homeletter
        if(cur.vector[cur.length-j+1] != 'z'){break}
      }
      inseq$typestring[i] <- paste(cur.vector, collapse = "")
    }
  }
  
  #make sure each day have 1440 mins and missing duration less than 8 hours
  outData <- inseq[0,]
  #k=0
  for(i in 1:nrow(inseq)){
    curSeq <- inseq$typestring[i]
    
    MI_char <- HHsubtypelist["MI"][[1]]
    if(nchar(curSeq) == stringLength & (grepl('z',curSeq, fixed = TRUE)==FALSE) & 
       str_count(curSeq,MI_char)<min_duration) {
      ADD <- TRUE
      
      # append the record to output if meet criteria
      if(ADD){
        #k <- k+1
        outData<- rbind(outData,inseq[i,])
      }
    }
  }
  
  # ndays contains user id and # of days observed
  outData <- outData %>%
    mutate(curdate = as.Date(curdate), weekdays = weekdays(curdate))
  outData$weekdays <- factor(outData$weekdays, levels = seven_days)
  
  return(outData)
}

# Apply the function
inseq.clean <- data_cleaning(inseq)

# save the files
write.csv(inseq.clean, "SeqClean_5min_Daynamica.csv", row.names=F)

# test if contain 'z' - NULL values; if yes, scripts above need to be revised
inseq.null <- inseq.clean
inseq.null <- inseq.clean[grepl("z", inseq.clean$typestring),]
print(inseq.null)


########################## 5. descriptive analysis ##########################
# distribution of # of days each participant
ndays <- inseq.clean %>% group_by(UserId) %>% summarise(nr = n()) %>% ungroup()
ndays.nusers = ndays %>% group_by(nr) %>% summarize(n=n())
print(ndays.nusers)
ggplot(ndays.nusers) + 
  geom_bar(aes(x=nr, y = n), width = 0.7,stat="identity") +
  geom_text(aes(x=nr, y = n,label = n), vjust=-0.2,size = 3.5) +
  labs(x = "Number of days", 
       y = "Number of participants") +
  theme(panel.background = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) + xlim(1,18)


# distribution of # of observations for each day of the week
nweekdays <- inseq.clean %>% group_by(weekdays) %>% summarise(nw = n()) %>% ungroup()
ggplot(nweekdays) + 
  geom_bar(aes(x = weekdays, y = nw), width = 0.6,stat="identity") + 
  geom_text(aes(x=weekdays, y = nw,label = nw), vjust=-0.2,size = 3.5) +
  labs(x = "Days of the week", 
       y = "Number of observations") +
  theme(panel.background = element_blank())
print(nweekdays)

## get summary counts of input and processed data
print(paste("Total input Episode", nrow(indata)))
print(paste("Total person days", nrow(inseq)))
print(paste("TOtal valid person days", nrow(inseq.clean)))

########################## 6. define cost matrix ##############################

#### define the matrix yourself
# subtypes in this database:
#   [1] "HOME.Y", "HOME.N","HOME.MI",  "OUTHOME.Y", "OUTHOME.N","OUTHOME.MI", 
#        'TRIP.N', 'TRIP.Y', 'TRIP.MI','MI'

#for example, the cost between "HOME.Y" and  "HOME.N" in the matrix is substitution.matrix[1,2] = 1
# symmetric matrix
substitution.matrix <- matrix( 
  data=c(  0,   1, 1.5,   1,   2, 1.5,   1,   2, 1.5,   1,
           1,   0, 0.5,   2,   1, 1.5,   2,   1, 1.5,   1,
           1.5, 0.5,   0, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5,   1,
           1,   2, 1.5,   0,   1, 0.5,   1,   2, 1.5,   1,
           2,   1, 1.5,   1,   0, 0.5,   2,   1, 1.5,   1,
           1.5, 1.5, 1.5, 0.5, 0.5,   0, 1.5, 1.5, 1.5,   1,
           1,   2, 1.5,   2,   2, 1.5,   0,   1, 0.5,   1,
           2,   1, 1.5,   1,   1, 1.5,   1,   0, 0.5,   1,
           1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 0.5, 0.5,   0,   1,
           1,   1,   1,   1,   1,   1,   1,   1,   1,   0),
  nrow = length(HHsubtypelist), ncol = length(HHsubtypelist),
  dimnames = list(letters[1:length(HHsubtypelist)],letters[1:length(HHsubtypelist)]))


########################## 7. calculate sequence distance ##############################
CalcDistanceMatrix <- function(data){
  # Step 1: convert string to data frame
  data.decomp <- seqdecomp(data$typestring, sep = '')
  ## create state sequence object
  data.seq <- seqdef(data.decomp, 
                     alphabet = letters[1:length(HHsubtypelist)],
                     states = levels(indata$subtype_category))
  
  # Step 2: calculate sequence dissimilarity 
  # (1b) cost matrix distinguishing activities and trips using optimal distance
  data.om <- seqdist(data.seq,
                     method = 'OM',
                     indel = max(substitution.matrix)/2,
                     sm = substitution.matrix)
  
  # (2b) cost matrix distinguishing activities and trips using ham distance
  data.ham <- seqdist(data.seq,
                      method = 'HAM',
                      sm = substitution.matrix)
  
  # Step 3: calculate mean distance between users 
  # define the function SequenceTransform to get the distance for each user-day
  SequenceTransform <- function(dist.sequence, match.data = data){
    # convert distance matrix to long format and add information about user_id and weekdays
    temp.data.long <- melt(dist.sequence) %>% 
      mutate(User1 = match.data$UserId[Var1],
             User2 = match.data$UserId[Var2])
    temp.data.long = temp.data.long[,c('User1','User2','value')]
    temp.data.wide <- spread(temp.data.long, User2, value) %>% 
      tibble::column_to_rownames(var = 'User1')
    return(temp.data.wide)
  } # end of SequenceTransform
  
  data.om <- SequenceTransform(data.om, data)
  data.ham <- SequenceTransform(data.ham, data)
  
  ###### Step 4: return values #######
  return(list(data.seq = data.seq,
              UserId = data$UserId,
              date= data$curdate,
              data.om = data.om,
              data.ham = data.ham))
}



# For each day of the week, calculate distance
#calculate weekedays and weekends saperately
#weekday
d = c('Monday','Tuesday','Wednesday','Thursday','Friday')
inseq.day <- inseq.clean[inseq.clean$weekdays %in% d,] 

#consider one user may have multiple Mondays or multiple other days of the week
inseq.day$UserId = as.character(inseq.day$UserId)
for (i in 1:nrow(inseq.day)) {
  inseq.day$UserId[i] = paste(inseq.day$UserId[i],i,sep = '#')
}

print(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
print(paste(">>>>>>>> Now processing day:", d))
print(head(inseq.day))

# for each weekday, get the pair-wise sequence distances
weekday.distance <- CalcDistanceMatrix(data = inseq.day)
#save distance Robject
saveRDS(weekday.distance, "distanceWeekday_bookchapter.rds")

#weekend
d = c('Saturday','Sunday')
inseq.day <- inseq.clean[inseq.clean$weekdays %in% d,] 

#consider one user may have multiple Mondays or multiple other days of the week
inseq.day$UserId = as.character(inseq.day$UserId)
for (i in 1:nrow(inseq.day)) {
  inseq.day$UserId[i] = paste(inseq.day$UserId[i],i,sep = '#')
}

print(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
print(paste(">>>>>>>> Now processing day:", d))
print(head(inseq.day))

# for each weekday, get the pair-wise sequence distances
weekend.distance <- CalcDistanceMatrix(data = inseq.day)
#save distance Robject
saveRDS(weekend.distance, "distanceWeekend_bookchapter.rds")
