# Data cleaning for spontaneous long calling
library(move)
library(ctmm)
library(readr)
library(plyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(purrr)

#SessionInfo()
#R version 4.1.3 (2022-03-10)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 10 x64 (build 22621)
#attached packages:
# [1] climateStability_0.1.3 spatialEco_1.3-7       move_4.1.8             rgdal_1.5-27           geosphere_1.5-10      
# [6] lubridate_1.7.10       forcats_0.5.1          stringr_1.4.0          purrr_0.3.4            tidyr_1.1.3           
#[11] tibble_3.1.4           ggplot2_3.4.0          tidyverse_1.3.1        dplyr_1.0.7            plyr_1.8.6            
#[16] amt_0.1.4              readr_2.0.1            raster_3.6-3           sp_1.4-5               ctmm_0.6.2    


# aim is to clean data to put on movebank

Tuanan_GPS_2012 <- read_csv("Tuanan_2012_GPS_data.csv")
as.data.frame(Tuanan_GPS_2012)
Tuanan_GPS_2012$ID_Month<-paste(Tuanan_GPS_2012$identity, Tuanan_GPS_2012$Month)
Tuanan_<-subset(Tuanan_GPS_2012, with(Tuanan_GPS_2012, ID_Month %in% names(which(table(ID_Month)>=25))))
nrow(Tuanan_)
nrow(Tuanan_GPS_2012)

# need to filter dataframe to remove duplicate pts or those within 25 minutes of one another
elapsed <- function(x)
{
  y <- abs(as.duration(x[2:length(x)] %--% x[1:(length(x)-1)]))
  y >= 10*60
}
Tuanan_GPS_to_Movebank<-Tuanan_ %>%
  group_split(ID) %>%  
  map_dfr(~ .[c(T, if (nrow(.) > 1) elapsed(.$timestamp)),])
str(Tuanan_GPS_to_Movebank)

# select attributes used on movebank database for storage
Tuanan_GPS_to_Movebank<-Tuanan_GPS_to_Movebank%>% dplyr::select('ID', 'latitude', 'longitude', 'timestamp')
table(is.na(Tuanan_GPS_to_Movebank$timestamp))
strptime(Tuanan_GPS_to_Movebank$timestamp, "%Y-%m-%d %H:%M:%S")

# for individual occurrence distributions

Tuanan_GPS_to_occurrence<-Tuanan_GPS_to_Movebank%>% dplyr::select('id', 'latitude', 'longitude', 'timestamp')

#write csv and add to movebank manually for ctmm
write.csv(Tuanan_GPS_to_Movebank, file="2012 GPS ID_month.csv")
Tuanan_GPS_to_Movebank<-read.csv(file="2012 GPS ID_month.csv")

#table(is.na(Tuanan_GPS_to_Movebank$timestamp))
strptime(Tuanan_GPS_to_Movebank$timestamp, "%Y-%m-%d %H:%M:%S")

# for individual occurrence distributions
Tuanan_GPS_to_occurrence<-Tuanan_GPS_to_Movebank%>% dplyr::select('ID', 'latitude', 'longitude', 'timestamp')

# extent based on Tuanan map
library(raster)
Tuanan_r<-raster()
extent(Tuanan_r)<-extent(212812, 219348, 9758957, 9773670)
res(Tuanan_r)<-25
projection(Tuanan_r) <- "+init=epsg:32750 +proj=utm +zone=50 +units=m +south"

# telemetry object for 2012 data
OD_tel<-as.telemetry(object=Tuanan_GPS_to_occurrence, projection = "+init=epsg:32750 +proj=utm +zone=50 +units=m +south", mark.rm=TRUE)

# create a dataframe of GPS points for "available" locations - to use later
Avail_pts<- do.call(rbind, lapply(OD_tel, function(x) as.data.frame(x)))
Avail_pts$ID <- rownames(Avail_pts)
library(tm)
removeNumbers(Avail_pts$ID)
Avail_pts$ID<- gsub("\\..*","",Avail_pts$ID)

# remove month from the end of the ID
Avail_pts$ID <- substr(as.character(Avail_pts$ID),
                       start= 1,
                       stop= nchar(as.character(Avail_pts$ID) )-2 )

#select males of interest (those that were present frequently in 2012)
str(Avail_pts$timestamp)

Avail_pts<-Avail_pts[Avail_pts$ID %in% c("Chili","Niko", "Henk", "Tomi", "Teju" , "Wodan"), ]
str(Avail_pts)

PT<-rep("Available",times=2222)
Avail_pts$PT<-PT


# get long call data on movebank
# These data were manually put on movebank
login <- movebankLogin(username="llabarge", password="Samango1992!")
LongCalls<-getMovebankData(study="Spontaneous Long Calls, Bornean Orangutan Flanged males 2012, Tuanan", login=login)

# format long call data so can be merged as "cases" along with the
# "control" available points
## of these "spontaneous" long calls, 1011 came from the PAM system
LC_tel<- as.telemetry(LongCalls, projection = "+init=epsg:32750 +proj=utm +zone=50 +south")
LC_pts<- do.call(rbind, lapply(LC_tel, function(x) as.data.frame(x)))
str(LC_pts)
PT<-rep("LongCall",times=1234)
LC_pts$PT<-PT

# format the dataset to remove extraneous symbols, and remove non-range resident individuals
library(stringr)
LC_pts$ID <- rownames(LC_pts)
unique(LC_pts$ID)
library(tm)
removeNumbers(LC_pts$ID)
LC_pts$ID<- gsub("\\..*","",LC_pts$ID)
LC_pts<-LC_pts[!grepl("Helium", LC_pts$ID),]
LC_pts<-LC_pts[!grepl("Preman", LC_pts$ID),]
LC_pts<-LC_pts[!grepl("Dayak", LC_pts$ID),]
LC_points<-LC_pts[,c("x", "y", "PT", "timestamp", "ID")]
rownames(LC_points) <- NULL
as.data.frame(LC_points)


# need to filter dataframe to remove duplicate pts or those within 25 minutes of one another
elapsed <- function(x)
{
  y <- abs(as.duration(x[2:length(x)] %--% x[1:(length(x)-1)]))
  y >= 25*60
}

LC_points %>%
  group_split(ID) %>%  
  map_dfr(~ .[c(T, if (nrow(.) > 1) elapsed(.$timestamp)),])

# removes around 200 points that were part of the same event leaving 1070
## this was because PAM system often recorded the same long calls as observation


# add a month column (for later indexing for raster extract)
# bind the long calls (cases) and available pts (controls) together

Avail_pts$Month <- str_sub(Avail_pts$timestamp, start = 6L, end = 7L)
LC_points$Month <- str_sub(LC_points$timestamp, start = 6L, end = 7L)

LC_points<-LC_points[LC_points$ID %in% c("Chili","Niko","Henk", "Tomi", "Teju" , "Wodan"), ]

LC_com<-LC_points%>% dplyr::select('ID', 'PT', 'Month', 'x', 'y', 'timestamp')

Av_com<-Avail_pts%>% dplyr::select('ID', 'PT', 'Month','x', 'y', 'timestamp')
LC_Control<-rbind(LC_com, Av_com)
as.data.frame(LC_Control)
write.csv(LC_Control, file="LC_case_control.csv")
LC_Control<-read_csv(file="LC_case_control.csv")



# denote 1 for long call, 0 for available
library(plyr)
LC_Control$PT <- revalue(LC_Control$PT, c("LongCall"=1))
LC_Control$PT <- revalue(LC_Control$PT, c("Available"=0))

Monthly_LC_df<-rbind(M_3,A_4, MA_5, J_6, Ju_7, Au_8, Sep_9,Oct_10, Nov_11, Dec_12)

X<-split(LC_Control, LC_Control$ID)

Chili_df<-X$Chili
str(Chili_df)
Henk_df<-X$Henk
Niko_df<-X$Niko
Teju_df<-X$Teju
Tomi_df<-X$Tomi
Wodan_df<-X$Wodan

#any NA points within ODs would be 0
Chili_df[is.na(Chili_df)] <- 0
Henk_df[is.na(Henk_df)] <- 0
Niko_df[is.na(Niko_df)] <- 0
Teju_df[is.na(Teju_df)] <- 0
Tomi_df[is.na(Tomi_df)] <- 0
Wodan_df[is.na(Wodan_df)] <- 0

# Add association data for each individual which has come manually from the Tuanan Database
library(data.table)
Chili_Associations <- read_csv("Chili_Associations.csv")
Chili_df<-as.data.frame(Chili_df)
Chili_Associations<-as.data.frame(Chili_Associations)

# format time so that timestamps can be matched up for a rolling join
Chili_Associations$Date<-as.POSIXct(x = as.character(Chili_Associations$Date), format = "%m/%d/%Y")
Chili_Associations$NewDate<-format(Chili_Associations$Date, "%Y-%m-%d")
Chili_Associations$Timestamp<-with(Chili_Associations,paste(NewDate,Time,sep=" "),"%Y-%m-%d %H:%M:%S")
Chili_Associations$Timestamp1<-as.POSIXct(x = as.character(Chili_Associations$Timestamp))

# already ran this script before, but if starting from raw data would need to include this for the long call data
Chili_df$Timestamp<-as.POSIXct(x = as.character(Chili_df$timestamp))

# set DT for data.table to roll on
Chili_Associations$DT<-as.numeric(Chili_Associations$Timestamp1)
Chili_df$DT<-as.numeric(Chili_df$Timestamp)

# set data.table key
setDT(Chili_Associations)[,join_time:=DT]
setDT(Chili_df)[, join_time:=DT]
setkey(Chili_df, join_time)
setkey(Chili_Associations, join_time)

# rolling window of an hour around observations
one_hour <- 60*60*1 # 60 seconds by 60 minutes by one - for data.table calculation
Chili_df_Act2<-Chili_Associations[Chili_df, nomatch=0, roll = one_hour]
View(Chili_df_Act2)

# remove NA
Chili_df<-Chili_df_Act2[!is.na(Chili_df_Act2$PT),]
str(Chili_df)
#Checked associations with "View" and only one is with a male - remove this

Chili_df<-Chili_df[!grepl("Wodan", Chili_df$`Party member 2`),]

# check for duplicates because row length doesnt match original d

duplicated(Chili_df$timestamp)

Chili_df<-Chili_df[!duplicated(Chili_df$timestamp),]

# select only necessary columns
Chili_df<-Chili_df[,c("x", "y", "timestamp", "ID","PT", "Party member 2", "Class Focal", "Name Focal", "Party Size")]

Chili_df$Association<-Chili_df$`Party Size`

# a one or zero for associations
Chili_df$Association[Chili_df$Association == 1 ] <- 0
Chili_df$Association[Chili_df$Association >= 2 ] <- 1

write.csv(Chili_df, file="Chili_df.csv")

# Henk has no associations during data collection in 2012

# Niko
str(Niko_df)
Niko_Associations <- read_csv("Niko_Associations.csv")

# format time so that timestamps can be matched up for a rolling join
Niko_Associations$Date<-as.POSIXct(x = as.character(Niko_Associations$Date), format = "%m/%d/%Y")
Niko_Associations$NewDate<-format(Niko_Associations$Date, "%Y-%m-%d")
Niko_Associations$Timestamp<-with(Niko_Associations,paste(NewDate,Time,sep=" "),"%Y-%m-%d %H:%M:%S")
Niko_Associations$Timestamp1<-as.POSIXct(x = as.character(Niko_Associations$Timestamp))

# already ran this script before, but if starting from raw data would need to include this for the long call data
Niko_df$Timestamp<-as.POSIXct(x = as.character(Niko_df$timestamp))

# set DT for data.table to roll on
Niko_Associations$DT<-as.numeric(Niko_Associations$Timestamp1)
Niko_df$DT<-as.numeric(Niko_df$Timestamp)

# set data.table key
setDT(Niko_Associations)[,join_time:=DT]
setDT(Niko_df)[, join_time:=DT]
setkey(Niko_df, join_time)
setkey(Niko_Associations, join_time)

# rolling window of an hour around observations
one_hour <- 60*60*1 # 60 seconds by 60 minutes by one - for data.table calculation - weird but that's what data.table requires
Niko_df_Act2<-Niko_Associations[Niko_df, roll = one_hour]

Niko_df<-Niko_df_Act2[,c("x", "y", "timestamp", "ID","PT", "Party member 2", "Class Focal", "Name Focal", "Party Size")]
Niko_df$ASC_F<-Niko_df$`Class Focal`
Niko_df$Association<-Niko_df$`Party Size`
Niko_df$Asso_ID<-Niko_df$`Party member 2`
Niko_df[is.na(Niko_df)] <- 0


# All associations were female - add column of 1 and 0 for whether row is during an association
# a one or zero for associations
Niko_df$Association[Niko_df$Association == 1 ] <- 0
Niko_df$Association[Niko_df$Association >= 2 ] <- 1

str(Niko_df)
write.csv(Niko_df, "Niko_df.csv")
# Teju
Teju_Associations <- read_csv("Teju_Associations.csv")

# format time so that timestamps can be matched up for a rolling join
Teju_Associations$Date<-as.POSIXct(x = as.character(Teju_Associations$Date), format = "%m/%d/%Y")
Teju_Associations$NewDate<-format(Teju_Associations$Date, "%Y-%m-%d")
Teju_Associations$Timestamp<-with(Teju_Associations,paste(NewDate,Time,sep=" "),"%Y-%m-%d %H:%M:%S")
Teju_Associations$Timestamp1<-as.POSIXct(x = as.character(Teju_Associations$Timestamp))

# already ran this script before, but if starting from raw data would need to include this for the long call data
Teju_df$Timestamp<-as.POSIXct(x = as.character(Teju_df$timestamp))

# set DT for data.table to roll on
Teju_Associations$DT<-as.numeric(Teju_Associations$Timestamp1)
Teju_df$DT<-as.numeric(Teju_df$Timestamp)

# set data.table key
setDT(Teju_Associations)[,join_time:=DT]
setDT(Teju_df)[, join_time:=DT]
setkey(Teju_df, join_time)
setkey(Teju_Associations, join_time)

# rolling window of an hour around observations
one_hour <- 60*60*1 # 60 seconds by 60 minutes by one - for data.table calculation - weird but that's what data.table requires
Teju_df_Act2<-Teju_Associations[Teju_df, roll = one_hour]

# Joined dataframes will contain all the extra rows from the association table

Teju_df<-Teju_df_Act2[,c("x", "y", "timestamp", "ID","PT", "Party member 2", "Name Focal", "Party Size")]
Teju_df$Association<-Teju_df$`Party Size`
Teju_df$Asso_ID<-Teju_df$`Party member 2`
Teju_df[is.na(Teju_df)] <- 0

View(Teju_df)
# All associations were female - add column of 1 and 0 for whether row is during an association
# a one or zero for associations
Teju_df$Association[Teju_df$Association == 1 ] <- 0
Teju_df$Association[Teju_df$Association >= 2 ] <- 1
write.csv(Teju_df, file="Teju_df.csv")

# Tomi

Tomi_Associations <- read_csv("Tomi_Associations.csv")

# format time so that timestamps can be matched up for a rolling join
Tomi_Associations$Date<-as.POSIXct(x = as.character(Tomi_Associations$Date), format = "%m/%d/%Y")
Tomi_Associations$NewDate<-format(Tomi_Associations$Date, "%Y-%m-%d")
Tomi_Associations$Timestamp<-with(Tomi_Associations,paste(NewDate,Time,sep=" "),"%Y-%m-%d %H:%M:%S")
Tomi_Associations$Timestamp1<-as.POSIXct(x = as.character(Tomi_Associations$Timestamp))

# already ran this script before, but if starting from raw data would need to include this for the long call data
Tomi_df$Timestamp<-as.POSIXct(x = as.character(Tomi_df$timestamp))

# set DT for data.table to roll on
Tomi_Associations$DT<-as.numeric(Tomi_Associations$Timestamp1)
Tomi_df$DT<-as.numeric(Tomi_df$Timestamp)

# set data.table key
setDT(Tomi_Associations)[,join_time:=DT]
setDT(Tomi_df)[, join_time:=DT]
setkey(Tomi_df, join_time)
setkey(Tomi_Associations, join_time)

# rolling window of an hour around observations
one_hour <- 60*60*1 # 60 seconds by 60 minutes by one - for data.table calculation - weird but that's what data.table requires
Tomi_df_Act2<-Tomi_Associations[Tomi_df, roll = one_hour]

#no adult male associations (but juv/infant with female)
Tomi_df<-Tomi_df_Act2[,c("x", "y", "timestamp", "ID","PT",  "Party member 2", "Name Focal", "Party Size")]
Tomi_df$Association<-Tomi_df$`Party Size`
Tomi_df$Asso_ID<-Tomi_df$`Party member 2`
Tomi_df[is.na(Tomi_df)] <- 0

# All associations were female - add column of 1 and 0 for whether row is during an association
# a one or zero for associations
Tomi_df$Association[Tomi_df$Association == 1 ] <- 0
Tomi_df$Association[Tomi_df$Association >= 2 ] <- 1
str(Tomi_df)

# Wodan

Wodan_Associations <- read_csv("Wodan_Associations.csv")

# format time so that timestamps can be matched up for a rolling join
Wodan_Associations$Date<-as.POSIXct(x = as.character(Wodan_Associations$Date), format = "%m/%d/%Y")
Wodan_Associations$NewDate<-format(Wodan_Associations$Date, "%Y-%m-%d")
Wodan_Associations$Timestamp<-with(Wodan_Associations,paste(NewDate,Time,sep=" "),"%Y-%m-%d %H:%M:%S")
Wodan_Associations$Timestamp1<-as.POSIXct(x = as.character(Wodan_Associations$Timestamp))

# already ran this script before, but if starting from raw data would need to include this for the long call data
Wodan_df$Timestamp<-as.POSIXct(x = as.character(Wodan_df$timestamp))

# set DT for data.table to roll on
Wodan_Associations$DT<-as.numeric(Wodan_Associations$Timestamp1)
Wodan_df$DT<-as.numeric(Wodan_df$Timestamp)

# set data.table key
setDT(Wodan_Associations)[,join_time:=DT]
setDT(Wodan_df)[, join_time:=DT]
setkey(Wodan_df, join_time)
setkey(Wodan_Associations, join_time)

# rolling window of an hour around observations
one_hour <- 60*60*1 # 60 seconds by 60 minutes by one - for data.table calculation - weird but that's what data.table requires
Wodan_df_Act2<-Wodan_Associations[Wodan_df, roll = one_hour]

#no adult male associations (but juv/infant with female)
Wodan_df<-Wodan_df_Act2[,c("x", "y", "timestamp", "ID","PT", "Party member 2", "Name Focal", "Party Size")]
Wodan_df$Association<-Wodan_df$`Party Size`
Wodan_df$Asso_ID<-Wodan_df$`Party member 2`
Wodan_df[is.na(Wodan_df)] <- 0

# All associations were female - add column of 1 and 0 for whether row is during an association
# a one or zero for associations
Wodan_df$Association[Wodan_df$Association == 1 ] <- 0
Wodan_df$Association[Wodan_df$Association >= 2 ] <- 1

# found some males that were associations in the dataframe - only a couple so remove these - not enough for analysis
View(Wodan_df)
Wodan_df<-Wodan_df[!grepl("Mister X", Wodan_df$`Party member 2`),]
Wodan_df<-Wodan_df[!grepl("Tomi", Wodan_df$`Party member 2`),]
Wodan_df<-Wodan_df[!grepl("Chili", Wodan_df$`Party member 2`),]

# save CSVs
save(Chili_df, file="Chili_df.csv")
save(Henk_df, file="Henk_df.csv")
save(Niko_df, file="Niko_df.csv")
save(Teju_df, file="Teju_df.csv")
save(Tomi_df, file="Tomi_df.csv")
save(Wodan_df, file="Wodan_df.csv")

load(file="Chili_df.csv")
load(file="Henk_df.csv")
load(file="Niko_df.csv")
load(file="Teju_df.csv")
load(file="Tomi_df.csv")
load(file="Wodan_df.csv")


str(Henk_df)
# for mapping 
# select out variables needed by movebank
Chili_mov<-Chili_df[,c("x", "y", "timestamp", "ID","PT")]
Henk_mov<-Henk_df[,c("x", "y", "timestamp", "ID","PT")]
Niko_mov<-Niko_df[,c("x", "y", "timestamp", "ID","PT")]
Teju_mov<-Teju_df[,c("x", "y", "timestamp", "ID","PT")]
Tomi_mov<-Tomi_df[,c("x", "y", "timestamp", "ID","PT")]
Wodan_mov<-Wodan_df[,c("x", "y", "timestamp", "ID","PT")]

# bind into a single dataframe
LongCalls_and_GPS<-rbind(Chili_mov, Henk_mov, Niko_mov, Teju_mov, Tomi_mov, Wodan_mov)
str(LongCalls_and_GPS)

LongCalls_and_GPS$ID_type<-paste(LongCalls_and_GPS$ID, LongCalls_and_GPS$PT)

# rename for plot
LongCalls_and_GPS$ID_type<- gsub("Chili 1", "Chili_LongCall", LongCalls_and_GPS$ID_type)
LongCalls_and_GPS$ID_type<- gsub("Chili 0", "Chili_GPS", LongCalls_and_GPS$ID_type)
LongCalls_and_GPS$ID_type<- gsub("Henk 1", "Henk_LongCall", LongCalls_and_GPS$ID_type)
LongCalls_and_GPS$ID_type<- gsub("Henk 0", "Henk_GPS", LongCalls_and_GPS$ID_type)
LongCalls_and_GPS$ID_type<- gsub("Niko 1", "Niko_LongCall", LongCalls_and_GPS$ID_type)
LongCalls_and_GPS$ID_type<- gsub("Niko 0", "Niko_GPS", LongCalls_and_GPS$ID_type)
LongCalls_and_GPS$ID_type<- gsub("Teju 1", "Teju_LongCall", LongCalls_and_GPS$ID_type)
LongCalls_and_GPS$ID_type<- gsub("Teju 0", "Teju_GPS", LongCalls_and_GPS$ID_type)
LongCalls_and_GPS$ID_type<- gsub("Tomi 1", "Tomi_LongCall", LongCalls_and_GPS$ID_type)
LongCalls_and_GPS$ID_type<- gsub("Tomi 0", "Tomi_GPS", LongCalls_and_GPS$ID_type)
LongCalls_and_GPS$ID_type<- gsub("Wodan 1", "Wodan_LongCall", LongCalls_and_GPS$ID_type)
LongCalls_and_GPS$ID_type<- gsub("Wodan 0", "Wodan_GPS", LongCalls_and_GPS$ID_type)

write.csv(LongCalls_and_GPS, file="LongCalls_and_GPS_Plots.csv")


# split into two dataframes based on if points are long calls or just GPS
LC<-split(LongCalls_and_GPS, LongCalls_and_GPS$PT)
GPS<-LC$`0`
LC<-LC$`1`

GPS<-GPS[,c("x", "y", "timestamp", "ID")] # select out movebank attributes
LC<-LC[,c("x", "y", "timestamp", "ID")]
str(LC)

write.csv(LC, "LC_for_plotting.csv")
write.csv(GPS, "GPS_LC_for_plotting.csv")

