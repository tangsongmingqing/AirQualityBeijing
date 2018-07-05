# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

#libraries needed in this file
libraries = c("lubridate", "reshape2", "readr")
lapply(libraries, library, quietly=TRUE, character.only=TRUE)

#set working directory
setwd("~/AirQualityBeijing/raw data/2013")
#custom function to merge all data from whole period 2013
multmerge = function(mypath){
  filenames=list.files(path=mypath, full.names=TRUE)         #list all file names in this forlder
  datalist = lapply(filenames, function(x){read.csv(file=x)}) #read all files
  Reduce(function(x,y) {rbind(x,y)}, datalist)               #bind all files by row
}
#apply thid multmerge function to 2 Messstations
beijing2013 = multmerge("~/AirQualityBeijing/raw data/2013") #whole period data frame of year 2013
saveRDS(beijing2013, file = "~/AirQualityBeijing/data/beijing2013.RDS")
#repeat this step also for raw data in year 2014-2018
beijing2014 = multmerge("~/AirQualityBeijing/raw data/2014") 
saveRDS(beijing2014, file = "~/AirQualityBeijing/data/beijing2014.RDS")
beijing2015 = multmerge("~/AirQualityBeijing/raw data/2015")
saveRDS(beijing2015, file = "~/AirQualityBeijing/data/beijing2015.RDS")
beijing2016 = multmerge("~/AirQualityBeijing/raw data/2016") 
saveRDS(beijing2016, file = "~/AirQualityBeijing/data/beijing2016.RDS")
beijing2017 = multmerge("~/AirQualityBeijing/raw data/2017") 
saveRDS(beijing2017, file = "~/AirQualityBeijing/data/beijing2017.RDS")
beijing2018 = multmerge("~/AirQualityBeijing/raw data/2018") 
saveRDS(beijing2018, file = "~/AirQualityBeijing/data/beijing2018.RDS")                                               

############################subset PM2.5############################################
s1 = beijing2013[ ,-c(27:33)] 
s2 = s1[which(s1$type=='PM2.5'), ]
colnames(s2) = c("date", "hour", "type", 1:28)
saveRDS(s2, file = "~/AirQualityBeijing/data/PM2.5/pm2.5_2013.RDS")
#do this step for year 2014-2018
s1 = beijing2014[ ,-c(27:33)] 
s2 = s1[which(s1$type=='PM2.5'), ]
colnames(s2) = c("date", "hour", "type", 1:28)
saveRDS(s2, file = "~/AirQualityBeijing/data/PM2.5/pm2.5_2014.RDS")
s1 = beijing2015[ ,-c(27:33)] 
s2 = s1[which(s1$type=='PM2.5'), ]
colnames(s2) = c("date", "hour", "type", 1:28)
saveRDS(s2, file = "~/AirQualityBeijing/data/PM2.5/pm2.5_2015.RDS")
s1 = beijing2016[ ,-c(27:33)] 
s2 = s1[which(s1$type=='PM2.5'), ]
colnames(s2) = c("date", "hour", "type", 1:28)
saveRDS(s2, file = "~/AirQualityBeijing/data/PM2.5/pm2.5_2016.RDS")
s1 = beijing2017[ ,-c(27:33)] 
s2 = s1[which(s1$type=='PM2.5'), ]
colnames(s2) = c("date", "hour", "type", 1:28)
saveRDS(s2, file = "~/AirQualityBeijing/data/PM2.5/pm2.5_2017.RDS")
s1 = beijing2018[ ,-c(27:33)] 
s2 = s1[which(s1$type=='PM2.5'), ]
colnames(s2) = c("date", "hour", "type", 1:28)
saveRDS(s2, file = "~/AirQualityBeijing/data/PM2.5/pm2.5_2018.RDS")
