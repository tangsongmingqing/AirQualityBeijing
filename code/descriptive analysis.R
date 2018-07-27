# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

#libraries needed in this file
libraries = c("ggplot2", "Amelia", "reshape2", "psych", "kimisc")
lapply(libraries, library, quietly=TRUE, character.only=TRUE)

#data quality
####################missing values##############################################
#count missing values for data from BJMEMC
pm2.5_2013 = readRDS("~/AirQualityBeijing/data/PM2.5/pm2.5_2013.RDS")#load data sets
na2013     = rep(NA, 28)
for (i in 4:31){
  nacount =     sum(is.na(pm2.5_2013[ ,i]))
  na2013[i-3] = nacount
}
#repeat this step for year 2014-2017
pm2.5_2014 = readRDS("~/AirQualityBeijing/data/PM2.5/pm2.5_2014.RDS")#load data sets
na2014     = rep(NA, 28)
for (i in 4:31){
  nacount =     sum(is.na(pm2.5_2014[ ,i]))
  na2014[i-3] = nacount
}
pm2.5_2015 = readRDS("~/AirQualityBeijing/data/PM2.5/pm2.5_2015.RDS")#load data sets
na2015     = rep(NA, 28)
for (i in 4:31){
  nacount =     sum(is.na(pm2.5_2015[ ,i]))
  na2015[i-3] = nacount
}
pm2.5_2016 = readRDS("~/AirQualityBeijing/data/PM2.5/pm2.5_2016.RDS")#load data sets
na2016     = rep(NA, 28)
for (i in 4:31){
  nacount =     sum(is.na(pm2.5_2016[ ,i]))
  na2016[i-3] = nacount
}
pm2.5_2017 = readRDS("~/AirQualityBeijing/data/PM2.5/pm2.5_2017.RDS")#load data sets
na2017     = rep(NA, 28)
for (i in 4:31){
  nacount =     sum(is.na(pm2.5_2017[ ,i]))
  na2017[i-3] = nacount
}
#joint table of missing value counts of each year and each station
natable = cbind.data.frame(na2013, na2014, na2015, na2016, na2017)
#save this table as output
setwd("~/AirQualityBeijing/output/missing value")
write.csv(natable, file = "nacount.csv")
#calculate percentage of missing values
nap2013 = rep(NA, 28)
for (i in 4:31){
  na_percent =  round(na2013[i-3]/nrow(pm2.5_2013)*100, 4)
  nap2013[i-3] = na_percent
}
nap2014 = rep(NA, 28)
for (i in 4:31){
  na_percent =  round(na2014[i-3]/nrow(pm2.5_2014)*100, 4)
  nap2014[i-3] = na_percent
}
nap2015 = rep(NA, 28)
for (i in 4:31){
  na_percent =  round(na2015[i-3]/nrow(pm2.5_2015)*100, 4)
  nap2015[i-3] = na_percent
}
nap2016 = rep(NA, 28)
for (i in 4:31){
  na_percent =  round(na2016[i-3]/nrow(pm2.5_2016)*100, 4)
  nap2016[i-3] = na_percent
}
nap2017 = rep(NA, 28)
for (i in 4:31){
  na_percent =  round(na2017[i-3]/nrow(pm2.5_2017)*100, 4)
  nap2017[i-3] = na_percent
}
napercent = cbind.data.frame(nap2013, nap2014, nap2015, nap2016, nap2017)
write.csv(napercent, file = "napercent.csv")

#missing values for data from U.S. embassy
usPM2.52017 = readRDS("~/AirQualityBeijing/data/usPM2.5/usPM2.52017.RDS")
usPM2.52016 = readRDS("~/AirQualityBeijing/data/usPM2.5/usPM2.52016.RDS")
usPM2.52015 = readRDS("~/AirQualityBeijing/data/usPM2.5/usPM2.52015.RDS")
usPM2.52014 = readRDS("~/AirQualityBeijing/data/usPM2.5/usPM2.52014.RDS")
usPM2.52013 = readRDS("~/AirQualityBeijing/data/usPM2.5/usPM2.52013.RDS")

usnacount = c(sum(nin.interval.lo(usPM2.52013$Value, 0, 500)), 
            sum(nin.interval.lo(usPM2.52014$Value, 0, 500)),
            sum(nin.interval.lo(usPM2.52015$Value, 0, 500)),
            sum(nin.interval.lo(usPM2.52016$Value, 0, 500)),
            sum(nin.interval.lo(usPM2.52017$Value, 0, 500)))
usnrow = c(nrow(usPM2.52013),nrow(usPM2.52014),nrow(usPM2.52015),nrow(usPM2.52016),nrow(usPM2.52017))
usna_percent = rep(NA, 5)
for(i in 1:5){
  usna_percent[i] = round(usnacount[i]/usnrow[i]*100, 4)
}
write.csv(usnacount, file = "usnacount.csv")
write.csv(usna_percent, file = "usnapercent.csv")


usna_percent = usnacount            
us2013 = usPM2.52013
us2013$Value[us2013$Value < 0 | us2013$Value > 500] <- NA
saveRDS(us2013, file = "~/AirQualityBeijing/data/usPM2.5NA/us2013.RDS")
us2014 = usPM2.52014
us2014$Value[us2014$Value < 0 | us2014$Value > 500] <- NA
saveRDS(us2014, file = "~/AirQualityBeijing/data/usPM2.5NA/us2014.RDS")
us2015 = usPM2.52015
us2015$Value[us2015$Value < 0 | us2015$Value > 500] <- NA
saveRDS(us2015, file = "~/AirQualityBeijing/data/usPM2.5NA/us2015.RDS")
us2016 = usPM2.52016
us2016$Value[us2016$Value < 0 | us2016$Value > 500] <- NA
saveRDS(us2016, file = "~/AirQualityBeijing/data/usPM2.5NA/us2016.RDS")
us2017 = usPM2.52017
us2017$Value[us2017$Value < 0 | us2017$Value > 500] <- NA
saveRDS(us2017, file = "~/AirQualityBeijing/data/usPM2.5NA/us2017.RDS")






naplot = as.data.frame(nalist)
naplot = cbind(c(rep("MC174",5), rep("MC42",5)), naplot)
naplot = cbind(rep(c("NO2","NOx","PM10","Benzol","Toluol"),2), naplot)
colnames(naplot) = c("station", "Pollutant", "Value")
#missingness map
missmap(allstation[, 2:11])
#bar plot of number of missing values
path = "~/luft_qualitaet/output/NAcount.pdf"
pdf(file = path)
xx = barplot(naplot$Value, beside=T, ylab="Number of Missing Values", 
             cex.names=0.8, las=2, ylim=c(0,350), col=c("darkblue","red"))
box(bty="l")
text(x=xx, y=naplot$Value, label=naplot$Value, pos = 3, cex = 0.8, col = "red")
axis(1, at = xx, labels = colnames(allstation)[-1], tick = F, las = 2, line = -0.5, cex.axis=0.8)
dev.off()
#percentage of missing values
pernaplot = naplot
pernaplot$Value = round(pernaplot$Value/3653*100, 4)
path = "~/luft_qualitaet/output/NApercentage.pdf"
pdf(file = path)
xx = barplot(pernaplot$Value, beside=T, ylab="Percentage of Missing Values", 
             cex.names=0.8, las=2, ylim=c(0,10), col=c("darkblue","red"))
box(bty="l")
text(x=xx, y=pernaplot$Value, label=paste(pernaplot$Value,"%"), pos = 3, cex = 0.8, col = "red")
axis(1, at = xx, labels = colnames(allstation)[-1], tick = F, las = 2, line = -0.5, cex.axis=0.8)
dev.off()