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

##################values outside of interval of [0,500] are assigned with "NA"############         
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

#create missing value percentage plots
x = read_csv("napercent.csv")
y = read_csv("usnapercent.csv")
z = cbind.data.frame(c("us"), t(y))
colnames(z) = c("X1", "nap2013", "nap2014", "nap2015", "nap2016", "nap2017")
m = rbind.data.frame(x, z[2, ])
colnames(m) = c("station", "2013", "2014", "2015", "2016", "2017")
write.csv(m, file = "allNA_percentage.csv")
#bar plot of number of missing values
x = read_csv("output/missing value/allNA_percentage.csv")
m=t(x[ ,3:7])
path = "~/AirQualityBeijing/output/NApercentage.pdf"
pdf(file = path)
par(xpd=T, mar=par()$mar+c(0,0,0,4))
xx = barplot(t(x[,3:7]), ylab="Percentage of Missing Values", 
             cex.names=0.8, las=2,
             col=c("red","orange", "yellow","green","blue"))
box(bty="l")
axis(1, at = xx, labels = x$station, tick = F, las = 1, line = -1.4, cex.axis=0.41)
legend(locator(1), inset=c(0,1), xpd=TRUE,bty='n',
       legend = c("2013", "2014", "2015", "2016","2017"), 
       fill = c("red","orange", "yellow","green","blue"))
dev.off()

###################################################################################

#################################################################################
###########daily average#########################################################
b2013 = aggregate(pm2.5_2013[, 4:31], list(pm2.5_2013$date), mean, na.rm = TRUE)
b2014 = aggregate(pm2.5_2014[, 4:31], list(pm2.5_2014$date), mean, na.rm = TRUE)
b2015 = aggregate(pm2.5_2015[, 4:31], list(pm2.5_2015$date), mean, na.rm = TRUE)
b2016 = aggregate(pm2.5_2016[, 4:31], list(pm2.5_2016$date), mean, na.rm = TRUE)
b2017 = aggregate(pm2.5_2017[, 4:31], list(pm2.5_2017$date), mean, na.rm = TRUE)
saveRDS(b2013, file = "~/AirQualityBeijing/data/daily average/b2013.RDS")
saveRDS(b2014, file = "~/AirQualityBeijing/data/daily average/b2014.RDS")
saveRDS(b2015, file = "~/AirQualityBeijing/data/daily average/b2015.RDS")
saveRDS(b2016, file = "~/AirQualityBeijing/data/daily average/b2016.RDS")
saveRDS(b2017, file = "~/AirQualityBeijing/data/daily average/b2017.RDS")
u2013 = aggregate(us2013$Value, list(us2013$Month, us2013$Day), mean, na.rm = TRUE)
x = u2013[-c(1:4), ]
saveRDS(u2013, file = "~/AirQualityBeijing/data/daily average/u2013.RDS")
u2014 = aggregate(us2014$Value, list(us2014$Month, us2014$Day), mean, na.rm = TRUE)
saveRDS(u2014, file = "~/AirQualityBeijing/data/daily average/u2014.RDS")
u2015 = aggregate(us2015$Value, list(us2015$Month, us2015$Day), mean, na.rm = TRUE)
saveRDS(u2015, file = "~/AirQualityBeijing/data/daily average/u2015.RDS")
u2016 = aggregate(us2016$Value, list(us2016$Month, us2016$Day), mean, na.rm = TRUE)
saveRDS(u2016, file = "~/AirQualityBeijing/data/daily average/u2016.RDS")
u2017 = aggregate(us2017$Value, list(us2017$Month, us2017$Day), mean, na.rm = TRUE)
saveRDS(u2017, file = "~/AirQualityBeijing/data/daily average/u2017.RDS")
##########################yearly average###########################################
yu2013 = colMeans(u2013, na.rm = T)
yu2014 = colMeans(u2014, na.rm = T)
yu2015 = colMeans(u2015, na.rm = T)
yu2016 = colMeans(u2016, na.rm = T)
yu2017 = colMeans(u2017, na.rm = T)
yb2013 = colMeans(b2013, na.rm = T)
yb2014 = colMeans(b2014, na.rm = T)
yb2015 = colMeans(b2015, na.rm = T)
yb2016 = colMeans(b2016, na.rm = T)
yb2017 = colMeans(b2017, na.rm = T)

x = data.frame(yb2013, yb2014, yb2015, yb2016, yb2017)
y = round(t(x[2:29, ]),2)
rownames(y) = c("2013", "2014", "2015", "2016", "2017")

m = data.frame(yu2013, yu2014, yu2015, yu2016, yu2017)
n = round(t(x[3, ]),2)
rownames(n) = c("2013", "2014", "2015", "2016", "2017")
colnames(n) = c("us")
allyear = cbind(y, n)
saveRDS(allyear, file = "~/AirQualityBeijing/data/yearly average/allyear.RDS")

#generate plots for yearly mean for each pollutants
allyear = readRDS("~/AirQualityBeijing/data/yearly average/allyear.RDS")
data = as.data.frame(allyear)

path = "~/AirQualityBeijing/output/mean.pdf"
pdf(file = path)
ggplot(data = data, aes(x = 2013:2017)) +
  geom_line(aes( y=data$`1`) , size = 0.2)+
  geom_line(aes( y=data$`2`), size = 0.2)+
  geom_line(aes( y=data$`3`), size = 0.2)+
  geom_line(aes( y=data$`4`), size = 0.2)+
  geom_line(aes( y=data$`5`), size = 0.2)+
  geom_line(aes( y=data$`6`), size = 0.9)+
  geom_line(aes( y=data$`7`) , size = 0.2)+
  geom_line(aes( y=data$`8`), size = 0.2)+
  geom_line(aes( y=data$`10`), size = 0.2)+
  geom_line(aes( y=data$`11`), size = 0.2)+
  geom_line(aes( y=data$`12`), size = 0.2)+
  geom_line(aes( y=data$us), size = 0.9, linetype = "dotted")+
  geom_hline(yintercept = 35, col="red")+
  ggtitle("urban area")+
  ylim(30,150)+
  theme(axis.title=element_blank(),
        panel.background = element_rect(fill = "white", color = "grey"),
        panel.grid.major = element_line(colour = "grey", size = 0.2),
        plot.title = element_text(size = 25),
        axis.text = element_text(size=20))

ggplot(data = data, aes(x = 2013:2017)) +
  geom_line(aes( y=data$`13`), size = 0.2)+
  geom_line(aes( y=data$`14`), size = 0.2)+
  geom_line(aes( y=data$`15`), size = 0.2)+
  geom_line(aes( y=data$`17`), size = 0.2)+
  geom_line(aes( y=data$`18`), size = 0.2)+
  geom_line(aes( y=data$`20`), size = 0.2)+
  geom_line(aes( y=data$`21`), size = 0.2)+
  geom_line(aes( y=data$`22`), size = 0.2)+
  geom_line(aes( y=data$`23`), size = 0.2)+
  geom_hline(yintercept = 35, col="red")+
  ggtitle("rural area")+
  ylim(30,150)+
  theme(axis.title=element_blank(),
        panel.background = element_rect(fill = "white", color = "grey"),
        panel.grid.major = element_line(colour = "grey", size = 0.2),
        plot.title = element_text(size = 25),
        axis.text = element_text(size=20))

ggplot(data = data, aes(x = 2013:2017)) +
  geom_line(aes( y=data$`24`), size = 0.2)+
  geom_line(aes( y=data$`25`), size = 0.2)+
  geom_line(aes( y=data$`26`), size = 0.2)+
  geom_line(aes( y=data$`27`), size = 0.2)+
  geom_line(aes( y=data$`28`), size = 0.2)+
  geom_hline(yintercept = 35, col="red")+
  ggtitle("traffic intensive area")+
  ylim(30,150)+
  theme(axis.title=element_blank(),
        panel.background = element_rect(fill = "white", color = "grey"),
        panel.grid.major = element_line(colour = "grey", size = 0.2),
        plot.title = element_text(size = 25),
        axis.text = element_text(size=20))
dev.off()
#######################box-plot#################################
rm(list = ls(all = TRUE))
graphics.off()
u2013 = readRDS("~/AirQualityBeijing/data/daily average/u2013.RDS")
u2014 = readRDS("~/AirQualityBeijing/data/daily average/u2014.RDS")
u2015 = readRDS("~/AirQualityBeijing/data/daily average/u2015.RDS")
u2016 = readRDS("~/AirQualityBeijing/data/daily average/u2016.RDS")
u2017 = readRDS("~/AirQualityBeijing/data/daily average/u2017.RDS")
a = merge(u2013, u2014, by = c("Group.1", "Group.2"), all = TRUE)
b = merge(a, u2015, by = c("Group.1", "Group.2"), all = TRUE)
c = merge(b, u2016, by = c("Group.1", "Group.2"), all = TRUE)
allus = merge(c, u2017, by = c("Group.1", "Group.2"), all = TRUE)
colnames(allus) = c("month", "day", "2013", "2014", "2015", "2016", "2017")
allus = round(allus, 2)
saveRDS(allus, file = "~/AirQualityBeijing/data/daily average/allus.RDS")
#load data set for each measure station
allM174 = readRDS("~/luft_qualitaet/data/allM174.RDS")
allM42 = readRDS("~/luft_qualitaet/data/allM42.RDS")
#generate box plot for each pollutants
path = "~/luft_qualitaet/output/boxplot.pdf"
pdf(file = path)
xx = boxplot(b2013[ ,-1], col = c("purple", "orange", "yellow","green", "blue"), outline=FALSE, ylim=c(0,210), main="Air Quality of MC174 2008-2017")
yy = boxplot(allM42[, 2:6], col = c("purple", "orange", "yellow","green", "blue"), outline=FALSE, ylim=c(0,210), main="Air Quality of MC42 2008-2017")
dev.off()