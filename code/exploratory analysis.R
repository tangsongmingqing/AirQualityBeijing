# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

#libraries needed in this file
libraries = c("reshape2", "mclust", "andrews", "zoo", "cluster", "psych", "lattice")
lapply(libraries, library, quietly=TRUE, character.only=TRUE)

#############################seasonality analysis##############################
##################autocorrelation##################
#load data sets
area2013 = readRDS("~/AirQualityBeijing/data/daily average/area2013.RDS")
area2014 = readRDS("~/AirQualityBeijing/data/daily average/area2014.RDS")
area2015 = readRDS("~/AirQualityBeijing/data/daily average/area2015.RDS")
area2016 = readRDS("~/AirQualityBeijing/data/daily average/area2016.RDS")
area2017 = readRDS("~/AirQualityBeijing/data/daily average/area2017.RDS")
allus = readRDS("~/AirQualityBeijing/data/daily average/allus.RDS")
allarea = rbind(as.matrix(area2013[ ,1:4]), as.matrix(area2014[ ,1:4]))
allarea = rbind(allarea, as.matrix(area2015[ ,1:4]))
allarea = rbind(allarea, as.matrix(area2016[ ,1:4]))
allarea = rbind(allarea, as.matrix(area2017[ ,1:4]))
allarea = as.data.frame(round(allarea, 3))
colnames(allarea) = c("date", "urban", "rural", "traffic")
Date = as.character(allarea$date)
allarea$date = as.Date(Date, '%Y%m%d')
df = data.frame(year = as.numeric(format(allarea$date, format = "%Y")),
                 month = as.numeric(format(allarea$date, format = "%m")),
                 day = as.numeric(format(allarea$date, format = "%d")))
alldaily = cbind(df, allarea[, 2:4])
x = melt(allus, measure.vars = c("2013", "2014", "2015", "2016", "2017"))
y = x[-c(1:339), c(3,1,2,4)]
colnames(y) = c("year", "month", "day", "us")
z = merge(alldaily, y, by = intersect(names(alldaily), names(y)), all = TRUE)
saveRDS(z, file = "~/AirQualityBeijing/data/daily average/allarea.RDS")
#calculate monthly average to see seasonal pattern
m = aggregate(allarea[ ,4:7],list(allarea$year, allarea$month), mean, na.rm=TRUE)
n = m[order(m$Group.1, m$Group.2), ]
saveRDS(n, file = "~/AirQualityBeijing/data/monthly average/allmonth.RDS")
#create autocorrelation coefficients
ac_table = as.data.frame(matrix(data= NA, ncol = 24, nrow = 4))
data = allmonth
path = "~/AirQualityBeijing/output/Autocorrelation/acf.pdf"
pdf(file = path)
for(i in 3:6){
  ac = acf(data[ ,i], lag.max = 24, na.action = na.pass, plot = FALSE)
  plot(ac, main = paste("Autocoefficient", colnames(data)[i], sep = " "))
  ac_table[i-2, ] = ac$acf[-1]
}
dev.off()
rownames(ac_table) = c("urban", "rural", "traffic", "us") # set rownames for output table
ac_table = round(ac_table, 4)              # round result within 4 digits
saveRDS(ac_table, file = "~/AirQualityBeijing/output/Autocorrelation/ac_table.RDS") # save table
#create monthly trend
allarea = readRDS("~/AirQualityBeijing/data/daily average/allarea.RDS")
Date = as.Date(paste(allarea$year, allarea$month, allarea$day, sep='-'))
allarea_Date = cbind(Date, allarea[ ,4:7])
saveRDS(allarea_Date, file = "~/AirQualityBeijing/data/daily average/allarea_Date.RDS")
ss = subset(allarea_Date, Date < as.Date("2017-7-1"))
path = "~/AirQualityBeijing/output/daily_mean.pdf"
pdf(file = path)
ggplot(ss)+
  geom_point(aes(x= Date, y= urban), size =0.5)+
  geom_line(aes(x =Date, y= 75, color = "red"), size=1)+
  theme(legend.position="none", axis.title=element_blank(), 
        panel.border = element_rect(fill = NA))+
  ggtitle("daily average of PM2.5 in urban area")+
  ylim(0, 500)
ggplot(ss)+
  geom_point(aes(x= Date, y= rural), size = 0.5)+
  geom_line(aes(x =Date, y= 75, color = "red"), size =1)+
  theme(legend.position="none", axis.title=element_blank(), 
        panel.border = element_rect(fill = NA))+
  ggtitle("daily average of PM2.5 in rural area")+
  ylim(0,500)
ggplot(ss)+
  geom_point(aes(x= Date, y= traffic), size = 0.5)+
  geom_line(aes(x =Date, y= 75, color = "red"), size = 1)+
  theme(legend.position="none", axis.title=element_blank(), 
        panel.border = element_rect(fill = NA))+
  ggtitle("daily average of PM2.5 in traffic intensive area")+
  ylim(0,500)
ggplot(ss)+
  geom_point(aes(x= Date, y= us), size = 0.5)+
  geom_line(aes(x =Date, y= 75, color = "red"), size = 1)+
  theme(legend.position="none", axis.title=element_blank(), 
        panel.border = element_rect(fill = NA))+
  ggtitle("daily average of PM2.5 in U.S. Embassy")+
  ylim(0,500)
dev.off()
##################calculate percentage of days that exceed the critical value#######
pu3 = sum(ss$urban[ss$Date<as.Date("2014-1-1")]>75, na.rm = TRUE)/sum(ss$Date<as.Date("2014-1-1"))
pu4 = sum(ss$urban[ss$Date<as.Date("2015-1-1")]>75, na.rm = TRUE)/sum(ss$Date<as.Date("2015-1-1"))
pu5 = sum(ss$urban[ss$Date<as.Date("2016-1-1")]>75, na.rm = TRUE)/sum(ss$Date<as.Date("2016-1-1"))
pu6 = sum(ss$urban[ss$Date<as.Date("2017-1-1")]>75, na.rm = TRUE)/sum(ss$Date<as.Date("2017-1-1"))
pu7 = sum(ss$urban[ss$Date<as.Date("2018-1-1")]>75, na.rm = TRUE)/sum(ss$Date<as.Date("2018-1-1"))
pr3 = sum(ss$rural[ss$Date<as.Date("2014-1-1")]>75, na.rm = TRUE)/sum(ss$Date<as.Date("2014-1-1"))
pr4 = sum(ss$rural[ss$Date<as.Date("2015-1-1")]>75, na.rm = TRUE)/sum(ss$Date<as.Date("2015-1-1"))
pr5 = sum(ss$rural[ss$Date<as.Date("2016-1-1")]>75, na.rm = TRUE)/sum(ss$Date<as.Date("2016-1-1"))
pr6 = sum(ss$rural[ss$Date<as.Date("2017-1-1")]>75, na.rm = TRUE)/sum(ss$Date<as.Date("2017-1-1"))
pr7 = sum(ss$rural[ss$Date<as.Date("2018-1-1")]>75, na.rm = TRUE)/sum(ss$Date<as.Date("2018-1-1"))
pt3 = sum(ss$traffic[ss$Date<as.Date("2014-1-1")]>75, na.rm = TRUE)/sum(ss$Date<as.Date("2014-1-1"))
pt4 = sum(ss$traffic[ss$Date<as.Date("2015-1-1")]>75, na.rm = TRUE)/sum(ss$Date<as.Date("2015-1-1"))
pt5 = sum(ss$traffic[ss$Date<as.Date("2016-1-1")]>75, na.rm = TRUE)/sum(ss$Date<as.Date("2016-1-1"))
pt6 = sum(ss$traffic[ss$Date<as.Date("2017-1-1")]>75, na.rm = TRUE)/sum(ss$Date<as.Date("2017-1-1"))
pt7 = sum(ss$traffic[ss$Date<as.Date("2018-1-1")]>75, na.rm = TRUE)/sum(ss$Date<as.Date("2018-1-1"))
pe3 = sum(ss$us[ss$Date<as.Date("2014-1-1")]>75, na.rm = TRUE)/sum(ss$Date<as.Date("2014-1-1"))
pe4 = sum(ss$us[ss$Date<as.Date("2015-1-1")]>75, na.rm = TRUE)/sum(ss$Date<as.Date("2015-1-1"))
pe5 = sum(ss$us[ss$Date<as.Date("2016-1-1")]>75, na.rm = TRUE)/sum(ss$Date<as.Date("2016-1-1"))
pe6 = sum(ss$us[ss$Date<as.Date("2017-1-1")]>75, na.rm = TRUE)/sum(ss$Date<as.Date("2017-1-1"))
pe7 = sum(ss$us[ss$Date<as.Date("2018-1-1")]>75, na.rm = TRUE)/sum(ss$Date<as.Date("2018-1-1"))
pu = round(c(pu3, pu4, pu5, pu6, pu7),3)
pr = round(c(pr3, pr4, pr5, pr6, pr7),3)
pt = round(c(pt3, pt4, pt5, pt6, pt7),3)
pe = round(c(pe3, pe4, pe5, pe6, pe7),3)
over_percent = data.frame(pu, pr, pt, pe)
colnames(over_percent)= c("urban", "rural", "traffic", "us")
row.names(over_percent)=c("2013", "2014", "2015", "2016", "2017")
write.csv(over_percent, file = "~/AirQualityBeijing/output/over_percent.CSV")



