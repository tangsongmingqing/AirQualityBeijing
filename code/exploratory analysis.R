# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

#libraries needed in this file
libraries = c("reshape2", "mclust", "andrews", "zoo", "cluster", "psych", "lattice")
lapply(libraries, library, quietly=TRUE, character.only=TRUE)

#############################seasonality analysis##############################
##################autocorrelation##################
#load data sets