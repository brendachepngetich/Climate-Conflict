#install packages
install.packages("ggplot2")
install.packages("rlist")
install.packages("raster")
#load required packages
library(raster)
library(ggplot2)
library(rlist)

#read data
Database <- readRDS("~/Documents/interview/Database.rds")

#view data object
Database

#Access individual raster
CDD <- Database$climate$CDD
NDWS <- Database$climate$NDWS
NT <- Database$climate$NTx35
NWLD <- Database$climate$NWLD
P95 <- Database$climate$P95
TR <- Database$climate$TR

#calculate mean for each climate factor
CDD_Mean <- calc(CDD, fun=mean)
NDWS_Mean <- calc(NDWS, fun=mean)
NT_Mean <- calc(NT, fun=mean)
NWLD_Mean <- calc(NWLD, fun=mean)
P95_Mean <- calc(P95, fun=mean)
TR_Mean <- calc(TR, fun=mean)

#plot consecutive dry days mean
plot(CDD_Mean, main="Mean Consecutive Dry Days", legend=TRUE, axes=TRUE)

#check if data follows normal distribution
hist(NDWS_Mean)