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

#Access individual raster and calculate sum
CDD <- Database$climate$CDD
NDWS <- Database$climate$NDWS
NT <- Database$climate$NTx35
NWLD <- Database$climate$NWLD
P95 <- Database$climate$P95
TR <- Database$climate$TR

#calculate sum for each climate factor
CDD_sum <- calc(CDD, fun=sum)
NDWS_sum <- calc(NDWS, fun=sum)
NT_sum <- calc(NT, fun=sum)
NWLD_sum <- calc(NWLD, fun=sum)
P95_sum <- calc(P95, fun=sum)
TR_sum <- calc(TR, fun=sum)

#assign names to sum layers
names(CDD_sum) <- "CDD"
names(NDWS_sum) <- "NDWS"
names(NT_sum) <- "NT"
names(NWLD_sum) <- "NWLD"
names(P95_sum) <- "P95"
names(TR_sum) <- "TR"


#plot consecutive dry days sum
plot(CDD_Mean, main="Mean Consecutive Dry Days", legend=TRUE, axes=TRUE)

#check if data follows normal distribution
hist(CDD_sum)
hist(NT_sum)
hist(NDWS_sum)
hist(NWLD_sum)
hist(P95_sum)
hist(TR_sum)
