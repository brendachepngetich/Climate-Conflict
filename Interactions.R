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

#create a list of climate factors
climate_lyrs <- list(CDD_sum,NDWS_sum,NT_sum,NWLD_sum,P95_sum,TR_sum)
climate_lyrs

#create variables for conflict data
ACTOR1 <- Database$conflict$ACTOR1_RICHNESS_rfsi
Actor2 <- Database$conflict$ACTOR2_RICHNESS_rfsi
Events <- Database$conflict$EVENTS_rfsi
Fatalities <- Database$conflict$FATALITIES_rfsi
Subtype <- Database$conflict$SUBTYPE_RICHNESS_rfsi

#create a list containing conflict layers
conflict_layers <- list(ACTOR1,Actor2,Events,Fatalities,Subtype)
conflict_layers

#Regression
#make linear model

#loop through all conflict layers
#rebuild conflict layers to have same extent as climate layers
rebuilt_layers <- list() #create an empty list
for (conf in conflict_layers)
{
  #obtain original name
  name <- names(conf)
  rebuilt_layer <- raster(vals=values(conf),ext=extent(CDD_Mean),crs=crs(CDD_Mean),
                          nrows=dim(conf)[1],ncols=dim(conf)[2])
  #resample to obtain similar resolution
  resampled <- resample(rebuilt_layer,CDD_Mean)
  stack()
  names(resampled) <- name
  rebuilt_layers<- c(rebuilt_layers,resampled)
}

#merge conflict list and climate list to have one list
layers <- c(climate_lyrs,rebuilt_layers)
layers
#stack all layers
stacked <- stack(layers)
stacked

#linear regression for climate versus conflict
actor1_model <- lm(ACTOR1_RICHNESS_rfsi ~ CDD + NDWS + NT + NWLD + P95 + TR, data=as.data.frame(stacked))
summary(actor1_model)
actor2_model <- lm(ACTOR2_RICHNESS_rfsi ~ CDD + NDWS + NT + NWLD + P95 + TR, data=as.data.frame(stacked))
summary(actor2_model)
print("lll")
EVENTS_rfsi <- lm(EVENTS_rfsi ~ CDD + NDWS + NT + NWLD + P95 + TR, data=as.data.frame(stacked))
summary(EVENTS_rfsi)
FATALITIES_rfsi <- lm(FATALITIES_rfsi ~ CDD + NDWS + NT + NWLD + P95 + TR, data=as.data.frame(stacked))
summary(FATALITIES_rfsi)
SUBTYPE_RICHNESS_rfsi <- lm(SUBTYPE_RICHNESS_rfsi ~ CDD + NDWS + NT + NWLD + P95 + TR, data=as.data.frame(stacked))
summary(SUBTYPE_RICHNESS_rfsi)
