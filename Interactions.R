#The following code can be found in this github repository
"**https://github.com/brendachepngetich/Climate-Conflict.git**"

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

#Question two
#create socioeconomic variables
access <- Database$socioeco$acess
production <- Database$socioeco$ub_npp
piped_water <-Database$socioeco$medn_piped_water
wealth <- Database$socioeco$KEN_rwi
underweight <- Database$socioeco$medn_underweight
education <- Database$socioeco$medn_difference_edu

#list socioeconomic layers
economic <- list(access,production,piped_water,wealth,underweight,education)
economic

#loop through all socioeconomic layers
#rebuild socioeconomic layers to have same extent as climate layers
socio_layers <- list()
for (econ in economic)
{
  #obtain original name
  name <- names(econ)
  rebuilt_layer <- raster(vals=values(econ),ext=extent(CDD_Mean),crs=crs(CDD_Mean),
                          nrows=dim(econ)[1],ncols=dim(econ)[2])
  #resample to obtain similar resolution
  resampled <- resample(rebuilt_layer,CDD_Mean)
  stack()
  names(resampled) <- name
  socio_layers<- c(socio_layers,resampled)
}
socio_layers

#merge socioeconomic list and climate list to have one list
climate_socio <- c(climate_lyrs,socio_layers)
climate_socio
#stack all layers
stacked_sc <- stack(climate_socio)
stacked_sc

#linear regression for climate versus socioeconomic
access_model <- lm(acess ~ CDD + NDWS + NT + NWLD + P95 + TR, data=as.data.frame(stacked_sc))
summary(access_model)
production_model <- lm(ub_npp ~ CDD + NDWS + NT + NWLD + P95 + TR, data=as.data.frame(stacked_sc))
summary(production_model)
piped_model <- lm(medn_piped_water ~ CDD + NDWS + NT + NWLD + P95 + TR, data=as.data.frame(stacked_sc))
summary(piped_model)
wealth_model <- lm(KEN_rwi ~ CDD + NDWS + NT + NWLD + P95 + TR, data=as.data.frame(stacked_sc))
summary(wealth_model)
underweight <- lm(medn_underweight ~ CDD + NDWS + NT + NWLD + P95 + TR, data=as.data.frame(stacked_sc))
summary(underweight)
education <- lm(medn_difference_edu ~ CDD + NDWS + NT + NWLD + P95 + TR, data=as.data.frame(stacked_sc))
summary(education)

#linear regression for socioeconomic versus conflict
#merge socioeconomic and conflict raster layers
conflict_socio <- c(rebuilt_layers,socio_layers)
conflict_socio
conflict_stack <- stack(conflict_socio)
conflict_stack
actor1 <- lm(ACTOR1_RICHNESS_rfsi ~ acess + ub_npp + medn_piped_water + KEN_rwi + medn_underweight + medn_difference_edu, data=as.data.frame(conflict_stack))
summary(actor1)
actor2 <- lm(ACTOR2_RICHNESS_rfsi ~ acess + ub_npp + medn_piped_water + KEN_rwi + medn_underweight + medn_difference_edu, data=as.data.frame(conflict_stack))
summary(actor2)
events <- lm(EVENTS_rfsi ~ acess + ub_npp + medn_piped_water + KEN_rwi + medn_underweight + medn_difference_edu, data=as.data.frame(conflict_stack))
summary(events)
fatalities <- lm(FATALITIES_rfsi ~ acess + ub_npp + medn_piped_water + KEN_rwi + medn_underweight + medn_difference_edu, data=as.data.frame(conflict_stack))
summary(fatalities)
subtype <- lm(SUBTYPE_RICHNESS_rfsi ~ acess + ub_npp + medn_piped_water + KEN_rwi + medn_underweight + medn_difference_edu, data=as.data.frame(conflict_stack))
summary(subtype)

#question3
#identify the most vulnerable areas using overlay analysis
#raster stack
econ <- stack(socio_layers)
econ
#overlay all socioeconomic layers using sum function
econ_overlay <- overlay(econ, fun=sum)
econ_overlay
plot(econ_overlay, main="Socio economic Vulnerability", legend=TRUE, axes=TRUE)
