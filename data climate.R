rm(list=ls())
# ------------- #
# PACKAGED USED #
# ------------- #
library(haven)
library(dplyr)
library(tidyr)
library(tibble)
library(writexl)
library(SPEI)
library(readxl)

#-----------------------------------------------------------#
###### COMBINING ALL SINGLE CSV DATA TO ONE CSV FILE ########
#___________________________________________________________#

vietnam <- read.csv("/Users/nguyenmui/Desktop/2912973.csv")
temp <- vietnam %>% group_by(STATION,NAME) %>% 
 summarise_each(funs(mean, max, min, sd, n()), TAVG) %>% arrange(desc(mean))

##Combine all csv file of weather
multmerge = function(mypath){
 filenames=list.files(path="/Volumes/STUDY/STUDY/Data/climate vietnam", full.names=TRUE)
 datalist = lapply(filenames, function(x){read.csv(file=x,header=T)})
 Reduce(function(x,y) {merge(x,y,all = TRUE)}, datalist)
}
full_data = multmerge("path_name for your csv folder")
write_xlsx(full_data,"/Volumes/STUDY/STUDY/Data/full_climate_data.xlsx")

#----------------------------#
#### COMPUTE SPI DATA #######
#----------------------------#
##Start with new raw data to create SPI

full_climate_data <- read_excel("/Volumes/STUDY/STUDY/Data/full_climate_data.xlsx")
daklak <- filter(full_climate_data, prov == "Dak Lak")
hatinh <- filter(full_climate_data, prov == "Ha Tinh")
hue <- filter(full_climate_data, prov == "Thua Thien Hue")
spi3_dl <- spi(daklak$prcp,3)
spi3_ht <- spi(hatinh$prcp,3)
spi3_hue <- spi(hue$prcp,3)
spi <- spi(full_climate_data$prcp,3)
spi6_dl <- spi(daklak$prcp,6)
spi6_ht <- spi(hatinh$prcp,6)
spi6_hue <- spi(hue$prcp,6)
#heello
#iam testing git on macos
