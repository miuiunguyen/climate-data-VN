#----------------------------------------------------------------#
###### THIS FILE IS TO COMPUTE SPI BASE ON PRECIPITATION DATA ####
### Author: Mui Nguyen
### Created: 8th August
### Last modified: 
#----------------------------------------------------------------#
## Packages used ##
rm(list=ls())
#library(tidyverse)
#library(stringr)
library(dplyr)
library(data.table)
library(writexl)

## Set up working directory
setwd("/Users/nguyenmui/Library/CloudStorage/OneDrive-VictoriaUniversityofWellington-STAFF/2nd_paper/3. Analysis/Climate data/CHIRPS_to_SPI")
getwd()
mypath <- "/Users/nguyenmui/Library/CloudStorage/OneDrive-VictoriaUniversityofWellington-STAFF/2nd_paper/3. Analysis/Climate data/CHIRPS_to_SPI/raw_rainfall_subdistrict"

##Import & combine all csv files of precipitation
all_csv <- rbindlist(mapply(c, (
  #read file path
  list.files(path = mypath, pattern = "*.csv",full.names = TRUE) %>%
    #read all file contents
      lapply(read.table, header = TRUE, sep = ",", encoding = "UTF-8")), (
        list.files( path = mypath, pattern = "*.csv", full.names = TRUE) %>%
          #read all file names
          basename() %>%
          #combine all and unlist
          as.list()), SIMPLIFY = FALSE), fill = T)
# change column names
names(all_csv) [4] <- "File.path"
library(writexl)
write_xlsx(all_csv,"full_precipitation_test.xlsx")

##Computing SPI using new raw data after cleansing by excel
rm(list=ls())
library(SPEI)
library(readxl)
spi_data <- read_excel("full_precipitation_data.xlsx",1)

library(SCI)
library(docstring)
library(git2r)

assign_SPI <- function(df,
                       tscale = 12,
                       stationvar = "commune",
                       rainvar = "Rainfall")
{
  #' Computes the SPI (Standardised Precipitation Index) and adds it to a 
  #' data.frame. 
  #' 
  #' The SPI column is named (paste0("SPI", n)), where `n`
  #'  is the timescale in months at which SPI values are computed 
  #'
  #' @param df A data.frame containing monthly rainfall data with a unique
  #' rainfall value for each month at each unique instance of `stationvar`
  #' @param tscale The (integer) time scale in months at which the SPI
  #' values are to calculated
  #' @param stationvar A character string containing the variable name used
  #' to distinguish locations from one another
  #' @param rainvar A character string containing the variable name in which
  #' monthly rainfall data are stored
  #'
  #' @return A data.frame with the SPI data added in a new column
  
  for (S in unique(pull(df, stationvar)))
  {
    rain <- pull(df[df[stationvar] == S, ], rainvar)
    mon1 <- pull(df[df[stationvar] == S, ], "Month")[1]
    SPIfit <- fitSCI(
      rain,
      first.mon = mon1,
      time.scale = tscale,
      distr = "gamma",
      p0 = TRUE,
      scaling = "sd"
    )
    SPI <- transformSCI(rain, first.mon = mon1, obj = SPIfit)
    df[df[stationvar] == S, paste0("SPI", tscale)] <- SPI
  }
  return(df)
}
a<-assign_SPI(spi_data)
library(xlsx)
write_xlsx(a,"test2.xlsx")

data(wichita) 
library(SPEI)
library(tidyverse)
#calculate 6-month SPI
spi_data %>% 
  filter(spi_data$File.path == "Rainfall_01.csv") %>% 
    plot(spi(ts(spi_data$Rainfall,freq=12,start=c(1981,1)),scale = 6))

plot(spi(ts(wichita$PRCP,freq=12,start=c(1980,1)),scale = 6))

test <- filter(spi_data, File.path=="Rainfall_100.csv")
library(SPEI)
SPI6 <- spi(test$Rainfall, scale = 6, distribution = 'Gamma')
SPI6
plot(SPI6)

hatinh <- filter(full_climate_data, prov == "Ha Tinh")
hue <- filter(full_climate_data, prov == "Thua Thien Hue")
spi3_dl <- spi(daklak$prcp,3)
spi3_ht <- spi(hatinh$prcp,3)
spi3_hue <- spi(hue$prcp,3)
spi <- spi(full_climate_data$prcp,3)
spi6_dl <- spi(daklak$prcp,6)
spi6_ht <- spi(hatinh$prcp,6)
spi6_hue <- spi(hue$prcp,6)

