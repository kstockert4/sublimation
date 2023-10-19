#NIWR Sublimation Project 
setwd("E:/SublimationR/ECdata") #choose your own if reviewing this code with .csv files on github 

# Load the required packages
warning=FALSE 
library(data.table)
library(bit64)
library(plyr)
library(ggplot2)
library(lubridate)
library(RCurl)
library(forecast)
library(dplyr)
library(naniar)
library(lfstat)
library(ggstatsplot)
library(hrbrthemes)
library(viridis)
library(patchwork)
library(scales)
remotes::install_github("USGS-R/smwrData")
remotes::install_github("USGS-R/smwrBase")
library("gridExtra")
library(ggridges)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(rticles)
library(rmarkdown)

#===============================================================================
#IMPORT AND FORMAT IMNAVAIT CREEK DATA==========================================
#===============================================================================

#import all Imnavait sites all data and convert to data table
Fen <- setDT(read.csv("ALL_IC_1523_gapfilled_20211231.csv", na.strings = -9999)) 
Tussock <- setDT(read.csv("ALL_IC_1993_gapfilled_20211231.csv", na.strings = -9999)) 
Ridge <- setDT(read.csv("ALL_IC_1991_gapfilled_20211231.csv", na.strings = -9999)) 


#CREATE AND FORMAT A DATE COLUMN 
#CONVERT "TIMESTAMPEND" TO DATE 
Fen$Date <- as.Date(as.character(Fen$TIMESTAMP_END), format = "%Y%m%d%H%M")
Tussock$Date <- as.Date(as.character(Tussock$TIMESTAMP_END), format = "%Y%m%d%H%M")
Ridge$Date <- as.Date(as.character(Ridge$TIMESTAMP_END), format = "%Y%m%d%H%M")

#water year function from smwrData and smwrBase (USGS GitHub, remote calls in package lines). 
library(smwrData)
data(QW05078470)
#'## Return an ordered factor
#waterYear(QW05078470$DATES)
waterYear <- function(x, numeric=FALSE) {
  x <- as.POSIXlt(x)
  yr <- x$year + 1900L
  mn <- x$mon + 1L
  ## adjust for water year
  yr <- yr + ifelse(mn < 10L, 0L, 1L)
  if(numeric)
    return(yr)
  ordered(yr)
}
Fen$WaterYear <- waterYear(Fen$Date, numeric = FALSE)
Tussock$WaterYear <- waterYear(Tussock$Date, numeric = FALSE)
Ridge$WaterYear <- waterYear(Ridge$Date, numeric = FALSE)

#selecting only columns that are needed for analysis 
Fen <- select(Fen, Year, DoY, Hour, Date, WaterYear, LE_F, TA, P, D_SNOW, WS, RH, VPD_F, NETRAD, TS_1_1_1, TS_2_1_1)
Ridge <- select(Ridge, Year, DoY, Hour, Date, WaterYear, LE_F,  TA, P, D_SNOW, WS, RH, VPD_F, NETRAD, TS_1_1_1, TS_2_1_1)
Tussock <- select(Tussock, Year, DoY, Hour, Date, WaterYear, LE_F,  TA, P, WS, RH, VPD_F, NETRAD, TS_1_1_1, TS_2_1_1)


#rename columns for simplicity and convert from character to numeric data 
# the pattern is new_col_name = old_col_name.....................................
attach(Fen)
Fen <- data.frame(Year = as.numeric(Year), 
                  DoY = as.numeric(DoY),
                  Hour = as.numeric(Hour), 
                  Date = Date,
                  WaterYear = WaterYear,
                  LE = as.numeric(LE_F),
                  AirTemp = as.numeric(TA),
                  Precip = as.numeric(P),
                  SnowD = as.numeric(D_SNOW), 
                  WindSpeed = as.numeric(WS),
                  RH = as.numeric(RH),
                  VPD = as.numeric(VPD_F),
                  NetRadiation = as.numeric(NETRAD), 
                  SoilTemp = as.numeric(TS_1_1_1))
Fen <- Fen %>%
  mutate(month = month(Date, label = TRUE))
detach(Fen)

attach(Tussock)
Tussock <- data.frame(Year = as.numeric(Year), 
                      DoY = as.numeric(DoY),
                      Hour = as.numeric(Hour), 
                      Date = Date,
                      WaterYear = WaterYear,
                      LE = as.numeric(LE_F),
                      AirTemp = as.numeric(TA),
                      Precip = as.numeric(P),
                      WindSpeed = as.numeric(WS),
                      RH = as.numeric(RH),
                      VPD = as.numeric(VPD_F),
                      NetRadiation = as.numeric(NETRAD), 
                      SoilTemp = as.numeric(TS_1_1_1))
Tussock <- Tussock %>%
  mutate(month = month(Date, label = TRUE))
detach(Tussock)

attach(Ridge)
Ridge <- data.frame(Year = as.numeric(Year), 
                    DoY = as.numeric(DoY),
                    Hour = as.numeric(Hour), 
                    Date = Date,
                    WaterYear = WaterYear,
                    LE = as.numeric(LE_F),
                    AirTemp = as.numeric(TA),
                    Precip = as.numeric(P),
                    SnowD = as.numeric(D_SNOW),
                    WindSpeed = as.numeric(WS),
                    RH = as.numeric(RH),
                    VPD = as.numeric(VPD_F),
                    NetRadiation = as.numeric(NETRAD), 
                    SoilTemp = as.numeric(TS_1_1_1))
Ridge <- Ridge %>%
  mutate(month = month(Date, label = TRUE))
detach(Ridge)


#===============================================================================
#IMPORT AND FORMAT BONANZA CREEK DATA===========================================
#===============================================================================

#import all Bonanza Creek sites and convert to data table
#import black spruce
BS2010 <- setDT(read.csv("YF_2472_2010_v20211231.csv"))
BS2010 <- BS2010[,-1]
BS2011 <- setDT(read.csv("YF_2472_2011_v20211231.csv"))
BS2011 <- BS2011[,-1]
BS2012 <- setDT(read.csv("YF_2472_2012_v20211231.csv"))
BS2012 <- BS2012[,-1]
BS2013 <- setDT(read.csv("2013_YF_2472_gapfilled_DT_20131231.csv"))
BS2014 <- setDT(read.csv("2014_YF_2472_gapfilled_DT_20141231.csv")) 
BS2015 <- setDT(read.csv("2015_YF_2472_gapfilled_20151231.csv")) 
BS2016 <- setDT(read.csv("2016_YF_2472_gapfilled_20161231.csv")) 
BS2017 <- setDT(read.csv("2017_YF_2472_gapfilled_20171231.csv")) 
BS2018 <- setDT(read.csv("2018_YF_2472_gapfilled_20181231.csv")) 
BS2019 <- setDT(read.csv("2019_YF_2472_gapfilled_20191231.csv")) 
BS2020 <- setDT(read.csv("2020_YF_2472_gapfilled_20201231.csv")) 
BS2021 <- setDT(read.csv("2021_YF_2472_gapfilled_20211231.csv")) 

#import bog sites 
bog2010 <- setDT(read.csv("2010_BC_5166_gapfilled_DT.csv")) 
bog2011 <- setDT(read.csv("2011_BC_5166_gapfilled_DT.csv")) 
bog2012 <- setDT(read.csv("2012_BC_5166_gapfilled_DT.csv")) 
bog2013 <- setDT(read.csv("2013_BC_5166_gapfilled_DT_20131231.csv")) 
bog2014 <- setDT(read.csv("2014_BC_5166_gapfilled_20141231.csv")) 
bog2015 <- setDT(read.csv("2015_BC_5166_gapfilled_20151231.csv")) 
bog2016 <- setDT(read.csv("2016_BC_5166_gapfilled_20161231.csv")) 
bog2017 <- setDT(read.csv("2017_BC_5166_gapfilled_20171231.csv")) 
bog2018 <- setDT(read.csv("2018_BC_5166_gapfilled_20181231.csv")) 
bog2019 <- setDT(read.csv("2019_BC_5166_gapfilled_20191231.csv")) 
bog2020 <- setDT(read.csv("2020_BC_5166_gapfilled_20201231.csv")) 
bog2021 <- setDT(read.csv("2021_BC_5166_gapfilled_20211231.csv")) 

#import fen sites  
fenBC2011 <- setDT(read.csv("2011_BC_FEN_gapfilled_DT.csv")) 
fenBC2012 <- setDT(read.csv("2012_BC_FEN_gapfilled_DT.csv")) 
fenBC2013 <- setDT(read.csv("2013_BC_FEN_gapfilled_DT_20131231.csv")) 
fenBC2014 <- setDT(read.csv("2014_BC_FEN_gapfilled_20141231.csv")) 
fenBC2015 <- setDT(read.csv("2015_BC_FEN_gapfilled_20151231.csv")) 
fenBC2016 <- setDT(read.csv("2016_BC_FEN_gapfilled_20161231.csv")) 
fenBC2017 <- setDT(read.csv("2017_BC_FEN_gapfilled_20171231.csv")) 
fenBC2018 <- setDT(read.csv("2018_BC_FEN_gapfilled_20181231.csv")) 
fenBC2019 <- setDT(read.csv("2019_BC_FEN_gapfilled_20191231.csv")) 
fenBC2020 <- setDT(read.csv("2020_BC_FEN_gapfilled_20201231.csv"))
fenBC2021 <- setDT(read.csv("2021_BC_FEN_gapfilled_20211231.csv")) 

#create and format date column ................................................

fenBC2011$Date <- as.Date(as.numeric(fenBC2011$DoY), origin = "2010-12-31")
fenBC2012$Date <- as.Date(as.numeric(fenBC2012$DoY), origin = "2011-12-31")
fenBC2013$Date <- as.Date(as.numeric(fenBC2013$DoY), origin = "2012-12-31")
fenBC2014$Date <- as.Date(as.numeric(fenBC2014$DoY), origin = "2013-12-31")
fenBC2015$Date <- as.Date(as.numeric(fenBC2015$DoY), origin = "2014-12-31")
fenBC2016$Date <- as.Date(as.numeric(fenBC2016$DoY), origin = "2015-12-31")
fenBC2017$Date <- as.Date(as.numeric(fenBC2017$DoY), origin = "2016-12-31")
fenBC2018$Date <- as.Date(as.numeric(fenBC2018$DoY), origin = "2017-12-31")
fenBC2019$Date <- as.Date(as.numeric(fenBC2019$DoY), origin = "2018-12-31")
fenBC2020$Date <- as.Date(as.numeric(fenBC2020$DoY), origin = "2019-12-31")
fenBC2021$Date <- as.Date(as.numeric(fenBC2021$DoY), origin = "2020-12-31")

bog2010$Date <- as.Date(as.numeric(bog2010$DoY), origin = "2009-12-31")
bog2011$Date <- as.Date(as.numeric(bog2011$DoY), origin = "2010-12-31")
bog2012$Date <- as.Date(as.numeric(bog2012$DoY), origin = "2011-12-31")
bog2013$Date <- as.Date(as.numeric(bog2013$DoY), origin = "2012-12-31")
bog2014$Date <- as.Date(as.numeric(bog2014$DoY), origin = "2013-12-31")
bog2015$Date <- as.Date(as.numeric(bog2015$DoY), origin = "2014-12-31")
bog2016$Date <- as.Date(as.numeric(bog2016$DoY), origin = "2015-12-31")
bog2017$Date <- as.Date(as.numeric(bog2017$DoY), origin = "2016-12-31")
bog2018$Date <- as.Date(as.numeric(bog2018$DoY), origin = "2017-12-31")
bog2019$Date <- as.Date(as.numeric(bog2019$DoY), origin = "2018-12-31")
bog2020$Date <- as.Date(as.numeric(bog2020$DoY), origin = "2019-12-31")
bog2021$Date <- as.Date(as.numeric(bog2021$DoY), origin = "2020-12-31")

BS2010$Date <- as.Date(as.numeric(BS2010$DoY), origin = "2009-12-31")
BS2011$Date <- as.Date(as.numeric(BS2011$DoY), origin = "2010-12-31")
BS2012$Date <- as.Date(as.numeric(BS2012$DoY), origin = "2011-12-31")
BS2013$Date <- as.Date(as.numeric(BS2013$DoY), origin = "2012-12-31")
BS2014$Date <- as.Date(as.numeric(BS2014$DoY), origin = "2013-12-31")
BS2015$Date <- as.Date(as.numeric(BS2015$DoY), origin = "2014-12-31")
BS2016$Date <- as.Date(as.numeric(BS2016$DoY), origin = "2015-12-31")
BS2017$Date <- as.Date(as.numeric(BS2017$DoY), origin = "2016-12-31")
BS2018$Date <- as.Date(as.numeric(BS2018$DoY), origin = "2017-12-31")
BS2019$Date <- as.Date(as.numeric(BS2019$DoY), origin = "2018-12-31")
BS2020$Date <- as.Date(as.numeric(BS2020$DoY), origin = "2019-12-31")
BS2021$Date <- as.Date(as.numeric(BS2021$DoY), origin = "2020-12-31")

#--------------------------------------------------
# All years combined into 1 DF per site using rbind 
#-9999 replaced with NA 
#FEN
FenBC <- data.table(Site="Fen", fenBC2011[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))])
FenBC <- rbind(FenBC, data.table(Site="Fen", fenBC2012[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))]), fill=TRUE, use.names = TRUE)
FenBC <- rbind(FenBC, data.table(Site="Fen", fenBC2013[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))]), fill=TRUE, use.names = TRUE)
FenBC <- rbind(FenBC, data.table(Site="Fen", fenBC2014[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))]), fill=TRUE, use.names = TRUE)
FenBC <- rbind(FenBC, data.table(Site="Fen", fenBC2015[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))]), fill=TRUE, use.names = TRUE)
FenBC <- rbind(FenBC, data.table(Site="Fen", fenBC2016[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))]), fill=TRUE, use.names = TRUE)
FenBC <- rbind(FenBC, data.table(Site="Fen", fenBC2017[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))]), fill=TRUE, use.names = TRUE)
FenBC <- rbind(FenBC, data.table(Site="Fen", fenBC2018[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))]), fill=TRUE, use.names = TRUE)
FenBC <- rbind(FenBC, data.table(Site="Fen", fenBC2019[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))]), fill=TRUE, use.names = TRUE)
FenBC <- rbind(FenBC, data.table(Site="Fen", fenBC2020[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))]), fill=TRUE, use.names = TRUE)
FenBC <- rbind(FenBC, data.table(Site="Fen", fenBC2021[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))]), fill=TRUE, use.names = TRUE)

#water year function from smwrData and smwrBase (USGS GitHub, remote calls in package lines). 
library(smwrData)
data(QW05078470)
#'## Return an ordered factor
#waterYear(QW05078470$DATES)
waterYear <- function(x, numeric=FALSE) {
  x <- as.POSIXlt(x)
  yr <- x$year + 1900L
  mn <- x$mon + 1L
  ## adjust for water year
  yr <- yr + ifelse(mn < 10L, 0L, 1L)
  if(numeric)
    return(yr)
  ordered(yr)
}
FenBC$WaterYear <- waterYear(FenBC$Date, numeric = FALSE)

#BOG

bog <- data.table(Site="bog", bog2010[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))])
bog <- rbind(bog, data.table(Site="bog", bog2011[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))]), fill=TRUE, use.names = TRUE)
bog <- rbind(bog, data.table(Site="bog", bog2012[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))]), fill=TRUE, use.names = TRUE)
bog <- rbind(bog, data.table(Site="bog", bog2013[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))]), fill=TRUE, use.names = TRUE)
bog <- rbind(bog, data.table(Site="bog", bog2014[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))]), fill=TRUE, use.names = TRUE)
bog <- rbind(bog, data.table(Site="bog", bog2015[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))]), fill=TRUE, use.names = TRUE)
bog <- rbind(bog, data.table(Site="bog", bog2016[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))]), fill=TRUE, use.names = TRUE)
bog <- rbind(bog, data.table(Site="bog", bog2017[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))]), fill=TRUE, use.names = TRUE)
bog <- rbind(bog, data.table(Site="bog", bog2018[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))]), fill=TRUE, use.names = TRUE)
bog <- rbind(bog, data.table(Site="bog", bog2019[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))]), fill=TRUE, use.names = TRUE)
bog <- rbind(bog, data.table(Site="bog", bog2020[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))]), fill=TRUE, use.names = TRUE)
bog <- rbind(bog, data.table(Site="bog", bog2021[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))]), fill=TRUE, use.names = TRUE)
bog$WaterYear <- waterYear(bog$Date, numeric = FALSE)

#BLACK SPRUCE
BS <- data.table(Site="BS", BS2010[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))])
BS <- rbind(BS, data.table(Site="BS", BS2011[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))]), fill=TRUE, use.names = TRUE)
BS <- rbind(BS, data.table(Site="BS", BS2012[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))]), fill=TRUE, use.names = TRUE)
BS <- rbind(BS, data.table(Site="BS", BS2013[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))]), fill=TRUE, use.names = TRUE)
BS <- rbind(BS, data.table(Site="BS", BS2014[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))]), fill=TRUE, use.names = TRUE)
BS <- rbind(BS, data.table(Site="BS", BS2015[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))]), fill=TRUE, use.names = TRUE)
BS <- rbind(BS, data.table(Site="BS", BS2016[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))]), fill=TRUE, use.names = TRUE)
BS <- rbind(BS, data.table(Site="BS", BS2017[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))]), fill=TRUE, use.names = TRUE)
BS <- rbind(BS, data.table(Site="BS", BS2018[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))]), fill=TRUE, use.names = TRUE)
BS <- rbind(BS, data.table(Site="BS", BS2019[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))]), fill=TRUE, use.names = TRUE)
BS <- rbind(BS, data.table(Site="BS", BS2020[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))]), fill=TRUE, use.names = TRUE)
BS <- rbind(BS, data.table(Site="BS", BS2021[-1, lapply(.SD, function(x) replace(x, which(x==-9999),NA))]), fill=TRUE, use.names = TRUE)
BS$WaterYear <- waterYear(BS$Date, numeric = FALSE)

#---------------------------------------------------
#selecting only columns that are needed for analysis 
FenBC <- select(FenBC, Site, Year, DoY, Hour, Date, WaterYear, LE_cw_f, qc_LE_cw_f, LE_cw_gf, qc_LE_cw_gf, Ta_gf, PRECIP, SnowD_f, WS_1, RH_gf, VPD_gf, RNET_f, Tsoil_gf)
bog <- select(bog, Site, Year, DoY, Hour, Date, WaterYear, LE_cw_f, qc_LE_cw_f, LE_cw_gf, qc_LE_cw_gf,  Ta_gf, PRECIP, SnowD_f, WS_1, RH_gf, VPD_gf, RNET_f, Tsoil_gf)
BS <- select(BS, Site, Year, DoY, Hour, Date, WaterYear, LE_cw_f, qc_LE_cw_f, LE_cw_gf, qc_LE_cw_gf,  Ta_gf, PRECIP, SnowD_f, WS_1, RH_gf, VPD_gf, RNET_f, Tsoil_gf)


#rename columns for simplicity and convert from character to numeric data 
# the pattern is new_col_name = old_col_name 
attach(FenBC)
FenBC <- data.frame(Year = as.numeric(Year), 
                    DoY = as.numeric(DoY),
                    Hour = as.numeric(Hour), 
                    Date = Date,
                    WaterYear = WaterYear,
                    LE_filtered = as.numeric(LE_cw_f),
                    LE_filtered_QC = as.factor(qc_LE_cw_f),
                    LE_QC = as.factor(qc_LE_cw_gf),
                    LE = as.numeric(LE_cw_gf),
                    AirTemp = as.numeric(Ta_gf),
                    Precip = as.numeric(PRECIP),
                    SnowD = as.numeric(SnowD_f), 
                    WindSpeed = as.numeric(WS_1),
                    RH = as.numeric(RH_gf),
                    VPD = as.numeric(VPD_gf),
                    NetRadiation = as.numeric(RNET_f), 
                    SoilTemp = as.numeric(Tsoil_gf))
FenBC <- FenBC %>%
  mutate(month = month(Date, label = TRUE))
detach(FenBC)

attach(BS)
BS <- data.frame(Year = as.numeric(Year), 
                 DoY = as.numeric(DoY),
                 Hour = as.numeric(Hour), 
                 Date = Date,
                 WaterYear = WaterYear,
                 LE_filtered = as.numeric(LE_cw_f),
                 LE_filtered_QC = as.factor(qc_LE_cw_f),
                 LE = as.numeric(LE_cw_gf),
                 LE_QC = as.factor(qc_LE_cw_gf),
                 AirTemp = as.numeric(Ta_gf),
                 Precip = as.numeric(PRECIP),
                 SnowD = as.numeric(SnowD_f),
                 WindSpeed = as.numeric(WS_1),
                 RH = as.numeric(RH_gf),
                 VPD = as.numeric(VPD_gf),
                 NetRadiation = as.numeric(RNET_f), 
                 SoilTemp = as.numeric(Tsoil_gf))
BS <- BS %>%
  mutate(month = month(Date, label = TRUE))
detach(BS)

attach(bog)
bog <- data.frame(Year = as.numeric(Year), 
                  DoY = as.numeric(DoY),
                  Hour = as.numeric(Hour), 
                  Date = Date,
                  WaterYear = WaterYear,
                  LE_filtered = as.numeric(LE_cw_f),
                  LE_filtered_QC = as.factor(qc_LE_cw_f),
                  LE = as.numeric(LE_cw_gf),
                  LE_QC = as.factor(qc_LE_cw_gf),
                  AirTemp = as.numeric(Ta_gf),
                  Precip = as.numeric(PRECIP),
                  SnowD = as.numeric(SnowD_f),
                  WindSpeed = as.numeric(WS_1),
                  RH = as.numeric(RH_gf),
                  VPD = as.numeric(VPD_gf),
                  NetRadiation = as.numeric(RNET_f), 
                  SoilTemp = as.numeric(Tsoil_gf))
bog <- bog %>%
  mutate(month = month(Date, label = TRUE))
detach(bog)


#===============================================================================
#CALCULATE WATER VAPOR FLUXES===================================================
#===============================================================================
#Sublimation definition: sublimation is calculated when the latent heat flux is positive and when snowpack is present. 

#snowpack presence is determined from albedo measurements. (determination conducted by E. Euskirchen)

#Lv = 2.454 MJ/kg
#Ls = 2.838 MJ/kg

#--------------------------------------
#IDENTIFY DATE RANGE OF SNOW PRESENCE. CREATE 'NOSNOW' COLUMN WHERE 1 = SNOW FREE AND 0 = SNOW COVER PRESENT  

#IMMAVAIT CREEK SITES
attach(Fen)
Fen$nosnow <- ifelse(Year == 2010 & (DoY >= 147 & DoY <= 268), 1,
                     ifelse(Year == 2011 & (DoY >= 145 & DoY <= 263), 1,
                            ifelse(Year == 2012 & (DoY >= 152 & DoY <= 271), 1,
                                   ifelse(Year == 2013 & (DoY >= 163 & DoY <= 275), 1,
                                          ifelse(Year == 2014 & (DoY >= 161 & DoY <= 263), 1,
                                                 ifelse(Year == 2015 & (DoY >= 156 & DoY <= 239), 1,
                                                        ifelse(Year == 2016 & (DoY >= 133 & DoY <= 256), 1,
                                                               ifelse(Year == 2017 & (DoY >= 154 & DoY <= 262), 1,
                                                                      ifelse(Year == 2018 & (DoY >= 170 & DoY <= 265), 1,
                                                                             ifelse(Year == 2019 & (DoY >= 140 & DoY <= 266), 1,
                                                                                    ifelse(Year == 2020 & (DoY >= 149 & DoY <= 254), 1,
                                                                                           ifelse(Year==2009 & (DoY <= 259), 1, 
                                                                                                  ifelse(Year==2021 & (DoY >= 159 & DoY <= 262), 1, 0)))))))))))))
detach(Fen)

attach(Ridge)
Ridge$nosnow <- ifelse(Year == 2010 & (DoY >= 147 & DoY <= 268), 1,
                       ifelse(Year == 2011 & (DoY >= 145 & DoY <= 263), 1,
                              ifelse(Year == 2012 & (DoY >= 152 & DoY <= 271), 1,
                                     ifelse(Year == 2013 & (DoY >= 163 & DoY <= 275), 1,
                                            ifelse(Year == 2014 & (DoY >= 161 & DoY <= 263), 1,
                                                   ifelse(Year == 2015 & (DoY >= 156 & DoY <= 239), 1,
                                                          ifelse(Year == 2016 & (DoY >= 133 & DoY <= 256), 1,
                                                                 ifelse(Year == 2017 & (DoY >= 154 & DoY <= 262), 1,
                                                                        ifelse(Year == 2018 & (DoY >= 170 & DoY <= 265), 1,
                                                                               ifelse(Year == 2019 & (DoY >= 140 & DoY <= 266), 1,
                                                                                      ifelse(Year == 2020 & (DoY >= 149 & DoY <= 254), 1,
                                                                                             ifelse(Year==2009 & (DoY <= 259), 1, 
                                                                                                    ifelse(Year==2021 & (DoY >= 159 & DoY <= 262), 1, 0)))))))))))))
detach(Ridge)


attach(Tussock)
Tussock$nosnow <- ifelse(Year == 2010 & (DoY >= 147 & DoY <= 268), 1,
                         ifelse(Year == 2011 & (DoY >= 145 & DoY <= 263), 1,
                                ifelse(Year == 2012 & (DoY >= 152 & DoY <= 271), 1,
                                       ifelse(Year == 2013 & (DoY >= 163 & DoY <= 275), 1,
                                              ifelse(Year == 2014 & (DoY >= 161 & DoY <= 263), 1,
                                                     ifelse(Year == 2015 & (DoY >= 156 & DoY <= 239), 1,
                                                            ifelse(Year == 2016 & (DoY >= 133 & DoY <= 256), 1,
                                                                   ifelse(Year == 2017 & (DoY >= 154 & DoY <= 262), 1,
                                                                          ifelse(Year == 2018 & (DoY >= 170 & DoY <= 265), 1,
                                                                                 ifelse(Year == 2019 & (DoY >= 140 & DoY <= 266), 1,
                                                                                        ifelse(Year == 2020 & (DoY >= 149 & DoY <= 254), 1,
                                                                                               ifelse(Year==2009 & (DoY <= 259), 1, 
                                                                                                      ifelse(Year==2021 & (DoY >= 159 & DoY <= 262), 1, 0)))))))))))))
detach(Tussock)

#BONANZE CREEK SITES

attach(FenBC)
FenBC$nosnow <- ifelse(Year == 2013 & (DoY > 141 & DoY < 300), 1, 
                       ifelse(Year == 2014 & (DoY >= 111 & DoY <= 276), 1,
                              ifelse(Year == 2015 & (DoY >= 109 & DoY <= 274), 1,
                                     ifelse(Year == 2016 & (DoY >= 103 & DoY <= 294), 1,
                                            ifelse(Year == 2017 & (DoY >= 118 & DoY <= 303), 1,
                                                   ifelse(Year == 2018 & (DoY >= 119 & DoY <= 316), 1,
                                                          ifelse(Year == 2019 & (DoY >= 86 & DoY <= 305), 1,
                                                                 ifelse(Year == 2020 & (DoY >= 110 & DoY <= 293), 1,
                                                                        ifelse(Year == 2021 & (DoY >= 114 & DoY <= 274), 1, 
                                                                               ifelse(Year == 2010 & (DoY >= 109 & DoY <= 284), 1,
                                                                                      ifelse(Year == 2011 & (DoY >= 113 & DoY <= 289), 1,
                                                                                             ifelse(Year == 2012 & (DoY >= 109 & DoY <= 288), 1,0))))))))))))
detach(FenBC)

attach(bog)
bog$nosnow <- ifelse(Year == 2013 & (DoY > 141 & DoY < 300), 1, 
                     ifelse(Year == 2014 & (DoY >= 111 & DoY <= 276), 1,
                            ifelse(Year == 2015 & (DoY >= 109 & DoY <= 274), 1,
                                   ifelse(Year == 2016 & (DoY >= 103 & DoY <= 294), 1,
                                          ifelse(Year == 2017 & (DoY >= 118 & DoY <= 303), 1,
                                                 ifelse(Year == 2018 & (DoY >= 119 & DoY <= 316), 1,
                                                        ifelse(Year == 2019 & (DoY >= 86 & DoY <= 305), 1,
                                                               ifelse(Year == 2020 & (DoY >= 110 & DoY <= 293), 1,
                                                                      ifelse(Year == 2021 & (DoY >= 114 & DoY <= 274), 1, 
                                                                             ifelse(Year == 2010 & (DoY >= 109 & DoY <= 284), 1,
                                                                                    ifelse(Year == 2011 & (DoY >= 113 & DoY <= 289), 1,
                                                                                           ifelse(Year == 2012 & (DoY >= 109 & DoY <= 288), 1,0))))))))))))
detach(bog)

attach(BS)
BS$nosnow <- ifelse(Year == 2013 & (DoY > 141 & DoY < 300), 1, 
                    ifelse(Year == 2014 & (DoY >= 111 & DoY <= 276), 1,
                           ifelse(Year == 2015 & (DoY >= 109 & DoY <= 274), 1,
                                  ifelse(Year == 2016 & (DoY >= 103 & DoY <= 294), 1,
                                         ifelse(Year == 2017 & (DoY >= 118 & DoY <= 303), 1,
                                                ifelse(Year == 2018 & (DoY >= 119 & DoY <= 316), 1,
                                                       ifelse(Year == 2019 & (DoY >= 86 & DoY <= 305), 1,
                                                              ifelse(Year == 2020 & (DoY >= 110 & DoY <= 293), 1,
                                                                     ifelse(Year == 2021 & (DoY >= 114 & DoY <= 274), 1,
                                                                            ifelse(Year == 2010 & (DoY >= 109 & DoY <= 284), 1,
                                                                                   ifelse(Year == 2011 & (DoY >= 113 & DoY <= 289), 1,
                                                                                          ifelse(Year == 2012 & (DoY >= 109 & DoY <= 288), 1,0))))))))))))
detach(BS)

#---------------------------------------
#CALCULATE HALF-HOURLY WATER VAPOR FLUXES
#If flux is positive, sublimation or evaporation; if NOSNOW == 1, evaporation or condensation

#IMNAVAIT CREEK FEN:
Fen$flux_hourly <- ifelse(Fen$nosnow == 1, (Fen$LE/2454000)*1800, (Fen$LE/2838000)*1800) #actually, it's half hour averages, not hourly

attach(Fen)
Fen$Flux_hourly_type <- ifelse(flux_hourly > 0, ifelse(nosnow == 1, "Evaporation", "Sublimation"), 
                               ifelse(nosnow == 1, "Condensation", "Deposition"))
detach(Fen)

Fen$Sublimation_hourly <- ifelse(Fen$Flux_hourly_type == "Sublimation", Fen$flux_hourly, 0) 
Fen$Evaporation_hourly <- ifelse(Fen$Flux_hourly_type == "Evaporation", Fen$flux_hourly, 0)
Fen$Condensation_hourly <- ifelse(Fen$Flux_hourly_type == "Condensation", Fen$flux_hourly, 0)
Fen$Deposition_hourly <- ifelse(Fen$Flux_hourly_type == "Deposition", Fen$flux_hourly, 0)

#IMNAVAIT CREEK RIDGE
Ridge$flux_hourly <- ifelse(Ridge$nosnow == 1, (Ridge$LE/2454000)*1800, (Ridge$LE/2838000)*1800)

attach(Ridge)
Ridge$Flux_hourly_type <- ifelse(flux_hourly > 0, ifelse(nosnow == 1, "Evaporation", "Sublimation"), 
                                 ifelse(nosnow == 1, "Condensation", "Deposition"))
detach(Ridge)

Ridge$Sublimation_hourly <- ifelse(Ridge$Flux_hourly_type == "Sublimation", Ridge$flux_hourly, 0) 
Ridge$Evaporation_hourly <- ifelse(Ridge$Flux_hourly_type == "Evaporation", Ridge$flux_hourly, 0)
Ridge$Condensation_hourly <- ifelse(Ridge$Flux_hourly_type == "Condensation", Ridge$flux_hourly, 0)
Ridge$Deposition_hourly <- ifelse(Ridge$Flux_hourly_type == "Deposition", Ridge$flux_hourly, 0)

#IMNAVAIT CREEK TUSSOCK
Tussock$flux_hourly <- ifelse(Tussock$nosnow == 1, (Tussock$LE/2454000)*1800, (Tussock$LE/2838000)*1800) 

attach(Tussock)
Tussock$Flux_hourly_type <- ifelse(flux_hourly > 0, ifelse(nosnow == 1, "Evaporation", "Sublimation"), 
                                   ifelse(nosnow == 1, "Condensation", "Deposition"))
detach(Tussock)

Tussock$Sublimation_hourly <- ifelse(Tussock$Flux_hourly_type == "Sublimation", Tussock$flux_hourly, 0)
Tussock$Evaporation_hourly <- ifelse(Tussock$Flux_hourly_type == "Evaporation", Tussock$flux_hourly, 0)
Tussock$Condensation_hourly <- ifelse(Tussock$Flux_hourly_type == "Condensation", Tussock$flux_hourly, 0)
Tussock$Deposition_hourly <- ifelse(Tussock$Flux_hourly_type == "Deposition", Tussock$flux_hourly, 0)

#BONANZA CREEK FEN
FenBC$flux_hourly <- ifelse(FenBC$nosnow == 1, (FenBC$LE/2454000)*1800, (FenBC$LE/2838000)*1800) 

attach(FenBC)
FenBC$Flux_hourly_type <- ifelse(flux_hourly > 0, ifelse(nosnow == 1, "Evaporation", "Sublimation"), 
                                 ifelse(nosnow == 1, "Condensation", "Deposition"))
detach(FenBC)

FenBC$Sublimation_hourly <- ifelse(FenBC$Flux_hourly_type == "Sublimation", FenBC$flux_hourly, 0)
FenBC$Evaporation_hourly <- ifelse(FenBC$Flux_hourly_type == "Evaporation", FenBC$flux_hourly, 0)
FenBC$Condensation_hourly <- ifelse(FenBC$Flux_hourly_type == "Condensation", FenBC$flux_hourly, 0)
FenBC$Deposition_hourly <- ifelse(FenBC$Flux_hourly_type == "Deposition", FenBC$flux_hourly, 0)

#BONANZA CREEK BOG
bog$flux_hourly <- ifelse(bog$nosnow == 1, (bog$LE/2454000)*1800, (bog$LE/2838000)*1800) 

attach(bog)
bog$Flux_hourly_type <- ifelse(flux_hourly > 0, ifelse(nosnow == 1, "Evaporation", "Sublimation"), 
                               ifelse(nosnow == 1, "Condensation", "Deposition"))
detach(bog)

bog$Sublimation_hourly <- ifelse(bog$Flux_hourly_type == "Sublimation", bog$flux_hourly, 0)  
bog$Evaporation_hourly <- ifelse(bog$Flux_hourly_type == "Evaporation", bog$flux_hourly, 0)
bog$Condensation_hourly <- ifelse(bog$Flux_hourly_type == "Condensation", bog$flux_hourly, 0)
bog$Deposition_hourly <- ifelse(bog$Flux_hourly_type == "Deposition", bog$flux_hourly, 0)

#BONANZA CREEK BLACK SPRUCE
BS$flux_hourly <- ifelse(BS$nosnow == 1, (BS$LE/2454000)*1800, (BS$LE/2838000)*1800) 

attach(BS)
BS$Flux_hourly_type <- ifelse(flux_hourly > 0, ifelse(nosnow == 1, "Evaporation", "Sublimation"), 
                              ifelse(nosnow == 1, "Condensation", "Deposition"))
detach(BS)

BS$Sublimation_hourly <- ifelse(BS$Flux_hourly_type == "Sublimation", BS$flux_hourly, 0) 
BS$Evaporation_hourly <- ifelse(BS$Flux_hourly_type == "Evaporation", BS$flux_hourly, 0)
BS$Condensation_hourly <- ifelse(BS$Flux_hourly_type == "Condensation", BS$flux_hourly, 0)
BS$Deposition_hourly <- ifelse(BS$Flux_hourly_type == "Deposition", BS$flux_hourly, 0)

#---------------------------------------
#FOR LOOPS-- 
# 1. PERFORMS MATH (ADDITION, MEAN, MAX, ETC) ON THE 48 HALF HOUR VALUES AND PROVIDES THE DAILY RESULT IN EACH HALF HOUR CELL. 
# 2. CREAKE DAILY DF WITH ALL DAILY RESULTS AT MIDNIGHT EACH DAY. 
# 3. RUN ADDITIONAL DATAFRAME TASKS TO FORMAT CORRECTLY FOR NEXT STEPS

#IMNAVAIT CREEK FEN
flux <- length(Fen$flux_hourly)
flux2 <- flux - 48
fenflux <- rep(0,flux)  # of obs
for (i in 1:flux2){
  fenflux[i]<-sum(Fen$flux_hourly[i:(i+47)], na.rm = TRUE)} 
Fen$flux_daily <- fenflux

fensubl <- rep(0,flux)  # of obs
for (i in 1:flux2){
  fensubl[i]<-sum(Fen$Sublimation_hourly[i:(i+47)], na.rm = TRUE)} 
Fen$sublimation_daily <- fensubl

fenwind <- rep(0,flux)  # of obs
for (i in 1:flux2){
  fenwind[i]<-sum(Fen$WindSpeed[i:(i+47)], na.rm = TRUE)} 
Fen$wind_daily_sum <- fenwind

fenwind2 <- rep(0,flux)  # of obs
for (i in 1:flux2){
  fenwind2[i]<-mean(Fen$WindSpeed[i:(i+47)], na.rm = TRUE)} 
Fen$wind_daily_mean <- fenwind2

fenwind3 <- rep(0,flux)  # of obs
for (i in 1:flux2){
  fenwind3[i]<-max(Fen$WindSpeed[i:(i+47)], na.rm = TRUE)} 
Fen$wind_daily_max <- fenwind3

fenevap <- rep(0,flux)  # of obs
for (i in 1:flux2){
  fenevap[i]<-sum(Fen$Evaporation_hourly[i:(i+47)], na.rm = TRUE)} 
Fen$Evaporation_daily <- fenevap

fendep <- rep(0,flux)  # of obs
for (i in 1:flux2){
  fendep[i]<-sum(Fen$Deposition_hourly[i:(i+47)], na.rm = TRUE)} 
Fen$Deposition_daily <- fendep

fencond <- rep(0,flux)  # of obs
for (i in 1:flux2){
  fencond[i]<-sum(Fen$Condensation_hourly[i:(i+47)], na.rm = TRUE)} 
Fen$Condensation_daily <- fencond

FenAT <- rep(0,flux)  # of obs
for (i in 1:flux2){
  FenAT[i]<-mean(Fen$AirTemp[i:(i+47)], na.rm = TRUE)} 
Fen$AT_daily_ave <- FenAT

FenST <- rep(0,flux)  # of obs
for (i in 1:flux2){
  FenST[i]<-mean(Fen$SurfaceTemp[i:(i+47)], na.rm = TRUE)} 
Fen$surf_temp_daily_ave <- FenST

FenSoilT <- rep(0,flux)  # of obs
for (i in 1:flux2){
  FenSoilT[i]<-mean(Fen$SoilTemp[i:(i+47)], na.rm = TRUE)} 
Fen$soil_temp_daily_ave <- FenSoilT

FenRH <- rep(0,flux)  # of obs
for (i in 1:flux2){
  FenRH[i]<-mean(Fen$RH[i:(i+47)], na.rm = TRUE)} 
Fen$RH_daily_ave <- FenRH

FenNR <- rep(0,flux)  # of obs
for (i in 1:flux2){
  FenNR[i]<-sum(Fen$NetRadiation[i:(i+47)], na.rm = TRUE)} 
Fen$daily_sum_NRad <- FenNR

FenVPD <- rep(0,flux)  # of obs
for (i in 1:flux2){
  FenVPD[i]<-mean(Fen$VPD[i:(i+47)], na.rm = TRUE)} 
Fen$VPD_daily_ave <- FenVPD

#----------------------------------
Fen_daily <- filter(Fen, Hour == 0) #dataframe with daily VALUES 

Fen_daily <- Fen_daily %>%
  mutate(month = month(Date, label = TRUE),
         day = day(Date))


Fen_daily <- Fen_daily %>%
  mutate(month_num = month(Date, label = FALSE))

Fen_daily$Date_POSIXct <- as.POSIXct(Fen_daily$Date+1)

Fen_daily$wind_daily_max[Fen_daily$wind_daily_max == "-inf"] <- 0

Fen_daily$DoWY <- ifelse(Fen_daily$DoY > 274, Fen_daily$DoY-274, 91 + Fen_daily$DoY)

#IMNAVAIT CREEK RIDGE
flux <- length(Ridge$flux_hourly)
flux2 <- flux - 48
Ridgeflux <- rep(0,flux)  # of obs
for (i in 1:flux2){
  Ridgeflux[i]<-sum(Ridge$flux_hourly[i:(i+47)], na.rm = TRUE)} 
Ridge$flux_daily <- Ridgeflux

Ridgesubl <- rep(0,flux)  # of obs
for (i in 1:flux2){
  Ridgesubl[i]<-sum(Ridge$Sublimation_hourly[i:(i+47)], na.rm = TRUE)} 
Ridge$sublimation_daily <- Ridgesubl

Ridgewind <- rep(0,flux)  # of obs
for (i in 1:flux2){
  Ridgewind[i]<-sum(Ridge$WindSpeed[i:(i+47)], na.rm = TRUE)} 
Ridge$wind_daily_sum <- Ridgewind

Ridgewind2 <- rep(0,flux)  # of obs
for (i in 1:flux2){
  Ridgewind2[i]<-mean(Ridge$WindSpeed[i:(i+47)], na.rm = TRUE)} 
Ridge$wind_daily_mean <- Ridgewind2

Ridgewind3 <- rep(0,flux)  # of obs
for (i in 1:flux2){
  Ridgewind3[i]<-max(Ridge$WindSpeed[i:(i+47)], na.rm = TRUE)} 
Ridge$wind_daily_max <- Ridgewind3

Ridgeevap <- rep(0,flux)  # of obs
for (i in 1:flux2){
  Ridgeevap[i]<-sum(Ridge$Evaporation_hourly[i:(i+47)], na.rm = TRUE)} 
Ridge$Evaporation_daily <- Ridgeevap

Ridgedep <- rep(0,flux)  # of obs
for (i in 1:flux2){
  Ridgedep[i]<-sum(Ridge$Deposition_hourly[i:(i+47)], na.rm = TRUE)} 
Ridge$Deposition_daily <- Ridgedep

Ridgecond <- rep(0,flux)  # of obs
for (i in 1:flux2){
  Ridgecond[i]<-sum(Ridge$Condensation_hourly[i:(i+47)], na.rm = TRUE)} 
Ridge$Condensation_daily <- Ridgecond

RidgeAT <- rep(0,flux)  # of obs
for (i in 1:flux2){
  RidgeAT[i]<-mean(Ridge$AirTemp[i:(i+47)], na.rm = TRUE)} 
Ridge$AT_daily_ave <- RidgeAT

RidgeSoilT <- rep(0,flux)  # of obs
for (i in 1:flux2){
  RidgeSoilT[i]<-mean(Ridge$SoilTemp[i:(i+47)], na.rm = TRUE)} 
Ridge$soil_temp_daily_ave <- RidgeSoilT

RidgeRH <- rep(0,flux)  # of obs
for (i in 1:flux2){
  RidgeRH[i]<-mean(Ridge$RH[i:(i+47)], na.rm = TRUE)} 
Ridge$RH_daily_ave <- RidgeRH

RidgeNR <- rep(0,flux)  # of obs
for (i in 1:flux2){
  RidgeNR[i]<-sum(Ridge$NetRadiation[i:(i+47)], na.rm = TRUE)} 
Ridge$daily_sum_NRad <- RidgeNR

RidgeVPD <- rep(0,flux)  # of obs
for (i in 1:flux2){
  RidgeVPD[i]<-mean(Ridge$VPD[i:(i+47)], na.rm = TRUE)} 
Ridge$VPD_daily_ave <- RidgeVPD


#---------------------------------------
Ridge_daily <- filter(Ridge, Hour == 0) #dataframe with daily VALUES

Ridge_daily <- Ridge_daily %>%
  mutate(month = month(Date, label = TRUE),
         day = day(Date))

Ridge_daily <- Ridge_daily %>%
  mutate(month_num = month(Date, label = FALSE))

Ridge_daily$Date_POSIXct <- as.POSIXct(Ridge_daily$Date+1)

Ridge_daily$DoWY <- ifelse(Ridge_daily$DoY > 274, Ridge_daily$DoY-274, 91 + Ridge_daily$DoY)

# LOOK AT DATA 
Ridge_daily %>%
  filter(WaterYear == 2010) %>%
  ggplot() +
  geom_line(aes(x=Date_POSIXct, y = sublimation_daily))+
  #scale_x_datetime(date_labels = "%b-%Y", date_breaks = "2 years") + 
  labs(title = "Ridge Daily Sublimation",
       y = "Water Flux (mm H2O)",
       x = " ")+
  geom_abline(slope = 0, intercept = 0, color = "darkgreen") +
  theme_bw(base_size = 15)+
  ylim(-0.1,1.5)


#IMNAVAIT CREEK TUSSOCK
flux <- length(Tussock$flux_hourly)
flux2 <- flux - 48
Tussockflux <- rep(0,flux)  # of obs
for (i in 1:flux2){
  Tussockflux[i]<-sum(Tussock$flux_hourly[i:(i+47)], na.rm = TRUE)} 
Tussock$flux_daily <- Tussockflux

Tussocksubl <- rep(0,flux)  # of obs
for (i in 1:flux2){
  Tussocksubl[i]<-sum(Tussock$Sublimation_hourly[i:(i+47)], na.rm = TRUE)} 
Tussock$sublimation_daily <- Tussocksubl

Tussockwind <- rep(0,flux)  # of obs
for (i in 1:flux2){
  Tussockwind[i]<-sum(Tussock$WindSpeed[i:(i+47)], na.rm = TRUE)} 
Tussock$wind_daily_sum <- Tussockwind

Tussockwind2 <- rep(0,flux)  # of obs
for (i in 1:flux2){
  Tussockwind2[i]<-mean(Tussock$WindSpeed[i:(i+47)], na.rm = TRUE)} 
Tussock$wind_daily_mean <- Tussockwind2

Tussockwind3 <- rep(0,flux)  # of obs
for (i in 1:flux2){
  Tussockwind3[i]<-max(Tussock$WindSpeed[i:(i+47)], na.rm = TRUE)} 
Tussock$wind_daily_max <- Tussockwind3

Tussockevap <- rep(0,flux)  # of obs
for (i in 1:flux2){
  Tussockevap[i]<-sum(Tussock$Evaporation_hourly[i:(i+47)], na.rm = TRUE)} 
Tussock$Evaporation_daily <- Tussockevap

Tussockdep <- rep(0,flux)  # of obs
for (i in 1:flux2){
  Tussockdep[i]<-sum(Tussock$Deposition_hourly[i:(i+47)], na.rm = TRUE)} 
Tussock$Deposition_daily <- Tussockdep

Tussockcond <- rep(0,flux)  # of obs
for (i in 1:flux2){
  Tussockcond[i]<-sum(Tussock$Condensation_hourly[i:(i+47)], na.rm = TRUE)} 
Tussock$Condensation_daily <- Tussockcond

TussockAT <- rep(0,flux)  # of obs
for (i in 1:flux2){
  TussockAT[i]<-mean(Tussock$AirTemp[i:(i+47)], na.rm = TRUE)} 
Tussock$AT_daily_ave <- TussockAT

TussockSoilT <- rep(0,flux)  # of obs
for (i in 1:flux2){
  TussockSoilT[i]<-mean(Tussock$SoilTemp[i:(i+47)], na.rm = TRUE)} 
Tussock$soil_temp_daily_ave <- TussockSoilT

TussockRH <- rep(0,flux)  # of obs
for (i in 1:flux2){
  TussockRH[i]<-mean(Tussock$RH[i:(i+47)], na.rm = TRUE)} 
Tussock$RH_daily_ave <- TussockRH

TussockNR <- rep(0,flux)  # of obs
for (i in 1:flux2){
  TussockNR[i]<-sum(Tussock$NetRadiation[i:(i+47)], na.rm = TRUE)} 
Tussock$daily_sum_NRad <- TussockNR

TussockVPD <- rep(0,flux)  # of obs
for (i in 1:flux2){
  TussockVPD[i]<-mean(Tussock$VPD[i:(i+47)], na.rm = TRUE)} 
Tussock$VPD_daily_ave <- TussockVPD


#-------------------------------------------
Tussock_daily <- filter(Tussock, Hour == 0) #dataframe with daily values

Tussock_daily <- Tussock_daily %>%
  mutate(month = month(Date, label = TRUE),
         day = day(Date))


Tussock_daily <- Tussock_daily %>%
  mutate(month_num = month(Date, label = FALSE))

Tussock_daily$Date_POSIXct <- as.POSIXct(Tussock_daily$Date+1)

Tussock_daily$DoWY <- ifelse(Tussock_daily$DoY > 274, Tussock_daily$DoY-274, 91 + Tussock_daily$DoY)

#300+ wind speeds impossible..removed those records 
#Tussock_daily <-Tussock_daily[-(3064:3065),]
Tussock_daily[(3063:3065),13] <- NA

#LOOK AT DATA
Tussock_daily %>%
  # filter(WaterYear == 2016) %>%
  ggplot() +
  geom_line(aes(x=Date_POSIXct, y = sublimation_daily))+
  #scale_x_datetime(date_labels = "%b-%Y", date_breaks = "2 years") + 
  labs(title = "Tussock Daily Sublimation",
       y = "Water Flux (mm H2O)",
       x = " ")+
  geom_abline(slope = 0, intercept = 0, color = "darkgreen") +
  theme_bw(base_size = 15)+
  ylim(-0.1,1.5)

#BONANZA CREEK FEN 
flux <- length(FenBC$flux_hourly)
flux2 <- flux - 48
fenflux <- rep(0,flux)  # of obs
for (i in 1:flux2){
  fenflux[i]<-sum(FenBC$flux_hourly[i:(i+47)], na.rm = TRUE)} 
FenBC$flux_daily <- fenflux

FenBCsubl <- rep(0,flux)  # of obs
for (i in 1:flux2){
  FenBCsubl[i]<-sum(FenBC$Sublimation_hourly[i:(i+47)], na.rm = TRUE)} 
FenBC$sublimation_daily <- FenBCsubl

FenBCevap <- rep(0,flux)  # of obs
for (i in 1:flux2){
  FenBCevap[i]<-sum(FenBC$Evaporation_hourly[i:(i+47)], na.rm = TRUE)} 
FenBC$Evaporation_daily <- FenBCevap

FenBCdep <- rep(0,flux)  # of obs
for (i in 1:flux2){
  FenBCdep[i]<-sum(FenBC$Deposition_hourly[i:(i+47)], na.rm = TRUE)} 
FenBC$Deposition_daily <- FenBCdep

FenBCcond <- rep(0,flux)  # of obs
for (i in 1:flux2){
  FenBCcond[i]<-sum(FenBC$Condensation_hourly[i:(i+47)], na.rm = TRUE)} 
FenBC$Condensation_daily <- FenBCcond

FenBCwind <- rep(0,flux)  # of obs
for (i in 1:flux2){
  FenBCwind[i]<-sum(FenBC$WindSpeed[i:(i+47)], na.rm = TRUE)} 
FenBC$wind_daily_sum <- FenBCwind

FenBCwind2 <- rep(0,flux)  # of obs
for (i in 1:flux2){
  FenBCwind2[i]<-mean(FenBC$WindSpeed[i:(i+47)], na.rm = TRUE)} 
FenBC$wind_daily_mean <- FenBCwind2

FenBCwind3 <- rep(0,flux)  # of obs
for (i in 1:flux2){
  FenBCwind3[i]<-max(FenBC$WindSpeed[i:(i+47)], na.rm = TRUE)} 
FenBC$wind_daily_max <- FenBCwind3

FenBCAT <- rep(0,flux)  # of obs
for (i in 1:flux2){
  FenBCAT[i]<-mean(FenBC$AirTemp[i:(i+47)], na.rm = TRUE)} 
FenBC$AT_daily_ave <- FenBCAT

FenBCSoilT <- rep(0,flux)  # of obs
for (i in 1:flux2){
  FenBCSoilT[i]<-mean(FenBC$SoilTemp[i:(i+47)], na.rm = TRUE)} 
FenBC$soil_temp_daily_ave <- FenBCSoilT

FenBCRH <- rep(0,flux)  # of obs
for (i in 1:flux2){
  FenBCRH[i]<-mean(FenBC$RH[i:(i+47)], na.rm = TRUE)} 
FenBC$RH_daily_ave <- FenBCRH

FenBCNR <- rep(0,flux)  # of obs
for (i in 1:flux2){
  FenBCNR[i]<-sum(FenBC$NetRadiation[i:(i+47)], na.rm = TRUE)} 
FenBC$daily_sum_NRad <- FenBCNR

FenBCVPD <- rep(0,flux)  # of obs
for (i in 1:flux2){
  FenBCVPD[i]<-mean(FenBC$VPD[i:(i+47)], na.rm = TRUE)} 
FenBC$VPD_daily_ave <- FenBCVPD

#---------------------------------------
FenBC_daily <- filter(FenBC, Hour == 0) #dataframe with daily values 

FenBC_daily <- FenBC_daily %>%
  mutate(month = month(Date, label = TRUE),
         day = day(Date))

FenBC_daily <- FenBC_daily %>%
  mutate(month_num = month(Date, label = FALSE))

FenBC_daily$Date_POSIXct <- as.POSIXct(FenBC_daily$Date+1)

FenBC_daily$DoWY <- ifelse(FenBC_daily$DoY > 274, FenBC_daily$DoY-274, 91 + FenBC_daily$DoY)

#BONANZA CREEK BOG
flux <- length(bog$flux_hourly)
flux2 <- flux - 48
bogflux <- rep(0,flux)  # of obs
for (i in 1:flux2){
  bogflux[i]<-sum(bog$flux_hourly[i:(i+47)], na.rm = TRUE)} 
bog$flux_daily <- bogflux

bogsubl <- rep(0,flux)  # of obs
for (i in 1:flux2){
  bogsubl[i]<-sum(bog$Sublimation_hourly[i:(i+47)], na.rm = TRUE)} 
bog$sublimation_daily <- bogsubl

bogevap <- rep(0,flux)  # of obs
for (i in 1:flux2){
  bogevap[i]<-sum(bog$Evaporation_hourly[i:(i+47)], na.rm = TRUE)} 
bog$Evaporation_daily <- bogevap

bogdep <- rep(0,flux)  # of obs
for (i in 1:flux2){
  bogdep[i]<-sum(bog$Deposition_hourly[i:(i+47)], na.rm = TRUE)} 
bog$Deposition_daily <- bogdep

bogcond <- rep(0,flux)  # of obs
for (i in 1:flux2){
  bogcond[i]<-sum(bog$Condensation_hourly[i:(i+47)], na.rm = TRUE)} 
bog$Condensation_daily <- bogcond

bogwind <- rep(0,flux)  # of obs
for (i in 1:flux2){
  bogwind[i]<-sum(bog$WindSpeed[i:(i+47)], na.rm = TRUE)} 
bog$wind_daily_sum <- bogwind

bogwind2 <- rep(0,flux)  # of obs
for (i in 1:flux2){
  bogwind2[i]<-mean(bog$WindSpeed[i:(i+47)], na.rm = TRUE)} 
bog$wind_daily_mean <- bogwind2

bogwind3 <- rep(0,flux)  # of obs
for (i in 1:flux2){
  bogwind3[i]<-max(bog$WindSpeed[i:(i+47)], na.rm = TRUE)} 
bog$wind_daily_max <- bogwind3

bogAT <- rep(0,flux)  # of obs
for (i in 1:flux2){
  bogAT[i]<-mean(bog$AirTemp[i:(i+47)], na.rm = TRUE)} 
bog$AT_daily_ave <- bogAT

bogSoilT <- rep(0,flux)  # of obs
for (i in 1:flux2){
  bogSoilT[i]<-mean(bog$SoilTemp[i:(i+47)], na.rm = TRUE)} 
bog$soil_temp_daily_ave <- bogSoilT

bogRH <- rep(0,flux)  # of obs
for (i in 1:flux2){
  bogRH[i]<-mean(bog$RH[i:(i+47)], na.rm = TRUE)} 
bog$RH_daily_ave <- bogRH

bogNR <- rep(0,flux)  # of obs
for (i in 1:flux2){
  bogNR[i]<-sum(bog$NetRadiation[i:(i+47)], na.rm = TRUE)} 
bog$daily_sum_NRad <- bogNR

bogVPD <- rep(0,flux)  # of obs
for (i in 1:flux2){
  bogVPD[i]<-mean(bog$VPD[i:(i+47)], na.rm = TRUE)} 
bog$VPD_daily_ave <- bogVPD

#-----------------------------------
bog_daily <- filter(bog, Hour == 0) #dataframe with daily values 

bog_daily <- bog_daily %>%
  mutate(month = month(Date, label = TRUE),
         day = day(Date))


bog_daily <- bog_daily %>%
  mutate(month_num = month(Date, label = FALSE))

bog_daily$Date_POSIXct <- as.POSIXct(bog_daily$Date+1)

bog_daily$DoWY <- ifelse(bog_daily$DoY > 274, bog_daily$DoY-274, 91 + bog_daily$DoY)


#BONANZA CREEK BLACK SPRUCE 
flux <- length(BS$flux_hourly)
flux2 <- flux - 48
BSflux <- rep(0,flux)  # of obs
for (i in 1:flux2){
  BSflux[i]<-sum(BS$flux_hourly[i:(i+47)], na.rm = TRUE)} 
BS$flux_daily <- BSflux

BSsubl <- rep(0,flux)  # of obs
for (i in 1:flux2){
  BSsubl[i]<-sum(BS$Sublimation_hourly[i:(i+47)], na.rm = TRUE)} 
BS$sublimation_daily <- BSsubl

BSevap <- rep(0,flux)  # of obs
for (i in 1:flux2){
  BSevap[i]<-sum(BS$Evaporation_hourly[i:(i+47)], na.rm = TRUE)} 
BS$Evaporation_daily <- BSevap

BSdep <- rep(0,flux)  # of obs
for (i in 1:flux2){
  BSdep[i]<-sum(BS$Deposition_hourly[i:(i+47)], na.rm = TRUE)} 
BS$Deposition_daily <- BSdep

BScond <- rep(0,flux)  # of obs
for (i in 1:flux2){
  BScond[i]<-sum(BS$Condensation_hourly[i:(i+47)], na.rm = TRUE)} 
BS$Condensation_daily <- BScond

BSwind <- rep(0,flux)  # of obs
for (i in 1:flux2){
  BSwind[i]<-sum(BS$WindSpeed[i:(i+47)], na.rm = TRUE)} 
BS$wind_daily_sum <- BSwind

BSwind2 <- rep(0,flux)  # of obs
for (i in 1:flux2){
  BSwind2[i]<-mean(BS$WindSpeed[i:(i+47)], na.rm = TRUE)} 
BS$wind_daily_mean <- BSwind2

BSwind3 <- rep(0,flux)  # of obs
for (i in 1:flux2){
  BSwind3[i]<-max(BS$WindSpeed[i:(i+47)], na.rm = TRUE)} 
BS$wind_daily_max <- BSwind3

BSAT <- rep(0,flux)  # of obs
for (i in 1:flux2){
  BSAT[i]<-mean(BS$AirTemp[i:(i+47)], na.rm = TRUE)} 
BS$AT_daily_ave <- BSAT

BSSoilT <- rep(0,flux)  # of obs
for (i in 1:flux2){
  BSSoilT[i]<-mean(BS$SoilTemp[i:(i+47)], na.rm = TRUE)} 
BS$soil_temp_daily_ave <- BSSoilT

BSRH <- rep(0,flux)  # of obs
for (i in 1:flux2){
  BSRH[i]<-mean(BS$RH[i:(i+47)], na.rm = TRUE)} 
BS$RH_daily_ave <- BSRH

BSNR <- rep(0,flux)  # of obs
for (i in 1:flux2){
  BSNR[i]<-sum(BS$NetRadiation[i:(i+47)], na.rm = TRUE)} 
BS$daily_sum_NRad <- BSNR

BSVPD <- rep(0,flux)  # of obs
for (i in 1:flux2){
  BSVPD[i]<-mean(BS$VPD[i:(i+47)], na.rm = TRUE)} 
BS$VPD_daily_ave <- BSVPD

#---------------------------------
BS_daily <- filter(BS, Hour == 0) #dataframe with daily values 

BS_daily <- BS_daily %>%
  mutate(month = month(Date, label = TRUE),
         day = day(Date))


BS_daily <- BS_daily %>%
  mutate(month_num = month(Date, label = FALSE))

BS_daily$Date_POSIXct <- as.POSIXct(BS_daily$Date+1)

BS_daily$DoWY <- ifelse(BS_daily$DoY > 274, BS_daily$DoY-274, 91 + BS_daily$DoY)


#===============================================================================
#BASIC QC AND DERIVE SUMMARY RESULTS FOR EACH SITE==============================
#===============================================================================
# 1. CHECK FOR MISSING RECORDS AND MISSING SUBLIMATION DATA TO DETERMINE WATER YEARS WITH COMPLETE DATA 
# 2. SUMMARY STATS: MEAN, SD, MAX, AND CUMULATIVE (DAILY AND ANNUAL)

#IMNAVAIT CREEK FEN
wateryears <- c(2010:2022)
FenQuality <- data.frame(wateryears)
FenQuality$NA_flux_hourly <- c(sum(is.na(Fen_daily$flux_hourly[Fen_daily$WaterYear == 2010 & Fen_daily$nosnow == 0])),
                               sum(is.na(Fen_daily$flux_hourly[Fen_daily$WaterYear == 2011 & Fen_daily$nosnow == 0])),                  
                               sum(is.na(Fen_daily$flux_hourly[Fen_daily$WaterYear == 2012 & Fen_daily$nosnow == 0])),                  
                               sum(is.na(Fen_daily$flux_hourly[Fen_daily$WaterYear == 2013 & Fen_daily$nosnow == 0])),
                               sum(is.na(Fen_daily$flux_hourly[Fen_daily$WaterYear == 2014 & Fen_daily$nosnow == 0])),
                               sum(is.na(Fen_daily$flux_hourly[Fen_daily$WaterYear == 2015 & Fen_daily$nosnow == 0])),
                               sum(is.na(Fen_daily$flux_hourly[Fen_daily$WaterYear == 2016 & Fen_daily$nosnow == 0])),
                               sum(is.na(Fen_daily$flux_hourly[Fen_daily$WaterYear == 2017 & Fen_daily$nosnow == 0])),
                               sum(is.na(Fen_daily$flux_hourly[Fen_daily$WaterYear == 2018 & Fen_daily$nosnow == 0])),
                               sum(is.na(Fen_daily$flux_hourly[Fen_daily$WaterYear == 2019 & Fen_daily$nosnow == 0])),
                               sum(is.na(Fen_daily$flux_hourly[Fen_daily$WaterYear == 2020 & Fen_daily$nosnow == 0])),
                               sum(is.na(Fen_daily$flux_hourly[Fen_daily$WaterYear == 2021 & Fen_daily$nosnow == 0])),
                               sum(is.na(Fen_daily$flux_hourly[Fen_daily$WaterYear == 2022 & Fen_daily$nosnow == 0])))
FenQuality
#missing 162 days in 2013 and 91 days in 2014

#determine complete water years by # of records 
Fen_records <- Fen %>%
  group_by(WaterYear) %>%
  tally()
Fen_records

#incomplete water years = 2007, 2022
#missing sublimation = 2013-2014
#-->Fen valid data period: 2010-2012, 2015-2021

#----------------------
Fen_WY <- Fen_daily %>% 
  filter(WaterYear >2009 & WaterYear<2022 & WaterYear != 2013 & WaterYear != 2014) %>%
  #filter(WaterYear == 2013 | WaterYear == 2014 | WaterYear == 2016 | WaterYear == 2017 | WaterYear == 2018 | WaterYear == 2019 | WaterYear == 2020) %>%
  filter(nosnow == 0) %>%
  group_by(WaterYear) %>%
  summarise(mean_daily_sublimation = mean(sublimation_daily, na.rm = TRUE),
            sd_daily_sublimation = sd(sublimation_daily, na.rm = TRUE), 
            max_daily_sublimation = max(sublimation_daily, na.rm = TRUE),
            cumulative_sublimation = sum(sublimation_daily, na.rm = TRUE))

mean(Fen_WY$mean_daily_sublimation) #0.10
mean(Fen_WY$sd_daily_sublimation) #0.16
max(Fen_WY$max_daily_sublimation) #2.44
mean(Fen_WY$cumulative_sublimation) #25
max(Fen_WY$cumulative_sublimation) #49
sd(Fen_WY$cumulative_sublimation) #12
#data in table 1

Fen_snow <- Fen_daily %>%
  filter(WaterYear>2009 & WaterYear<2022) %>%
  #filter(WaterYear >2009 & WaterYear<2022 & WaterYear != 2013 & WaterYear != 2014) %>%
  # filter(WaterYear == 2013 | WaterYear == 2014 | WaterYear == 2016 | WaterYear == 2017 | WaterYear == 2018 | WaterYear == 2019 | WaterYear == 2020) %>%
  group_by(WaterYear) %>%
  summarise(no_snow_days = sum(nosnow),
            snow_days = 365 - no_snow_days)
mean(Fen_snow$snow_days)#254


#IMNAVAIT CREEK RIDGE
wateryears <- c(2010:2022)
RidgeQuality <- data.frame(wateryears)
RidgeQuality$NA_flux_hourly <- c(sum(is.na(Ridge_daily$flux_hourly[Ridge_daily$WaterYear == 2010 & Ridge_daily$nosnow == 0])),
                                 sum(is.na(Ridge_daily$flux_hourly[Ridge_daily$WaterYear == 2011 & Ridge_daily$nosnow == 0])),                  
                                 sum(is.na(Ridge_daily$flux_hourly[Ridge_daily$WaterYear == 2012 & Ridge_daily$nosnow == 0])),                  
                                 sum(is.na(Ridge_daily$flux_hourly[Ridge_daily$WaterYear == 2013 & Ridge_daily$nosnow == 0])),
                                 sum(is.na(Ridge_daily$flux_hourly[Ridge_daily$WaterYear == 2014 & Ridge_daily$nosnow == 0])),
                                 sum(is.na(Ridge_daily$flux_hourly[Ridge_daily$WaterYear == 2015 & Ridge_daily$nosnow == 0])),
                                 sum(is.na(Ridge_daily$flux_hourly[Ridge_daily$WaterYear == 2016 & Ridge_daily$nosnow == 0])),
                                 sum(is.na(Ridge_daily$flux_hourly[Ridge_daily$WaterYear == 2017 & Ridge_daily$nosnow == 0])),
                                 sum(is.na(Ridge_daily$flux_hourly[Ridge_daily$WaterYear == 2018 & Ridge_daily$nosnow == 0])),
                                 sum(is.na(Ridge_daily$flux_hourly[Ridge_daily$WaterYear == 2019 & Ridge_daily$nosnow == 0])),
                                 sum(is.na(Ridge_daily$flux_hourly[Ridge_daily$WaterYear == 2020 & Ridge_daily$nosnow == 0])),
                                 sum(is.na(Ridge_daily$flux_hourly[Ridge_daily$WaterYear == 2021 & Ridge_daily$nosnow == 0])),
                                 sum(is.na(Ridge_daily$flux_hourly[Ridge_daily$WaterYear == 2022 & Ridge_daily$nosnow == 0])))
RidgeQuality #no NAs

#determine complete water years by # of records 
Ridge_records <- Ridge %>%
  group_by(WaterYear) %>%
  tally()
Ridge_records
#incomplete water years b/w 2010 and 2021 = NA
#missing sublimation = NA
#---> valid data period 2010-2021

#--------------------------
Ridge_WY <- Ridge_daily %>% 
  filter(WaterYear > 2009 & WaterYear < 2022) %>%
  #filter(WaterYear ==2013 | WaterYear == 2014| WaterYear == 2016| WaterYear == 2017| WaterYear == 2018| WaterYear == 2019| WaterYear == 2020) %>%
  filter(nosnow == 0) %>%
  group_by(WaterYear) %>%
  summarise(mean_daily_sublimation = mean(sublimation_daily, na.rm = TRUE),
            sd_daily_sublimation = sd(sublimation_daily, na.rm = TRUE), 
            max_daily_sublimation = max(sublimation_daily, na.rm = TRUE),
            cumulative_sublimation = sum(sublimation_daily, na.rm = TRUE))

mean(Ridge_WY$mean_daily_sublimation) #0.08 
mean(Ridge_WY$sd_daily_sublimation) #0.13
max(Ridge_WY$max_daily_sublimation) #2.25
mean(Ridge_WY$cumulative_sublimation) #20
max(Ridge_WY$cumulative_sublimation) #39
sd(Ridge_WY$cumulative_sublimation) #9
#data in table 1

Ridge_snow <- Ridge_daily %>%
  # filter(WaterYear ==2013 | WaterYear == 2014| WaterYear == 2016| WaterYear == 2017| WaterYear == 2018| WaterYear == 2019| WaterYear == 2020) %>%
  filter(WaterYear > 2009 & WaterYear < 2022) %>%
  group_by(WaterYear) %>%
  summarise(no_snow_days = sum(nosnow),
            snow_days = 365 - no_snow_days)
mean(Ridge_snow$snow_days) #254, Table 2
#IMNAVAIT CREEK TUSSOCK
wateryears <- c(2010:2022)
TussockQuality <- data.frame(wateryears)
TussockQuality$NA_flux_hourly <- c(sum(is.na(Tussock_daily$flux_hourly[Tussock_daily$WaterYear == 2010 & Tussock_daily$nosnow == 0])),
                                   sum(is.na(Tussock_daily$flux_hourly[Tussock_daily$WaterYear == 2011 & Tussock_daily$nosnow == 0])),                  
                                   sum(is.na(Tussock_daily$flux_hourly[Tussock_daily$WaterYear == 2012 & Tussock_daily$nosnow == 0])),                  
                                   sum(is.na(Tussock_daily$flux_hourly[Tussock_daily$WaterYear == 2013 & Tussock_daily$nosnow == 0])),
                                   sum(is.na(Tussock_daily$flux_hourly[Tussock_daily$WaterYear == 2014 & Tussock_daily$nosnow == 0])),
                                   sum(is.na(Tussock_daily$flux_hourly[Tussock_daily$WaterYear == 2015 & Tussock_daily$nosnow == 0])),
                                   sum(is.na(Tussock_daily$flux_hourly[Tussock_daily$WaterYear == 2016 & Tussock_daily$nosnow == 0])),
                                   sum(is.na(Tussock_daily$flux_hourly[Tussock_daily$WaterYear == 2017 & Tussock_daily$nosnow == 0])),
                                   sum(is.na(Tussock_daily$flux_hourly[Tussock_daily$WaterYear == 2018 & Tussock_daily$nosnow == 0])),
                                   sum(is.na(Tussock_daily$flux_hourly[Tussock_daily$WaterYear == 2019 & Tussock_daily$nosnow == 0])),
                                   sum(is.na(Tussock_daily$flux_hourly[Tussock_daily$WaterYear == 2020 & Tussock_daily$nosnow == 0])),
                                   sum(is.na(Tussock_daily$flux_hourly[Tussock_daily$WaterYear == 2021 & Tussock_daily$nosnow == 0])),
                                   sum(is.na(Tussock_daily$flux_hourly[Tussock_daily$WaterYear == 2022 & Tussock_daily$nosnow == 0])))
TussockQuality
#missing data during snowcover in 2010 (214 days), 2011 (153 days), 2012(147 days), 2015(127 days)



#determine complete water years by # of records 

Tussock_records <- Tussock %>%
  group_by(WaterYear) %>%
  tally()
Tussock_records
#incomplete water years = NA

#--->Tussock valid data period: 2013-2014, 2016-2020

#-----------------------------
Tussock_WY <- Tussock_daily %>% 
  filter(WaterYear ==2013 | WaterYear == 2014| WaterYear == 2016| WaterYear == 2017| WaterYear == 2018| WaterYear == 2019| WaterYear == 2020| WaterYear == 2021) %>%
  filter(nosnow == 0) %>%
  group_by(WaterYear) %>%
  summarise(mean_daily_sublimation = mean(sublimation_daily, na.rm = TRUE),
            sd_daily_sublimation = sd(sublimation_daily, na.rm = TRUE), 
            max_daily_sublimation = max(sublimation_daily, na.rm = TRUE),
            cumulative_sublimation = sum(sublimation_daily, na.rm = TRUE))

mean(Tussock_WY$mean_daily_sublimation) #0.10
mean(Tussock_WY$sd_daily_sublimation) #0.18
max(Tussock_WY$max_daily_sublimation) #1.78
mean(Tussock_WY$cumulative_sublimation) #26
max(Tussock_WY$cumulative_sublimation) #38
sd(Tussock_WY$cumulative_sublimation) #7
#data in Table 1

Tussock_snow <- Tussock_daily %>%
  filter(WaterYear > 2009 & WaterYear < 2022) %>%
  #filter(WaterYear ==2013 | WaterYear == 2014| WaterYear == 2016| WaterYear == 2017| WaterYear == 2018| WaterYear == 2019| WaterYear == 2020| WaterYear==2021) %>%
  group_by(WaterYear) %>%
  summarise(no_snow_days = sum(nosnow),
            snow_days = 365 - no_snow_days)
mean(Tussock_snow$snow_days) #254 #Table 2

#BONANZA CREEK FEN 
wateryears <- c(2010:2022)
FenBCQuality <- data.frame(wateryears)
FenBCQuality$NA_Sublimation_hourly <- c(sum(is.na(FenBC$Sublimation_hourly[FenBC$WaterYear == 2010 & FenBC$nosnow == 0])),
                                        sum(is.na(FenBC$Sublimation_hourly[FenBC$WaterYear == 2011 & FenBC$nosnow == 0])),                  
                                        sum(is.na(FenBC$Sublimation_hourly[FenBC$WaterYear == 2012 & FenBC$nosnow == 0])),                  
                                        sum(is.na(FenBC$Sublimation_hourly[FenBC$WaterYear == 2013 & FenBC$nosnow == 0])),
                                        sum(is.na(FenBC$Sublimation_hourly[FenBC$WaterYear == 2014 & FenBC$nosnow == 0])),
                                        sum(is.na(FenBC$Sublimation_hourly[FenBC$WaterYear == 2015 & FenBC$nosnow == 0])),
                                        sum(is.na(FenBC$Sublimation_hourly[FenBC$WaterYear == 2016 & FenBC$nosnow == 0])),
                                        sum(is.na(FenBC$Sublimation_hourly[FenBC$WaterYear == 2017 & FenBC$nosnow == 0])),
                                        sum(is.na(FenBC$Sublimation_hourly[FenBC$WaterYear == 2018 & FenBC$nosnow == 0])),
                                        sum(is.na(FenBC$Sublimation_hourly[FenBC$WaterYear == 2019 & FenBC$nosnow == 0])),
                                        sum(is.na(FenBC$Sublimation_hourly[FenBC$WaterYear == 2020 & FenBC$nosnow == 0])),
                                        sum(is.na(FenBC$Sublimation_hourly[FenBC$WaterYear == 2021 & FenBC$nosnow == 0])),
                                        sum(is.na(FenBC$Sublimation_hourly[FenBC$WaterYear == 2022 & FenBC$nosnow == 0])))
FenBCQuality
#missing data in 2012 (5368 half hour sublimtation values = 112 days) 2014 (2176 half hour sublimation values = 45 days) and 2016 (1840 half hour values = 38 days)

#determine complete water years by # of records 
FenBC_records <- FenBC %>%
  group_by(WaterYear) %>%
  tally()
FenBC_records
#incomplete water years = 2010, 2011, 2013, 2022
#missing sublimation = 2012, 2014,2016
# -----> complete records and no missing sublimation at BCFEN site: 2015, 2017 - 2021

FenBC_WY_stats <- FenBC_daily %>%
  filter(WaterYear == 2015 | (WaterYear > 2016 & WaterYear < 2022)) %>%
  filter(nosnow == 0) %>%
  group_by(WaterYear) %>%
  summarise(mean_daily_sublimation = mean(sublimation_daily, na.rm = TRUE),
            sd_daily_sublimation = sd(sublimation_daily, na.rm = TRUE), 
            max_daily_sublimation = max(sublimation_daily, na.rm = TRUE),
            cumulative_sublimation = sum(sublimation_daily, na.rm = TRUE))

mean(FenBC_WY_stats$mean_daily_sublimation) #0.10
mean(FenBC_WY_stats$sd_daily_sublimation) #0.16
max(FenBC_WY_stats$max_daily_sublimation) #1.92
round(mean(FenBC_WY_stats$cumulative_sublimation), 0) #17
max(FenBC_WY_stats$cumulative_sublimation) #22
sd(FenBC_WY_stats$cumulative_sublimation) #5
#data in Table 1

#BONANZA CREEK BOG
wateryears <- c(2010:2022)
bogQuality <- data.frame(wateryears)
bogQuality$NA_Sublimation_hourly <- c(sum(is.na(bog$Sublimation_hourly[bog$WaterYear == 2010 & bog$nosnow == 0])),
                                      sum(is.na(bog$Sublimation_hourly[bog$WaterYear == 2011 & bog$nosnow == 0])),
                                      sum(is.na(bog$Sublimation_hourly[bog$WaterYear == 2012 & bog$nosnow == 0])),
                                      sum(is.na(bog$Sublimation_hourly[bog$WaterYear == 2013 & bog$nosnow == 0])),
                                      sum(is.na(bog$Sublimation_hourly[bog$WaterYear == 2014 & bog$nosnow == 0])),
                                      sum(is.na(bog$Sublimation_hourly[bog$WaterYear == 2015 & bog$nosnow == 0])),
                                      sum(is.na(bog$Sublimation_hourly[bog$WaterYear == 2016 & bog$nosnow == 0])),
                                      sum(is.na(bog$Sublimation_hourly[bog$WaterYear == 2017 & bog$nosnow == 0])),
                                      sum(is.na(bog$Sublimation_hourly[bog$WaterYear == 2018 & bog$nosnow == 0])),
                                      sum(is.na(bog$Sublimation_hourly[bog$WaterYear == 2019 & bog$nosnow == 0])),
                                      sum(is.na(bog$Sublimation_hourly[bog$WaterYear == 2020 & bog$nosnow == 0])),
                                      sum(is.na(bog$Sublimation_hourly[bog$WaterYear == 2021 & bog$nosnow == 0])),
                                      sum(is.na(bog$Sublimation_hourly[bog$WaterYear == 2022 & bog$nosnow == 0])))
bogQuality
#missing sublimation data in WY 2012

#determine complete water years by # of records 
bog %>%
  group_by(WaterYear) %>%
  tally()
#incomplete water years = 2011, 2013, 2022
#missing sublimation = 2012

#-----------> complete bog records and no missing sublimation at BOG siteS: 2014-2021


bog_WY <- bog_daily %>% 
  filter(nosnow == 0) %>%
  filter(WaterYear > 2013 & WaterYear < 2022) %>%
  group_by(WaterYear) %>%
  summarise(mean_daily_sublimation = mean(sublimation_daily, na.rm = TRUE),
            sd_daily_sublimation = sd(sublimation_daily, na.rm = TRUE), 
            max_daily_sublimation = max(sublimation_daily, na.rm = TRUE),
            cumulative_sublimation = sum(sublimation_daily, na.rm = TRUE))
round(mean(bog_WY$mean_daily_sublimation), 2) #0.08
mean(bog_WY$sd_daily_sublimation) #0.13
max(bog_WY$max_daily_sublimation) #1.52
mean(bog_WY$cumulative_sublimation) #15
max(bog_WY$cumulative_sublimation) #21
sd(bog_WY$cumulative_sublimation) #5

#BONANZA CREEK BLACK SPRUCE 
wateryears <- c(2010:2022)
BSQuality <- data.frame(wateryears)
BSQuality$NA_Sublimation_hourly <- c(sum(is.na(BS$Sublimation_hourly[BS$WaterYear == 2010 & BS$nosnow == 0])),
                                     sum(is.na(BS$Sublimation_hourly[BS$WaterYear == 2011 & BS$nosnow == 0])),
                                     sum(is.na(BS$Sublimation_hourly[BS$WaterYear == 2012 & BS$nosnow == 0])),
                                     sum(is.na(BS$Sublimation_hourly[BS$WaterYear == 2013 & BS$nosnow == 0])),
                                     sum(is.na(BS$Sublimation_hourly[BS$WaterYear == 2014 & BS$nosnow == 0])),
                                     sum(is.na(BS$Sublimation_hourly[BS$WaterYear == 2015 & BS$nosnow == 0])),
                                     sum(is.na(BS$Sublimation_hourly[BS$WaterYear == 2016 & BS$nosnow == 0])),
                                     sum(is.na(BS$Sublimation_hourly[BS$WaterYear == 2017 & BS$nosnow == 0])),
                                     sum(is.na(BS$Sublimation_hourly[BS$WaterYear == 2018 & BS$nosnow == 0])),
                                     sum(is.na(BS$Sublimation_hourly[BS$WaterYear == 2019 & BS$nosnow == 0])),
                                     sum(is.na(BS$Sublimation_hourly[BS$WaterYear == 2020 & BS$nosnow == 0])),
                                     sum(is.na(BS$Sublimation_hourly[BS$WaterYear == 2021 & BS$nosnow == 0])),
                                     sum(is.na(BS$Sublimation_hourly[BS$WaterYear == 2022 & BS$nosnow == 0]))) 
BSQuality
#determine complete water years by # of records 
BS %>%
  group_by(WaterYear)%>%
  tally()
#incomplete water years = 2010, 2013, 2022


#-----------> complete black spruce records and no missing sublimation at BOG siteS: 2011-2012, 2014 - 2021


BS_WY <- BS_daily %>% 
  filter(nosnow == 0) %>%
  group_by(WaterYear) %>%
  filter(WaterYear > 2010 & WaterYear<2022 & WaterYear != 2013) %>% 
  summarise(mean_daily_sublimation = mean(sublimation_daily, na.rm = TRUE),
            sd_daily_sublimation = sd(sublimation_daily, na.rm = TRUE), 
            max_daily_sublimation = max(sublimation_daily, na.rm = TRUE),
            cumulative_sublimation = sum(sublimation_daily, na.rm = TRUE))
mean(BS_WY$mean_daily_sublimation) #0.15
mean(BS_WY$sd_daily_sublimation) #0.18
max(BS_WY$max_daily_sublimation) #2.08
mean(BS_WY$cumulative_sublimation) #27
max(BS_WY$cumulative_sublimation) #35
sd(BS_WY$cumulative_sublimation) #6
#data in Table 1


##==============================================================================
#DERIVE SUMMARY STATS AT BOTH REGIONS===========================================
#===============================================================================

#ANNUAL IMNAVAIT SUBLIMATION
wateryears <- c(2010:2021)
IB_annual_subl <- data.frame(wateryears)
IB_annual_subl$fen <- round(c(Fen_WY$cumulative_sublimation[Fen_WY$WaterYear==2010], Fen_WY$cumulative_sublimation[Fen_WY$WaterYear==2011], 
                              Fen_WY$cumulative_sublimation[Fen_WY$WaterYear==2012], NA, NA, Fen_WY$cumulative_sublimation[Fen_WY$WaterYear==2015], 
                              Fen_WY$cumulative_sublimation[Fen_WY$WaterYear==2016], Fen_WY$cumulative_sublimation[Fen_WY$WaterYear==2017], 
                              Fen_WY$cumulative_sublimation[Fen_WY$WaterYear==2018], Fen_WY$cumulative_sublimation[Fen_WY$WaterYear==2019], 
                              Fen_WY$cumulative_sublimation[Fen_WY$WaterYear==2020], Fen_WY$cumulative_sublimation[Fen_WY$WaterYear==2021]), 0)
IB_annual_subl$tussock <-  round(c(NA, NA, NA, Tussock_WY$cumulative_sublimation[Tussock_WY$WaterYear==2013], 
                                   Tussock_WY$cumulative_sublimation[Tussock_WY$WaterYear==2014], NA, 
                                   Tussock_WY$cumulative_sublimation[Tussock_WY$WaterYear==2016], Tussock_WY$cumulative_sublimation[Tussock_WY$WaterYear==2017], 
                                   Tussock_WY$cumulative_sublimation[Tussock_WY$WaterYear==2018], Tussock_WY$cumulative_sublimation[Tussock_WY$WaterYear==2019], 
                                   Tussock_WY$cumulative_sublimation[Tussock_WY$WaterYear==2020],Tussock_WY$cumulative_sublimation[Tussock_WY$WaterYear==2021]), 0)
IB_annual_subl$ridge <-  round(c(Ridge_WY$cumulative_sublimation[Ridge_WY$WaterYear==2010], Ridge_WY$cumulative_sublimation[Ridge_WY$WaterYear==2011], 
                                 Ridge_WY$cumulative_sublimation[Ridge_WY$WaterYear==2012], Ridge_WY$cumulative_sublimation[Ridge_WY$WaterYear==2013], 
                                 Ridge_WY$cumulative_sublimation[Ridge_WY$WaterYear==2014], Ridge_WY$cumulative_sublimation[Ridge_WY$WaterYear==2015], 
                                 Ridge_WY$cumulative_sublimation[Ridge_WY$WaterYear==2016], Ridge_WY$cumulative_sublimation[Ridge_WY$WaterYear==2017], 
                                 Ridge_WY$cumulative_sublimation[Ridge_WY$WaterYear==2018], Ridge_WY$cumulative_sublimation[Ridge_WY$WaterYear==2019], 
                                 Ridge_WY$cumulative_sublimation[Ridge_WY$WaterYear==2020],Ridge_WY$cumulative_sublimation[Ridge_WY$WaterYear==2021]), 0)
IB_annual_subl$mean <- round(rowMeans(IB_annual_subl[,2:4], na.rm=T), 0)

round(mean(IB_annual_subl$mean),0) #24
round(mean(IB_annual_subl$fen, na.rm = T),0)#25
round(mean(IB_annual_subl$tussock, na.rm = T),0) #26
round(mean(IB_annual_subl$ridge, na.rm = T),0) #20

#ANNUAL BONANZA SUBLIMATION
wateryears <- c(2011:2021)
BC_annual_subl <- data.frame(wateryears)
BC_annual_subl$fenBC <- c(NA, NA, NA, NA, FenBC_WY_stats$cumulative_sublimation[FenBC_WY_stats$WaterYear==2015], NA, 
                          FenBC_WY_stats$cumulative_sublimation[FenBC_WY_stats$WaterYear==2017], FenBC_WY_stats$cumulative_sublimation[FenBC_WY_stats$WaterYear==2018],
                          FenBC_WY_stats$cumulative_sublimation[FenBC_WY_stats$WaterYear==2019], FenBC_WY_stats$cumulative_sublimation[FenBC_WY_stats$WaterYear==2020], 
                          FenBC_WY_stats$cumulative_sublimation[FenBC_WY_stats$WaterYear==2021])
BC_annual_subl$bogBC <- c(NA, NA, NA, bog_WY$cumulative_sublimation[bog_WY$WaterYear==2014],
                          bog_WY$cumulative_sublimation[bog_WY$WaterYear==2015], bog_WY$cumulative_sublimation[bog_WY$WaterYear==2016], 
                          bog_WY$cumulative_sublimation[bog_WY$WaterYear==2017], bog_WY$cumulative_sublimation[bog_WY$WaterYear==2018],
                          bog_WY$cumulative_sublimation[bog_WY$WaterYear==2019], bog_WY$cumulative_sublimation[bog_WY$WaterYear==2020], 
                          bog_WY$cumulative_sublimation[bog_WY$WaterYear==2021])
BC_annual_subl$BSBC <- c(BS_WY$cumulative_sublimation[BS_WY$WaterYear==2011], BS_WY$cumulative_sublimation[BS_WY$WaterYear==2012], 
                         NA, BS_WY$cumulative_sublimation[BS_WY$WaterYear==2014], BS_WY$cumulative_sublimation[BS_WY$WaterYear==2015], 
                         BS_WY$cumulative_sublimation[BS_WY$WaterYear==2016], BS_WY$cumulative_sublimation[BS_WY$WaterYear==2017], 
                         BS_WY$cumulative_sublimation[BS_WY$WaterYear==2018], BS_WY$cumulative_sublimation[BS_WY$WaterYear==2019], 
                         BS_WY$cumulative_sublimation[BS_WY$WaterYear==2020], BS_WY$cumulative_sublimation[BS_WY$WaterYear==2021])
BC_annual_subl$meanBC <- round(rowMeans(BC_annual_subl[,2:4], na.rm=T), 0)

mean(BC_annual_subl$fenBC, na.rm = T)#17
mean(BC_annual_subl$bogBC, na.rm = T) #15
mean(BC_annual_subl$BSBC, na.rm = T) #27

BC_annual_subl %>% 
  summarise(mean = round(mean(meanBC, na.rm=T), 0),
            SD = round(sd(meanBC, na.rm=T), 0),
            max = round(max(meanBC, na.rm=T), 0),
            CV = SD/mean*100,
            times_greater = max/mean,
            meanfen = round(mean(fenBC, na.rm=T), 0),
            SDfen = round(sd(fenBC, na.rm=T), 0),
            maxfen = round(max(fenBC, na.rm=T), 0),
            CVfen = SDfen/meanfen*100,
            times_greaterfen = maxfen/meanfen,
            meanbog = round(mean(bogBC, na.rm=T), 0),
            SDbog = round(sd(bogBC, na.rm=T), 0),
            maxbog = round(max(bogBC, na.rm=T), 0),
            CVbog = SDbog/meanbog*100,
            times_greaterbog = maxbog/meanbog,
            meanBS = round(mean(BSBC, na.rm=T), 0),
            SDBS = round(sd(BSBC, na.rm=T), 0),
            maxBS = round(max(BSBC, na.rm=T), 0),
            CVBS = SDBS/meanBS*100,
            times_greaterBS = maxBS/meanBS)

#======COMBINE
annual_subl_IB_BC <- bind_rows(IB_annual_subl, BC_annual_subl)

#DAILY IMNAVAIT SUBLIMATION
wateryears <- c(2010:2021)
IB_daily_subl <- data.frame(wateryears)
IB_daily_subl$fen <- round(c(Fen_WY$mean_daily_sublimation[Fen_WY$WaterYear==2010], Fen_WY$mean_daily_sublimation[Fen_WY$WaterYear==2011], 
                             Fen_WY$mean_daily_sublimation[Fen_WY$WaterYear==2012], NA, NA, Fen_WY$mean_daily_sublimation[Fen_WY$WaterYear==2015], 
                             Fen_WY$mean_daily_sublimation[Fen_WY$WaterYear==2016], Fen_WY$mean_daily_sublimation[Fen_WY$WaterYear==2017], 
                             Fen_WY$mean_daily_sublimation[Fen_WY$WaterYear==2018], Fen_WY$mean_daily_sublimation[Fen_WY$WaterYear==2019], 
                             Fen_WY$mean_daily_sublimation[Fen_WY$WaterYear==2020],Fen_WY$mean_daily_sublimation[Fen_WY$WaterYear==2021]), 2)
IB_daily_subl$tussock <-  round(c(NA, NA, NA, Tussock_WY$mean_daily_sublimation[Tussock_WY$WaterYear==2013], 
                                  Tussock_WY$mean_daily_sublimation[Tussock_WY$WaterYear==2014], NA, 
                                  Tussock_WY$mean_daily_sublimation[Tussock_WY$WaterYear==2016], Tussock_WY$mean_daily_sublimation[Tussock_WY$WaterYear==2017], 
                                  Tussock_WY$mean_daily_sublimation[Tussock_WY$WaterYear==2018], Tussock_WY$mean_daily_sublimation[Tussock_WY$WaterYear==2019], 
                                  Tussock_WY$mean_daily_sublimation[Tussock_WY$WaterYear==2020],Tussock_WY$mean_daily_sublimation[Tussock_WY$WaterYear==2021]), 2)
IB_daily_subl$ridge <-  round(c(Ridge_WY$mean_daily_sublimation[Ridge_WY$WaterYear==2010], Ridge_WY$mean_daily_sublimation[Ridge_WY$WaterYear==2011], 
                                Ridge_WY$mean_daily_sublimation[Ridge_WY$WaterYear==2012], Ridge_WY$mean_daily_sublimation[Ridge_WY$WaterYear==2013], 
                                Ridge_WY$mean_daily_sublimation[Ridge_WY$WaterYear==2014], Ridge_WY$mean_daily_sublimation[Ridge_WY$WaterYear==2015], 
                                Ridge_WY$mean_daily_sublimation[Ridge_WY$WaterYear==2016], Ridge_WY$mean_daily_sublimation[Ridge_WY$WaterYear==2017], 
                                Ridge_WY$mean_daily_sublimation[Ridge_WY$WaterYear==2018], Ridge_WY$mean_daily_sublimation[Ridge_WY$WaterYear==2019], 
                                Ridge_WY$mean_daily_sublimation[Ridge_WY$WaterYear==2020], Ridge_WY$mean_daily_sublimation[Ridge_WY$WaterYear==2021]), 2)
IB_daily_subl$meanIB <- round(rowMeans(IB_daily_subl[,2:4], na.rm=T), 2)


round(mean(IB_daily_subl$meanIB),2) #0.1
round(mean(IB_daily_subl$fen, na.rm = T), 2) #0.1
round(mean(IB_daily_subl$tussock, na.rm = T),2) #0.1
round(mean(IB_daily_subl$ridge, na.rm = T),2) #0.08

#DAILY BONANZA SUBLIMATION
wateryears <- c(2011:2021)
BC_daily_subl <- data.frame(wateryears)
BC_daily_subl$fenBC <- round(c(NA, NA, NA, NA, 
                               FenBC_WY_stats$mean_daily_sublimation[FenBC_WY_stats$WaterYear==2015], NA, FenBC_WY_stats$mean_daily_sublimation[FenBC_WY_stats$WaterYear==2017], 
                               FenBC_WY_stats$mean_daily_sublimation[FenBC_WY_stats$WaterYear==2018], FenBC_WY_stats$mean_daily_sublimation[FenBC_WY_stats$WaterYear==2019], 
                               FenBC_WY_stats$mean_daily_sublimation[FenBC_WY_stats$WaterYear==2020], FenBC_WY_stats$mean_daily_sublimation[FenBC_WY_stats$WaterYear==2021]), 2)
BC_daily_subl$bogBC <-  round(c(NA, NA, NA, 
                                bog_WY$mean_daily_sublimation[bog_WY$WaterYear==2014], bog_WY$mean_daily_sublimation[bog_WY$WaterYear==2015], 
                                bog_WY$mean_daily_sublimation[bog_WY$WaterYear==2016], bog_WY$mean_daily_sublimation[bog_WY$WaterYear==2017], 
                                bog_WY$mean_daily_sublimation[bog_WY$WaterYear==2018], bog_WY$mean_daily_sublimation[bog_WY$WaterYear==2019], 
                                bog_WY$mean_daily_sublimation[bog_WY$WaterYear==2020], bog_WY$mean_daily_sublimation[bog_WY$WaterYear==2021]), 2)
BC_daily_subl$BSBC <-  round(c(BS_WY$mean_daily_sublimation[BS_WY$WaterYear==2011], 
                               BS_WY$mean_daily_sublimation[BS_WY$WaterYear==2012],NA, 
                               BS_WY$mean_daily_sublimation[BS_WY$WaterYear==2014], BS_WY$mean_daily_sublimation[BS_WY$WaterYear==2015], 
                               BS_WY$mean_daily_sublimation[BS_WY$WaterYear==2016], BS_WY$mean_daily_sublimation[BS_WY$WaterYear==2017], 
                               BS_WY$mean_daily_sublimation[BS_WY$WaterYear==2018], BS_WY$mean_daily_sublimation[BS_WY$WaterYear==2019], 
                               BS_WY$mean_daily_sublimation[BS_WY$WaterYear==2020], BS_WY$mean_daily_sublimation[BS_WY$WaterYear==2021]), 2)
BC_daily_subl$meanBC <- round(rowMeans(BC_daily_subl[,2:4], na.rm=T), 2)

round(mean(BC_daily_subl$meanBC, na.rm = T), 2) #0.12
round(mean(BC_daily_subl$fenBC, na.rm = T), 2) #0.10
round(mean(BC_daily_subl$bogBC, na.rm = T), 2) #0.09
round(mean(BC_daily_subl$BSBC, na.rm = T),2) #0.15

#======COMBINE
daily_subl_IB_BC <- bind_rows(IB_daily_subl, BC_daily_subl)

#add snow cover duration, SWE, solic precip 

#----------------------------
#to IB_Daily_subl dataframeS:
#----------------------------
#mean snow cover duration across sites:
IB_snow_duration <- cbind(Fen_snow$snow_days, Ridge_snow$snow_days, Tussock_snow$snow_days)
IB_snow_duration <- data.frame(IB_snow_duration)
IB_snow_duration$mean <- round(rowMeans(IB_snow_duration[,1:3]),0)

IB_annual_subl$mean_snow_duration <- IB_snow_duration$mean

IB_annual_subl$seasonal_max_SWE_mm <- c(121,174,150,161,NA,207,138,176,NA,NA,NA,128)
IB_annual_subl$SWEpercent <- round(IB_annual_subl$mean/IB_annual_subl$seasonal_max_SWE_mm*100, 0)


IB_annual_subl$solid_precip_mm <- c(104,137,84,114,140,183,NA,NA,157,99,97,112)
IB_annual_subl$solid_precip_percent <- round(IB_annual_subl$mean/IB_annual_subl$solid_precip_mm*100, 0)
IB_annual_subl$remaining_solid_precip <- IB_annual_subl$solid_precip_mm - IB_annual_subl$mean


mean(IB_annual_subl$mean, na.rm = TRUE) #IB mean sublimation is 24mm water per year
mean(IB_annual_subl$seasonal_max_SWE_mm, na.rm = TRUE) #157 mm H20 of snowpack in IB
sd(IB_annual_subl$seasonal_max_SWE_mm, na.rm = T) #SWE SD = 29
mean(IB_annual_subl$SWEpercent, na.rm = TRUE) #on average, 16% of the IB snowpack sublimates 
round(sd(IB_annual_subl$SWEpercent, na.rm = T), 0) #SWE PCT SD = 7
mean(IB_annual_subl$solid_precip_mm, na.rm = TRUE) #122 mm H20 of measured solid precipitation
sd(IB_annual_subl$solid_precip_mm, na.rm = T) #SP SD = 31
mean(IB_annual_subl$solid_precip_percent, na.rm = TRUE) #on average, 20% of the IB measured solid precipitation sublimates 
round(sd(IB_annual_subl$solid_precip_percent, na.rm = T), 0) #SP PCT SD = 8
#Table 2

#----------------------------
#BC_Daily_subl dataframe:
#----------------------------
BC_annual_subl$mean_snow_duration_BC <- c(194,185,219,176,198,194,190,181,136,170,187)
mean(BC_annual_subl$mean_snow_duration) #185, Table 2

BC_annual_subl$seasonal_max_SWE_mm_BC <- c(106,125,160,132,82,120,183,188,125,192,177)
BC_annual_subl$SWEpercent_BC <- round(BC_annual_subl$meanBC/BC_annual_subl$seasonal_max_SWE_mm_BC*100, 0)

BC_annual_subl$solid_precip_mm_BC <- c(91,90,112,91,115,64,121,181,83,148,153)
BC_annual_subl$solid_precip_percent_BC <- round(BC_annual_subl$meanBC/BC_annual_subl$solid_precip_mm_BC*100, 0)
BC_annual_subl$remaining_solid_precip_BC <- BC_annual_subl$solid_precip_mm_BC - BC_annual_subl$meanBC


#BC mean sublimation is 20mm water per year
mean(BC_annual_subl$seasonal_max_SWE_mm_BC, na.rm = TRUE) #145 mm H20 of snowpack in BC
round(sd(BC_annual_subl$seasonal_max_SWE_mm_BC, na.rm = T), 0) #SWE SD = 37
mean(BC_annual_subl$SWEpercent_BC, na.rm = TRUE) #on average, 16% of the BC snowpack sublimates 
round(sd(BC_annual_subl$SWEpercent_BC, na.rm = T), 0) #SWE PCT SD = 7
mean(BC_annual_subl$solid_precip_mm_BC, na.rm = TRUE) #114 mm H20 of measured solid precipitation
round(sd(BC_annual_subl$solid_precip_mm_BC, na.rm = T), 0) #SP SD = 35
mean(BC_annual_subl$solid_precip_percent_BC, na.rm = TRUE) #on average, 21% of the BC measured solid precipitation sublimates 
round(sd(BC_annual_subl$solid_precip_percent_BC, na.rm = T), 0) #SP PCT SD = 10
#Table 2  

#IB and BC solid precip and snow cover 

wateryears <- c(2010:2021)
annual_rates <- data.frame(wateryears)
annual_rates$fenBC <- c(NA, NA, NA, NA, NA, FenBC_WY_stats$cumulative_sublimation[FenBC_WY_stats$WaterYear==2015], NA, 
                        FenBC_WY_stats$cumulative_sublimation[FenBC_WY_stats$WaterYear==2017], FenBC_WY_stats$cumulative_sublimation[FenBC_WY_stats$WaterYear==2018],
                        FenBC_WY_stats$cumulative_sublimation[FenBC_WY_stats$WaterYear==2019], FenBC_WY_stats$cumulative_sublimation[FenBC_WY_stats$WaterYear==2020], 
                        FenBC_WY_stats$cumulative_sublimation[FenBC_WY_stats$WaterYear==2021])
annual_rates$bogBC <- c(NA, NA, NA, NA, bog_WY$cumulative_sublimation[bog_WY$WaterYear==2014],
                        bog_WY$cumulative_sublimation[bog_WY$WaterYear==2015], bog_WY$cumulative_sublimation[bog_WY$WaterYear==2016], 
                        bog_WY$cumulative_sublimation[bog_WY$WaterYear==2017], bog_WY$cumulative_sublimation[bog_WY$WaterYear==2018],
                        bog_WY$cumulative_sublimation[bog_WY$WaterYear==2019], bog_WY$cumulative_sublimation[bog_WY$WaterYear==2020], 
                        bog_WY$cumulative_sublimation[bog_WY$WaterYear==2021])
annual_rates$BSBC <- c(NA, BS_WY$cumulative_sublimation[BS_WY$WaterYear==2011], BS_WY$cumulative_sublimation[BS_WY$WaterYear==2012], 
                       NA, BS_WY$cumulative_sublimation[BS_WY$WaterYear==2014], BS_WY$cumulative_sublimation[BS_WY$WaterYear==2015], 
                       BS_WY$cumulative_sublimation[BS_WY$WaterYear==2016], BS_WY$cumulative_sublimation[BS_WY$WaterYear==2017], 
                       BS_WY$cumulative_sublimation[BS_WY$WaterYear==2018], BS_WY$cumulative_sublimation[BS_WY$WaterYear==2019], 
                       BS_WY$cumulative_sublimation[BS_WY$WaterYear==2020], BS_WY$cumulative_sublimation[BS_WY$WaterYear==2021])
annual_rates$mean <- round(rowMeans(annual_rates[,2:4], na.rm=T), 0)

annual_rates <- select(annual_rates, -fenBC, -bogBC, -BSBC)


annual_rates$mean_snow_duration <- c(180,194,185,219,176,198,194,190,181,136,170,187)

annual_rates$seasonal_max_SWE_mm <- c(39,106,125,160,132,82,120,183,188,125,192,177)

annual_rates$solid_precip_mm <- c(37,91,90,112,91,115,64,121,181,83,148,153)

annual_rates$Region <- "Lowland Boreal"

annual_rates_IB <- data.frame(wateryears)

annual_rates_IB$mean <- IB_annual_subl$mean

annual_rates_IB$mean_snow_duration <- c(226,222,214,225,240,238,237,237,238,212,228,233)

annual_rates_IB$seasonal_max_SWE_mm <- c(121,174,150,161,NA,207,138,176,NA,NA,NA,128)

annual_rates_IB$solid_precip_mm <- c(104,137,84,114,140,183,NA,NA,157,99,97,112)

annual_rates_IB$Region <- "Tundra"

annual_rates_IB_BC <- rbind(annual_rates, annual_rates_IB)

annual_rates_IB_BC$wateryears <- factor(annual_rates_IB_BC$wateryears, levels=c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021))

annual_rates_IB_BC <- select(annual_rates_IB_BC, -mean, -seasonal_max_SWE_mm)


LB_LM_rates <- subset(annual_rates_IB_BC, Region == "Lowland Boreal")
T_LM_rates <- subset(annual_rates_IB_BC, Region == "Tundra")
LB_LM_rates$wateryears<-as.integer(LB_LM_rates$wateryears)
T_LM_rates$wateryears<-as.integer(T_LM_rates$wateryears)


str(T_LM_rates)
str(annual_rates_IB_BC)


summary(lm(solid_precip_mm~wateryears, data=LB_LM_rates)) #p value = 0.018, adj. r sq: 0.4
summary(lm(mean_snow_duration~wateryears, data=LB_LM_rates)) #p value = 0.1899, adj r sq 0.08

summary(lm(solid_precip_mm~wateryears, data=T_LM_rates)) #p value = 0.9, adj. r sq: -0.1
summary(lm(mean_snow_duration~wateryears, data=T_LM_rates)) #p value = 0.48, adj r sq -0.04


summary(lm(mean_snow_duration~wateryears, data=annual_rates_IB_BC)) #p value = 0.9823 = insignificant



#===============================================================================
#==================== Relative rate of 4 types of fluxes: ======================
#===============================================================================

#IMNAVAIT
Fen_sub <- Fen_daily %>%
  group_by(WaterYear) %>%
  filter((WaterYear > 2014 & WaterYear < 2022) | (WaterYear > 2009 & WaterYear <2013)) %>%
  summarise(mmH20 = sum(sublimation_daily, na.rm = T))
Fen_sub$type <- "Sublimation"
Fen_sub$tower <- "VB"
Fen_cond <- Fen_daily %>%
  group_by(WaterYear) %>%
  filter((WaterYear > 2014 & WaterYear < 2022) | (WaterYear > 2009 & WaterYear <2013)) %>%
  summarise(mmH20 = sum(abs(Condensation_daily), na.rm = T))
Fen_cond$type <- "Condensation"
Fen_cond$tower <- "VB"
Fen_evap <- Fen_daily %>%
  group_by(WaterYear) %>%
  filter((WaterYear > 2014 & WaterYear < 2022) | (WaterYear > 2009 & WaterYear <2013)) %>%
  summarise(mmH20 = sum(Evaporation_daily, na.rm = T))
Fen_evap$type <- "Evapotranspiration"
Fen_evap$tower <- "VB"
Fen_dep <- Fen_daily %>%
  group_by(WaterYear) %>%
  filter((WaterYear > 2014 & WaterYear < 2022) | (WaterYear > 2009 & WaterYear <2013)) %>%
  summarise(mmH20 = sum(abs(Deposition_daily), na.rm = T))
Fen_dep$type <- "Deposition"
Fen_dep$tower <- "VB"

Fen_flux <- rbind(Fen_evap, Fen_sub, Fen_cond, Fen_dep)


Fen_flux %>%
  mutate(type = factor(type, levels=c("Condensation", "Deposition", "Sublimation", "Evapotranspiration"))) %>%
  ggplot(aes(x = WaterYear, y = mmH20, fill = type)) + 
  geom_bar(stat = "identity", color = "black") +
  guides(fill = guide_legend(title = " ")) +
  # scale_fill_brewer(palette = "YlGnBu") + #Paired,YlGnBu, BrBG
  scale_fill_viridis(discrete = T, option = "G") +
  labs(title = "Fen",
       y = "Water Vapor Flux (mm H2O/year)",
       x = "Water Year")

Ridge_sub <- Ridge_daily %>%
  group_by(WaterYear) %>%
  filter(WaterYear > 2009 & WaterYear < 2022) %>%
  summarise(mmH20 = sum(sublimation_daily, na.rm = T))
Ridge_sub$type <- "Sublimation"
Ridge_sub$tower <- "R"
Ridge_cond <- Ridge_daily %>%
  group_by(WaterYear) %>%
  filter(WaterYear > 2009 & WaterYear < 2022) %>%
  summarise(mmH20 = sum(abs(Condensation_daily), na.rm = T))
Ridge_cond$type <- "Condensation"
Ridge_cond$tower <- "R"
Ridge_evap <- Ridge_daily %>%
  group_by(WaterYear) %>%
  filter(WaterYear > 2009 & WaterYear < 2022) %>%
  summarise(mmH20 = sum(Evaporation_daily, na.rm = T))
Ridge_evap$type <- "Evapotranspiration"
Ridge_evap$tower <- "R"
Ridge_dep <- Ridge_daily %>%
  group_by(WaterYear) %>%
  filter(WaterYear > 2009 & WaterYear < 2022) %>%
  summarise(mmH20 = sum(abs(Deposition_daily), na.rm = T))
Ridge_dep$type <- "Deposition"
Ridge_dep$tower <- "R"

Ridge_flux <- rbind(Ridge_evap, Ridge_sub, Ridge_cond, Ridge_dep)


Ridge_flux %>%
  mutate(type = factor(type, levels=c("Condensation", "Deposition", "Sublimation", "Evapotranspiration"))) %>%
  ggplot(aes(x = WaterYear, y = mmH20, fill = type)) + 
  geom_bar(stat = "identity", color = "black") +
  guides(fill = guide_legend(title = " ")) +
  # scale_fill_brewer(palette = "YlGnBu") + #Paired,YlGnBu, BrBG
  scale_fill_viridis(discrete = T, option = "G") +
  labs(title = "Ridge",
       y = "Water Vapor Flux (mm H2O/year)",
       x = "Water Year")


Tussock_sub <- Tussock_daily %>%
  group_by(WaterYear) %>%
  filter(WaterYear == 2013 | WaterYear == 2014 | WaterYear == 2016 | WaterYear == 2017 | WaterYear == 2018 | WaterYear == 2019 | WaterYear == 2020 | WaterYear == 2021) %>%
  summarise(mmH20 = sum(sublimation_daily, na.rm = T))
Tussock_sub$type <- "Sublimation"
Tussock_sub$tower <- "MS"
Tussock_cond <- Tussock_daily %>%
  group_by(WaterYear) %>%
  filter(WaterYear == 2013 | WaterYear == 2014 | WaterYear == 2016 | WaterYear == 2017 | WaterYear == 2018 | WaterYear == 2019 | WaterYear == 2020 | WaterYear == 2021) %>%
  summarise(mmH20 = sum(abs(Condensation_daily), na.rm = T))
Tussock_cond$type <- "Condensation"
Tussock_cond$tower <- "MS"
Tussock_evap <- Tussock_daily %>%
  group_by(WaterYear) %>%
  filter(WaterYear == 2013 | WaterYear == 2014 | WaterYear == 2016 | WaterYear == 2017 | WaterYear == 2018 | WaterYear == 2019 | WaterYear == 2020 | WaterYear == 2021) %>%
  summarise(mmH20 = sum(Evaporation_daily, na.rm = T))
Tussock_evap$type <- "Evapotranspiration"
Tussock_evap$tower <- "MS"
Tussock_dep <- Tussock_daily %>%
  group_by(WaterYear) %>%
  filter(WaterYear == 2013 | WaterYear == 2014 | WaterYear == 2016 | WaterYear == 2017 | WaterYear == 2018 | WaterYear == 2019 | WaterYear == 2020 | WaterYear == 2021) %>%
  summarise(mmH20 = sum(abs(Deposition_daily), na.rm = T))
Tussock_dep$type <- "Deposition"
Tussock_dep$tower <- "MS"

Tussock_flux <- rbind(Tussock_evap, Tussock_sub, Tussock_cond, Tussock_dep)


Tussock_flux %>%
  mutate(type = factor(type, levels=c("Condensation", "Deposition", "Sublimation", "Evapotranspiration"))) %>%
  ggplot(aes(x = WaterYear, y = mmH20, fill = type)) + 
  geom_bar(stat = "identity", color = "black") +
  guides(fill = guide_legend(title = " ")) +
  # scale_fill_brewer(palette = "YlGnBu") + #Paired,YlGnBu, BrBG
  scale_fill_viridis(discrete = T, option = "G") +
  labs(title = "Tussock",
       y = "Water Vapor Flux (mm H2O/year)",
       x = "Water Year")


tower_flux_IB <- rbind(Tussock_flux, Fen_flux, Ridge_flux)


tower_flux_IB %>%
  mutate(type = factor(type, levels=c("Condensation", "Deposition", "Sublimation", "Evapotranspiration"))) %>%
  #filter(WaterYear == 2013 | WaterYear == 2014 | WaterYear == 2016 | WaterYear == 2017 | WaterYear == 2018 | WaterYear == 2019 | WaterYear == 2020) %>%
  filter(WaterYear > 2015 & WaterYear < 2022) %>%
  ggplot()+
  geom_bar(aes(x = tower, y = mmH20, fill = type), position = "stack", stat = "identity", color = "black") +  #position = 'dodge', width = 0.9
  facet_wrap(~WaterYear, nrow = 1) +
  labs(y = "Water Vapor Flux (mm H2O/year)", 
       caption = "MS=Mid-Slope  R=Ridge  VB=Valley Bottom", 
       x = " ",
       title = "Comparison of all moisture fluxes by water year at Imnavait Creek")+
  guides(fill = guide_legend(title = " ")) +
  scale_fill_viridis(discrete = T, option = "D", direction = -1, alpha = 0.9) + #OR "D" ("G" if type order reversed)
  theme_bw(base_size = 15)

tower_flux_IB %>%
            # group_by(tower)%>%
            #filter(WaterYear == 2013 | WaterYear == 2014 | WaterYear == 2016 | WaterYear == 2017 | WaterYear == 2018 | WaterYear == 2019 | WaterYear == 2020 | WaterYear == 2021) %>%
            summarise(mean_evap = mean(mmH20[type == "Evapotranspiration"]),
                      SD_evap = sd(mmH20[type == "Evapotranspiration"]),
                      mean_subl = mean(mmH20[type == "Sublimation"]), 
                      sd_subl = sd(mmH20[type == "Sublimation"]),
                      mean_cond = mean(mmH20[type == "Condensation"]), 
                      sd_cond = sd(mmH20[type == "Condensation"]),
                      mean__dep = mean(mmH20[type == "Deposition"]),
                      sd_dep = sd(mmH20[type == "Deposition"]))
#Table 3

#BONANZA
FenBC_sub <- FenBC_daily %>%
  group_by(WaterYear) %>%
  filter((WaterYear == 2015) | (WaterYear > 2016 & WaterYear <2022)) %>%
  summarise(mmH20 = sum(sublimation_daily, na.rm = T))
FenBC_sub$type <- "Sublimation"
FenBC_sub$tower <- "Fen"
FenBC_cond <- FenBC_daily %>%
  group_by(WaterYear) %>%
  filter((WaterYear == 2015) | (WaterYear > 2016 & WaterYear <2022)) %>%
  summarise(mmH20 = sum(abs(Condensation_daily), na.rm = T))
FenBC_cond$type <- "Condensation"
FenBC_cond$tower <- "Fen"
FenBC_evap <- FenBC_daily %>%
  group_by(WaterYear) %>%
  filter((WaterYear == 2015) | (WaterYear > 2016 & WaterYear <2022)) %>%
  summarise(mmH20 = sum(Evaporation_daily, na.rm = T))
FenBC_evap$type <- "Evapotranspiration"
FenBC_evap$tower <- "Fen"
FenBC_dep <- FenBC_daily %>%
  group_by(WaterYear) %>%
  filter((WaterYear == 2015) | (WaterYear > 2016 & WaterYear <2022)) %>%
  summarise(mmH20 = sum(abs(Deposition_daily), na.rm = T))
FenBC_dep$type <- "Deposition"
FenBC_dep$tower <- "Fen"

FenBC_flux <- rbind(FenBC_evap, FenBC_sub, FenBC_cond, FenBC_dep)


FenBC_flux %>%
  mutate(type = factor(type, levels=c("Condensation", "Deposition", "Sublimation", "Evapotranspiration"))) %>%
  ggplot(aes(x = WaterYear, y = mmH20, fill = type)) + 
  geom_bar(stat = "identity", color = "black") +
  guides(fill = guide_legend(title = " ")) +
  # scale_fill_brewer(palette = "YlGnBu") + #Paired,YlGnBu, BrBG
  scale_fill_viridis(discrete = T, option = "G") +
  labs(title = "FenBC",
       y = "Water Vapor Flux (mm H2O/year)",
       x = "Water Year")

bog_sub <- bog_daily %>%
  group_by(WaterYear) %>%
  filter(WaterYear > 2013 & WaterYear < 2022) %>%
  summarise(mmH20 = sum(sublimation_daily, na.rm = T))
bog_sub$type <- "Sublimation"
bog_sub$tower <- "Bog"
bog_cond <- bog_daily %>%
  group_by(WaterYear) %>%
  filter(WaterYear > 2013 & WaterYear < 2022) %>%
  summarise(mmH20 = sum(abs(Condensation_daily), na.rm = T))
bog_cond$type <- "Condensation"
bog_cond$tower <- "Bog"
bog_evap <- bog_daily %>%
  group_by(WaterYear) %>%
  filter(WaterYear > 2013 & WaterYear < 2022) %>%
  summarise(mmH20 = sum(Evaporation_daily, na.rm = T))
bog_evap$type <- "Evapotranspiration"
bog_evap$tower <- "Bog"
bog_dep <- bog_daily %>%
  group_by(WaterYear) %>%
  filter(WaterYear > 2013 & WaterYear < 2022) %>%
  summarise(mmH20 = sum(abs(Deposition_daily), na.rm = T))
bog_dep$type <- "Deposition"
bog_dep$tower <- "Bog"

bog_flux <- rbind(bog_evap, bog_sub, bog_cond, bog_dep)


bog_flux %>%
  mutate(type = factor(type, levels=c("Condensation", "Deposition", "Sublimation", "Evapotranspiration"))) %>%
  ggplot(aes(x = WaterYear, y = mmH20, fill = type)) + 
  geom_bar(stat = "identity", color = "black") +
  guides(fill = guide_legend(title = " ")) +
  # scale_fill_brewer(palette = "YlGnBu") + #Paired,YlGnBu, BrBG
  scale_fill_viridis(discrete = T, option = "G") +
  labs(title = "bog",
       y = "Water Vapor Flux (mm H2O/year)",
       x = "Water Year")


BS_sub <- BS_daily %>%
  group_by(WaterYear) %>%
  filter(WaterYear == 2011 | WaterYear == 2012 | (WaterYear > 2013 & WaterYear < 2022)) %>%
  summarise(mmH20 = sum(sublimation_daily, na.rm = T))
BS_sub$type <- "Sublimation"
BS_sub$tower <- "BS"
BS_cond <- BS_daily %>%
  group_by(WaterYear) %>%
  filter(WaterYear == 2011 | WaterYear == 2012 | (WaterYear > 2013 & WaterYear < 2022)) %>%
  summarise(mmH20 = sum(abs(Condensation_daily), na.rm = T))
BS_cond$type <- "Condensation"
BS_cond$tower <- "BS"
BS_evap <- BS_daily %>%
  group_by(WaterYear) %>%
  filter(WaterYear == 2011 | WaterYear == 2012 | (WaterYear > 2013 & WaterYear < 2022)) %>%
  summarise(mmH20 = sum(Evaporation_daily, na.rm = T))
BS_evap$type <- "Evapotranspiration"
BS_evap$tower <- "BS"
BS_dep <- BS_daily %>%
  group_by(WaterYear) %>%
  filter(WaterYear == 2011 | WaterYear == 2012 | (WaterYear > 2013 & WaterYear < 2022)) %>%
  summarise(mmH20 = sum(abs(Deposition_daily), na.rm = T))
BS_dep$type <- "Deposition"
BS_dep$tower <- "BS"

BS_flux <- rbind(BS_evap, BS_sub, BS_cond, BS_dep)


BS_flux %>%
  mutate(type = factor(type, levels=c("Condensation", "Deposition", "Sublimation", "Evapotranspiration"))) %>%
  ggplot(aes(x = WaterYear, y = mmH20, fill = type)) + 
  geom_bar(stat = "identity", color = "black") +
  guides(fill = guide_legend(title = " ")) +
  # scale_fill_brewer(palette = "YlGnBu") + #Paired,YlGnBu, BrBG
  scale_fill_viridis(discrete = T, option = "G") +
  labs(title = "BS",
       y = "Water Vapor Flux (mm H2O/year)",
       x = "Water Year")


tower_flux_BC <- rbind(FenBC_flux, bog_flux, BS_flux)


tower_flux_BC %>%
  mutate(type = factor(type, levels=c("Condensation", "Deposition", "Sublimation", "Evapotranspiration"))) %>%
  #filter(WaterYear == 2013 | WaterYear == 2014 | WaterYear == 2016 | WaterYear == 2017 | WaterYear == 2018 | WaterYear == 2019 | WaterYear == 2020) %>%
  filter(WaterYear > 2015 & WaterYear < 2022) %>%
  ggplot()+
  geom_bar(aes(x = tower, y = mmH20, fill = type), position = "stack", stat = "identity", color = "black") +  #position = 'dodge', width = 0.9
  facet_wrap(~WaterYear, nrow = 1) +
  labs(y = "Water Vapor Flux (mm H2O/year)", 
       caption = "BS=Black Spruce", 
       x = " ",
       title = "Comparison of all moisture fluxes by water year at Bonanza Creek")+
  guides(fill = guide_legend(title = " ")) +
  scale_fill_viridis(discrete = T, option = "D", direction = -1, alpha = 0.9) + #OR "D" ("G" if type order reversed)
  theme_bw(base_size = 15)

tower_flux_BC %>%
  # group_by(tower)%>%
  #filter(WaterYear == 2013 | WaterYear == 2014 | WaterYear == 2016 | WaterYear == 2017 | WaterYear == 2018 | WaterYear == 2019 | WaterYear == 2020 | WaterYear == 2021) %>%
  summarise(mean_evap = mean(mmH20[type == "Evapotranspiration"]),
            SD_evap = sd(mmH20[type == "Evapotranspiration"]),
            mean_subl = round(mean(mmH20[type == "Sublimation"]), 0), 
            sd_subl = round(sd(mmH20[type == "Sublimation"]), 0),
            mean_cond = mean(mmH20[type == "Condensation"]), 
            sd_cond = sd(mmH20[type == "Condensation"]),
            mean__dep = mean(mmH20[type == "Deposition"]),
            sd_dep = sd(mmH20[type == "Deposition"]))
#Table 3

#===============================================================================
#MONTHLY AND ANNUAL PLOTS ======================================================
#===============================================================================

#IMNAVAIT MONTHLY
#sum of monthly sublimation 
TussockStats_month <- Tussock_daily %>%
  filter(Tussock_daily$WaterYear == 2013 | Tussock_daily$WaterYear == 2014 | Tussock_daily$WaterYear == 2016 | Tussock_daily$WaterYear == 2017 | Tussock_daily$WaterYear == 2018 | Tussock_daily$WaterYear == 2019 | Tussock_daily$WaterYear == 2020 | WaterYear == 2021) %>%
  group_by(month, WaterYear) %>%
  summarise(daily_subl_sum = sum(sublimation_daily, na.rm = TRUE)) 
#summarise(mean_daily_subl = mean(sublimation_daily, na.rm = TRUE))
TussockStats_month <- TussockStats_month %>%
  group_by(month) %>%
  summarise(monthly_subl_mean = mean(daily_subl_sum, na.rm = TRUE),
            SE = sd(daily_subl_sum)/ sqrt(length(daily_subl_sum)), 
            SD = sd(daily_subl_sum),
            CV = sd(daily_subl_sum)/mean(daily_subl_sum))

FenStats_month <- Fen_daily %>%
  filter((WaterYear > 2009 & WaterYear < 2013) | (WaterYear > 2014 & WaterYear <2022)) %>%
  group_by(month, WaterYear) %>%
  summarise(daily_subl_sum = sum(sublimation_daily, na.rm = TRUE)) 
#summarise(mean_daily_subl = mean(sublimation_daily, na.rm = TRUE))
FenStats_month <- FenStats_month %>%
  group_by(month) %>%
  summarise(monthly_subl_mean = mean(daily_subl_sum, na.rm = TRUE),
            SE = sd(daily_subl_sum)/ sqrt(length(daily_subl_sum)), 
            SD = sd(daily_subl_sum),
            CV = sd(daily_subl_sum)/mean(daily_subl_sum))


RidgeStats_month <- Ridge_daily %>%
  filter(WaterYear > 2009 & WaterYear < 2022) %>%
  group_by(month, WaterYear) %>%
  summarise(daily_subl_sum = sum(sublimation_daily, na.rm = TRUE)) 

RidgeStats_month <- RidgeStats_month %>%
  group_by(month) %>%
  summarise(monthly_subl_mean = mean(daily_subl_sum, na.rm = TRUE),
            SE = sd(daily_subl_sum)/ sqrt(length(daily_subl_sum)), 
            SD = sd(daily_subl_sum),
            CV = sd(daily_subl_sum)/mean(daily_subl_sum))


RidgeStats_month$Site <- "US-ICh"
FenStats_month$Site <- "US-ICs"
TussockStats_month$Site <- "US-ICt"

tower_month_IB <- rbind(RidgeStats_month, FenStats_month, TussockStats_month)

tower_month_IB %>%
  group_by(month) %>%
  summarise(CV = mean(CV, na.rm=T),
            mean = mean(monthly_subl_mean, na.rm=T))

IB_month <- tower_month_IB %>%
  mutate(month = factor(month, levels=c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))) %>%
  ggplot(aes(x = month, y = monthly_subl_mean, fill = Site)) + 
  geom_bar(position = position_dodge(width=0.9), stat = "identity", color = "black") +
  labs(y = " ",
       x = " ",
       subtitle = "a)")+
  # ylim(0,3.1)+
  geom_errorbar(aes(ymin=monthly_subl_mean-SE, ymax=monthly_subl_mean+SE), 
                size=0.3, 
                width = 0.3,
                position = position_dodge(width=0.9))+
  # geom_col(position = "dodge", stat = "identity", color = "black") +
  scale_fill_viridis(discrete = T, option = "E") +
  theme_bw(base_size = 18) + 
  theme(axis.text.x=element_blank()) +
  ylim(0,10)

#----------------
#BONANZA MONTHLY
bog_month <- bog_daily %>%
  filter(WaterYear > 2013 & WaterYear<2022) %>%
  group_by(month, WaterYear) %>%
  summarise(daily_subl_sum = sum(sublimation_daily, na.rm = TRUE)) 
#summarise(mean_daily_subl = mean(sublimation_daily, na.rm = TRUE))
bog_month <- bog_month %>%
  group_by(month) %>%
  summarise(monthly_subl_mean = mean(daily_subl_sum, na.rm = TRUE),
            SE = sd(daily_subl_sum)/ sqrt(length(daily_subl_sum)), 
            SD = sd(daily_subl_sum),
            CV = sd(daily_subl_sum)/mean(daily_subl_sum))


FenBC_month <- FenBC_daily %>%
  filter(WaterYear == 2015 | (WaterYear > 2016 & WaterYear < 2022)) %>%
  group_by(month, WaterYear) %>%
  summarise(daily_subl_sum = sum(sublimation_daily, na.rm = TRUE)) 
#summarise(mean_daily_subl = mean(sublimation_daily, na.rm = TRUE))
FenBC_month <- FenBC_month %>%
  group_by(month) %>%
  summarise(monthly_subl_mean = mean(daily_subl_sum, na.rm = TRUE),
            SE = sd(daily_subl_sum)/ sqrt(length(daily_subl_sum)), 
            SD = sd(daily_subl_sum),
            CV = sd(daily_subl_sum)/mean(daily_subl_sum))


BS_month <- BS_daily %>%
  filter((WaterYear > 2013 & WaterYear<2022) | (WaterYear > 2010 & WaterYear < 2013)) %>%
  group_by(month, WaterYear) %>%
  summarise(daily_subl_sum = sum(sublimation_daily, na.rm = TRUE)) 

BS_month <- BS_month %>%
  group_by(month) %>%
  summarise(monthly_subl_mean = mean(daily_subl_sum, na.rm = TRUE),
            SE = sd(daily_subl_sum)/ sqrt(length(daily_subl_sum)), 
            SD = sd(daily_subl_sum),
            CV = sd(daily_subl_sum)/mean(daily_subl_sum))


BS_month$Site <- "US-BZS"
FenBC_month$Site <- "US-BZF"
bog_month$Site <- "US-BZB"

tower_month_BC <- rbind(BS_month, FenBC_month, bog_month)

tower_month_BC %>%
  group_by(Site) %>%
  summarise(CV = mean(CV, na.rm=T))

tower_month_BC %>%
  group_by(month) %>%
  summarise(CV = mean(CV, na.rm=T),
            mean = mean(monthly_subl_mean, na.rm=T))


BC_month <- tower_month_BC %>%
  mutate(month = factor(month, levels=c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))) %>%
  ggplot(aes(x = month, y = monthly_subl_mean, fill = Site)) + 
  geom_bar(position = position_dodge(width=0.9), stat = "identity", color = "black") +
  labs(y = " ",
       x = " ",
       subtitle = "b)",
       # subtitle = "WYs 2011-2012 & 2014-2021 for Black Spruce site; WYs 2014-2021 for Bog site; WYs 2015 & 2017-2021 for Fen site",
       #caption = "Error bars represent the standard error of the mean"
  )+
  # ylim(0,3.1)+
  geom_errorbar(aes(ymin=monthly_subl_mean-SE, ymax=monthly_subl_mean+SE), 
                size=0.3, 
                width = 0.3,
                position = position_dodge(width=0.9))+
  # geom_col(position = "dodge", stat = "identity", color = "black") +
  scale_fill_viridis(discrete = T, option = "D") +
  theme_bw(base_size = 18)

#-----------------
#COMBINED MONTHLY
grid.arrange(IB_month, BC_month, ncol = 1, left = ("Sublimation (mm H2O/month)"))  

library(grid)
grid.arrange(IB_month, BC_month, ncol=1,
             left = textGrob("Sublimation (mm H2O/month)", rot=90,gp=gpar(fontsize=18)))



#---------------ANNUAL PLOTS----------------------------------------------------

#IMNAVAIT ANNUAL

#grouped barplots 
Fen_year <- Fen_daily %>%
  group_by(WaterYear) %>%
  filter(WaterYear > 2009 & WaterYear < 2022) %>%
  summarise(sublimation = sum(sublimation_daily))
#remove years 2013,2014
Fen_year$sublimation[4:5]<- NA
Fen_year$Site <- "US-ICs"
Fen_year$Trees <- "Treeless"
Fen_year$Region <- "Tundra"

Ridge_year <- Ridge_daily %>%
  group_by(WaterYear) %>%
  filter(WaterYear > 2009 & WaterYear < 2022) %>%
  summarise(sublimation = sum(sublimation_daily))
Ridge_year$Site <- "US-ICh"
Ridge_year$Trees <- "Treeless"
Ridge_year$Region <- "Tundra"


Tussock_year <- Tussock_daily %>%
  group_by(WaterYear) %>%
  filter(WaterYear > 2009 & WaterYear < 2022) %>%
  summarise(sublimation = sum(sublimation_daily))
#remove 2010, 2011, 2012, 2015
Tussock_year$sublimation[1:3]<- NA
Tussock_year$sublimation[6]<- NA
Tussock_year$Site <- "US-ICt"
Tussock_year$Trees <- "Treeless"
Tussock_year$Region <- "Tundra"

tower_year_IB <- rbind(Fen_year, Ridge_year, Tussock_year)

IB_year <- tower_year_IB %>%
  # filter(WaterYear > 2010 & WaterYear < 2022) %>%
  ggplot(aes(x = WaterYear, y = sublimation, fill = Site)) + 
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  labs(y = "",
       x = "", 
       title = "a)")+
  # ylim(0,30)+
  # geom_col(position = "dodge", stat = "identity", color = "black") +
  scale_fill_viridis(discrete = T, direction = 1, option = "E") +
  theme_bw(base_size = 18)+
  ylim(0,50)

#CV
ooh <- tower_year_IB %>%
  group_by(Site)%>%
  filter(sublimation>0)%>%
  summarise(mean = mean(sublimation, na.rm=T),
            CV_year = sd(sublimation, na.rm = T)/mean(sublimation, na.rm = T)*100,
            SD_year = sd(sublimation, na.rm=T))
mean(ooh$CV_year) #40

oof <- tower_year_IB %>%
  group_by(WaterYear) %>%
  filter(sublimation>0)%>%
  summarise(CV_site = sd(sublimation, na.rm=T)/mean(sublimation, na.rm=T)*100,
            mean_site = mean(sublimation, na.rm=T),
            SD_site = sd(sublimation, na.rm=T)) 
mean(oof$CV_site) #21 

#greater variability between years than between sites 

#---------------
#BONANAZA ANNUAL

FenBC_year <- FenBC_daily %>%
  group_by(WaterYear) %>%
  #filter(WaterYear == 2015 | (WaterYear > 2016 & WaterYear < 2022)) %>%
  summarise(sublimation = sum(sublimation_daily))
FenBC_year$sublimation[1:4]<- NA
FenBC_year$sublimation[6]<- NA
FenBC_year$Site <- "US-BZF"
FenBC_year$Trees <- "Treeless"
FenBC_year$Region <- "Lowland Boreal"


bog_year <- bog_daily %>%
  group_by(WaterYear) %>%
  #filter(WaterYear>2013 & WaterYear<2022) %>%
  summarise(sublimation = sum(sublimation_daily))
bog_year$sublimation[1:3]<- NA
bog_year$Site <- "US-BZB"
bog_year$Trees <- "Treeless"
bog_year$Region <- "Lowland Boreal"

BS_year <- BS_daily %>%
  group_by(WaterYear) %>%
  #  filter((WaterYear > 2013 & WaterYear<2022) | (WaterYear > 2010 & WaterYear < 2013)) %>%
  summarise(sublimation = sum(sublimation_daily))
BS_year$sublimation[4]<- NA
BS_year$Site <- "US-BZS"
BS_year$Trees <- "Trees"
BS_year$Region <- "Lowland Boreal"


towerBC_year <- rbind(FenBC_year, bog_year, BS_year)


BC_year <- towerBC_year %>%
  filter(WaterYear < 2022) %>%
  #filter(WaterYear == 2015 | (WaterYear > 2016 & WaterYear < 2022)) %>%
  ggplot(aes(x = WaterYear, y = sublimation, fill = Site)) + 
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  labs(
    y = "",
    x = "", 
    title = "b)")+
  # ylim(0,30)+
  # geom_col(position = "dodge", stat = "identity", color = "black") +
  scale_fill_viridis(discrete = T, direction = 1, option = "D") +
  theme_bw(base_size = 18)+
  ylim(0,50)

grid.arrange(IB_year, BC_year, ncol=1,
             left = textGrob("Sublimation (mm H2O/year)", rot=90,gp=gpar(fontsize=18)))

#CV
ooh <- towerBC_year %>%
  group_by(Site)%>%
  filter(sublimation>0)%>%
  summarise(mean = mean(sublimation),
            CV_year = sd(sublimation)/mean(sublimation)*100,
            SD_year = sd(sublimation))
mean(ooh$CV_year) #32

oof <- towerBC_year %>%
  group_by(WaterYear) %>%
  filter(sublimation>0)%>%
  summarise(CV_site = sd(sublimation)/mean(sublimation)*100,
            mean_site = mean(sublimation),
            SD_site = sd(sublimation)) 
mean(oof$CV_site, na.rm = T) #32

#variability between years and variability between sites comparable; notably different than tundra sites. 

#===============================================================================
#============= ANALYSIS -- CORRELATIONS & REGRESSIONS ==========================
#===============================================================================

#IMNAVAIT

Fen$temp_gradient <- Fen$SoilTemp - Fen$AirTemp
Ridge$temp_gradient <- Ridge$SoilTemp - Ridge$AirTemp
Tussock$temp_gradient <- Tussock$SoilTemp - Tussock$AirTemp

Fen$DoWY <- mutate(Fen, DoWY = ifelse(DoY > 274, DoY-274, 91 + DoY))
Ridge$DoWY <- mutate(Ridge, DoWY = ifelse(DoY > 274, DoY-274, 91 + DoY))
Tussock$DoWY <- mutate(Tussock, DoWY = ifelse(DoY > 274, DoY-274, 91 + DoY))

#-------------------
#CORRELATIONS HOURLY -- TABLE 4
#Pearson / Bivariate Correlation Coefficients 
Fen2 <- Fen %>%
  filter((WaterYear > 2009 & WaterYear < 2013) | (WaterYear > 2014 & WaterYear <2022)) %>%
  select(Date, WaterYear, Hour, LE, AirTemp, WindSpeed, RH, VPD, NetRadiation, SoilTemp, flux_hourly, Sublimation_hourly, temp_gradient)


Fen3 <- Fen2 %>%
  filter(Sublimation_hourly > 0)

cor(Fen3[,5:13], use = "pairwise.complete.obs", method = "pearson")



Ridge2 <- Ridge %>%
  filter(WaterYear == 2013 | WaterYear == 2014 | WaterYear == 2016 | WaterYear == 2017 | WaterYear == 2018 | WaterYear == 2019 | WaterYear == 2020) %>%
  select(Date, WaterYear, Hour, LE, AirTemp, WindSpeed, RH, VPD, NetRadiation, SoilTemp, flux_hourly, Sublimation_hourly, temp_gradient)
Ridge3 <- Ridge2 %>%
  filter(Sublimation_hourly > 0)

cor(Ridge3[,5:13], use = "pairwise.complete.obs", method = "pearson")


Tussock2 <- Tussock %>%
  filter(WaterYear == 2013 | WaterYear == 2014 | WaterYear == 2016 | WaterYear == 2017 | WaterYear == 2018 | WaterYear == 2019 | WaterYear == 2020) %>%
  select(Date, WaterYear, Hour, LE, AirTemp, WindSpeed, RH, VPD, NetRadiation, SoilTemp, flux_hourly, Sublimation_hourly, temp_gradient)

Tussock3 <- Tussock2 %>%
  filter(Sublimation_hourly > 0)
 
cor(Tussock3[,5:13], use = "pairwise.complete.obs", method = "pearson")

FenBC$temp_gradient <- FenBC$SoilTemp - FenBC$AirTemp
bog$temp_gradient <- bog$SoilTemp - bog$AirTemp
BS$temp_gradient <- BS$SoilTemp - BS$AirTemp

bog2 <- bog %>%
  filter(WaterYear > 2013 & WaterYear < 2022) %>%
  filter(Sublimation_hourly > 0) %>%
  select(Date, WaterYear, Hour, LE, AirTemp, WindSpeed, RH, VPD, NetRadiation, SoilTemp, temp_gradient, flux_hourly, Sublimation_hourly)

cor(bog2[,5:13], use = "pairwise.complete.obs", method = "pearson")


FenBC2 <- FenBC %>%
  filter(WaterYear > 2016 & WaterYear < 2022 | WaterYear == 2015) %>%
  filter(Sublimation_hourly>0) %>%
  select(Date, WaterYear, Hour, LE, AirTemp, WindSpeed, RH, VPD, NetRadiation, SoilTemp, temp_gradient, flux_hourly, Sublimation_hourly)

cor(FenBC2[,5:13], use = "pairwise.complete.obs", method = "pearson")


BS2 <- BS %>%
  filter(WaterYear > 2013 & WaterYear < 2022 | WaterYear == 2011 | WaterYear==2012) %>%
  filter(Sublimation_hourly>0) %>%
  select(Date, WaterYear, Hour, LE, AirTemp, WindSpeed, RH, VPD, NetRadiation, SoilTemp, temp_gradient, flux_hourly, Sublimation_hourly)

cor(BS2[,5:13], use = "pairwise.complete.obs", method = "pearson")


#------------------
#CORRELATIONS DAILY -- TABLE 4
Fen_daily$temp_gradient <- Fen_daily$SoilTemp - Fen_daily$AirTemp
Ridge_daily$temp_gradient <- Ridge_daily$SoilTemp - Ridge_daily$AirTemp
Tussock_daily$temp_gradient <- Tussock_daily$SoilTemp - Tussock_daily$AirTemp

Fen_daily2 <- Fen_daily %>%
  filter((WaterYear>2009 & WaterYear<2013) | (WaterYear>2014 & WaterYear< 2022)) %>%
  filter(sublimation_daily > 0) %>%
  select(Date, WaterYear, DoWY, month, flux_daily, sublimation_daily, RH_daily_ave, VPD_daily_ave, AT_daily_ave, wind_daily_mean, daily_sum_NRad, temp_gradient,
         soil_temp_daily_ave, surf_temp_daily_ave)

cor(Fen_daily2[,5:13], use = "pairwise.complete.obs", method = "pearson")

Ridge_daily2 <- Ridge_daily %>%
  filter(WaterYear>2009 & WaterYear<2022) %>%
  filter(sublimation_daily > 0) %>%
  select(Date, WaterYear, DoWY, month, flux_daily, sublimation_daily, RH_daily_ave, VPD_daily_ave, 
         AT_daily_ave, wind_daily_mean, daily_sum_NRad, temp_gradient,
         soil_temp_daily_ave)

cor(Ridge_daily2[,5:13], use = "pairwise.complete.obs", method = "pearson")

Tussock_daily2 <- Tussock_daily %>%
  #filter(WaterYear > 2009 & WaterYear < 2021) %>%
  filter(WaterYear == 2013 | WaterYear == 2014 | WaterYear == 2016 | WaterYear == 2017 | WaterYear == 2018 | WaterYear == 2019 | WaterYear == 2020) %>%
  filter(sublimation_daily > 0) %>%
  select(Date, WaterYear, DoWY, month, flux_daily, sublimation_daily, RH_daily_ave, VPD_daily_ave, 
         AT_daily_ave, wind_daily_mean, daily_sum_NRad,
         soil_temp_daily_ave, temp_gradient)
cor(Tussock_daily2[,5:13], use = "pairwise.complete.obs", method = "pearson")


FenBC_daily$temp_gradient <- FenBC_daily$SoilTemp - FenBC_daily$AirTemp
bog_daily$temp_gradient <- bog_daily$SoilTemp - bog_daily$AirTemp
BS_daily$temp_gradient <- BS_daily$SoilTemp - BS_daily$AirTemp


BS_daily2 <- BS_daily %>%
  #filter(WaterYear > 2009 & WaterYear < 2021) %>%
  filter(WaterYear == 2013 | WaterYear == 2014 | WaterYear == 2016 | WaterYear == 2017 | WaterYear == 2018 | WaterYear == 2019 | WaterYear == 2020) %>%
  filter(sublimation_daily > 0) %>%
  select(Date, WaterYear, DoWY, month, flux_daily, RH_daily_ave, VPD_daily_ave, AT_daily_ave, wind_daily_mean, daily_sum_NRad, temp_gradient,
         soil_temp_daily_ave, sublimation_daily)


cor(BS_daily2[,5:13], use = "pairwise.complete.obs", method = "pearson")


FenBC_daily2 <- FenBC_daily %>%
  #filter(WaterYear > 2009 & WaterYear < 2021) %>%
  filter(WaterYear > 2016 & WaterYear < 2022 | WaterYear == 2015) %>%
  filter(sublimation_daily > 0) %>%
  select(Date, WaterYear, DoWY, month, flux_daily, RH_daily_ave, VPD_daily_ave, AT_daily_ave, wind_daily_mean, daily_sum_NRad, temp_gradient,
         soil_temp_daily_ave, sublimation_daily)

cor(FenBC_daily2[,5:13], use = "pairwise.complete.obs", method = "pearson")


bog_daily2 <- bog_daily %>%
  filter(WaterYear > 2013 & WaterYear < 2022) %>%
  filter(sublimation_daily > 0) %>%
  select(Date, WaterYear, DoWY, month, flux_daily, RH_daily_ave, VPD_daily_ave, AT_daily_ave, wind_daily_mean, daily_sum_NRad, temp_gradient,
         soil_temp_daily_ave, sublimation_daily)

cor(bog_daily2[,5:13], use = "pairwise.complete.obs", method = "pearson")


#-----------------------------------------------
#SINGLE LINEAR OLS REGRESSIONS ON HALF HOUR DATA
attach(Fen3)
summary(lm(Sublimation_hourly~AirTemp))
summary(lm(Sublimation_hourly~WindSpeed))
summary(lm(Sublimation_hourly~RH))
summary(lm(Sublimation_hourly~VPD))
summary(lm(Sublimation_hourly~NetRadiation))
summary(lm(Sublimation_hourly~SoilTemp))
detach(Fen3)
#as expected, all OLS regressions p < 0.05


FenLM <- Fen %>%
  filter(Sublimation_hourly > 0) %>%
  filter((WaterYear>2009 & WaterYear<2013) | (WaterYear>2014 & WaterYear< 2022)) %>%
  mutate(lm_Ta = summary(lm(Sublimation_hourly ~ AirTemp))$r.squared,
         lm_VPD = summary(lm(Sublimation_hourly ~ VPD))$r.squared,
         lm_u = summary(lm(Sublimation_hourly ~ WindSpeed))$r.squared,
         lm_RH = summary(lm(Sublimation_hourly ~ RH))$r.squared,
         lm_Rnet = summary(lm(Sublimation_hourly ~ NetRadiation))$r.squared,
         lm_Ts = summary(lm(Sublimation_hourly ~ SoilTemp))$r.squared,
         lm_TG = summary(lm(Sublimation_hourly ~ temp_gradient))$r.squared)

attach(FenLM)
c(mean(lm_Ta), mean(lm_u), mean(lm_RH), mean(lm_VPD), mean(lm_Rnet), mean(lm_TG))
detach(FenLM)
#data used in table 5

attach(Ridge3)
summary(lm(Sublimation_hourly~AirTemp))
summary(lm(Sublimation_hourly~WindSpeed))
summary(lm(Sublimation_hourly~RH))
summary(lm(Sublimation_hourly~VPD))
summary(lm(Sublimation_hourly~NetRadiation))
summary(lm(Sublimation_hourly~SoilTemp))
detach(Ridge3)
#all OLS regressions p < 0.05

RidgeLM <- Ridge %>%
  filter(Sublimation_hourly > 0) %>%
  filter(WaterYear>2009 & WaterYear<2022) %>%
  mutate(lm_Ta = summary(lm(Sublimation_hourly ~ AirTemp))$r.squared,
         lm_VPD = summary(lm(Sublimation_hourly ~ VPD))$r.squared,
         lm_u = summary(lm(Sublimation_hourly ~ WindSpeed))$r.squared,
         lm_RH = summary(lm(Sublimation_hourly ~ RH))$r.squared,
         lm_Rnet = summary(lm(Sublimation_hourly ~ NetRadiation))$r.squared,
         lm_Ts = summary(lm(Sublimation_hourly ~ SoilTemp))$r.squared,
         lm_TG = summary(lm(Sublimation_hourly ~ temp_gradient))$r.squared)

attach(RidgeLM)
c(mean(lm_Ta), mean(lm_u), mean(lm_RH), mean(lm_VPD), mean(lm_Rnet), mean(lm_TG))
detach(RidgeLM)
#data used in Table 5

attach(Tussock3)
summary(lm(Sublimation_hourly~AirTemp))
summary(lm(Sublimation_hourly~WindSpeed))
summary(lm(Sublimation_hourly~RH))
summary(lm(Sublimation_hourly~VPD))
summary(lm(Sublimation_hourly~NetRadiation))
summary(lm(Sublimation_hourly~SoilTemp))
detach(Tussock3)
#all OLS regressions p < 0.05

TussockLM <- Tussock %>%
  filter(Sublimation_hourly > 0) %>%
  filter(WaterYear == 2013 | WaterYear == 2014 | WaterYear == 2016 | WaterYear == 2017 | WaterYear == 2018 | WaterYear == 2019 | WaterYear == 2020) %>%
  mutate(lm_Ta = summary(lm(Sublimation_hourly ~ AirTemp))$r.squared,
         lm_VPD = summary(lm(Sublimation_hourly ~ VPD))$r.squared,
         lm_u = summary(lm(Sublimation_hourly ~ WindSpeed))$r.squared,
         lm_RH = summary(lm(Sublimation_hourly ~ RH))$r.squared,
         lm_Rnet = summary(lm(Sublimation_hourly ~ NetRadiation))$r.squared,
         lm_Ts = summary(lm(Sublimation_hourly ~ SoilTemp))$r.squared,
         lm_TG = summary(lm(Sublimation_hourly ~ temp_gradient))$r.squared)

attach(TussockLM)
c(mean(lm_Ta), mean(lm_u), mean(lm_RH), mean(lm_VPD), mean(lm_Rnet), mean(lm_TG))
detach(TussockLM)
#data used in Table 5

#---------------------------------------------
#MULTIPLE LINEAR REGRESSIONS ON HALF HOUR DATA 
attach(Fen3)
summary(lm(Sublimation_hourly~AirTemp*VPD*NetRadiation*temp_gradient*WindSpeed)) #fully crossed MLR used in table 5
detach(Fen3)

attach(Ridge3)
summary(lm(Sublimation_hourly~AirTemp*VPD*NetRadiation*temp_gradient*WindSpeed)) #fully crossed MLR used in table 5
detach(Ridge3)

attach(Tussock3)
summary(lm(Sublimation_hourly~AirTemp*VPD*NetRadiation*temp_gradient*WindSpeed)) #fully crossed MLR used in table 5
detach(Tussock3)

#----------------------------------------------------------
#MULTIPLE LINEAR REGRESSIONS ON DAILY DATA 

attach(Fen_daily2) 
summary(lm(sublimation_daily~AT_daily_ave))$r.squared
summary(lm(sublimation_daily~wind_daily_mean))$r.squared
summary(lm(sublimation_daily~RH_daily_ave))$r.squared
summary(lm(sublimation_daily~VPD_daily_ave))$r.squared
summary(lm(sublimation_daily~daily_sum_NRad))$r.squared
summary(lm(sublimation_daily~temp_gradient))$r.squared

summary(lm(sublimation_daily~AT_daily_ave*wind_daily_mean*daily_sum_NRad*VPD_daily_ave*temp_gradient))$adj.r.squared
#r.squared = 0.57, p value < 0.05
detach(Fen_daily2)
#data used in Table 5

attach(Ridge_daily2) 
summary(lm(sublimation_daily~AT_daily_ave))$r.squared
summary(lm(sublimation_daily~wind_daily_mean))$r.squared
summary(lm(sublimation_daily~RH_daily_ave))$r.squared
summary(lm(sublimation_daily~VPD_daily_ave))$r.squared
summary(lm(sublimation_daily~daily_sum_NRad))$r.squared
summary(lm(sublimation_daily~soil_temp_daily_ave))$r.squared
summary(lm(sublimation_daily~temp_gradient))$r.squared

summary(lm(sublimation_daily~AT_daily_ave*wind_daily_mean*daily_sum_NRad*VPD_daily_ave*temp_gradient))$adj.r.squared
#0.42
detach(Ridge_daily2)
#Data used in Table 5

attach(Tussock_daily2) 
summary(lm(sublimation_daily~AT_daily_ave))$r.squared
summary(lm(sublimation_daily~wind_daily_mean))$r.squared
summary(lm(sublimation_daily~RH_daily_ave))$r.squared
summary(lm(sublimation_daily~VPD_daily_ave))$r.squared
summary(lm(sublimation_daily~daily_sum_NRad))$r.squared
summary(lm(sublimation_daily~soil_temp_daily_ave))$r.squared
summary(lm(sublimation_daily~temp_gradient))$r.squared

summary(lm(sublimation_daily~wind_daily_mean*temp_gradient*VPD_daily_ave*daily_sum_NRad*AT_daily_ave))$adj.r.squared
#0.63
detach(Tussock_daily2)


#--------------------
#BONANZA REGRESSIONS:
#is there a relationship between years of high snow and high sublimation? 
#OLS (Ordinary Least Squares) Regression
summary(lm(meanBC ~ mean_snow_duration_BC, data = BC_annual_subl)) #pvalue = 0.02 r2 = 0.44 signicant relationship between snow cover duration and sublimation
summary(lm(meanBC ~ seasonal_max_SWE_mm_BC, data = BC_annual_subl)) #insignicant relationship between max SWE and sublimation
summary(lm(meanBC ~ solid_precip_mm_BC, data = BC_annual_subl)) #insignicant relationship between solid precipitation and sublimation

#test the same thing but divide by site: fenBC, bogBC, BSBC
summary(lm(BSBC ~ mean_snow_duration_BC, data = BC_annual_subl)) #fen sublimation p value = 0.009 and r2 = 0.81, bog sublimation p value = 0.02 r2 = 0.5774, black spruce p value = 0.058 r2 = 0.3
summary(lm(BSBC ~ seasonal_max_SWE_mm_BC, data = BC_annual_subl))  #fen ridge and tussock have insignicant relationship
summary(lm(BSBC ~ solid_precip_mm_BC, data = BC_annual_subl)) # #fen ridge and tussock have insignicant relationship


SCD <- BC_annual_subl %>%
  select(c(wateryears,fenBC, bogBC, BSBC, meanBC, mean_snow_duration_BC))%>%
  pivot_longer(-c(wateryears, mean_snow_duration_BC, meanBC), names_to = "site", values_to = "sublimation")

SCD$site <- ifelse(SCD$site == "fenBC", "US-BZF",
                   ifelse(SCD$site == "bogBC", "US-BZB", "US-BZS"))


SCD %>%
  filter(sublimation>0)%>%
  ggplot(aes(x=mean_snow_duration_BC, y=sublimation, group = site, color = site)) +
  # geom_line() +
  geom_smooth(method=lm, se=F) +  
  geom_point(size = 3) +
  scale_color_viridis(discrete = T, option = "D", direction = -1) +
  theme_classic(base_size = 18)+
  labs(y = "Sublimation (mm H2O/year)",
       x = "Snow Cover Duration (days)")+
  stat_regline_equation(
    aes())

#manually choose colors in plot:
show_col(viridis_pal(option = "D")(6))
viridis_colors <- viridis(3)

BC_annual_subl %>%
  ggplot(aes(x=mean_snow_duration_BC)) +
  geom_point(aes(y = fenBC), color = viridis_colors[1]) +
  geom_smooth(aes(y = fenBC), method = "lm", se = FALSE, color = viridis_colors[1]) +
  geom_point(aes(y = bogBC), color = viridis_colors[2]) +
  geom_smooth(aes(y = bogBC), method = "lm", se = FALSE, color = viridis_colors[2]) +
  geom_point(aes(y = BSBC), color = viridis_colors[3]) +
  geom_smooth(aes(y = BSBC), method = "lm", se = FALSE, color = viridis_colors[3]) +
  stat_regline_equation(aes(y = fenBC, 
                            label =  
                              paste(..eq.label..,
                                    ..rr.label..,
                                    #"p-value = ", round(fenp_value,2), 
                                    sep = "~~~")),
                        #paste("p-value = ", round(fenp_value,3), sep = "")), 
                        formula = y ~ x, 
                        color = viridis_colors[1], 
                        label.x = 150, label.y = 32) +
  stat_regline_equation(aes(y = bogBC, label =  paste(..eq.label..,..rr.label.., sep = "~~~")), formula = y ~ x, color = viridis_colors[2], label.x = 150, label.y = 30) +
  stat_regline_equation(aes(y = BSBC, label =  paste(..eq.label..,..rr.label.., sep = "~~~")), formula = y ~ x, color = viridis_colors[3], label.x = 150, label.y = 34) +
  labs(x = "Snow Cover Duration (days)", 
       y = "Sublimation (mm H2O/year)")+
  scale_color_manual()+
  theme_classic(base_size = 18)
#FIGURE 6



#-----------------------------------
#BONANZA MULTIPLE LINEAR REGRESSIONS

#hourly
attach(FenBC2)
summary(lm(Sublimation_hourly~AirTemp))$r.squared
summary(lm(Sublimation_hourly~WindSpeed))$r.squared
summary(lm(Sublimation_hourly~RH))$r.squared
summary(lm(Sublimation_hourly~VPD))$r.squared
summary(lm(Sublimation_hourly~NetRadiation))$r.squared
summary(lm(Sublimation_hourly~temp_gradient))$r.squared
summary(lm(Sublimation_hourly~AirTemp*VPD*temp_gradient*WindSpeed*NetRadiation))$adj.r.squared
detach(FenBC2)

#daily
attach(FenBC_daily2)  
summary(lm(sublimation_daily~AT_daily_ave))$r.squared
summary(lm(sublimation_daily~wind_daily_mean))$r.squared
summary(lm(sublimation_daily~RH_daily_ave))$r.squared
summary(lm(sublimation_daily~VPD_daily_ave))$r.squared
summary(lm(sublimation_daily~daily_sum_NRad))$r.squared
summary(lm(sublimation_daily~temp_gradient))$r.squared
summary(lm(sublimation_daily~AT_daily_ave*wind_daily_mean*VPD_daily_ave*temp_gradient*daily_sum_NRad))$adj.r.squared
detach(FenBC_daily2)
#DATA USED IN TABLE 5 -- REPLACE ATTACHED VALUES BW BS_daily2, FenBC_daily2, and bog_daily2, BS2, bog2, FenBC2





#===============================================================================
#============= ANALYSIS -- DIFFERENCES BETWEEN SITES  ==========================
#===============================================================================
#ANOVA AND TUKEY TEST TO EVALUATE ANNUAL DIFFERENCES
aov_annual <- tower_year_IB_BC %>%
  filter(sublimation>0)
aov_annual <- aov(log(sublimation) ~ Site, data = aov_annual) 
summary(aov_annual) #p value = 0.00309
tukey_annual <- TukeyHSD(aov_annual, conf.level = 0.95) #TO DETERMINE WHICH SITES DIFFER (IN TEXT REFERENCES TO THIS INFO)


aov_annual_trees <- tower_year_IB_BC %>%
  filter(sublimation>0)
aov_annual_trees <- aov(log(sublimation) ~ Trees, data = aov_annual_trees) 
summary(aov_annual_trees) #p value = 0.0847, IN TEXT REFERENCE

aov_annual_region <- tower_year_IB_BC %>%
  filter(sublimation>0)
aov_annual_region <- aov(log(sublimation) ~ Region, data = aov_annual_region) 
summary(aov_annual_region) #p value = 0.0706, IN TEXT REFERENCE 

#BOXPLOTS TO SHOW ABOVE DATA                              

#FIGURE 5:
#create dataframe with mean ANNUAL imnavait and mean ANNUAL bonanza sublimation 
tower_year_IB_BC <- rbind(towerBC_year, tower_year_IB)

#boxplots by site 

# sample size
tower_size <- tower_year_IB_BC %>% 
  filter(sublimation > 0) %>%
  group_by(Site) %>% 
  summarize(num=n())

str(tower_year_IB_BC)
tower_year_IB_BC$WaterYear <- factor(tower_year_IB_BC$WaterYear, levels=c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022))

tower_year_IB_BC %>%
  filter(sublimation > 0) %>%
  count(Site)

tower_year_IB_BC %>%
  # filter(sublimation > 0) %>%
  left_join(tower_size) %>%
  mutate(myaxis = paste0(Site, "\n", "n=", num)) %>%
  #filter(WaterYear == 2013 | WaterYear == 2014 | WaterYear == 2016 | WaterYear == 2017 | WaterYear == 2018 | WaterYear == 2019 | WaterYear == 2020) %>%
  #mutate(month = factor(month, levels=c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))) %>%
  ggplot(aes(x=reorder(myaxis, sublimation), y=sublimation)) + 
  stat_boxplot(geom = "errorbar")+ 
  facet_grid(.~Region,scales = "free_x", space="free_x")+
  geom_boxplot()+
  stat_summary(fun="mean", color="darkred") +
  theme_light() +
  theme(legend.position="none") + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + 
  theme(axis.title.x=element_blank())+
  #coord_flip() +
  # theme_classic(base_size = 14)+
  theme_bw(base_size = 18)+
  labs(y = "Sublimation (H2O/year)",
       x = " ", 
       #title = "Cumulative Annual Sublimation by Site",
       #caption = "red asterisk denotes mean"
  )

#FIGURE 5

#Bonanza boxplots by trees vs treeless ---- NOT INCLUDED IN REPORT 
tree_size <- tower_year_IB_BC %>% 
  filter(sublimation > 0) %>%
  group_by(Trees) %>% 
  summarize(num=n())

tower_year_IB_BC %>%
  filter(sublimation > 0) %>%
  left_join(tree_size) %>%
  mutate(myaxis = paste0(Trees, "\n", "n=", num)) %>%
  #filter(WaterYear == 2013 | WaterYear == 2014 | WaterYear == 2016 | WaterYear == 2017 | WaterYear == 2018 | WaterYear == 2019 | WaterYear == 2020) %>%
  #mutate(month = factor(month, levels=c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))) %>%
  ggplot(aes(x=reorder(myaxis, sublimation), y=sublimation)) + 
  stat_boxplot(geom = "errorbar")+ 
  geom_boxplot()+
  #geom_text(aes(label=count), 
  #         position=position_dodge(width=1.0))
  stat_summary(fun = "mean", colour="darkred", 
               #geom = "point", 
               #position = position_dodge(width = 0.75), 
               na.rm = T) +
  #coord_flip() +
  theme_bw(base_size = 18)+
  labs(y = "Sublimation (H2O/year)",
       x = " ", 
       #title = "Cumulative Annual Sublimation: Treeless vs Sites with Trees",
       #caption = "red asterisk denotes mean"
  )

#Bonanza boxplots by region (Tundra vs Lowland Boreal) -- NOT INCLUDED IN REPORT
region_size <- tower_year_IB_BC %>% 
  filter(sublimation > 0) %>%
  group_by(Region) %>% 
  summarize(num=n())

tower_year_IB_BC %>%
  filter(sublimation > 0) %>%
  left_join(region_size) %>%
  mutate(myaxis = paste0(Region, "\n", "n=", num)) %>%
  #filter(WaterYear == 2013 | WaterYear == 2014 | WaterYear == 2016 | WaterYear == 2017 | WaterYear == 2018 | WaterYear == 2019 | WaterYear == 2020) %>%
  #mutate(month = factor(month, levels=c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))) %>%
  ggplot(aes(x=reorder(myaxis, sublimation), y=sublimation)) + 
  stat_boxplot(geom = "errorbar")+ 
  geom_boxplot()+
  stat_summary(fun = "mean", colour="darkred", 
               #geom = "point", 
               #position = position_dodge(width = 0.75), 
               na.rm = T) +
  #coord_flip() +
  guides(fill = "none") +
  theme_bw(base_size = 18)+
  labs(y = "Sublimation (H2O/year)",
       x = " ", 
       #title = "Cumulative Annual Sublimation by Region",
       #caption = "red asterisk denotes mean"
  )


#=====CALCULATE # OF DAYS W/ SUBLIMATION RECORDS=======================

sum(BS_daily2$sublimation_daily > 0)
sum(bog_daily2$sublimation_daily > 0)
sum(FenBC_daily2$sublimation_daily > 0)
sum(Fen_daily2$sublimation_daily > 0)
sum(Tussock_daily2$sublimation_daily > 0)
sum(Ridge_daily2$sublimation_daily > 0)
#TABLE 1
