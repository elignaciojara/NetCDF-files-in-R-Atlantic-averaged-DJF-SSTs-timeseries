## 1 packages
library(chron);
library(RColorBrewer);
library(lattice);
library(lubridate)

##package ncdf4
library("ncdf4")


## data: SST of the Altlantic 25°N-45°S // 70°W-20°E
#### Period (1951-2020)

#set the working directory:

ncpath<-"C:/Users/elign/My Drive/2018 dell/Data/Reanalysis/ERA5/Sea Surface Temparature/Atlantic (25°N-45°S)/"


#now upload the 1950-1978 data

ncname<-"ERA5_1950_1978_DJF_SST_montly"
ncfname<-paste(ncpath, ncname, ".nc", sep ="")
dname<-"sst"

ncin<-nc_open(ncfname)

print(ncin)


long<-ncvar_get(ncin, "longitude")
nlong<-dim(long)
head(long)
tail(long)


lat<-ncvar_get(ncin, "latitude")
nlat<-dim(lat)
head(lat)
tail(lat)

#get time information
time<-ncvar_get(ncin, "time")
time
dim(time)
tunits<-ncatt_get(ncin, "time", "units")
nt<-dim(time)
nt
tunits



#get variables

pre.array_1950 <-ncvar_get(ncin, dname)


dim(pre.array_1950)


dlname<-ncatt_get(ncin, dname, "long_name")

fillvalue<-ncatt_get(ncin, dname, "_FillValue")

dim(pre.array_1950)

variable.names(ncin)

#get global attributes

title<-ncatt_get(ncin, 0, "title")
institution <-ncatt_get(ncin, 0, "institution")
datasource<-ncatt_get(ncin, 0, "source")
reference<-ncatt_get(ncin, 0, "reference")
history<-ncatt_get(ncin, 0,  "history")
Conventions<-ncatt_get(ncin, 0, "Conventions")


##explore your data
dim(pre.array_1950)


#1 reshape the array into vector with lat and long poligon
pre.vector.long_1950<-as.vector(pre.array_1950)


length(pre.vector.long_1950)




#2 reshape the vector into a matrix with one column for each grid point 123 columns for each 3-month period of the 40 years. 

pre.mat_1950<-matrix(pre.vector.long_1950, nrow = nlong*nlat, ncol=87)

dim(pre.mat_1950)

head(pre.mat_1950[1:10,1])



##isolate  from 1980-2018 MEAN montly SSTs

pre.df.1950<-t(pre.mat_1950)
dim(pre.df.1950)

pre.df.1950[1:4,1:4]

summer_1950 <- apply(pre.df.1950[c(1: 2), ], 2, mean)   
summer_1951 <- apply(pre.df.1950[c(3: 5), ], 2, mean)   
summer_1952 <- apply(pre.df.1950[c(6: 8), ], 2, mean)   
summer_1953 <- apply(pre.df.1950[c(9: 11), ], 2, mean)   
summer_1954 <- apply(pre.df.1950[c(12: 14), ], 2, mean)   
summer_1955 <- apply(pre.df.1950[c(15: 17), ], 2, mean)   
summer_1956 <- apply(pre.df.1950[c(18: 20), ], 2, mean)   
summer_1957 <- apply(pre.df.1950[c(21: 23), ], 2, mean)   
summer_1958 <- apply(pre.df.1950[c(24: 26), ], 2, mean)   
summer_1959 <- apply(pre.df.1950[c(27: 29), ], 2, mean)   
summer_1960 <- apply(pre.df.1950[c(30: 32), ], 2, mean)   
summer_1961 <- apply(pre.df.1950[c(33: 35), ], 2, mean)   
summer_1962 <- apply(pre.df.1950[c(36: 38), ], 2, mean)   
summer_1963 <- apply(pre.df.1950[c(39: 41), ], 2, mean)   
summer_1964 <- apply(pre.df.1950[c(42: 44), ], 2, mean)   

summer_1965 <- apply(pre.df.1950[c(45: 47), ], 2, mean)   
summer_1966 <- apply(pre.df.1950[c(48: 50), ], 2, mean)   
summer_1967 <- apply(pre.df.1950[c(51: 53), ], 2, mean)   
summer_1968 <- apply(pre.df.1950[c(54: 56), ], 2, mean)   
summer_1969 <- apply(pre.df.1950[c(57: 59), ], 2, mean)   
summer_1970 <- apply(pre.df.1950[c(60: 62), ], 2, mean)   

summer_1971 <- apply(pre.df.1950[c(63: 65), ], 2, mean)   
summer_1972 <- apply(pre.df.1950[c(66: 68), ], 2, mean)   
summer_1973 <- apply(pre.df.1950[c(69: 71), ], 2, mean)   
summer_1974 <- apply(pre.df.1950[c(72: 74), ], 2, mean)   
summer_1975 <- apply(pre.df.1950[c(75: 77), ], 2, mean)   
summer_1976 <- apply(pre.df.1950[c(78: 80), ], 2, mean)   
summer_1977 <- apply(pre.df.1950[c(81: 83), ], 2, mean)   
summer_1978 <- apply(pre.df.1950[c(84: 86), ], 2, mean)   



## do not use 1950 value because there is no 1949 -december value
pre_df06<-rbind(
  summer_1951,
  summer_1952,    
  summer_1953,   
  summer_1954, 
  summer_1955, 
  summer_1956, 
  summer_1957,   
  summer_1958,   
  summer_1959,   
  summer_1960,  
  summer_1961,   
  summer_1962,
  summer_1963, 
  summer_1964, 
  summer_1965,    
  summer_1966, 
  summer_1967, 
  summer_1968, 
  summer_1969, 
  summer_1970,   
  summer_1971,    
  summer_1972,    
  summer_1973,    
  summer_1974,
  summer_1975,
  summer_1976,   
  summer_1977,
  summer_1978)







#
##
## ###no get the 1979-2022 SST data from the nc file


ncname<-"ERA5_1979_2021_DJF_SST_monthly"
ncfname<-paste(ncpath, ncname, ".nc", sep ="")
dname<-"sst"

ncin<-nc_open(ncfname)

print(ncin)


long<-ncvar_get(ncin, "longitude")
nlong<-dim(long)
head(long)
tail(long)


lat<-ncvar_get(ncin, "latitude")
nlat<-dim(lat)
head(lat)
tail(lat)


#get time information
time<-ncvar_get(ncin, "time")
time
dim(time)
tunits<-ncatt_get(ncin, "time", "units")
nt<-dim(time)
nt
tunits

#convert time units to year-month-day

timestamp <- as_datetime(c(time*60*60),origin="1900-01-01")
length(timestamp)

timestamp


#get variables

pre.array_2020 <-ncvar_get(ncin, dname)


dim(pre.array_2020)


dlname<-ncatt_get(ncin, dname, "long_name")

fillvalue<-ncatt_get(ncin, dname, "_FillValue")

dim(pre.array_2020)

variable.names(ncin)

#get global attributes

title<-ncatt_get(ncin, 0, "title")
institution <-ncatt_get(ncin, 0, "institution")
datasource<-ncatt_get(ncin, 0, "source")
reference<-ncatt_get(ncin, 0, "reference")
history<-ncatt_get(ncin, 0,  "history")
Conventions<-ncatt_get(ncin, 0, "Conventions")


##explore your data
dim(pre.array_2020)



#1 reshape the array into vector with lat and long poligon
pre.vector.long_2020<-as.vector(pre.array_2020)


length(pre.vector.long_2020)

pre.vector.long_2020[100:120]



#2 reshape the vector into a matrix with one column for each grid point 123 columns for each 3-month period of the 40 years. 

pre.mat_2020<-matrix(pre.vector.long_2020, nrow = nlong*nlat, ncol=128)

dim(pre.mat_2020)

head(pre.mat_2020[1:10,1])


##isolate mean SSTs values from 1979-2022

pre.df04<-t(pre.mat_2020)
dim(pre.df04)

pre.df04[1:4,1:4]

summer_1979 <- apply(pre.df04[c(1: 2), ], 2, sum) # sum Jan and Feb 1979  

#sun dec-1978 and calculte average by dividing by 3

summer_1979<- (summer_1979 + pre.df.1950[c(87), ]) /3

summer_1980 <- apply(pre.df04[c(3: 5), ], 2, mean)   
summer_1981 <- apply(pre.df04[c(6: 8), ], 2, mean)   
summer_1982 <- apply(pre.df04[c(9: 11), ], 2, mean)   
summer_1983 <- apply(pre.df04[c(12: 14), ], 2, mean)   
summer_1984 <- apply(pre.df04[c(15: 17), ], 2, mean)   
summer_1985 <- apply(pre.df04[c(18: 20), ], 2, mean)   
summer_1986 <- apply(pre.df04[c(21: 23), ], 2, mean)   
summer_1987 <- apply(pre.df04[c(24: 26), ], 2, mean)   
summer_1988 <- apply(pre.df04[c(27: 29), ], 2, mean)   
summer_1989 <- apply(pre.df04[c(30: 32), ], 2, mean)   
summer_1990 <- apply(pre.df04[c(33: 35), ], 2, mean)   
summer_1991 <- apply(pre.df04[c(36: 38), ], 2, mean)   
summer_1992 <- apply(pre.df04[c(39: 41), ], 2, mean)   
summer_1993 <- apply(pre.df04[c(42: 44), ], 2, mean)   

summer_1994 <- apply(pre.df04[c(45: 47), ], 2, mean)   
summer_1995 <- apply(pre.df04[c(48: 50), ], 2, mean)   
summer_1996 <- apply(pre.df04[c(51: 53), ], 2, mean)   
summer_1997 <- apply(pre.df04[c(54: 56), ], 2, mean)   
summer_1998 <- apply(pre.df04[c(57: 59), ], 2, mean)   
summer_1999 <- apply(pre.df04[c(60: 62), ], 2, mean)   
summer_2000 <- apply(pre.df04[c(63: 65), ], 2, mean)   
summer_2001 <- apply(pre.df04[c(66: 68), ], 2, mean)   
summer_2002 <- apply(pre.df04[c(69: 71), ], 2, mean)   
summer_2003 <- apply(pre.df04[c(72: 74), ], 2, mean)   
summer_2004 <- apply(pre.df04[c(75: 77), ], 2, mean)   
summer_2005 <- apply(pre.df04[c(78: 80), ], 2, mean)   
summer_2006 <- apply(pre.df04[c(81: 83), ], 2, mean)   
summer_2007 <- apply(pre.df04[c(84: 86), ], 2, mean)   
summer_2008 <- apply(pre.df04[c(87: 89), ], 2, mean)   
summer_2009 <- apply(pre.df04[c(90: 92), ], 2, mean)   

summer_2010 <- apply(pre.df04[c(93: 95), ], 2, mean)   
summer_2011 <- apply(pre.df04[c(96: 98), ], 2, mean)   
summer_2012 <- apply(pre.df04[c(99: 101), ], 2, mean)   
summer_2013 <- apply(pre.df04[c(102: 104), ], 2, mean)   
summer_2014 <- apply(pre.df04[c(105: 107), ], 2, mean)  
summer_2015 <- apply(pre.df04[c(108: 110), ], 2, mean)  
summer_2016 <- apply(pre.df04[c(111: 113), ], 2, mean)  
summer_2017 <- apply(pre.df04[c(114: 116), ], 2, mean)  
summer_2018 <- apply(pre.df04[c(117: 119), ], 2, mean)  
summer_2019 <- apply(pre.df04[c(120: 122), ], 2, mean)  
summer_2020<- apply(pre.df04[c(123: 125), ], 2, mean)  
summer_2021<- apply(pre.df04[c(126: 128), ], 2, mean)

#now join all meanmer totals

pre_df05<-rbind(summer_1979,
                
                summer_1980,
                summer_1981,    
                summer_1982,   
                summer_1983, 
                summer_1984, 
                summer_1985, 
                summer_1986,   
                summer_1987,   
                summer_1988,   
                summer_1989,  
                summer_1990,   
                summer_1991,
                summer_1992, 
                summer_1993, 
                summer_1994,    
                summer_1995, 
                summer_1996, 
                summer_1997, 
                summer_1998, 
                summer_1999,   
                summer_2000,    
                summer_2001,    
                summer_2002,    
                summer_2003, 
                summer_2004, 
                summer_2005, 
                summer_2006,    
                summer_2007, 
                summer_2008, 
                summer_2009,  
                summer_2010, 
                summer_2011,
                summer_2012,
                summer_2013,   
                summer_2014,
                summer_2015,
                summer_2016,
                summer_2017, 
                summer_2018,
                summer_2019,
                summer_2020,
                summer_2021)

dim(pre_df05)

pre_df05[, 1:10]


#
##
###
#### now bind the 1950-1978 and 1979-2020 djf precipitation data

pre_df07<-rbind(pre_df06, pre_df05)

dim(pre_df07)

pre_df07[1:10, 1:10]


pre_df07[,1:5]

### no use z-score to calculate annomalies and correlations


##transform to z-scores
#mean (1950-2020) DJF total precipitation altiplano

pre_mean<-apply(pre_df07, 2, mean)
length(pre_mean)

pre_sd<-apply(pre_df07, 2, sd)
length(pre_sd)


pre_df_mean<-sweep(pre_df07, 2, pre_mean, "-")



pre_zscore<-sweep(pre_df_mean, 2, pre_sd, "/")


## Z score files could be used for PCA !!

dim(pre_zscore)




head(pre_zscore[, 1:5])

pre_zscore[,1:5]




pre_df07[15:20, 10:20]

pre_mean[10:20]

## check that each year is within the expected range!!
pre_zscore[, 1:3]



##select z-score for annomalous wet and dry summers (18-25°S)

annomaly_1955<-pre_zscore[5,]
annomaly_1966<-pre_zscore[16,]
annomaly_1975<-pre_zscore[25,]
annomaly_1980<-pre_zscore[30,]
annomaly_1983<-pre_zscore[33,]
annomaly_1984<-pre_zscore[34,]
annomaly_1987<-pre_zscore[37,]
annomaly_1998<-pre_zscore[48,]
annomaly_2003<-pre_zscore[53,]
annomaly_2012<-pre_zscore[62,]
annomaly_2016<-pre_zscore[66,]

length(annomaly_1983)



## add orography filter
### in this case, use orography as a vector


annomaly_1955_orography<-annomaly_1955 * altiplano
annomaly_1966_orography<-annomaly_1966 * altiplano
annomaly_1975_orography<-annomaly_1975 * altiplano
annomaly_1980_orography<-annomaly_1980 * altiplano
annomaly_1983_orography<-annomaly_1983 * altiplano
annomaly_1984_orography<-annomaly_1984 * altiplano
annomaly_1987_orography<-annomaly_1987 * altiplano
annomaly_1998_orography<-annomaly_1998 * altiplano
annomaly_2003_orography<-annomaly_2003 * altiplano
annomaly_2012_orography<-annomaly_2012 * altiplano
annomaly_2016_orography<-annomaly_2016 * altiplano



altiplano[25:35]
annomaly_2016_orography[25:35]


#time2 should have the same dimentions that the total number of years
#with a new time dimention including all years.
#UNITS: HOURS AFTER 1900
## nov 2011
time2<-as.array(c(439056, 447816, 456576, 465360, 474120, 482880, 491640, 500424, 509184, 517944, 526704, 535488, 544248, 553008, 561768,
                  570552, 579312, 588072, 596832, 605616, 614376, 623136, 631896, 640680, 649440, 658200, 666960, 675744, 684504, 693264,
                  702024, 710808, 719568, 728328, 737088, 745872, 754632, 763392, 772152, 780936, 789696, 798456, 807216, 816000, 824760,
                  833520, 842280, 851064, 859824, 868584, 877344, 886128, 894888, 903648, 912408, 921192, 929952, 938712, 947472, 956256,
                  965016, 973776, 982536, 991320, 1000080, 1008840, 1017600, 1026384, 1035144, 1043904, 1052664))

#remove 1950 in the time timeseries
time3<-as.array(c(447816, 456576, 465360, 474120, 482880, 491640, 500424, 509184, 517944, 526704, 535488, 544248, 553008, 561768,
                  570552, 579312, 588072, 596832, 605616, 614376, 623136, 631896, 640680, 649440, 658200, 666960, 675744, 684504, 693264,
                  702024, 710808, 719568, 728328, 737088, 745872, 754632, 763392, 772152, 780936, 789696, 798456, 807216, 816000, 824760,
                  833520, 842280, 851064, 859824, 868584, 877344, 886128, 894888, 903648, 912408, 921192, 929952, 938712, 947472, 956256,
                  965016, 973776, 982536, 991320, 1000080, 1008840, 1017600, 1026384, 1035144, 1043904, 1052664))

length(time3)

tunits2<-tunits
tunits2$value
#nt debe tener las dimensiones del total de años
## finalmente no es necesario usar el nt2 - pues es solo el promedio de un año

long2<-long
lat2<-lat

nlong2<-dim(long2)
nlat2<-dim(lat2)



nt2<-1

time4<-as.array(c(439056))

### reshaping a full dataframe to an array

ptm<-proc.time()


### 
#reshape the array containing the z-score values of SST for each year of the 70-year timeseries

## file: pre_zscore 

total_mean_array<-array(t(pre_zscore), dim=c(nlong2, nlat2, 70))


dim(total_mean_array)

proc.time() - ptm


## create at netCDF file 

#1 create path and names
ncpath<-"C:/Users/elign/My Drive/2018 dell/Data/Reanalysis/ERA5/Sea Surface Temparature/Atlantic (25°N-45°S)/"

ncname<-"ERA5_1951_2020_djf_SST_Atlantic_annomaly_v3"


ncfname<-paste(ncpath, ncname, ".nc", sep = "")
dname<-"zscores"

#create and write the netCDF file

#define dimensions
longdim<-ncdim_def("long", "degrees_east", as.double(long2))
latdim<-ncdim_def("lat", "degrees_north", as.double(lat2))

timedim<-ncdim_def("time", tunits2$value, as.double(time3))



#define variables
fillvalue<-1e32
dlname<-"ERA5_1951_2020_djf_SST_Alantic_annomaly_v1"

## add "timedim" in case of created a Net CDF file with all 70 years included

ond_2011_annonaly.def<-ncvar_def("1951_2020_djf_SST_annomaly", "z-score", list(longdim, latdim, timedim), fillvalue, dlname, prec = "single")

length(ond_2011_annonaly.def)


#create and nc file for the mean DJF 1980-2019 precipitation
ncout <- nc_create(ncfname, ond_2011_annonaly.def)

#put variables
## "total mean array" or another previously-modified array

ncvar_put(ncout, ond_2011_annonaly.def, total_mean_array)


#put additional attributres into dimension and data variables

ncatt_put(ncout, "long", "axis", "x")
ncatt_put(ncout, "lat", "axis", "y")

#add globlal atribute
ncatt_put(ncout,0,"title",title$value)
ncatt_put(ncout,0,"institution",institution$value)
ncatt_put(ncout,0,"source",datasource$value)
ncatt_put(ncout,0,"reference",reference$value)
history <- paste("P.J. Bartlein", date(), sep=", ")
ncatt_put(ncout,0,"history",history)
ncatt_put(ncout,0,"Conventions",Conventions$value)


#get a meanmary of the new file

ncout

nc_close(ncout) 







##### PC1 CORRELATED WITH DJF PRECIPITATION AT EACH GRIDDED POINT

#1. packages

library(chron);
library(RColorBrewer);
library(lattice);

library(maptools);
data(wrld_simpl);
library(fields);

#DJF timeseries and PC1 correlation 

####
#2.1 now for the mean annual (1980-2017) el NINO 3.4

NINO_34<-read.csv("pc1_atlantico_2021.csv")
head(NINO_34)
tail(NINO_34)


#select the values corresponding to the 1980-2017 period
NINO_cor<-NINO_34[,2]

NINO_cor #it should have X years values

#this tow arrays should have the same dimentions


dim(NINO_cor) #should be null
length(NINO_cor)


## take the 71-yr normalized (z-score) timeseries for each gridded point 

pre<-array(t(pre_zscore), dim=c(361, 281, 71))

dim(pre)



##### formula = c (long, lat, n years)
pre<-array(t(pre_zscore), dim=c(361, 281, 71))



# 3 correlate variables 

corr_pre_NINO<-matrix(0,361,281) # created a container. two dimentions for cordinates

for (i in 1:361){
  for (j in 1:281){
    corr_pre_NINO[i,j]<-cor(pre[i,j,], NINO_cor)
  }
}


corr_pre_NINO[1:20]

dim(corr_pre_NINO)

##p-value matrix
r<-corr_pre_NINO

# n-2 = 68
ab<-r*sqrt(68)

dim(ab)

be<-sqrt(1-r^2)

t<-ab/be

pvalue<-2*pnorm(-abs(t))

r[11,21]

pvalue[11,21]

# for all pvalues<0.05, keep correlation value, for the rest <-NA

corr_pre_signif<-ifelse(pvalue[,]<0.01, corr_pre_NINO[,],NA)


r[60,21]
corr_pre_signif[60,21]

dim(corr_pre_signif)

#save as an NC file
#convert the correlation matrix back into an array

dim(corr_pre_signif)





corr_pre_NINO_array<-array(corr_pre_signif, dim=c(361, 281))

dim(corr_pre_NINO_array)

#set path and other vainas

ncpath<-"C:/Users/elign/My Drive/2018 dell/Data/Reanalysis/ERA5/Sea Surface Temparature/Atlantic (25°N-45°S)/"

ncname<-"corr_1950_2021_djf_SST__EOF1"
ncfname<-paste(ncpath, ncname, ".nc", sep = "")
dname<-"corr"

#create and write the netCDF file

longdim<-ncdim_def("long", "degrees_east", as.double(long2))
latdim<-ncdim_def("lat", "degrees_north", as.double(lat2))

#define variables
fillvalue<-1e32
dlname<-"cor_1950_2021_djf_tp_elnino34"
corr_pre_PC1.def<-ncvar_def("cor_1950_2021_djf_tp_elnino34", "non-units", list(longdim, latdim), fillvalue, dlname, prec = "single")

#create and nc file for the mean DJF 1980-2017 precipitation
ncout <- nc_create(ncfname, corr_pre_PC1.def)

#put variables
ncvar_put(ncout, corr_pre_PC1.def, corr_pre_NINO_array)

#put additional attributres into dimension and data variables

ncatt_put(ncout, "long", "axis", "x")
ncatt_put(ncout, "lat", "axis", "y")

#get a meanmary of the new file

ncout

nc_close(ncout)


