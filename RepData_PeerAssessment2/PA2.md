# The Impact of Storms on Public Health and the Economy of United States

## Summary
This report is to analysis the impact of storms and other servere weather events on public health and economy of United States, based on the storm data collected from U.S. National Oceanic and Atmospheric Administration's from 1950 - 2011. From the analysis, we found out that the Tornado and Heat are the most dangerous weather events for people's health, while floods, hurricanes and tornados have largest economic impact.

## Data Processsing
- Loading Libraries

```r
echo = TRUE
library("stringr")
library(plyr)
library(ggplot2)
require(gridExtra)
```

```
## Loading required package: gridExtra
## Loading required package: grid
```

```r
options(scipen = 1)
```

- Loading and Caching Data

```r
#Download and unzip the file if the file is not exist in the folder.
if (!"Data.csv.bz2" %in% dir("./RepData_PeerAssessment2/")) {
        print("clear")
        download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile = "./Data.csv.bz2")
        library(R.utils)
        bunzip2("./Data.csv.bz2", overwrite=T, remove=F)
}
```

```
## [1] "clear"
```

```
## Loading required package: R.oo
## Loading required package: R.methodsS3
## R.methodsS3 v1.6.1 (2014-01-04) successfully loaded. See ?R.methodsS3 for help.
## R.oo v1.18.0 (2014-02-22) successfully loaded. See ?R.oo for help.
## 
## Attaching package: 'R.oo'
## 
## The following objects are masked from 'package:methods':
## 
##     getClasses, getMethods
## 
## The following objects are masked from 'package:base':
## 
##     attach, detach, gc, load, save
## 
## R.utils v1.34.0 (2014-10-07) successfully loaded. See ?R.utils for help.
## 
## Attaching package: 'R.utils'
## 
## The following object is masked from 'package:utils':
## 
##     timestamp
## 
## The following objects are masked from 'package:base':
## 
##     cat, commandArgs, getOption, inherits, isOpen, parse, warnings
```

```r
# read the file and stored as rawData.
if (!"Data" %in% ls()) {
        classes = c("character", "character", "NULL", "NULL", "NULL", "NULL", "character", "character", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "character", "character", "character", "character", "character", "character", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "numeric")
        rawData <- read.csv("./Data.csv", sep = ",",colClasses = classes, comment.char="")
}
```

- Cleaning Data
Fixing wrong spelling of the Event Type. 

```r
rawData$EVTYPE <- tolower(rawData$EVTYPE) # set to lower case
rawData$EVTYPE <- sub("avalance", "avalanche", rawData$EVTYPE)
rawData$EVTYPE <- sub("trawDatam wind (g45)", "trawDatam wind", rawData$EVTYPE)
rawData$EVTYPE <- sub("high winds","wind",rawData$EVTYPE)
rawData$EVTYPE <- sub("high wind and seas","wind",rawData$EVTYPE)
rawData$EVTYPE <- sub("high wind/seas","wind",rawData$EVTYPE)
rawData$EVTYPE <- sub("high winds/cold","wind",rawData$EVTYPE)
rawData$EVTYPE <- sub("winds","wind",rawData$EVTYPE)
rawData$EVTYPE <- sub("rawDatarong wind","wind",rawData$EVTYPE)
rawData$EVTYPE <- sub("high wind","wind",rawData$EVTYPE)
rawData$EVTYPE <- sub("high wind/heavy snow","wind/snow",rawData$EVTYPE)
rawData$EVTYPE <- sub("high winds/snow","wind/snow",rawData$EVTYPE)
rawData$EVTYPE <- sub("snow/wind","wind/snow",rawData$EVTYPE)
rawData$EVTYPE <- sub("heavy snow and wind","wind/snow",rawData$EVTYPE)
rawData$EVTYPE <- sub("cold wave","cold",rawData$EVTYPE)
rawData$EVTYPE <- sub("extreme cold","cold",rawData$EVTYPE)
rawData$EVTYPE <- sub("extended cold","cold",rawData$EVTYPE)
rawData$EVTYPE <- sub("cold weather","cold",rawData$EVTYPE)
rawData$EVTYPE <- sub("record cold","cold",rawData$EVTYPE)
rawData$EVTYPE <- sub("ccold/wind chill","cold",rawData$EVTYPE)
rawData$EVTYPE <- sub("unseasonably cold","cold",rawData$EVTYPE)
rawData$EVTYPE <- sub("icy roads","ice",rawData$EVTYPE)
rawData$EVTYPE <- sub("ice roads","ice",rawData$EVTYPE)
rawData$EVTYPE <- sub("ice jam flooding","ice",rawData$EVTYPE)
rawData$EVTYPE <- sub("coarawDataal  flood/erosion","flood",rawData$EVTYPE)
rawData$EVTYPE <- sub("coarawDataal  flooding/erosion","flood",rawData$EVTYPE)
rawData$EVTYPE <- sub("coarawDataal flooding/erosion","flood",rawData$EVTYPE)
rawData$EVTYPE <- sub("coarawDataal flood","flood",rawData$EVTYPE)
rawData$EVTYPE <- sub("rain and flood","flood",rawData$EVTYPE)
rawData$EVTYPE <- sub("flash flooding/flood","flood",rawData$EVTYPE)
rawData$EVTYPE <- sub("flash flood/flood","flood",rawData$EVTYPE)
rawData$EVTYPE <- sub("erosion/crawDatal flood","flood",rawData$EVTYPE)
rawData$EVTYPE <- sub("coarawDataal surge","flood",rawData$EVTYPE)
rawData$EVTYPE <- sub("river and rawDataream flood","flood",rawData$EVTYPE)
rawData$EVTYPE <- sub("major flood","flood",rawData$EVTYPE)
rawData$EVTYPE <- sub("flood/flash flood","flood",rawData$EVTYPE)
rawData$EVTYPE <- sub("river flooding","flood",rawData$EVTYPE)
rawData$EVTYPE <- sub("river flood","flood",rawData$EVTYPE)
rawData$EVTYPE <- sub("flash flooding","flood",rawData$EVTYPE)
rawData$EVTYPE <- sub("flood/flood","flood",rawData$EVTYPE)
rawData$EVTYPE <- sub("flash flood/","flood",rawData$EVTYPE)
rawData$EVTYPE <- sub("flash flood","flood",rawData$EVTYPE)
rawData$EVTYPE <- sub("flood/flash","flood",rawData$EVTYPE)
rawData$EVTYPE <- sub("urban/sml rawDataream fld","flood",rawData$EVTYPE)
rawData$EVTYPE <- sub("urban flood","flood",rawData$EVTYPE)
rawData$EVTYPE <- sub("heavy rains/flooding","flood",rawData$EVTYPE)
rawData$EVTYPE <- sub("heavy rain/flooding","flood",rawData$EVTYPE)
rawData$EVTYPE <- sub("snowmelt flooding","flood",rawData$EVTYPE)
rawData$EVTYPE <- sub("flood & heavy rain","flood",rawData$EVTYPE)
rawData$EVTYPE <- sub("flooding","flood",rawData$EVTYPE)
rawData$EVTYPE <- sub("floods","flood",rawData$EVTYPE)
rawData$EVTYPE <- sub("winter weather mix","winter",rawData$EVTYPE)
rawData$EVTYPE <- sub("wintry mix","winter",rawData$EVTYPE)
rawData$EVTYPE <- sub("winter weather","winter",rawData$EVTYPE)
rawData$EVTYPE <- sub("winter weather/mix","winter",rawData$EVTYPE)
rawData$EVTYPE <- sub("winter/mix","winter",rawData$EVTYPE)
rawData$EVTYPE <- sub("tornado f2","tornado",rawData$EVTYPE)
rawData$EVTYPE <- sub("tornadoes, trawDatam wind, hail","tornado",rawData$EVTYPE)
rawData$EVTYPE <- sub("tornado f3","tornado",rawData$EVTYPE)
rawData$EVTYPE <- sub("tornado f1","tornado",rawData$EVTYPE)
rawData$EVTYPE <- sub("rip currents/heavy surf","rip current",rawData$EVTYPE)
rawData$EVTYPE <- sub("rip currents","rip current",rawData$EVTYPE)
rawData$EVTYPE <- sub("heavy rains","rain",rawData$EVTYPE)
rawData$EVTYPE <- sub("excessive rainfall","rain",rawData$EVTYPE)
rawData$EVTYPE <- sub("freezing rain","rain",rawData$EVTYPE)
rawData$EVTYPE <- sub("heavy rain","rain",rawData$EVTYPE)
rawData$EVTYPE <- sub("rain/severe weather","rain",rawData$EVTYPE)
rawData$EVTYPE <- sub("freezing drizzle","rain",rawData$EVTYPE)
rawData$EVTYPE <- sub("heavy snow","snow",rawData$EVTYPE)
rawData$EVTYPE <- sub("light snow","snow",rawData$EVTYPE)
rawData$EVTYPE <- sub("snow squall","snow",rawData$EVTYPE)
rawData$EVTYPE <- sub("record snow","snow",rawData$EVTYPE)
rawData$EVTYPE <- sub("excessive snow","snow",rawData$EVTYPE)
rawData$EVTYPE <- sub("snow/snow","snow",rawData$EVTYPE)
rawData$EVTYPE <- sub("wild/forerawData fire","wildfire",rawData$EVTYPE)
rawData$EVTYPE <- sub("wild fires","wildfire",rawData$EVTYPE)
rawData$EVTYPE <- sub("hard freeze","frorawData",rawData$EVTYPE)
rawData$EVTYPE <- sub("damaging freeze","frorawData",rawData$EVTYPE)
rawData$EVTYPE <- sub("frorawData/freeze","frorawData",rawData$EVTYPE)
rawData$EVTYPE <- sub("agricultural freeze","frorawData",rawData$EVTYPE)
rawData$EVTYPE <- sub("early frorawData","frorawData",rawData$EVTYPE)
rawData$EVTYPE <- sub("frorawData.freeze","frorawData",rawData$EVTYPE)
rawData$EVTYPE <- sub("freeze","frorawData",rawData$EVTYPE)
rawData$EVTYPE <- sub("small hail","hail",rawData$EVTYPE)
rawData$EVTYPE <- sub("precipitation","rain",rawData$EVTYPE)
rawData$EVTYPE <- sub("avalance","avalanche",rawData$EVTYPE)
rawData$EVTYPE <- sub("dense fog","fog",rawData$EVTYPE)
rawData$EVTYPE <- sub("freezing fog","fog",rawData$EVTYPE)
rawData$EVTYPE <- sub("heavy surf/high surf","heavy surf",rawData$EVTYPE)
rawData$EVTYPE <- sub("lightning injury","lightning",rawData$EVTYPE)
rawData$EVTYPE <- sub("lightning fire","lightning",rawData$EVTYPE)
rawData$EVTYPE <- sub("excessive heat","heat",rawData$EVTYPE)
rawData$EVTYPE <- sub("unseasonably warm","heat",rawData$EVTYPE)
rawData$EVTYPE <- sub("heat wave drought","heat",rawData$EVTYPE)
rawData$EVTYPE <- sub("heat and dry","heat",rawData$EVTYPE)
rawData$EVTYPE <- sub("record heat","heat",rawData$EVTYPE)
rawData$EVTYPE <- sub("extreme heat","heat",rawData$EVTYPE)
rawData$EVTYPE <- sub("heat waves","heat",rawData$EVTYPE)
rawData$EVTYPE <- sub("heat wave","heat",rawData$EVTYPE)
rawData$EVTYPE <- sub("heats","heat",rawData$EVTYPE)
rawData$EVTYPE <- sub("hurricane edouard","hurricane",rawData$EVTYPE)
rawData$EVTYPE <- sub("hurricane erin","hurricane",rawData$EVTYPE)
rawData$EVTYPE <- sub("hurricane opal/high winds","hurricane",rawData$EVTYPE)
rawData$EVTYPE <- sub("hurricane/typhoon","hurricane",rawData$EVTYPE)
rawData$EVTYPE <- sub("hurricane emily","hurricane",rawData$EVTYPE)
rawData$EVTYPE <- sub("hurricane opal","hurricane",rawData$EVTYPE)
rawData$EVTYPE <- sub("hurricane gordon","hurricane",rawData$EVTYPE)
rawData$EVTYPE <- sub("hurricane felix","hurricane",rawData$EVTYPE)
rawData$EVTYPE <- sub("hurricane-generated swells","hurricane",rawData$EVTYPE)
rawData$EVTYPE <- sub("trawDatam wind (g40)","thunderrawDataorm",rawData$EVTYPE)
rawData$EVTYPE <- sub("trawDatam wind (g45)","thunderrawDataorm",rawData$EVTYPE)
rawData$EVTYPE <- sub("thundersnow","thunderrawDataorm",rawData$EVTYPE)
rawData$EVTYPE <- sub("severe thunderrawDataorm","thunderrawDataorm",rawData$EVTYPE)
rawData$EVTYPE <- sub("thunderrawDataorm  winds","thunderrawDataorm",rawData$EVTYPE)
rawData$EVTYPE <- sub("thunderrawDataorm winds","thunderrawDataorm",rawData$EVTYPE)
rawData$EVTYPE <- sub("thunderrawDataorm winds/hail","thunderrawDataorm",rawData$EVTYPE)
rawData$EVTYPE <- sub("thunderrawDataorms winds","thunderrawDataorm",rawData$EVTYPE)
rawData$EVTYPE <- sub("thunderrawDataorm wind","thunderrawDataorm",rawData$EVTYPE)
rawData$EVTYPE <- sub("thunderrawDataorm winds 13","thunderrawDataorm",rawData$EVTYPE)
rawData$EVTYPE <- sub("thunderrawDataorm windss","thunderrawDataorm",rawData$EVTYPE)
rawData$EVTYPE <- sub("thunderrawDataorm 13","thunderrawDataorm",rawData$EVTYPE)
rawData$EVTYPE <- sub("thunderrawDataorms","thunderrawDataorm",rawData$EVTYPE)
rawData$EVTYPE <- sub("thunderrawDataormw","thunderrawDataorm",rawData$EVTYPE)
rawData$EVTYPE <- sub("trawDatam wind","thunderrawDataorm",rawData$EVTYPE)
rawData$EVTYPE <- sub("winter rawDataorm high winds","rawDataorm",rawData$EVTYPE)
rawData$EVTYPE <- sub("winter rawDataorms","rawDataorm",rawData$EVTYPE)
rawData$EVTYPE <- sub("tropical rawDataorm gordon","rawDataorm",rawData$EVTYPE)
rawData$EVTYPE <- sub("tropical rawDataorm","rawDataorm",rawData$EVTYPE)
rawData$EVTYPE <- sub("winter rawDataorm","rawDataorm",rawData$EVTYPE)
rawData$EVTYPE <- sub("rawDataorm dean","rawDataorm",rawData$EVTYPE)
rawData$EVTYPE <- sub("rawDataorm alberto","rawDataorm",rawData$EVTYPE)
rawData$EVTYPE <- sub("rawDataorm jerry","rawDataorm",rawData$EVTYPE)
rawData$EVTYPE <- sub("high wind 48","rawDataorm",rawData$EVTYPE)
rawData$EVTYPE <- sub("rawDataorm surge/tide","rawDataorm surge",rawData$EVTYPE)
```

- Impact on Public Health
There are two types of impact on Public Health, Fatalities and Injuries. 
We will analysis the data of these two types one by one.

1. Fatalities

```r
rawData$FATALITIES <- as.integer(rawData$FATALITIES) 
FATAData <- rawData[which(rawData$FATALITIES!="0"), ] #select the data which caused fatalities
FATAbyType <- ddply(FATAData, .(EVTYPE), summarize, sum= sum(FATALITIES)) 
FATAbyType <- arrange(FATAbyType, desc(sum))
TopFATA <- FATAbyType[1:5,] #select the top 5 Events Types caused most fatalities 
```

2. Injuries 

```r
rawData$INJURIES <- as.integer(rawData$INJURIES)
INJData <- rawData[which(rawData$INJURIES!="0"), ] #select the data which caused injuries
INJbyType <- ddply(INJData, .(EVTYPE), summarize, sum= sum(INJURIES))
INJbyType <- arrange(INJbyType, desc(sum))
TopINJ <- INJbyType[1:5,] #select the top 5 Events Types caused most injuries
```

- Impact on U.S. Economy
There are two types of impact on Economy, Property Damage and Crop Damage.
We will analysis the data of these two types one by one.
1. Property Damage

```r
rawData$PROPDMG <- as.integer(rawData$PROPDMG)
PROPData <- rawData[which(rawData$PROPDMGEXP == "B"), ] #selected Billion level 
PORPbyType <- ddply(PROPData, .(EVTYPE), summarize, sum= sum(PROPDMG))
PORPbyType <- arrange(PORPbyType, desc(sum))
TopPORP <- PORPbyType[1:5,]
```

2. Crop Damage

```r
rawData$CROPDMG <- as.integer(rawData$CROPDMG)
CROPData <- rawData[which(rawData$CROPDMGEXP == "B"),] # selected Billion level
CORPbyType <- ddply(CROPData, .(EVTYPE), summarize, sum= sum(CROPDMG))
CORPbyType <- arrange(CORPbyType, desc(sum))
TopCORP <- CORPbyType[1:5,]
```

###Results
- Top 5 Dangours Event Types to Public Health:

```r
FATAplot <- qplot(data=TopFATA, x = EVTYPE, y= sum, geom ="bar", stat = "identity") + xlab("Type") + scale_y_continuous("Number of Fatalities") + ggtitle("Top 5 Severe Weather Types led to Fatalities ")
INJplot <- qplot(data=TopINJ, x = EVTYPE, y= sum, geom ="bar", stat = "identity")+ xlab("Type") + scale_y_continuous("Number of Injuries") + ggtitle("Top 5 Severe Weather Types led to Injuries ")

grid.arrange(FATAplot, INJplot, nrow = 2)
```

![](./PA2_files/figure-html/unnamed-chunk-8-1.png) 

-Top 5 Dangours Event Types to Economics:

```r
PROPplot <- qplot(data=TopPORP, x = EVTYPE, y = sum, geom ="bar", stat = "identity")+ xlab("Type") + scale_y_continuous("Property Damages in U.S. Dollars(Billions)") + ggtitle("Top 5 Severe Weather Types led to Property Damages")
CROPplot <- qplot(data=TopCORP, x = EVTYPE, y = sum, geom ="bar", stat = "identity")+ xlab("Type") + scale_y_continuous("Crop Damages in U.S. Dollars(Billions)") + ggtitle("Top 5 Severe Weather Types led to Crop Damages")

PROPplot
```

![](./PA2_files/figure-html/unnamed-chunk-9-1.png) 

```r
CROPplot
```

![](./PA2_files/figure-html/unnamed-chunk-9-2.png) 

### Conclusion
Regarding the result of the data anaylsis, we know that #tornado# and #heat# are most harmful to Public Health. #Flood#, #Drought#, and #Hurricane# have the largest economic consequences.
