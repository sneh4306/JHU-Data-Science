---
title: "Effects of weather events on health and economic factors"
output: 
  html_document:
    keep_md: true
---

```r
knitr::opts_chunk$set(echo = TRUE,cache = TRUE)
```
## Synopsis:
1. Data is Read.  
2. For population health, two variables are considered viz Fatalities, Injuries, and another dataframe is generated which calculates the sum of both these factors according to each event.  
3. This dataframe is sorted in descending order of sum of fatalities and injuries.  
4. scatter plot is plotted for only the first 10 rows of this dataframe showing the impact of events on population health.  
5. For economic consequences, 2 variables namely Property damage, Crop Damage are considered.  
6. Propdmgexp and Cropdmgexp are variable which represent alpha notations for property damage and crop damage.For instance: H or h means multiply the number by 100,K or k means multiply the number by 1000,etc.  
7. Thus the above calculations are performed and a dataframe "damage" is created which has 3 variables namely Event, property damage and crop damage.  
8. Dataframe is arranged in descending order of the sum of property damage and crop damage.   
9. A scatterplot for only the first 10 rows is plotted which represent the events which that have the greatest economic consquences.  

## Data Processing
### Impact on population health
1. Read the data.

```r
data<-read.csv(bzfile("repdata-data-StormData.csv.bz2"))
```
2. group data by events and calculate sum of fatalities for each event and injuries for each event.

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
databyevent<-data %>% group_by(EVTYPE) %>% summarise(Tfatal=sum(FATALITIES),tinjured=sum(INJURIES))
```
3. Calculte the total of injuries and fatalities for each event and arrange in descending order.

```r
databyevent$health<-databyevent$Tfatal+databyevent$tinjured
databyevent<-databyevent %>% arrange(desc(health))
```
### Impact on Economic Factors
1. Tables for property damage and crop damage

```r
table(data$PROPDMGEXP)
```

```
## 
##             -      ?      +      0      1      2      3      4      5      6 
## 465934      1      8      5    216     25     13      4      4     28      4 
##      7      8      B      h      H      K      m      M 
##      5      1     40      1      6 424665      7  11330
```

```r
 table(data$CROPDMGEXP)
```

```
## 
##             ?      0      2      B      k      K      m      M 
## 618413      7     19      1      9     21 281832      1   1994
```
2. As we can see the aplha notations, we must convert them into numeric value

```r
stormdata<-data %>% select(EVTYPE,PROPDMGEXP,CROPDMGEXP,PROPDMG,CROPDMG)
stormdata$PROPDMGCALC [stormdata$PROPDMG==0] <- 0  
stormdata$CROPDMGCALC [stormdata$CROPDMG==0] <- 0 
stormdata$PROPDMGCALC [stormdata$PROPDMGEXP=="H"|stormdata$PROPDMGEXP=="h"]<-stormdata$PROPDMG[stormdata$PROPDMGEXP=="H"|stormdata$PROPDMGEXP=="h"]*100
stormdata$CROPDMGCALC [stormdata$CROPDMGEXP=="H"|stormdata$CROPDMGEXP=="h"]<-stormdata$CROPDMG[stormdata$CROPDMGEXP=="H"|stormdata$CROPDMGEXP=="h"]*100
stormdata$PROPDMGCALC [stormdata$PROPDMGEXP=="K"|stormdata$PROPDMGEXP=="k"]<-stormdata$PROPDMG[stormdata$PROPDMGEXP=="K"|stormdata$PROPDMGEXP=="k"]*1000
stormdata$CROPDMGCALC [stormdata$CROPDMGEXP=="K"|stormdata$CROPDMGEXP=="k"]<-stormdata$CROPDMG[stormdata$CROPDMGEXP=="K"|stormdata$CROPDMGEXP=="k"]*1000
stormdata$PROPDMGCALC [stormdata$PROPDMGEXP=="M"|stormdata$PROPDMGEXP=="m"]<-stormdata$PROPDMG[stormdata$PROPDMGEXP=="M"|stormdata$PROPDMGEXP=="m"]*1000000
stormdata$CROPDMGCALC [stormdata$CROPDMGEXP=="M"|stormdata$CROPDMGEXP=="m"]<-stormdata$CROPDMG[stormdata$CROPDMGEXP=="M"|stormdata$CROPDMGEXP=="m"]*1000000
stormdata$PROPDMGCALC [stormdata$PROPDMGEXP=="B"|stormdata$PROPDMGEXP=="b"]<-stormdata$PROPDMG[stormdata$PROPDMGEXP=="B"|stormdata$PROPDMGEXP=="b"]*1000000000
stormdata$CROPDMGCALC [stormdata$CROPDMGEXP=="B"|stormdata$CROPDMGEXP=="b"]<-stormdata$CROPDMG[stormdata$CROPDMGEXP=="B"|stormdata$CROPDMGEXP=="b"]*1000000000
```
3. Group according to event and arrange the dataframe in descending order with respect to sum of the new variables calculated(PROPDMGCALC,CROPDMGCALC).

```r
damage<- aggregate(cbind(PROPDMGCALC,CROPDMGCALC)~EVTYPE, data = stormdata, sum, na.rm=TRUE)
damage<- arrange(damage, desc(PROPDMGCALC+CROPDMGCALC))
head(damage)
```

```
##              EVTYPE  PROPDMGCALC CROPDMGCALC
## 1             FLOOD 144657709800  5661968450
## 2 HURRICANE/TYPHOON  69305840000  2607872800
## 3           TORNADO  56936990480   364950110
## 4       STORM SURGE  43323536000        5000
## 5              HAIL  15732262220  3000949450
## 6       FLASH FLOOD  16140811510  1420717100
```
## Results

1. Population Health (scatterplot).

```r
g<-ggplot(databyevent[c(1:10),],aes(Tfatal,tinjured,col=EVTYPE))+geom_point()+labs(x="Fatlities",y="Injured")+ggtitle("Impact of events on Population Health")
update_labels(g,list(colour="Event Type"))
```

![](storm_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

2. Economic Consequences (scatterplot).

```r
k<-ggplot(damage[1:10,],aes(PROPDMGCALC,CROPDMGCALC,col=EVTYPE))+geom_point()+labs(x="Property Damage",y="Crop Damage")+ggtitle("Impact of events on Economic Damage")
update_labels(k,list(colour="Event Type"))
```

![](storm_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
