####################################################
# Script Assignment
####################################################
####################################################
# Script must be executed in same folder as dataset

#load libraries
library(dplyr)
library(ggplot2)

#check if file exists
dataFile<-dir(pattern="*.bz2")
if (!file.exists(dataFile)) { 
    stop("Dataset file not present!") 
}

#load storm data
stormSet<-read.csv(dataFile)

# Prepare data to answer question 1:
# - Across the United States, which types of events are most harmful with respect to population health?
# Results will be limited to top 10 most harmfull events

#create dataset with only one column for fatalities and injuries count

#dataset for fatalities only
stormSetFatalities<-stormSet[stormSet$FATALITIES>0,c("STATE","EVTYPE","FATALITIES")]
stormSetFatalities$EFFTYPE<-"Fatalities"
names(stormSetFatalities)<-c("State","EventType","Count","EffectType")
stormSetFatalitiesFinal <- stormSetFatalities %>%
    group_by(EventType,EffectType) %>%
    summarize(Count = sum(Count)) %>%
    as.data.frame() 

#sort data by count and store top 10
stormSetFatalitiesFinal<- head(stormSetFatalitiesFinal[order(as.numeric(-stormSetFatalitiesFinal[,"Count"])),],10)

#dataset for injuries only
stormSetInjuries<-stormSet[stormSet$INJURIES>0,c("STATE","EVTYPE","INJURIES")]
stormSetInjuries$EFFTYPE<-"Injuries"
names(stormSetInjuries)<-c("State","EventType","Count","EffectType")
stormSetInjuriesFinal <- stormSetInjuries %>%
    group_by(EventType,EffectType) %>%
    summarize(Count = sum(Count)) %>%
    as.data.frame() 

#sort data by count and store top 10
stormSetInjuriesFinal<- head(stormSetInjuriesFinal[order(as.numeric(-stormSetInjuriesFinal[,"Count"])),],10)


#merge both objects
stormSetHealthEffects<-rbind(stormSetFatalitiesFinal,stormSetInjuriesFinal)

#clear temporary objects
rm(stormSetFatalities,stormSetInjuries,stormSetFatalitiesFinal,stormSetInjuriesFinal)


# Prepare data to answer question 2:
# - Across the United States, which types of events have the greatest economic consequences?

#convert exponent multipliers
convUnit<-function(x){
    expUnit <- 0 # default value
    expUnit <- ifelse(toupper(x) == "B",9,expUnit)
    expUnit <- ifelse(toupper(x) == "M",6,expUnit)
    expUnit <- ifelse(toupper(x) == "K",3,expUnit)
    expUnit <- ifelse(toupper(x) == "H",2,expUnit)
    
    return(as.numeric(expUnit))
}

#dataset for property damage
stormSetDamageProp<-stormSet[,c("EVTYPE","PROPDMG","PROPDMGEXP")] %>%
    mutate(TOTALPROPDMG = PROPDMG * (10^convUnit(PROPDMGEXP))) %>%
    group_by(EVTYPE) %>%
    summarize(Count = sum(TOTALPROPDMG)) %>%
    as.data.frame() 

names(stormSetDamageProp)<-c("EventType","TotalDamage")
stormSetDamageProp$DamageType <- "Property Damages"

#sort data by count and store top 10
stormSetDamageProp<- head(stormSetDamageProp[order(as.numeric(-stormSetDamageProp[,"TotalDamage"])),],10)
    

stormSetDamageCrop<-stormSet[,c("EVTYPE","CROPDMG","CROPDMGEXP")] %>%
    mutate(TOTALCROPDMG = CROPDMG * (10^convUnit(CROPDMGEXP))) %>%
    group_by(EVTYPE) %>%
    summarize(Count = sum(TOTALCROPDMG)) %>%
    as.data.frame() 

names(stormSetDamageCrop)<-c("EventType", "TotalDamage")
stormSetDamageCrop$DamageType <- "Crop Damages"

#sort data by count and store top 10
stormSetDamageCrop<- head(stormSetDamageCrop[order(as.numeric(-stormSetDamageCrop[,"TotalDamage"])),],10)


#merge both objects
stormSetDamageFinal<-rbind(stormSetDamageProp,stormSetDamageCrop)

#clear temporary objects
rm(stormSetDamageProp,stormSetDamageCrop)


#Present Data for question 1
stormHealthEffectsPlot <-ggplot(stormSetHealthEffects, aes(EventType,Count,
                                                           fill=EffectType)) 
stormHealthEffectsPlot + labs(x="Event Type",y ="Total", 
                              title ="Top 10 Harmfull Weather Events accross US") + 
    geom_bar(stat="identity", position="dodge") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_wrap(~EffectType,scales = "free")

#Present Data for question 2
stormSetDamagePlot <-ggplot(stormSetDamageFinal, aes(EventType,TotalDamage,
                                                           fill=DamageType)) 
stormSetDamagePlot + labs(x="Event Type",y ="Amount (in Dollars)", 
                              title ="Top 10 Greatest Economic Impact Weather Events accross US") + 
    geom_bar(stat="identity", position="dodge") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_wrap(~DamageType,scales = "free")