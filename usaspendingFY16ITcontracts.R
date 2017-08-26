## Title: USAspending FY16 IT Contracts
## Purpose: Analysis of IT contract data from USAspending.gov
## Date: August 26, 2017

## set working directory to active path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## install packages and load libraries
library(ggplot2)
library(dplyr)

## read in data
fpdsvar <- c(8,11,95,99,96,81,3,16)

ITpsc <- read.csv("pscmapping_IT.csv", header=TRUE, as.is = TRUE)
BY17ITDBcontracts <- read.csv("BY17ITDBcontracts.csv", header = TRUE, as.is=TRUE)[,c(4,10)]

## download data sets for these agencies for fiscal year 2016, go to 
## https://www.usaspending.gov/DownloadCenter/Pages/DataDownload.aspx
Energy16 <- read.csv("Energy2016.csv", header=TRUE, as.is = TRUE)[,fpdsvar]
HHS16 <- read.csv("HHS2016.csv", header=TRUE, as.is = TRUE)[,fpdsvar]
Justice16 <- read.csv("Justice2016.csv", header=TRUE, as.is = TRUE)[,fpdsvar]
Treasury16 <- read.csv("Treasury2016.csv", header=TRUE, as.is = TRUE)[,fpdsvar]

## download data sets for these agencies for fiscal year 2017, go to 
## https://www.usaspending.gov/DownloadCenter/Pages/DataDownload.aspx
Energy17 <- read.csv("Energy2017.csv", header=TRUE, as.is = TRUE)[,fpdsvar]
HHS17 <- read.csv("HHS2017.csv", header=TRUE, as.is = TRUE)[,fpdsvar]
Justice17 <- read.csv("Justice2017.csv", header=TRUE, as.is = TRUE)[,fpdsvar]
Treasury17 <- read.csv("Treasury2017.csv", header=TRUE, as.is = TRUE)[,fpdsvar]

## merge data together into combined data frames
dfFY16combined <- bind_rows(list(Energy16,HHS16,Justice16,Treasury16))
dfFY17combined <- bind_rows(list(Energy17,HHS17,Justice17,Treasury17))
dfITpsc <- data.frame(ITpsc)

## remove separate data sets
rm(Energy16,HHS16,Justice16,Treasury16) 
rm(Energy17,HHS17,Justice17,Treasury17)
rm(ITpsc)

## rename columns
names(dfFY16combined)[1]<-"fundingagencycat"
names(dfFY16combined)[2]<-"fundingrequestagencyid"
names(dfFY16combined)[3]<-"agencyid"
names(dfFY16combined)[4]<-"fiscalyear"
names(dfFY16combined)[5]<-"piid"
names(dfFY16combined)[6]<-"psccode"
names(dfFY16combined)[7]<-"dollarsobligated"
names(dfFY16combined)[8]<-"currentcompletiondate"
names(dfFY17combined)[1]<-"fundingagencycat"
names(dfFY17combined)[2]<-"fundingrequestagencyid"
names(dfFY17combined)[3]<-"agencyid"
names(dfFY17combined)[4]<-"fiscalyear"
names(dfFY17combined)[5]<-"piid"
names(dfFY17combined)[6]<-"psccode"
names(dfFY17combined)[7]<-"dollarsobligated"
names(dfFY17combined)[8]<-"currentcompletiondate"
names(dfITpsc)[1]<-"PSCCode"
names(dfITpsc)[2]<-"FourdigitPSC"
names(dfITpsc)[3]<-"Levelone"
names(dfITpsc)[4]<-"Levelonecategory"
names(dfITpsc)[5]<-"Leveltwo"
names(dfITpsc)[6]<-"Leveltwocategory"
names(BY17ITDBcontracts)[1]<-"agencyname"
names(BY17ITDBcontracts)[2]<-"piid_BY17ITDB"

## change dates from character to date format
dfFY16combined$currentcompletiondate <- as.POSIXct(dfFY16combined$currentcompletiondate, format="%m/%d/%Y")

## create a column that strips out 4 digit psc code
dfFY16combined$psccode_short <- substr(dfFY16combined$psccode, 1, 4)
dfFY17combined$psccode_short <- substr(dfFY17combined$psccode, 1, 4)

## calculate FY16 total obligations
a <- group_by(dfFY16combined, psccode_short, fundingagencycat)
b <- filter(a, psccode_short %in% dfITpsc$FourdigitPSC)
dfFY16ITobs <- b
write.csv(b,file="FY16ITobs.csv")
c <- group_by(b, fundingagencycat)
d <- summarise(c, 
               sumdollarsobligated = round(sum(dollarsobligated),0),
               count = n())
d
rm(a,b,c,d)

## find missing (NA) values, export for further analysis, and keep only values 
## that are not NA
dfmissing <- data.frame()
dfmissing <- dfFY16combined[!complete.cases(dfFY16combined),]
write.csv(dfmissing,file='missingvalues_FY16.csv')
rm(dfmissing)
dfmissing <- data.frame()
dfmissing <- dfFY17combined[!complete.cases(dfFY17combined),]
write.csv(dfmissing,file='missingvalues_FY17.csv')
rm(dfmissing)

dfFY16combined <- dfFY16combined[complete.cases(dfFY16combined),]
dfFY17combined <- dfFY17combined[complete.cases(dfFY17combined),]

## match contracts in 2016 combined data set with 4-digit IT PSC codes
## filter contracts with expected completion date after 9/30/2016
a <- group_by(dfFY16combined, psccode_short, fundingagencycat)
b <- filter(a, currentcompletiondate > "2016-09-30", 
            psccode_short %in% dfITpsc$FourdigitPSC)
dfFY16ITexpFY17 <- b
write.csv(b,file="FY16ITexpFY17.csv")
c <- group_by(b, fundingagencycat)
d <- summarise(c, 
               sumdollarsobligated = round(sum(dollarsobligated),0),
               count = n())
d
rm(a,b,c,d)

## identify filtered 2016 IT contracts that were in the BY17 IT Dashboard
## submission, "contracts" data feed (for major IT investments)
a <- group_by(dfFY16ITexpFY17, fundingagencycat)
b <- filter(a, piid %in% BY17ITDBcontracts$piid_BY17ITDB)
dfFY16ITexpFY17_ITDBmatch <- b
write.csv(b,file="FY16ITexpFY17ITDBmatch.csv")
c <- group_by(b, fundingagencycat)
d <- summarise(c, 
               sumdollarsobligated = round(sum(dollarsobligated),0),
               count = n())
d
rm(a,b,c,d)

## identify filtered 2016 IT contracts that were NOT in the BY17 IT Dashboard
## submission, "contracts" data feed (for major IT investments)
a <- group_by(dfFY16ITexpFY17, fundingagencycat)
b <- filter(a, ! piid %in% BY17ITDBcontracts$piid_BY17ITDB)
dfFY16ITexpFY17_ITDBnomatch <- b
write.csv(b,file="FY16ITexpFY17ITDBnomatch.csv")
c <- group_by(b, fundingagencycat)
d <- summarise(c, 
               sumdollarsobligated = round(sum(dollarsobligated),0),
               count = n())
d
rm(a,b,c,d)

## identify obligations in FY2017 for filtered FY2016 IT contracts
## not found in the BY17 IT Dashboard submission, "contracts" data feed
## (for major IT investments)
a <- group_by(dfFY16ITexpFY17_ITDBnomatch, fundingagencycat, psccode_short)
b <- filter(a, piid %in% dfFY17combined$piid)
dfFY16ITexpFY17_ITDBnomatch_FY17spend2 <- b
write.csv(b,file="FY16ITexpFY17ITDBnomatch_FY17spend.csv")
c <- group_by(b, fundingagencycat)
d <- summarise(c, 
               sumdollarsobligated = round(sum(dollarsobligated),0),
               count = n())
d
rm(a,b,c,d)


