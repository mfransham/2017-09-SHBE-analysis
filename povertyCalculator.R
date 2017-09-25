# this file takes a SHBE file as input specified in 'shbefile' variable
# calculates equivalised household income for the benefit unit on a subset of income sources
# compares this to a given poverty threshold for the reference year in question
# spits out aggregation at LSOA level and HH level file with defined characteristics

library(dplyr)
library(eeptools)

# load data for desired time point
shbe <- read.csv(shbefile, 
                 sep = "|",
                 header=F, 
                 strip.white=T,
                 stringsAsFactors = F)

# add column names, remove header and footer
shbecolnames <- read.csv("data/shbe-colnames-Apr2015.csv",
                         header=F,
                         strip.white=T)
colnames(shbe) <- droplevels(shbecolnames$V1) # use droplevels() as doesn't work otherwise
rm(shbecolnames)
headandfoot <- shbe[c(1, nrow(shbe)), ] # store header and trailer records
shbe <- shbe[-c(1, nrow(shbe)), ] # remove header and trailer records
refdate <- headandfoot[1,3]

# find the appropriate poverty threshold, based upon refdate variable
refyear <- as.numeric(substr(refdate, 7, 10))
povThresholdData <- read.csv("data/hmrcPovThresholds.csv", header=T)
povthreshold <- as.numeric( povThresholdData %>% filter(date==refyear) %>% select(threshold) )*100
rm(povThresholdData)

# table of households
hh.allvars <- shbe[shbe$REC_TYPE=="D", ] 
hh <- hh.allvars[ , c(1:3, 8:14, 123, 177:178, 213)]

# table of other household members, with age and dependancy status
deps <- shbe[shbe$REC_TYPE=="S", ] 
deps <- deps[, c(1:3, 308, 315)]
deps$CCSUBTYPE <- factor(deps$CCSUBTYPE,
                         levels = c(1,2),
                         labels = c("dependant", "non-dependant"))

# create single reference number for joining tables
hh$id <- ifelse(is.na(hh$LCHBREF) | hh$LCHBREF=="", 
                hh$LCLCTRREF, hh$LCHBREF)
deps$id <- ifelse(is.na(deps$LCHBREF) | deps$LCHBREF=="", 
                  deps$LCLCTRREF, deps$LCHBREF)

# simplify data
hh.addresses <- hh[, c(15,5,6,7)] # save address data in separate table
hh <- hh[, c(15,4,9:14)] # just keep HH composition data

# table showing benefit type claimed
hh.bentype <- shbe %>% filter(REC_TYPE=="D") %>% select(LCHBREF, LCHBSTAT, LCLCTRREF, LCLCTRSTAT)
hh.bentype$id <- ifelse(is.na(hh.bentype$LCHBREF) | hh.bentype$LCHBREF=="", 
                        hh.bentype$LCLCTRREF, hh.bentype$LCHBREF)

# include only households which are claiming housing benefit
hh.hbOnly <- filter(hh.bentype, LCHBSTAT==1 | LCHBSTAT==2)
hh <- filter(hh, id %in% hh.hbOnly$id)
rm(hh.hbOnly)

# function to calculate age for this dataset
SHBEage <- function(birthdate) {
  floor(age_calc(dob = as.Date(birthdate, format = "%d-%m-%Y"), 
                 enddate = as.Date(refdate, format = "%d-%m-%Y"),
                 units="years"))
}

# insert dummy DOB if missing
deps$CDSUBDOB <- ifelse(deps$CDSUBDOB=="", "01-01-1800", deps$CDSUBDOB)
hh$CDDOB <- ifelse(hh$CDDOB=="", "01-01-1800", hh$CDDOB)

# calculate ages for dependents, claimants, partners
deps$age <- SHBEage(deps$CDSUBDOB)
hh$CCage  <- SHBEage(hh$CDDOB)
# hh$PPage <- SHBEage(hh$PDDOB) # this doesn't work

# partner age is being weird!! so this workaround
hh2 <- hh[hh$PCPTNRFG==1, ]
# hh2$PPage <- SHBEage(hh$PDDOB) # this doesn't work either
hh2$PPdob <- as.Date(hh2$PDDOB, format = "%d-%m-%Y")
hh2$PPage <- floor(age_calc(hh2$PPdob, enddate = as.Date(refdate, format = "%d-%m-%Y"), units="years"))
hh2 <- hh2[, c(1,11)]
hh <- left_join(hh, hh2, by="id")
rm(hh2)

# categorise households: note can't use if() because it doesn't deal with vectors
hh$type <- "" # initialise as empty variable
hh$type <- ifelse(hh$DNNUMDEPS > 0 & hh$PCPTNRFG == 0, 
                  "lone parent", hh$type) 
hh$type <- ifelse(hh$DNNUMDEPS > 0 & hh$PCPTNRFG == 1, 
                  "couple w children", hh$type) 
hh$type <- ifelse(hh$PCPTNRFG==0 & hh$DNNUMDEPS==0 & hh$NNNONDEPS==0, 
                  "living alone", hh$type)
hh$type <- ifelse(hh$PCPTNRFG==1 & hh$DNNUMDEPS==0 & hh$NNNONDEPS==0, 
                  "couple only", hh$type)
hh$type <- ifelse(hh$PCPTNRFG==1 & hh$DNNUMDEPS==0 & hh$NNNONDEPS>0, 
                  "couple w nondeps", hh$type)
hh$type <- ifelse(hh$PCPTNRFG==0 & hh$DNNUMDEPS==0 & hh$NNNONDEPS>0, 
                  "single w nondeps", hh$type)

# categorise as working / pensionable age
hh$workage <- "working age" # initialise with default value
hh$workage <- ifelse(hh$PCPTNRFG==0 & hh$CCage>64, 
                     "single pensioner", hh$workage)
hh$workage <- ifelse(hh$PCPTNRFG==1 & (hh$CCage>64 | hh$PPage>64), 
                     "couple, one pensioner", hh$workage)
hh$workage <- ifelse(hh$PCPTNRFG==1 & hh$CCage>64 & hh$PPage>64, 
                     "couple pensioners", hh$workage)

# modified-OECD equivalence scale
# usually has first adult as 1.0, but rescaled here for two adults as 1.0 (as in HBAI)
# first adult: 0.67
# subsequent adults: 0.33
# children 14+: 0.33
# children under 13: 0.2

# calculate equivalence scale for each household / benefit unit
deps$equiv_deps <- ifelse(deps$age > 13, 0.33, 0.2)
depsequiv <- deps %>% 
  filter(CCSUBTYPE=="dependant") %>%
  group_by(id) %>% 
  summarise(equiv_deps=sum(equiv_deps))
hh$equiv_claimants <- ifelse(hh$PCPTNRFG==1, 1, 0.67)
hh <- left_join(hh, depsequiv, by="id")
hh$equiv_deps <- ifelse(is.na(hh$equiv_deps), 0, hh$equiv_deps)
hh$equiv_total <- hh$equiv_claimants + hh$equiv_deps 
rm(depsequiv)

# import the income sources to use, add them up for main claimant and partner
incomeSources <- read.csv("data/HMRCpovIncomeVariables.csv", header=T)
incomeSources.clmt <- as.vector(incomeSources$field.clmnt)
incomeSources.ptnr <- as.vector(incomeSources$field.ptnr)[!is.na(incomeSources$field.ptnr)]

# figures are in pence per week
hh.income  <- shbe[shbe$REC_TYPE=="D", ] # table for household incomes
hh.income$inc.claimant <- rowSums(hh.income[incomeSources.clmt], 
                                  na.rm=T) # calculate income of main claimant, pence per week
hh.income$inc.partner <- rowSums(hh.income[incomeSources.ptnr],
                                 na.rm=T) # income of partner (net), pence per week

# note that rowSums(.., na.rm=T) evaluates to 0 if all income data is missing - hence need to reinsert missing value below for some records

# sum total income and calculate equivalised income
hh.income <- select(hh.income, REC_TYPE, LCHBREF, LCLCTRREF, PASSPORTIND, inc.claimant, inc.partner)
hh.income <- mutate(hh.income, 
                    inc.total = inc.claimant + inc.partner, 
                    # this next line inserts an absurd value for records which have neither income data or passport benefit indicator
                    inc.total = ifelse(is.na(PASSPORTIND), 99999999, inc.total)) 
hh.income$id <- ifelse(is.na(hh.income$LCHBREF) | hh.income$LCHBREF=="", 
                       hh.income$LCLCTRREF, hh.income$LCHBREF)
hh <- left_join(hh, hh.income[,c(4:8)], by="id")
hh$inc.equiv <- hh$inc.total / hh$equiv_total
hh <- mutate(hh, inc.equiv.as.pc.median = (inc.equiv/povthreshold)*0.6)
hh$poverty <- 0
hh$poverty <- ifelse(hh$PASSPORTIND %in% c(1,2,3,5) | hh$inc.equiv < povthreshold, 
                     1, 
                     hh$poverty)

# numbers of pensioners and working age people in each household
hh$numpensioners <- 0
hh$numpensioners <- ifelse(hh$workage=="couple pensioners", 2, hh$numpensioners)
hh$numpensioners <- ifelse(hh$workage=="couple, one pensioner", 1, hh$numpensioners)
hh$numpensioners <- ifelse(hh$workage=="single pensioner", 1, hh$numpensioners)
hh$numworkage <- 0
hh$numworkage <- ifelse(hh$workage=="couple, one pensioner", 1, hh$numworkage)
hh$numworkage <- ifelse(hh$workage=="working age" & hh$equiv_claimants==0.67, 
                        1, hh$numworkage)
hh$numworkage <- ifelse(hh$workage=="working age" & hh$equiv_claimants==1, 
                        2, hh$numworkage)

# link addresses to LSOA codes 
hh <- left_join(hh, hh.addresses, by="id")
postcodelkup <- read.csv("data/oxonpostcodeswithOAs-May2015.csv",
                         header=T)
hh <- left_join(hh, select(postcodelkup, pcds, lsoa11), by=c("CCPCODE"="pcds"))

# create a data frame with variable names appropriate for longitudinal analysis
var.yr <- function(dataframe, refyear) {
  names(dataframe) <- paste0(names(dataframe), ".", refyear)
  return(dataframe)
}

# # this version for creating data frame outputs
# assign(paste0("hh.", refyear, ".", substr(refdate, 4, 5)), 
#        hh %>% 
#         select(id, DNNUMDEPS, numpensioners, numworkage, poverty, lsoa11, CCSEX,  
#                equiv_total, PASSPORTIND, inc.equiv.as.pc.median, CCTEN) %>% 
#         left_join(select(hh.allvars, LCHBREF, CASSSC, CABENCAP, LADISPYT, CACRENT, CCCRENTP, CCNONSELF, CNLHAROOMS, LCLHA, 
#                          LCLHA, CALHA, CCNHRA, CNWKLHRS, PNWKLHRS, LNBDRMS), by=c("id"="LCHBREF")) %>% 
#         var.yr(refyear))
# 
# # calculate numbers by LSOA
# assign(paste0("HBpovbyLSOA.", refyear, ".", substr(refdate, 4, 5)),
#        hh %>%
#         filter(poverty==1) %>%
#         group_by(lsoa11) %>%
#         summarise(HBchildpov=sum(DNNUMDEPS),
#             HBpensionpov=sum(numpensioners),
#             HBworkingpov=sum(numworkage)) %>%
#         mutate(HBallpov=HBchildpov + HBpensionpov + HBworkingpov))

# # this version for creating csv file outputs
hhForAnalysis <- hh %>% 
  select(id, DNNUMDEPS, numpensioners, numworkage, poverty, lsoa11, CCSEX,  
         equiv_total, PASSPORTIND, inc.equiv.as.pc.median, CCTEN) %>% 
  left_join(select(hh.allvars, LCHBREF, CASSSC, CABENCAP, LADISPYT, CACRENT, CCCRENTP, CCNONSELF, CNLHAROOMS, LCLHA, 
                   LCLHA, CALHA, CCNHRA, CNWKLHRS, PNWKLHRS, LNBDRMS), by=c("id"="LCHBREF")) %>% 
  var.yr(refyear)
write.csv(hhForAnalysis, paste0("../notForGitHub/hh.", refyear, ".", substr(refdate, 4, 5), ".csv"))

# HBpovbyLSOA <- hh %>%
#         filter(poverty==1) %>%
#         group_by(lsoa11) %>%
#         summarise(HBchildpov=sum(DNNUMDEPS),
#             HBpensionpov=sum(numpensioners),
#             HBworkingpov=sum(numworkage)) %>%
#         mutate(HBallpov=HBchildpov + HBpensionpov + HBworkingpov)
# write.csv(HBpovbyLSOA, paste("outputs/povEst-hbOnly-", refyear, "-", substr(refdate, 4, 5), ".csv", sep=""))

rm(deps, headandfoot, hh, hh.addresses, hh.bentype, hh.income, shbe, refyear, refdate, povthreshold, shbefile, hh.allvars)
