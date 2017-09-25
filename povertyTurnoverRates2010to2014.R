##########################################################
# calculate poverty turnover rates by LSOA
##########################################################

library(tmap)
library(ggplot2)
library(dplyr)
library(reshape2)
library(pander)
library(rgeos)
library(maptools)

############################
# data processing
############################

# create data frame
hh.2010 <- read.csv("../notForGitHub/hh.2010.07.csv")
hh.2014 <- read.csv("../notForGitHub/hh.2014.07.csv")
hh.lper <- read.csv("../notForGitHub/hh.join.2010-2014.csv")
hh.lper <- left_join(hh.lper, hh.2010, by="id.2010")
hh.lper <- select(hh.lper, id.2010, id.2014, lsoa11.2010, DNNUMDEPS.2010, numpensioners.2010, 
                  numworkage.2010, poverty.2010, CCSEX.2010, PASSPORTIND.2010,
                   CCTEN.2010, CNWKLHRS.2010, PNWKLHRS.2010)
hh.lper <- left_join(hh.lper, hh.2014[, c(2,6,7,12,23,24)], by="id.2014")
hh.lper <- filter(hh.lper, poverty.2010==1)
hh.lper <- mutate(hh.lper, poverty.2014 = ifelse(is.na(poverty.2014), 0, poverty.2014))
rm(hh.2010,hh.2014)

# recode categorical variables as factors
hh.lper$PASSPORTIND.2010 <- factor(hh.lper$PASSPORTIND.2010, 
                                    levels = c(1:5),
                                    labels = c("Income Support", 
                                               "Pension Credit (GC)", 
                                               "JSA (IB)", 
                                               "Standard", 
                                               "ESA (IB)"))
hh.lper <- mutate(hh.lper, 
                   CCTEN.2010 = ifelse(CCTEN.2010 %in% c(2,5,8,9), 99, CCTEN.2010),
                   CCTEN.2010 = ifelse(CCTEN.2010 %in% c(1,4), 98, CCTEN.2010), 
                   CCTEN.2010 = factor(CCTEN.2010, levels = c(3,98,99), labels = c("PRS", "RSL", "Other")))
hh.lper <- mutate(hh.lper, 
                  CCTEN.2014 = ifelse(CCTEN.2014 %in% c(2,5,8,9), 99, CCTEN.2014),
                  CCTEN.2014 = ifelse(CCTEN.2014 %in% c(1,4), 98, CCTEN.2014), 
                  CCTEN.2014 = factor(CCTEN.2014, levels = c(3,98,99), labels = c("PRS", "RSL", "Other")))

# define HH types
hhtypeDefs <- read.csv("data/hhtypeDefinitions.csv", stringsAsFactors = F)
names(hhtypeDefs) <- paste0(names(hhtypeDefs), ".2010")
hh.lper <- mutate(hh.lper, children.2010 = ifelse(DNNUMDEPS.2010>0, "kids", "no kids"))
hh.lper <- left_join(hh.lper, hhtypeDefs, by=c("children.2010", "numpensioners.2010", "numworkage.2010")) %>% 
  mutate(hhtype.2010 = factor(hhtype.2010))
rm(hhtypeDefs)

# recode working hours variables
hh.lper <- mutate(hh.lper, 
                   CNWKLHRS.2010 = ifelse(is.na(CNWKLHRS.2010), 0, CNWKLHRS.2010), 
                   PNWKLHRS.2010 = ifelse(is.na(PNWKLHRS.2010), 0, PNWKLHRS.2010),
                   workhours.2010 = ifelse(CNWKLHRS.2010 + PNWKLHRS.2010 == 0, "out of work", "in work"),
                    CNWKLHRS.2014 = ifelse(is.na(CNWKLHRS.2014), 0, CNWKLHRS.2014), 
                    PNWKLHRS.2014 = ifelse(is.na(PNWKLHRS.2014), 0, PNWKLHRS.2014),
                    workhours.2014 = ifelse(CNWKLHRS.2014 + PNWKLHRS.2014 == 0, "out of work", "in work"))

# code variables for poverty exit, migration, tenancy change and poverty turnover
hh.lper <- hh.lper %>% mutate(povexit.2014 = ifelse(poverty.2014==0, 1, 0), 
                           migrated.2014 = ifelse(!(as.character(lsoa11.2010)==as.character(lsoa11.2014)), 1, 0), 
                           tenancy.chg = ifelse(CCTEN.2010==CCTEN.2014, 0, 1),
                           localexit = ifelse(povexit.2014==1 | migrated.2014==1, 1, 0))

# calculate LA mean exit rate
lper.LAmean <- sum(hh.lper$localexit, na.rm=T) / nrow(hh.lper)

# aggregate proportion of turnover by 2010 LSOA
LSOA.lper <- hh.lper %>% 
  group_by(lsoa11.2010) %>% 
  summarise(exits = sum(localexit, na.rm=T), total = n()) %>% 
  mutate(lper = exits / total, 
         se = sqrt(lper*(1-lper)/total),
         CIupper = lper + qnorm(0.975)*se,
         CIlower = lper - qnorm(0.975)*se,
         sigtext = ifelse(CIlower>lper.LAmean & total>50, "high exit rate", 
                          ifelse(CIupper<lper.LAmean & total>50, "low exit rate", "")))

# select Oxford LSOAs only
oxfLSOAs <- read.csv("../../../Data/Lookups/LSOA11_BUASD11_BUA11_LAD11_RGN11_EW_LU/LSOA11_BUASD11_BUA11_LAD11_RGN11_EW_LU.csv", 
                     header=T, 
                     stringsAsFactors = F) %>% 
  filter(LAD11NM=="Oxford") %>% select(LSOA11CD)
LSOA.lper <- LSOA.lper %>% filter(lsoa11.2010 %in% oxfLSOAs$LSOA11CD)
rm(oxfLSOAs)

#############################################################
# funnel plot of persistent poverty rate vs all in poverty
#############################################################

f.pov <- c(1:350)
f.CI <- qnorm(0.975)*sqrt(lper.LAmean*(1 - lper.LAmean)/f.pov)
f.low <- (lper.LAmean - f.CI)
f.high <- (lper.LAmean + f.CI)
f.CI99 <- qnorm(0.995)*sqrt(lper.LAmean*(1 - lper.LAmean)/f.pov)
f.low99 <- (lper.LAmean - f.CI99)
f.high99 <- (lper.LAmean + f.CI99)
funnel <- as.data.frame(cbind(f.pov, f.low, f.high), stringsAsFactors = F)
funnel <- cbind(funnel[1], funnel[2:3]*100)
ggplot(LSOA.lper, aes(x=total, y=lper*100)) +
  geom_point() +
  ylim(c(20,80)) +
  theme_minimal() +
  geom_hline(yintercept=lper.LAmean*100, linetype=1) +
  geom_line(data=funnel, aes(x=f.pov, y=f.low), linetype=2) +
  geom_line(data=funnel, aes(x=f.pov, y=f.high), linetype=2) +
  # geom_line(data=funnel, aes(x=f.pov, y=f.low99), linetype=3) +
  # geom_line(data=funnel, aes(x=f.pov, y=f.high99), linetype=3) +
  annotate("text", x = 275, y=lper.LAmean+0.023, label="Oxford average", size=3.5) +
  labs(x="Number of households poor in 2010", y="Local poverty exit rate (%)")

###################################
# LSOA map
###################################

# read data and join Oxford poverty data
load("C:/Users/DPhil/OneDrive for Business/DPhil/Data/Boundary files/Output Areas/LSOA_2011_EW_BGC_shp/LSOA_2011_EW_BGC_Oxford.RData")
lsoas2 <- oxflsoas2011 %>% 
  left_join(LSOA.lper, by=c("LSOA11CD"="lsoa11.2010"))

# sort labels and significance text
lsoas2 <- lsoas2 %>% 
  mutate(sigtext2 = factor(sigtext,
                            levels=c("low exit rate", "", "high exit rate"),
                            labels = c("Lower", "No difference", "Higher"),
                            ordered = T), 
         lper.label = ifelse(!sigtext=="", round(lper*100, 0), "") )

# plot
ggplot(lsoas2, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=sigtext2), colour="Dark grey") +
  scale_fill_brewer(palette="Greys") +
  coord_equal() +
  scale_x_continuous("", breaks=NULL) + 
  scale_y_continuous("", breaks=NULL) +
  theme(legend.position=c(0.15,0.15), 
        panel.background = element_rect(fill="white")) +
  geom_text(aes(x=xCentroid, y=yCentroid, label=lper.label), size=4)
