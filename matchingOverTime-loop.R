# variables to match benefit units over time

library(dplyr)
library(ggplot2)

shbefile.1 <- as.character(filter(SHBEfile.matches, matchNo == matchIndex) %>% select(shbefile.1))
shbefile.2 <- as.character(filter(SHBEfile.matches, matchNo == matchIndex) %>% select(shbefile.2))

# read files
shbe.1 <- read.csv(shbefile.1, 
                 sep = "|",
                 header=F, 
                 strip.white=T,
                 stringsAsFactors = F)
shbe.2 <- read.csv(shbefile.2, 
                   sep = "|",
                   header=F, 
                   strip.white=T,
                   stringsAsFactors = F)

# variable names
shbecolnames <- read.csv("data/shbe-colnames-Apr2015.csv",
                         header=F,
                         strip.white=T)
colnames(shbe.1) <- shbecolnames$V1 
colnames(shbe.2) <- shbecolnames$V1 
rm(shbecolnames)

# remove header and footer, store elsewhere, create reference dates and years
headandfoot.1 <- shbe.1[c(1, nrow(shbe.1)), ] # store header and trailer records
shbe.1 <- shbe.1[-c(1, nrow(shbe.1)), ] # remove header and trailer records
refdate.1 <- headandfoot.1[1,3]
refyear.1 <- as.numeric(substr(refdate.1, 7, 10))

headandfoot.2 <- shbe.2[c(1, nrow(shbe.2)), ] # store header and trailer records
shbe.2 <- shbe.2[-c(1, nrow(shbe.2)), ] # remove header and trailer records
refdate.2 <- headandfoot.2[1,3]
refyear.2 <- as.numeric(substr(refdate.2, 7, 10))

# create a single 'id' variable
shbe.1 <- shbe.1 %>% 
  mutate(id = ifelse(is.na(LCHBREF) | LCHBREF=="", LCLCTRREF, LCHBREF))
shbe.2 <- shbe.2 %>% 
  mutate(id = ifelse(is.na(LCHBREF) | LCHBREF=="", LCLCTRREF, LCHBREF))

# select main D records for each time point
shbe.1.hh <- shbe.1 %>% filter(REC_TYPE=="D") %>% select(id, CCNINO, CCFORENAME, CCFORENAME2, CCSURNAME, CDDOB, CCSEX, CCPCODE, PCNINO, PCSURNAME, PCFORENAME)
colnames(shbe.1.hh) <- paste(colnames(shbe.1.hh), "1", sep=".")
shbe.2.hh <- shbe.2 %>% filter(REC_TYPE=="D") %>% select(id, CCNINO, CCFORENAME, CCFORENAME2, CCSURNAME, CDDOB, CCSEX, CCPCODE, PCNINO, PCSURNAME, PCFORENAME)
colnames(shbe.2.hh) <- paste(colnames(shbe.2.hh), "2", sep=".")

# link these together
hh.join <- full_join(shbe.1.hh, shbe.2.hh, by=c("id.1"="id.2"))

# let's see if there are people I can match who aren't matched by housing benefit ID
hh.join.unmatch <- filter(hh.join, is.na(CCNINO.1) | is.na(CCNINO.2)) 
hh.join.match <- filter(hh.join, !id.1 %in% hh.join.unmatch$id.1) %>% 
  select(id.1) %>% mutate(id.2=id.1)

# match on NI number
hh.join.match2 <- inner_join(hh.join.unmatch[c(1,2:11)], hh.join.unmatch[c(1,12:21)], by=c("CCNINO.1"="CCNINO.2")) %>% 
  filter(!CCNINO.1=="") 
hh.join.match <- rbind(hh.join.match, 
                       select(hh.join.match2, id.1.x, id.1.y) %>% 
                         rename(id.1=id.1.x, id.2=id.1.y))
hh.join.unmatch <- filter(hh.join, !id.1 %in% hh.join.match$id.1 & !id.1 %in% hh.join.match$id.2)

# match on surname and DoB
hh.join.match3 <- inner_join(hh.join.unmatch[c(1,2:11)], hh.join.unmatch[c(1,12:21)], 
                             by=c("CCSURNAME.1"="CCSURNAME.2", 
                                  "CDDOB.1"="CDDOB.2")) 
# Not using because it will be matched below when joining partner and claimant NI numbers

# match on partner NI number - main claimant becoming partner
hh.join.match4 <- inner_join(hh.join.unmatch[c(1,2:11)], hh.join.unmatch[c(1,12:21)], 
                             by=c("CCNINO.1"="PCNINO.2")) %>% 
  filter(!CCNINO.1=="") 
hh.join.match <- rbind(hh.join.match, 
                       select(hh.join.match4, id.1.x, id.1.y) %>% 
                         rename(id.1=id.1.x, id.2=id.1.y))
hh.join.unmatch <- filter(hh.join, !id.1 %in% hh.join.match$id.1 & !id.1 %in% hh.join.match$id.2)

# match on partner NI number - partner becoming main claimant
hh.join.match5 <- inner_join(hh.join.unmatch[c(1,2:11)], hh.join.unmatch[c(1,12:21)], 
                             by=c("PCNINO.1"="CCNINO.2")) %>% 
  filter(!PCNINO.1=="") 
hh.join.match <- rbind(hh.join.match, 
                       select(hh.join.match5, id.1.x, id.1.y) %>% 
                         rename(id.1=id.1.x, id.2=id.1.y))
hh.join.unmatch <- filter(hh.join, !id.1 %in% hh.join.match$id.1 & !id.1 %in% hh.join.match$id.2)

# add in the unmatched id numbers
hh.join.unmatch <- select(hh.join.unmatch, id.1, CCNINO.1, CCNINO.2)
hh.join.unmatch <- mutate(hh.join.unmatch,
                          id = id.1,
                          id.1 = ifelse(is.na(CCNINO.1), NA, id),
                          id.2 = ifelse(is.na(CCNINO.2), NA, id))
hh.join.unmatch <- select(hh.join.unmatch, id.1, id.2)
hh.join.final <- rbind(hh.join.match, hh.join.unmatch)

# name the variables with the reference year
stem <- substr(colnames(hh.join.final), 1, nchar(colnames(hh.join.final))-1)
suffix <- substr(colnames(hh.join.final), nchar(colnames(hh.join.final)), nchar(colnames(hh.join.final)))
suffix <- ifelse(suffix=="1", refyear.1, refyear.2)
colnames(hh.join.final) <-  paste0(stem, suffix)
rm(stem, suffix)

# write out to a csv file without NI numbers
write.csv(hh.join.final, paste0("../notForGitHub/hh.join.", refyear.1, "-", refyear.2, ".csv"))