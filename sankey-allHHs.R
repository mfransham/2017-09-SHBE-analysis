#######################################################################
#### Sankey diagrams to show poverty persistence - all households chart
#######################################################################

library(dplyr)
library(riverplot)

###################
# data prep
###################

# read in data
hhjoin.10.12 <- read.csv("../notForGitHub/hh.join.2010-2012.csv",
                         header=T,
                         stringsAsFactors = F)
hhjoin.12.14 <- read.csv("../notForGitHub/hh.join.2012-2014.csv",
                         header = T,
                         stringsAsFactors = F)
hh2010 <- read.csv("../notForGitHub/hh.2010.07.csv",
                   header=T,
                   stringsAsFactors = F)
hh2012 <- read.csv("../notForGitHub/hh.2012.08.csv",
                   header=T,
                   stringsAsFactors = F)
hh2014 <- read.csv("../notForGitHub/hh.2014.07.csv",
                   header=T,
                   stringsAsFactors = F)

# define HH types
hhtypeDefs <- read.csv("data/hhtypeDefinitions.csv", stringsAsFactors = F)
hh2010 <- mutate(hh2010, children.2010 = ifelse(DNNUMDEPS.2010>0, "kids", "no kids"))
names(hhtypeDefs) <- paste0(names(hhtypeDefs), ".2010")
hh2010 <- left_join(hh2010, hhtypeDefs, by=c("children.2010", "numpensioners.2010", "numworkage.2010"))

# 2012
hhtypeDefs <- read.csv("data/hhtypeDefinitions.csv", stringsAsFactors = F)
hh2012 <- mutate(hh2012, children.2012 = ifelse(DNNUMDEPS.2012>0, "kids", "no kids"))
names(hhtypeDefs) <- paste0(names(hhtypeDefs), ".2012")
hh2012 <- left_join(hh2012, hhtypeDefs, by=c("children.2012", "numpensioners.2012", "numworkage.2012"))

# 2014
hhtypeDefs <- read.csv("data/hhtypeDefinitions.csv", stringsAsFactors = F)
hh2014 <- mutate(hh2014, children.2014 = ifelse(DNNUMDEPS.2014>0, "kids", "no kids"))
names(hhtypeDefs) <- paste0(names(hhtypeDefs), ".2014")
hh2014 <- left_join(hh2014, hhtypeDefs, by=c("children.2014", "numpensioners.2014", "numworkage.2014"))
rm(hhtypeDefs)

# join equivalised income and household type to longitudinal files
hhjoin.10.12.v2 <- hhjoin.10.12 %>% 
  left_join(select(hh2010, id.2010, inc.equiv.as.pc.median.2010, hhtype.2010), by="id.2010") %>% 
  left_join(select(hh2012, id.2012, inc.equiv.as.pc.median.2012, hhtype.2012), by="id.2012") %>% 
  mutate(income2010 = ifelse(is.na(inc.equiv.as.pc.median.2010), "notclaiming2010", 
                            ifelse(inc.equiv.as.pc.median.2010 < 0.6, "poor2010", 
                                   ifelse(inc.equiv.as.pc.median.2010 < 0.8, "nearpoor2010", "notpoor2010"))),
         income2012 = ifelse(is.na(inc.equiv.as.pc.median.2012), "notclaiming2012", 
                             ifelse(inc.equiv.as.pc.median.2012 < 0.6, "poor2012", 
                                    ifelse(inc.equiv.as.pc.median.2012 < 0.8, "nearpoor2012", "notpoor2012")))) %>% 
  group_by(income2010, income2012) %>% 
  summarise(count=n()) %>% 
  ungroup()

# 2012 to 2014
hhjoin.12.14.v2 <- hhjoin.12.14 %>% 
  left_join(select(hh2012, id.2012, inc.equiv.as.pc.median.2012, hhtype.2012), by="id.2012") %>% 
  left_join(select(hh2014, id.2014, inc.equiv.as.pc.median.2014, hhtype.2014), by="id.2014") %>% 
  mutate(income2012 = ifelse(is.na(inc.equiv.as.pc.median.2012), "notclaiming2012", 
                             ifelse(inc.equiv.as.pc.median.2012 < 0.6, "poor2012", 
                                    ifelse(inc.equiv.as.pc.median.2012 < 0.8, "nearpoor2012", "notpoor2012"))),
         income2014 = ifelse(is.na(inc.equiv.as.pc.median.2014), "notclaiming2014", 
                             ifelse(inc.equiv.as.pc.median.2014 < 0.6, "poor2014", 
                                    ifelse(inc.equiv.as.pc.median.2014 < 0.8, "nearpoor2014", "notpoor2014")))) %>% 
  group_by(income2012, income2014) %>% 
  summarise(count=n()) %>% 
  ungroup()

# combine data for use in Sankey chart
names(hhjoin.10.12.v2) <- c("N1", "N2", "Value")
names(hhjoin.12.14.v2) <- c("N1", "N2", "Value")
edges <- bind_rows(hhjoin.10.12.v2, hhjoin.12.14.v2)
rm(hhjoin.10.12.v2, hhjoin.12.14.v2)

# create the nodes and their positions on the chart
nodes <- data.frame(ID = paste0(c(rep("poor",3), rep("nearpoor",3), rep("notpoor",3), rep("notclaiming",3)), c("2010","2012","2014")), 
                        x = rep(c(1:3),4),
                        labels = c("", "poor", "", "", "near poor", "", "", "not poor", "", "", "not claiming HB in Oxford", ""),
                        stringsAsFactors = F)

########################
# plot chart
########################
						
# set cex value for charts
par()              # view current settings
opar <- par()      # make a copy of current settings
par(cex=0.9) 

# set styles for Sankey charts
style <- default.style()
style[["col"]] <- "#80808050" # transparent grey
plotarea = 0.7
linetype = 0
nodemargin = 0.5
labelrotate = 0 

# create data for plotting 
sankeyIncomes <- makeRiver(nodes = as.data.frame(nodes), 
                           edges = as.data.frame(edges))
sankeyIncomes$nodes$labels[11] <- "" # remove extraneous label

# remove flows between 'not claiming' statuses
sankeyIncomes$edges[sankeyIncomes$edges$ID=="notclaiming2010->notclaiming2012", ]$Value <- 0 
sankeyIncomes$edges[sankeyIncomes$edges$ID=="notclaiming2012->notclaiming2014", ]$Value <- 0 

# set styles and plot
style <- default.style()
style[["col"]] <- "#80808050" # transparent grey
plot(sankeyIncomes, default_style=style, lty=linetype, node_margin=nodemargin, srt=labelrotate, plot_area=plotarea)

# add some text to show the dates 
text(1, -36000, "2010")
text(2, -36000, "2012")
text(3, -36000, "2014")

# rectangle to obscure 'unobserved' transitions
rect(xleft=0.9, xright=3.1, ybottom = -6000, ytop = 2000, col = "#75757599", lty = 2)
text(2, -460, "not claiming housing benefit in Oxford", col="white")
text(2, -3000, "(unobserved unless housing benefit claim is made)", col="white")

# restore original settings 
par(opar)          
rm(opar)
