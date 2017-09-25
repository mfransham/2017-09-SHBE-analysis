#################
# loops
#################

# calculate poverty status for a list of SHBE files
SHBEfiles <- dir("../../SHBE archive/", pattern = ".txt", full.names = T) # list all the SHBE file names in this directory
for (shbefile in SHBEfiles) source("povertyCalculator.R") # run povertyCalculator.R on each file

# link files over time
SHBEfile.matches <- read.csv("data/shbe-filematches.csv", stringsAsFactors = F) # make the links specified in this file
for (matchIndex in SHBEfile.matches$matchNo) source("matchingOverTime-loop.R") # run matchingOverTime-loop.R on each pair of specified files

