# Neighbourhood income poverty dynamics - analysis using SHBE (housing benefit) data

This repository contains R code which uses housing benefit data (SHBE - single housing benefit extract) to analyse neighbourhood income poverty dynamics.  This code was used to produce the analysis in 

Fransham, M (forthcoming) Income and population dynamics in deprived neighbourhoods - measuring the poverty turnover rate using administrative data, *Applied Spatial Analysis and Policy*

It is provided so that other researchers can reproduce the analysis using their own data.  There are five code files included in this repository which are briefly described below.  Additional files in the /data/ folder contain data used by the code.  

## loops.R

Loops through a series of files, running povertyCalculator.R on SHBE files in a specified directory, and linking specified SHBE files together over time.  

## povertyCalculator.R

This code is used in one of the loops above.  It:

* takes a SHBE file as input (in pipe-separated format) specified in 'shbefile' variable and appends variable names (data/shbe-colnames-Apr2015.csv)
* calculates equivalised household income for the benefit unit on a subset of income sources (listed in data/HMRCpovIncomeVariables.csv)
* compares this to a given poverty threshold for the reference year in question (contained in data/hmrcPovThresholds.csv)
* spits out aggregation at LSOA level (using a postcode lookup in data/oxonpostcodeswithOAs-May2015.csv) and HH level file with defined characteristics (using household types defined in data/hhtypeDefinitions.csv).

The code removes benefit units claiming council tax benefit only.  These cases were only included in files before April 2013 when CTB was abolished.  The cases are removed from the analysis in order to provide comparability over time.  It has the effect of removing a large number of pensionable age families from the analysis.  

## matchingOverTime-loop.R

This code is also used in loops.R.  It takes a pair of files (the list of which is specified in data/shbe-filematches.csv) and matches benefit units within these files over time, using a series of deterministic matching procedures.  For each pair of matched files it outputs a file which contains lookups between files.  

## povertyTurnoverRates2010to2014.R

This calculates poverty turnover rates for Oxford between 2010 and 2014, producing a funnel plot and LSOA map of the results. 

## sankey-allHHs.R

This code produces a Sankey diagram showing transitions of families between time points.  
