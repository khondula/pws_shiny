# this file will run once per session on the machine that shiny is running on
# use this to load packages up and the data files to be used throughout the app

# load required packages including shiny
if("shiny" %in% rownames(installed.packages()) == FALSE) {
  install.packages("shiny", repos="http://cran.us.r-project.org")
}
library(shiny)

if("ggplot2" %in% rownames(installed.packages()) == FALSE) {
  install.packages("ggplot2", repos="http://cran.us.r-project.org")
}
library(ggplot2)

if("readr" %in% rownames(installed.packages()) == FALSE) {
  install.packages("readr", repos="http://cran.us.r-project.org")
}
library(readr)

# if("permute" %in% rownames(installed.packages()) == FALSE) {
#   install.packages("permute", repos="http://cran.us.r-project.org")
# }
# library(permute)

# read in extra data files for shiny app
All_Field_defs <- read_csv("All_Field_defs.csv")
pvalues_glance <- read_csv("pvalues_glance.csv")

# data_pws <- read_csv("/nfs/pws-data/data/city_join.csv")

# read in data files
Country_Data = read_csv("/nfs/pws-data/data/Country_Data.csv")
CWM_City_Data = read_csv("/nfs/pws-data/data/CWM_City_Data.csv")
# CWM_City_Admin = read.csv("CWM_City_Admin.csv") 
# Program_Data = read.csv("Program_Data.csv") 
# Watershed_Data = read.csv("Watershed_Data.csv") 

#######################################################################
#######################################################################
#####################  Clean and prepare data  ########################
#######################################################################
#######################################################################

# change column names in different tables so they match 
# these will be the "keys" used for joining tables by city or country names
names(Country_Data)[names(Country_Data)=="Country"] <- "country_name"
names(CWM_City_Data)[names(CWM_City_Data)=="Country"] <- "country_name"
names(CWM_City_Data)[names(CWM_City_Data)=="CWM_City"] <- "city_name"
# names(Watershed_Data)[names(Watershed_Data)=="City_Name"] <- "city_name"

# join data tables: CWM_City_Data contains the info from Watershed_data, so 
# only really need to join CWM_City_Data and Country_Data
# merge1 <- merge(CWM_City_Data, Watershed_Data, by="city_name", all=TRUE) # outer join
# merge1 <- merge(CWM_City_Data, Watershed_Data, by="city_name", all.x=TRUE) # left outer join
data_pws <- merge(CWM_City_Data, Country_Data, by="country_name", all=TRUE)

# note: if data ends up with more than 534 obs, may need to delete extra rows from Country_Data csv file

# check out the outcome variable, PWS_Final
# table(data$PWS_Final) # need to fix PWS_Final 
# levels(data$PWS_Final) # check the factor levels. there are 3 levels.
levels(data_pws$PWS_Final)[1] <- "NO" # change the first level into "NO"
data_pws$PWS_Final[is.na(data_pws$PWS_Final)] <- "NO" # make all NA's have factor level "NO"

# # make new column for binary outcome variable
data_pws$PWS_binary[data_pws$PWS_Final == "YES"] <- 1
data_pws$PWS_binary[data_pws$PWS_Final == "NO"] <- 0

# make a list of the numeric variables to use for inputs to the shiny
# take out the row IDs column
# data_pws <- data_pws[,-1]
# data_pws$city_name <- data_pws$CWM_City
pws_numeric_vars <- names(data_pws)[sapply(names(data_pws), function(x) is.numeric(data_pws[,x]))]
