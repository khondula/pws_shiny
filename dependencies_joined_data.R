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

data_pws <- read_csv("/nfs/pws-data/data/city_join.csv")

levels(data_pws$PWS_Final)[1] <- "NO" # change the first level into "NO"
data_pws$PWS_Final[is.na(data_pws$PWS_Final)] <- "NO" # make all NA's have factor level "NO"

# # make new column for binary outcome variable
data_pws$PWS_binary[data_pws$PWS_Final == "YES"] <- 1
data_pws$PWS_binary[data_pws$PWS_Final == "NO"] <- 0

# make a list of the numeric variables to use for inputs to the shiny
# take out the row IDs column
data_pws <- data_pws[,-1]
data_pws$city_name <- data_pws$CWM_City
pws_numeric_vars <- names(data_pws)[sapply(names(data_pws), function(x) is.numeric(data_pws[,x]))]
