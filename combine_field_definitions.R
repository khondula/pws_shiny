# combine field defintions from xlsx files into one csv file to use in shiny app

if("readxl" %in% rownames(installed.packages()) == FALSE) {
  install.packages("readxl", repos="http://cran.us.r-project.org")
}
library(readxl)

CWM_City_Data_Defs <- read_excel("/nfs/pws-data/data/CWM_City_Data_Final.xlsx", sheet = 2)
Country_Data_Defs <- read_excel("/nfs/pws-data/data/Country_Data_Final.xlsx", sheet = 2)

All_Field_defs <- rbind(CWM_City_Data_Defs, Country_Data_Defs)

write.csv(All_Field_defs, "All_Field_defs.csv", row.names = FALSE)
