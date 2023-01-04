# Creating Data Files

#Step 1: Delete the old "Data_All.Rmd" in data/data_main

#Step 2: Add new vaccination data into data/data_main, making sure its in the same style as the most recent data

#Step 3: In ALL code below, replace the old raw data file name with the new data

#Step 4: Run all the following code, line by line

#Step 5: Close this window, and run the app

# dataName <- 

#For Total
weights.df <- read_excel("data/data_main/Weights.xlsx", sheet = "HSU&ERP")
TFVacc_nontotal.df <- read_excel("data/data_main/VaccinationData/2021-11-01.xlsx", sheet = "TotalFullVacc")
MFVacc_nontotal.df <- read_excel("data/data_main/VaccinationData/2021-11-01.xlsx", sheet = "MaoriFullVacc")
NMFVacc_nontotal.df <- read_excel("data/data_main/VaccinationData/2021-11-01.xlsx", sheet = "NonMaoriFullVacc")
TFVacc_DHBtotal.df <- read_excel("data/data_main/VaccinationData/2021-11-01.xlsx", sheet = "TotalFullVacc")
MFVacc_DHBtotal.df <- read_excel("data/data_main/VaccinationData/2021-11-01.xlsx", sheet = "MaoriFullVacc")
NMFVacc_DHBtotal.df <- read_excel("data/data_main/VaccinationData/2021-11-01.xlsx", sheet = "NonMaoriFullVacc")

source("data/data_main/Data_Total.R")

#For Females
weights.df <- read_excel("data/data_main/Weights.xlsx", sheet = "HSU&ERP")
TFVacc_SFnontotal.df <- read_excel("data/data_main/VaccinationData/2021-11-01.xlsx", sheet = "TotalFullVacc")
MFVacc_SFnontotal.df <- read_excel("data/data_main/VaccinationData/2021-11-01.xlsx", sheet = "MaoriFullVacc")
NMFVacc_SFnontotal.df <- read_excel("data/data_main/VaccinationData/2021-11-01.xlsx", sheet = "MaoriFullVacc")
TFVacc_DHB_SFtotal.df <- read_excel("data/data_main/VaccinationData/2021-11-01.xlsx", sheet = "TotalFullVacc")
MFVacc_DHB_SFtotal.df <- read_excel("data/data_main/VaccinationData/2021-11-01.xlsx", sheet = "MaoriFullVacc")
NMFVacc_DHB_SFtotal.df <- read_excel("data/data_main/VaccinationData/2021-11-01.xlsx", sheet = "MaoriFullVacc")


source("data/data_main/Data_Female.R")

#For Females
weights.df <- read_excel("data/data_main/Weights.xlsx", sheet = "HSU&ERP")
TFVacc_SMnontotal.df <- read_excel("data/data_main/VaccinationData/2021-11-01.xlsx", sheet = "TotalFullVacc")
MFVacc_SMnontotal.df <- read_excel("data/data_main/VaccinationData/2021-11-01.xlsx", sheet = "MaoriFullVacc")
NMFVacc_SMnontotal.df <- read_excel("data/data_main/VaccinationData/2021-11-01.xlsx", sheet = "MaoriFullVacc")
TFVacc_DHB_SMtotal.df <- read_excel("data/data_main/VaccinationData/2021-11-01.xlsx", sheet = "TotalFullVacc")
MFVacc_DHB_SMtotal.df <- read_excel("data/data_main/VaccinationData/2021-11-01.xlsx", sheet = "MaoriFullVacc")
NMFVacc_DHB_SMtotal.df <- read_excel("data/data_main/VaccinationData/2021-11-01.xlsx", sheet = "MaoriFullVacc")


source("data/data_main/Data_Male.R")


#Step 3: Save this data so it can be used

Data_All <- list(Total = DataTotal,
                 Male = DataMale,
                 Female = DataFemale)

save(Data_All, file = "data/shiny_app_data.Rmd")
