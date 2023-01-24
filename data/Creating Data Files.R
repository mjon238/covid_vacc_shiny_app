# Creating Data Files

#Step 1: Delete the old "Data_All.Rmd" in data/data_main

#Step 2: Add new vaccination data into data/data_main, making sure its in the same style as the most recent data

#Step 3: In ALL code below, replace the old raw data file name with the new data

#Step 4: Run all the following code, line by line

#Step 5: Close this window, and run the app

#For Total

Data_All <- list(`2021-11-01` = NULL,
                 `2023-01-17` = NULL)
weights.df <- read_excel("data/data_main/Weights-2021-11-01.xlsx", sheet = "HSU&ERP")


#2021-11-01 Data ------
#Total
TFVacc_nontotal.df <- TFVacc_DHBtotal.df <- 
  TFVacc_SFnontotal.df <- TFVacc_DHB_SFtotal.df <- 
  TFVacc_SMnontotal.df <- TFVacc_DHB_SMtotal.df <- 
  read_excel("data/data_main/VaccinationData/2021-11-01.xlsx", sheet = "TotalFullVacc")

#Maori
MFVacc_nontotal.df <- MFVacc_DHBtotal.df <- 
  MFVacc_SFnontotal.df <- MFVacc_DHB_SFtotal.df <- 
  MFVacc_SMnontotal.df <- MFVacc_DHB_SMtotal.df <- 
  read_excel("data/data_main/VaccinationData/2021-11-01.xlsx", sheet = "MaoriFullVacc")

#Non-Maori
NMFVacc_nontotal.df <- NMFVacc_DHBtotal.df <- 
  NMFVacc_SFnontotal.df <- NMFVacc_DHB_SFtotal.df <-
  NMFVacc_SMnontotal.df <- NMFVacc_DHB_SMtotal.df <- 
  read_excel("data/data_main/VaccinationData/2021-11-01.xlsx", sheet = "NonMaoriFullVacc")

source("data/data_main/Data_Total.R")
source("data/data_main/Data_Female.R")
source("data/data_main/Data_Male.R")

#Remove Everything except all data
rm(list=setdiff(ls(), c("Data_All",
                        "DataTotal", 
                        "DataFemale", 
                        "DataMale")))


#Step 3: Save this data so it can be used

Data_All$`2021-11-01` <-  list(Total = DataTotal,
                         Male = DataMale,
                         Female = DataFemale)

rm(list=setdiff(ls(), c("Data_All")))           


#2023-01-01 Data -----
#Total
weights.df <- read_excel("data/data_main/Weights-2023-01-17.xlsx", sheet = "HSU&ERP")

TFVacc_nontotal.df <- TFVacc_DHBtotal.df <- 
  TFVacc_SFnontotal.df <- TFVacc_DHB_SFtotal.df <- 
  TFVacc_SMnontotal.df <- TFVacc_DHB_SMtotal.df <- 
  read_excel("data/data_main/VaccinationData/2023-01-17.xlsx", sheet = "TotalCPC")

#Maori
MFVacc_nontotal.df <- MFVacc_DHBtotal.df <- 
  MFVacc_SFnontotal.df <- MFVacc_DHB_SFtotal.df <- 
  MFVacc_SMnontotal.df <- MFVacc_DHB_SMtotal.df <- 
  read_excel("data/data_main/VaccinationData/2023-01-17.xlsx", sheet = "MaoriCPC")

#Non-Maori
NMFVacc_nontotal.df <- NMFVacc_DHBtotal.df <- 
  NMFVacc_SFnontotal.df <- NMFVacc_DHB_SFtotal.df <-
  NMFVacc_SMnontotal.df <- NMFVacc_DHB_SMtotal.df <- 
  read_excel("data/data_main/VaccinationData/2023-01-17.xlsx", sheet = "NonMaoriCPC")

source("data/data_main/Data_Total.R")
source("data/data_main/Data_Female.R")
source("data/data_main/Data_Male.R")

#Remove Everything except all data
rm(list=setdiff(ls(), c("Data_All",
                        "DataTotal", 
                        "DataFemale", 
                        "DataMale")))


#Step 3: Save this data so it can be used

Data_All$`2023-01-17` = list(Total = DataTotal,
                              Male = DataMale,
                              Female = DataFemale)


save(Data_All, file = "data/shiny_app_data.Rmd")

#FINALLY: Control Shift S to Run all!
