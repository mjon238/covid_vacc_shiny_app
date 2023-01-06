library(readxl); 
library(magrittr); 
library(dplyr); 
library(tidyr); 
library(reshape2); 
library(patchwork);
library(ggnewscale)

## Creating weights for each population type ----

## creating weights df
# weights.df <- read_excel("Weights.xlsx", sheet = "HSU&ERP")
# TFVacc_SFnontotal.df <- read_excel("VaccinationDataR Updated HSU2021.xlsx", sheet = "TotalFullVacc")
# MFVacc_SFnontotal.df <- read_excel("VaccinationDataR Updated HSU2021.xlsx", sheet = "MaoriFullVacc")
# NMFVacc_SFnontotal.df <- read_excel("VaccinationDataR Updated HSU2021.xlsx", sheet = "MaoriFullVacc")
# TFVacc_DHB_SFtotal.df <- read_excel("VaccinationDataR Updated HSU2021.xlsx", sheet = "TotalFullVacc")
# MFVacc_DHB_SFtotal.df <- read_excel("VaccinationDataR Updated HSU2021.xlsx", sheet = "MaoriFullVacc")
# NMFVacc_DHB_SFtotal.df <- read_excel("VaccinationDataR Updated HSU2021.xlsx", sheet = "MaoriFullVacc")


# HSU total weights
# using numbers
HSUn.df <- weights.df[-c(2:12), ]
HSUn.df <- pivot_longer(HSUn.df, cols = 3:20, names_to = "AgeGroup", values_to = "Weights") # changing from rows to column
HSUn.df <- HSUn.df[, c(3, 4)] # only showing weights by age-groups

# HSU Maori weights
# using numbers
HSUMn.df <- weights.df[-c(1:2, 4:12), ]
HSUMn.df <- pivot_longer(HSUMn.df, cols = 3:20, names_to = "AgeGroup", values_to = "Weights") # changing from rows to column
HSUMn.df <- HSUMn.df[, c(3, 4)] # only showing weights by age-groups

# HSU NonMaori weights
# using numbers
HSUNMn.df <- weights.df[-c(1:4, 6:12), ]
HSUNMn.df <- pivot_longer(HSUNMn.df, cols = 3:20, names_to = "AgeGroup", values_to = "Weights") # changing from rows to column
HSUNMn.df <- HSUNMn.df[, c(3, 4)] # only showing weights by age-groups


# ERP total weights
# using numbers
ERPn.df <- weights.df[-c(1:6, 8:12), ]
ERPn.df <- pivot_longer(ERPn.df, cols = 3:20, names_to = "AgeGroup", values_to = "Weights") # changing from rows to column
ERPn.df <- ERPn.df[, c(3, 4)] # only showing weights by age-groups

# ERP Maori weights
# using numbers
ERPMn.df <- weights.df[-c(1:8, 10:12), ]
ERPMn.df <- pivot_longer(ERPMn.df, cols = 3:20, names_to = "AgeGroup", values_to = "Weights") # changing from rows to column
ERPMn.df <- ERPMn.df[, c(3, 4)] # only showing weights by age-groups

# ERP NonMaori weights
# using numbers
ERPNMn.df <- weights.df[-c(1:10, 12), ]
ERPNMn.df <- pivot_longer(ERPNMn.df, cols = 3:20, names_to = "AgeGroup", values_to = "Weights") # changing from rows to column
ERPNMn.df <- ERPNMn.df[, c(3, 4)] # only showing weights by age-groups






## Total Fully Vaccinated Rates -----
# Total fully vaccinated
TFVacc_SFnontotal.df <- subset(TFVacc_SFnontotal.df, Group != "Total" & Group != "Male" & DHB != "Total")

TFVacc_SFnontotal.df <- pivot_longer(TFVacc_SFnontotal.df, cols = 4:20, names_to = "AgeGroup", values_to = "Count") # changing from rows to column
TFVacc_SFnontotal.df <- TFVacc_SFnontotal.df[, c(1, 4, 5, 3)] # rearranging columns to match Daniel's example below


# Using HSU Weights

## Join on the standard population weights (adds in a column called "Weights")
HSU_TFVacc_SFnontot.df <- left_join(TFVacc_SFnontotal.df, HSUn.df, by = "AgeGroup")


## Create a dataset for the total of each DHB (all age groups summed), doesn't include all DHB named 'Total'
HSU_TFVacc_SFtot.df <- HSU_TFVacc_SFnontot.df %>%
  group_by(DHB) %>%
  summarise(
    AgeGroup = "Total",
    Count = sum(Count), Total = sum(Weights) # sum weights as want to divide by total HSU population for Rate
  )

## Calculate the age-specific rates for each age group in each year
alpha <- 0.05 # added this in for 95% CI
HSU_TFVacc_SFnontot.df <- HSU_TFVacc_SFnontot.df %>%
  mutate(
    Rate = Count / Weights, # dividing by age-band population for rate
    RateMult = Rate * 100000,
    Variance = Rate / Weights, ## Var(R) = Count / Weights^2
    
    Rate_KeyfitzLwr = (pmax(0, Rate - qnorm(1 - alpha / 2) * sqrt(Variance), na.rm = TRUE)) * 100000,
    Rate_KeyfitzUpr = (Rate + qnorm(1 - alpha / 2) * sqrt(Variance)) * 100000,
    Rate_Gamma1Lwr = (0.5 * qchisq(alpha / 2, 2 * Count) / Weights) * 100000,
    Rate_Gamma1Upr = (0.5 * qchisq(1 - alpha / 2, 2 * (Count + 1)) / Weights) * 100000
  )

# Using ERP Weights

## Join on the standard population weights (adds in a column called "Weights")
ERP_TFVacc_SFnontot.df <- left_join(TFVacc_SFnontotal.df, ERPn.df, by = "AgeGroup")


## Create a dataset for the total of each DHB (all age groups summed), doesn't include all DHB named 'Total'
ERP_TFVacc_SFtot.df <- ERP_TFVacc_SFnontot.df %>%
  group_by(DHB) %>%
  summarise(
    AgeGroup = "Total",
    Count = sum(Count), Total = sum(Weights) # sum weights as want to divide by total HSU population for Rate
  )

## Calculate the age-specific rates for each age group in each year
alpha <- 0.05 # added this in for 95% CI
ERP_TFVacc_SFnontot.df <- ERP_TFVacc_SFnontot.df %>%
  mutate(
    Rate = Count / Weights, # dividing by age-band population for rate
    RateMult = Rate * 100000,
    Variance = Rate / Weights, ## Var(R) = Count / Weights^2
    
    Rate_KeyfitzLwr = (pmax(0, Rate - qnorm(1 - alpha / 2) * sqrt(Variance), na.rm = TRUE)) * 100000,
    Rate_KeyfitzUpr = (Rate + qnorm(1 - alpha / 2) * sqrt(Variance)) * 100000,
    Rate_Gamma1Lwr = (0.5 * qchisq(alpha / 2, 2 * Count) / Weights) * 100000,
    Rate_Gamma1Upr = (0.5 * qchisq(1 - alpha / 2, 2 * (Count + 1)) / Weights) * 100000
  )

## Maori Fully Vaccinated Rates ----

# Total fully vaccinated
MFVacc_SFnontotal.df <- subset(MFVacc_SFnontotal.df, Group != "Total" & Group != "Male" & DHB != "Total")

MFVacc_SFnontotal.df <- pivot_longer(MFVacc_SFnontotal.df, cols = 4:20, names_to = "AgeGroup", values_to = "Count") # changing from rows to column
MFVacc_SFnontotal.df <- MFVacc_SFnontotal.df[, c(1, 4, 5, 3)] # rearranging columns to match Daniel's example below


# Using HSU Weights

## Join on the standard population weights (adds in a column called "Weights")
HSU_MFVacc_SFnontot.df <- left_join(MFVacc_SFnontotal.df, HSUMn.df, by = "AgeGroup")


## Create a dataset for the total of each DHB (all age groups summed), doesn't include all DHB named 'Total'
HSU_MFVacc_SFtot.df <- HSU_MFVacc_SFnontot.df %>%
  group_by(DHB) %>%
  summarise(
    AgeGroup = "Total",
    Count = sum(Count), Total = sum(Weights) # sum weights as want to divide by total HSU population for Rate
  )

## Calculate the age-specific rates for each age group in each year
alpha <- 0.05 # added this in for 95% CI
HSU_MFVacc_SFnontot.df <- HSU_MFVacc_SFnontot.df %>%
  mutate(
    Rate = Count / Weights, # dividing by age-band population for rate
    RateMult = Rate * 100000,
    Variance = Rate / Weights, ## Var(R) = Count / Weights^2
    
    Rate_KeyfitzLwr = (pmax(0, Rate - qnorm(1 - alpha / 2) * sqrt(Variance), na.rm = TRUE)) * 100000,
    Rate_KeyfitzUpr = (Rate + qnorm(1 - alpha / 2) * sqrt(Variance)) * 100000,
    Rate_Gamma1Lwr = (0.5 * qchisq(alpha / 2, 2 * Count) / Weights) * 100000,
    Rate_Gamma1Upr = (0.5 * qchisq(1 - alpha / 2, 2 * (Count + 1)) / Weights) * 100000
  )

# Using ERP Weights

## Join on the standard population weights (adds in a column called "Weights")
ERP_MFVacc_SFnontot.df <- left_join(MFVacc_SFnontotal.df, ERPMn.df, by = "AgeGroup")


## Create a dataset for the total of each DHB (all age groups summed), doesn't include all DHB named 'Total'
ERP_MFVacc_SFtot.df <- ERP_MFVacc_SFnontot.df %>%
  group_by(DHB) %>%
  summarise(
    AgeGroup = "Total",
    Count = sum(Count), Total = sum(Weights) # sum weights as want to divide by total HSU population for Rate
  )

## Calculate the age-specific rates for each age group in each year
alpha <- 0.05 # added this in for 95% CI
ERP_MFVacc_SFnontot.df <- ERP_MFVacc_SFnontot.df %>%
  mutate(
    Rate = Count / Weights, # dividing by age-band population for rate
    RateMult = Rate * 100000,
    Variance = Rate / Weights, ## Var(R) = Count / Weights^2
    
    Rate_KeyfitzLwr = (pmax(0, Rate - qnorm(1 - alpha / 2) * sqrt(Variance), na.rm = TRUE)) * 100000,
    Rate_KeyfitzUpr = (Rate + qnorm(1 - alpha / 2) * sqrt(Variance)) * 100000,
    Rate_Gamma1Lwr = (0.5 * qchisq(alpha / 2, 2 * Count) / Weights) * 100000,
    Rate_Gamma1Upr = (0.5 * qchisq(1 - alpha / 2, 2 * (Count + 1)) / Weights) * 100000
  )


#Non-Maori Fully Vaccinated Rates -----

# Total fully vaccinated
# NMFVacc_SFnontotal.df <- read_excel("VaccinationDataR Updated HSU2021.xlsx", sheet = "NonMaoriFullVacc")
NMFVacc_SFnontotal.df<- subset(NMFVacc_SFnontotal.df, Group!="Total" & Group!="Male" & DHB!="Total")

NMFVacc_SFnontotal.df <- pivot_longer(NMFVacc_SFnontotal.df, cols = 4:20, names_to = "AgeGroup", values_to = "Count" ) #changing from rows to column
NMFVacc_SFnontotal.df <- NMFVacc_SFnontotal.df[,c(1,4,5,3)] #rearranging columns to match Daniel's example below


# Using HSU Weights

## Join on the standard population weights (adds in a column called "Weights")
HSU_NMFVacc_SFnontot.df <- left_join(NMFVacc_SFnontotal.df, HSUNMn.df, by = "AgeGroup")


## Create a dataset for the total of each DHB (all age groups summed), doesn't include all DHB named 'Total'
HSU_NMFVacc_SFtot.df <- HSU_NMFVacc_SFnontot.df %>%
  group_by(DHB) %>%
  summarise(
    AgeGroup = "Total",
    Count = sum(Count), Total = sum(Weights) #sum weights as want to divide by total HSU population for Rate
  )

## Calculate the age-specific rates for each age group in each year
alpha = 0.05 #added this in for 95% CI
HSU_NMFVacc_SFnontot.df <- HSU_NMFVacc_SFnontot.df %>%
  mutate(
    Rate     = Count / Weights, #dividing by age-band population for rate
    RateMult = Rate * 100000,
    Variance = Rate / Weights, ## Var(R) = Count / Weights^2
    
    Rate_KeyfitzLwr = (pmax(0, Rate - qnorm(1 - alpha / 2) * sqrt(Variance), na.rm = TRUE))*100000,
    Rate_KeyfitzUpr = (Rate + qnorm(1 - alpha / 2) * sqrt(Variance))*100000,
    
    Rate_Gamma1Lwr  = (0.5 * qchisq(alpha / 2, 2 * Count) / Weights)*100000,
    Rate_Gamma1Upr  = (0.5 * qchisq(1 - alpha / 2, 2 * (Count + 1)) / Weights)*100000
  )

# Using ERP Weights

## Join on the standard population weights (adds in a column called "Weights")
ERP_NMFVacc_SFnontot.df <- left_join(NMFVacc_SFnontotal.df, ERPNMn.df, by = "AgeGroup")


## Create a dataset for the total of each DHB (all age groups summed), doesn't include all DHB named 'Total'
ERP_NMFVacc_SFtot.df <- ERP_NMFVacc_SFnontot.df %>%
  group_by(DHB) %>%
  summarise(
    AgeGroup = "Total",
    Count = sum(Count), Total = sum(Weights) #sum weights as want to divide by total HSU population for Rate
  )

## Calculate the age-specific rates for each age group in each year
alpha = 0.05 #added this in for 95% CI
ERP_NMFVacc_SFnontot.df <- ERP_NMFVacc_SFnontot.df %>%
  mutate(
    Rate     = Count / Weights, #dividing by age-band population for rate
    RateMult = Rate * 100000,
    Variance = Rate / Weights, ## Var(R) = Count / Weights^2
    
    Rate_KeyfitzLwr = (pmax(0, Rate - qnorm(1 - alpha / 2) * sqrt(Variance), na.rm = TRUE))*100000,
    Rate_KeyfitzUpr = (Rate + qnorm(1 - alpha / 2) * sqrt(Variance))*100000,
    
    Rate_Gamma1Lwr  = (0.5 * qchisq(alpha / 2, 2 * Count) / Weights)*100000,
    Rate_Gamma1Upr  = (0.5 * qchisq(1 - alpha / 2, 2 * (Count + 1)) / Weights)*100000
  )




## Rates of DHB Total by Age groups (Total pop) ----

# Total fully vaccinated
TFVacc_DHB_SFtotal.df <- subset(TFVacc_DHB_SFtotal.df, Group != "Total" & Group != "Male" & DHB == "Total")

TFVacc_DHB_SFtotal.df <- pivot_longer(TFVacc_DHB_SFtotal.df, cols = 3, values_to = "Total") # changing from rows to column
TFVacc_DHB_SFtotal.df <- pivot_longer(TFVacc_DHB_SFtotal.df, cols = 3:19, names_to = "AgeGroup", values_to = "Count") # changing from rows to column

TFVacc_DHB_SFtotal.df <- TFVacc_DHB_SFtotal.df[, c(1, 5, 6, 4)] # rearranging columns to match Daniel's example below



# Using HSU Weights


HSU_TFVacc_DHB_SFtot.df <- left_join(TFVacc_DHB_SFtotal.df, HSUn.df, by = "AgeGroup")


## Calculate the age-specific rates for each age group for total regions
alpha <- 0.05 # added this in for 95% CI
HSU_TFVacc_DHB_SFtot.df <- HSU_TFVacc_DHB_SFtot.df %>%
  mutate(
    Rate = Count / Weights,
    RateMult = Rate * 100000,
    Variance = Rate / Weights, ## Var(R) = Count / Total^2
    
    Rate_KeyfitzLwr = (pmax(0, Rate - qnorm(1 - alpha / 2) * sqrt(Variance), na.rm = TRUE)) * 100000,
    Rate_KeyfitzUpr = (Rate + qnorm(1 - alpha / 2) * sqrt(Variance)) * 100000,
    Rate_Gamma1Lwr = (0.5 * qchisq(alpha / 2, 2 * Count) / Weights) * 100000,
    Rate_Gamma1Upr = (0.5 * qchisq(1 - alpha / 2, 2 * (Count + 1)) / Weights) * 100000
  )


# Using ERP Weights


ERP_TFVacc_DHB_SFtot.df <- left_join(TFVacc_DHB_SFtotal.df, ERPn.df, by = "AgeGroup")


## Calculate the age-specific rates for each age group for total regions
alpha <- 0.05 # added this in for 95% CI
ERP_TFVacc_DHB_SFtot.df <- ERP_TFVacc_DHB_SFtot.df %>%
  mutate(
    Rate = Count / Weights,
    RateMult = Rate * 100000,
    Variance = Rate / Weights, ## Var(R) = Count / Total^2
    
    Rate_KeyfitzLwr = (pmax(0, Rate - qnorm(1 - alpha / 2) * sqrt(Variance), na.rm = TRUE)) * 100000,
    Rate_KeyfitzUpr = (Rate + qnorm(1 - alpha / 2) * sqrt(Variance)) * 100000,
    Rate_Gamma1Lwr = (0.5 * qchisq(alpha / 2, 2 * Count) / Weights) * 100000,
    Rate_Gamma1Upr = (0.5 * qchisq(1 - alpha / 2, 2 * (Count + 1)) / Weights) * 100000
  )


## Rates of DHB Total by Age groups (Maori pop) -----

# Total fully vaccinated
MFVacc_DHB_SFtotal.df <- subset(MFVacc_DHB_SFtotal.df, Group != "Total" & Group != "Male" & DHB == "Total")

MFVacc_DHB_SFtotal.df <- pivot_longer(MFVacc_DHB_SFtotal.df, cols = 3, values_to = "Total") # changing from rows to column
MFVacc_DHB_SFtotal.df <- pivot_longer(MFVacc_DHB_SFtotal.df, cols = 3:19, names_to = "AgeGroup", values_to = "Count") # changing from rows to column

MFVacc_DHB_SFtotal.df <- MFVacc_DHB_SFtotal.df[, c(1, 5, 6, 4)] # rearranging columns to match Daniel's example below



# Using HSU Weights


HSU_MFVacc_DHB_SFtot.df <- left_join(MFVacc_DHB_SFtotal.df, HSUMn.df, by = "AgeGroup")


## Calculate the age-specific rates for each age group for total regions
alpha <- 0.05 # added this in for 95% CI
HSU_MFVacc_DHB_SFtot.df <- HSU_MFVacc_DHB_SFtot.df %>%
  mutate(
    Rate = Count / Weights,
    RateMult = Rate * 100000,
    Variance = Rate / Weights, ## Var(R) = Count / Total^2
    
    Rate_KeyfitzLwr = (pmax(0, Rate - qnorm(1 - alpha / 2) * sqrt(Variance), na.rm = TRUE)) * 100000,
    Rate_KeyfitzUpr = (Rate + qnorm(1 - alpha / 2) * sqrt(Variance)) * 100000,
    Rate_Gamma1Lwr = (0.5 * qchisq(alpha / 2, 2 * Count) / Weights) * 100000,
    Rate_Gamma1Upr = (0.5 * qchisq(1 - alpha / 2, 2 * (Count + 1)) / Weights) * 100000
  )


# Using ERP Weights


ERP_MFVacc_DHB_SFtot.df <- left_join(MFVacc_DHB_SFtotal.df, ERPMn.df, by = "AgeGroup")


## Calculate the age-specific rates for each age group for total regions
alpha <- 0.05 # added this in for 95% CI
ERP_MFVacc_DHB_SFtot.df <- ERP_MFVacc_DHB_SFtot.df %>%
  mutate(
    Rate = Count / Weights,
    RateMult = Rate * 100000,
    Variance = Rate / Weights, ## Var(R) = Count / Total^2
    
    Rate_KeyfitzLwr = (pmax(0, Rate - qnorm(1 - alpha / 2) * sqrt(Variance), na.rm = TRUE)) * 100000,
    Rate_KeyfitzUpr = (Rate + qnorm(1 - alpha / 2) * sqrt(Variance)) * 100000,
    Rate_Gamma1Lwr = (0.5 * qchisq(alpha / 2, 2 * Count) / Weights) * 100000,
    Rate_Gamma1Upr = (0.5 * qchisq(1 - alpha / 2, 2 * (Count + 1)) / Weights) * 100000
  )

## Rates of DHB Total by Age groups (Non-Maori pop) -----

# Total fully vaccinated
# NMFVacc_DHB_SFtotal.df <- read_excel("VaccinationDataR Updated HSU2021.xlsx", sheet = "NonMaoriFullVacc")
NMFVacc_DHB_SFtotal.df<- subset(NMFVacc_DHB_SFtotal.df, Group!="Total" & Group!="Male" & DHB=="Total")

NMFVacc_DHB_SFtotal.df <- pivot_longer(NMFVacc_DHB_SFtotal.df, cols = 3, values_to = "Total" ) #changing from rows to column
NMFVacc_DHB_SFtotal.df <- pivot_longer(NMFVacc_DHB_SFtotal.df, cols = 3:19, names_to = "AgeGroup", values_to = "Count" ) #changing from rows to column

NMFVacc_DHB_SFtotal.df <- NMFVacc_DHB_SFtotal.df[,c(1,5,6,4)] #rearranging columns to match Daniel's example below



# Using HSU Weights


HSU_NMFVacc_DHB_SFtot.df <- left_join(NMFVacc_DHB_SFtotal.df, HSUNMn.df, by = "AgeGroup")


## Calculate the age-specific rates for each age group for total regions
alpha = 0.05 #added this in for 95% CI
HSU_NMFVacc_DHB_SFtot.df <- HSU_NMFVacc_DHB_SFtot.df %>%
  mutate(
    Rate     = Count / Weights,
    RateMult = Rate * 100000,
    Variance = Rate / Weights, ## Var(R) = Count / Total^2
    
    Rate_KeyfitzLwr = (pmax(0, Rate - qnorm(1 - alpha / 2) * sqrt(Variance), na.rm = TRUE))*100000,
    Rate_KeyfitzUpr = (Rate + qnorm(1 - alpha / 2) * sqrt(Variance))*100000,
    
    Rate_Gamma1Lwr  = (0.5 * qchisq(alpha / 2, 2 * Count) / Weights)*100000,
    Rate_Gamma1Upr  = (0.5 * qchisq(1 - alpha / 2, 2 * (Count + 1)) / Weights)*100000
  )


# Using ERP Weights


ERP_NMFVacc_DHB_SFtot.df <- left_join(NMFVacc_DHB_SFtotal.df, ERPNMn.df, by = "AgeGroup")


## Calculate the age-specific rates for each age group for total regions
alpha = 0.05 #added this in for 95% CI
ERP_NMFVacc_DHB_SFtot.df <- ERP_NMFVacc_DHB_SFtot.df %>%
  mutate(
    Rate     = Count / Weights,
    RateMult = Rate * 100000,
    Variance = Rate / Weights, ## Var(R) = Count / Total^2
    
    Rate_KeyfitzLwr = (pmax(0, Rate - qnorm(1 - alpha / 2) * sqrt(Variance), na.rm = TRUE))*100000,
    Rate_KeyfitzUpr = (Rate + qnorm(1 - alpha / 2) * sqrt(Variance))*100000,
    
    Rate_Gamma1Lwr  = (0.5 * qchisq(alpha / 2, 2 * Count) / Weights)*100000,
    Rate_Gamma1Upr  = (0.5 * qchisq(1 - alpha / 2, 2 * (Count + 1)) / Weights)*100000
  )

#PLOT DATA -----

HSUvsERP_TFVacc_DHB_SFtot.df <- rbind(
  data.frame(HSU_TFVacc_DHB_SFtot.df, population = "HSU"),
  data.frame(ERP_TFVacc_DHB_SFtot.df, population = "ERP")
)

HSUvsERP_MFVacc_DHB_SFtot.df <- rbind(
  data.frame(HSU_MFVacc_DHB_SFtot.df, population = "HSU"),
  data.frame(ERP_MFVacc_DHB_SFtot.df, population = "ERP")
)

HSUvsERP_NMFVacc_DHB_SFtot.df <- rbind(
  data.frame(HSU_MFVacc_DHB_SFtot.df, population = "HSU"),
  data.frame(ERP_MFVacc_DHB_SFtot.df, population = "ERP")
)



#Ratio Rates -----

## code for setting HSU as baseline for Total  Vacc -----

HSU_TFVacc_SFbaseline.total <- HSU_TFVacc_DHB_SFtot.df
ERP_TFVacc_SFpopulation.total <- ERP_TFVacc_DHB_SFtot.df

## Rename variables so we can use them after joining to the other dataset (otherwise we get Rate.x and Rate.y which is a bit confusing).
HSU_TFVacc_SFbaseline.total <- HSU_TFVacc_SFbaseline.total %>%
  dplyr::rename(
    RateBaseline = Rate,
    VarianceBaseline = Variance,
    # W_meanBaseline   = W_mean,
    RateBaselineLwr = Rate_KeyfitzLwr,
    RateBaselineUpr = Rate_KeyfitzUpr
  ) %>%
  dplyr::mutate(
    RRVarBaseline = ((Total - Count) / Total) / Count,
    ARVarBaseline = VarianceBaseline
  ) %>%
  dplyr::select(DHB, AgeGroup, RateBaseline, RRVarBaseline, ARVarBaseline, VarianceBaseline, RateBaselineLwr, RateBaselineUpr)


HSU_TFVacc_SFbaseline.other <- HSU_TFVacc_SFnontot.df
ERP_TFVacc_SFpopulation.other <- ERP_TFVacc_SFnontot.df



HSU_TFVacc_SFbaseline.other <- HSU_TFVacc_SFbaseline.other %>%
  dplyr::mutate(
    RateBaseline = Rate,
    RRVarBaseline = (1 / Count - 1 / Total),
    CountBaseline = Count,
    TotalBaseline = Total,
    RateBaselineLwr = Rate_KeyfitzLwr,
    RateBaselineUpr = Rate_KeyfitzUpr
  ) %>%
  dplyr::select(DHB, AgeGroup, RateBaseline, RRVarBaseline, CountBaseline, TotalBaseline, RateBaselineLwr, RateBaselineUpr)

## Join the baseline and other population datasets, calculating the RR and asssociated CI
ERP_TFVacc_SFpopulation.other <- ERP_TFVacc_SFpopulation.other %>%
  dplyr::left_join(HSU_TFVacc_SFbaseline.other, by = c("DHB", "AgeGroup")) %>%
  dplyr::mutate(
    RelativeRisk = Rate / RateBaseline,
    AttributableRisk = Rate - RateBaseline,
    # RRVar = (1 / Count) * ((Total - Count) / Total),
    RRVar = (1 / Count - 1 / Total),
    RelativeRiskLwr = exp(log(RelativeRisk) - 1.96 * sqrt(RRVar + RRVarBaseline)),
    RelativeRiskUpr = exp(log(RelativeRisk) + 1.96 * sqrt(RRVar + RRVarBaseline)),
    # ARVar = Rate * (1 - Rate) * (1 / Total + 1 / TotalBaseline),
    ARVar = (Count + CountBaseline) / (Total + TotalBaseline) * (1 - (Count + CountBaseline) / (Total + TotalBaseline)) * (1 / Total + 1 / TotalBaseline),
    AttributableRiskLwr = AttributableRisk - 1.96 * sqrt(ARVar),
    AttributableRiskUpr = AttributableRisk + 1.96 * sqrt(ARVar)
  ) %>%
  dplyr::select(-RRVarBaseline, -RRVar, -ARVar, -CountBaseline, -TotalBaseline, -RateBaselineLwr, -RateBaselineUpr)

TFVacc_DHB_SFpopulation.df <- ERP_TFVacc_SFpopulation.total %>%
  dplyr::left_join(HSU_TFVacc_SFbaseline.total, by = c("DHB", "AgeGroup")) %>%
  dplyr::mutate(
    RelativeRisk = Rate / RateBaseline,
    RRVar = ((Total - Count) / Total) / Count,
    RelativeRiskLwr = exp(log(RelativeRisk) - 1.96 * sqrt(RRVar + RRVarBaseline)),
    RelativeRiskUpr = exp(log(RelativeRisk) + 1.96 * sqrt(RRVar + RRVarBaseline)),
    AttributableRisk = Rate - RateBaseline,
    ARVar = Variance,
    AttributableRiskLwr = AttributableRisk - 1.96 * sqrt(ARVar + ARVarBaseline),
    AttributableRiskUpr = AttributableRisk + 1.96 * sqrt(ARVar + ARVarBaseline)
  ) %>%
  dplyr::select(-RRVarBaseline, -RRVar, -ARVarBaseline, -ARVar) %>%
  dplyr::bind_rows(ERP_TFVacc_SFpopulation.other)





## code for setting HSU as baseline for Maori Vaccinations ----

HSU_MFVacc_SFbaseline.total <- HSU_MFVacc_DHB_SFtot.df
ERP_MFVacc_SFpopulation.total <- ERP_MFVacc_DHB_SFtot.df

## Rename variables so we can use them after joining to the other dataset (otherwise we get Rate.x and Rate.y which is a bit confusing).
HSU_MFVacc_SFbaseline.total <- HSU_MFVacc_SFbaseline.total %>%
  dplyr::rename(
    RateBaseline = Rate,
    VarianceBaseline = Variance,
    # W_meanBaseline   = W_mean,
    RateBaselineLwr = Rate_KeyfitzLwr,
    RateBaselineUpr = Rate_KeyfitzUpr
  ) %>%
  dplyr::mutate(
    RRVarBaseline = ((Total - Count) / Total) / Count,
    ARVarBaseline = VarianceBaseline
  ) %>%
  dplyr::select(DHB, AgeGroup, RateBaseline, RRVarBaseline, ARVarBaseline, VarianceBaseline, RateBaselineLwr, RateBaselineUpr)


HSU_MFVacc_SFbaseline.other <- HSU_MFVacc_SFnontot.df
ERP_MFVacc_SFpopulation.other <- ERP_MFVacc_SFnontot.df



HSU_MFVacc_SFbaseline.other <- HSU_MFVacc_SFbaseline.other %>%
  dplyr::mutate(
    RateBaseline = Rate,
    RRVarBaseline = (1 / Count - 1 / Total),
    CountBaseline = Count,
    TotalBaseline = Total,
    RateBaselineLwr = Rate_KeyfitzLwr,
    RateBaselineUpr = Rate_KeyfitzUpr
  ) %>%
  dplyr::select(DHB, AgeGroup, RateBaseline, RRVarBaseline, CountBaseline, TotalBaseline, RateBaselineLwr, RateBaselineUpr)

## Join the baseline and other population datasets, calculating the RR and asssociated CI
ERP_MFVacc_SFpopulation.other <- ERP_MFVacc_SFpopulation.other %>%
  dplyr::left_join(HSU_MFVacc_SFbaseline.other, by = c("DHB", "AgeGroup")) %>%
  dplyr::mutate(
    RelativeRisk = Rate / RateBaseline,
    AttributableRisk = Rate - RateBaseline,
    # RRVar = (1 / Count) * ((Total - Count) / Total),
    RRVar = (1 / Count - 1 / Total),
    RelativeRiskLwr = exp(log(RelativeRisk) - 1.96 * sqrt(RRVar + RRVarBaseline)),
    RelativeRiskUpr = exp(log(RelativeRisk) + 1.96 * sqrt(RRVar + RRVarBaseline)),
    # ARVar = Rate * (1 - Rate) * (1 / Total + 1 / TotalBaseline),
    ARVar = (Count + CountBaseline) / (Total + TotalBaseline) * (1 - (Count + CountBaseline) / (Total + TotalBaseline)) * (1 / Total + 1 / TotalBaseline),
    AttributableRiskLwr = AttributableRisk - 1.96 * sqrt(ARVar),
    AttributableRiskUpr = AttributableRisk + 1.96 * sqrt(ARVar)
  ) %>%
  dplyr::select(-RRVarBaseline, -RRVar, -ARVar, -CountBaseline, -TotalBaseline, -RateBaselineLwr, -RateBaselineUpr)

MFVacc_DHB_SFpopulation.df <- ERP_MFVacc_SFpopulation.total %>%
  dplyr::left_join(HSU_MFVacc_SFbaseline.total, by = c("DHB", "AgeGroup")) %>%
  dplyr::mutate(
    RelativeRisk = Rate / RateBaseline,
    RRVar = ((Total - Count) / Total) / Count,
    RelativeRiskLwr = exp(log(RelativeRisk) - 1.96 * sqrt(RRVar + RRVarBaseline)),
    RelativeRiskUpr = exp(log(RelativeRisk) + 1.96 * sqrt(RRVar + RRVarBaseline)),
    AttributableRisk = Rate - RateBaseline,
    ARVar = Variance,
    AttributableRiskLwr = AttributableRisk - 1.96 * sqrt(ARVar + ARVarBaseline),
    AttributableRiskUpr = AttributableRisk + 1.96 * sqrt(ARVar + ARVarBaseline)
  ) %>%
  dplyr::select(-RRVarBaseline, -RRVar, -ARVarBaseline, -ARVar) %>%
  dplyr::bind_rows(ERP_MFVacc_SFpopulation.other)



## code for setting HSU as baseline for NOn-Maori Vaccinations ----

HSU_NMFVacc_SFbaseline.total <- HSU_NMFVacc_DHB_SFtot.df
ERP_NMFVacc_SFpopulation.total <- ERP_NMFVacc_DHB_SFtot.df

## Rename variables so we can use them after joining to the other dataset (otherwise we get Rate.x and Rate.y which is a bit confusing).
HSU_NMFVacc_SFbaseline.total <- HSU_NMFVacc_SFbaseline.total %>%
  dplyr::rename(
    RateBaseline     = Rate,
    VarianceBaseline = Variance,
    #W_meanBaseline   = W_mean,
    RateBaselineLwr = Rate_KeyfitzLwr,
    RateBaselineUpr = Rate_KeyfitzUpr
  ) %>%
  dplyr::mutate(
    RRVarBaseline = ((Total - Count) / Total) / Count,
    ARVarBaseline = VarianceBaseline
  ) %>%
  dplyr::select(DHB, AgeGroup, RateBaseline, RRVarBaseline, ARVarBaseline, VarianceBaseline, RateBaselineLwr, RateBaselineUpr)


HSU_NMFVacc_SFbaseline.other <- HSU_NMFVacc_SFnontot.df
ERP_NMFVacc_SFpopulation.other <- ERP_NMFVacc_SFnontot.df



HSU_NMFVacc_SFbaseline.other <- HSU_NMFVacc_SFbaseline.other %>%
  dplyr::mutate(
    RateBaseline = Rate,
    RRVarBaseline = (1 / Count - 1 / Total),
    CountBaseline = Count,
    TotalBaseline = Total,
    
    RateBaselineLwr = Rate_KeyfitzLwr,
    RateBaselineUpr = Rate_KeyfitzUpr
  ) %>%
  dplyr::select(DHB, AgeGroup, RateBaseline, RRVarBaseline, CountBaseline, TotalBaseline, RateBaselineLwr, RateBaselineUpr)

## Join the baseline and other population datasets, calculating the RR and asssociated CI
ERP_NMFVacc_SFpopulation.other <-ERP_NMFVacc_SFpopulation.other %>%
  dplyr::left_join(HSU_NMFVacc_SFbaseline.other, by = c("DHB", "AgeGroup")) %>%
  dplyr::mutate(
    RelativeRisk = Rate / RateBaseline,
    AttributableRisk = Rate - RateBaseline,
    # RRVar = (1 / Count) * ((Total - Count) / Total),
    RRVar = (1 / Count - 1 / Total),
    RelativeRiskLwr = exp(log(RelativeRisk) - 1.96 * sqrt(RRVar + RRVarBaseline)),
    RelativeRiskUpr = exp(log(RelativeRisk) + 1.96 * sqrt(RRVar + RRVarBaseline)),
    # ARVar = Rate * (1 - Rate) * (1 / Total + 1 / TotalBaseline),
    ARVar = (Count + CountBaseline) / (Total + TotalBaseline) * (1 - (Count + CountBaseline) / (Total + TotalBaseline)) * (1 / Total + 1 / TotalBaseline),
    AttributableRiskLwr = AttributableRisk - 1.96 * sqrt(ARVar),
    AttributableRiskUpr = AttributableRisk + 1.96 * sqrt(ARVar)
  ) %>%
  dplyr::select(-RRVarBaseline, -RRVar, -ARVar, -CountBaseline, -TotalBaseline, -RateBaselineLwr, -RateBaselineUpr)

NMFVacc_DHB_SFpopulation.df <- ERP_NMFVacc_SFpopulation.total %>%
  dplyr::left_join(HSU_NMFVacc_SFbaseline.total, by = c("DHB", "AgeGroup")) %>%
  dplyr::mutate(
    RelativeRisk    = Rate / RateBaseline,
    RRVar           = ((Total - Count) / Total) / Count,
    RelativeRiskLwr = exp(log(RelativeRisk) - 1.96 * sqrt(RRVar + RRVarBaseline)),
    RelativeRiskUpr = exp(log(RelativeRisk) + 1.96 * sqrt(RRVar + RRVarBaseline)),
    
    AttributableRisk    = Rate - RateBaseline,
    ARVar               = Variance,
    AttributableRiskLwr = AttributableRisk - 1.96 * sqrt(ARVar + ARVarBaseline),
    AttributableRiskUpr = AttributableRisk + 1.96 * sqrt(ARVar + ARVarBaseline)
  ) %>%
  dplyr::select(-RRVarBaseline, -RRVar, -ARVarBaseline, -ARVar) %>%
  dplyr::bind_rows(ERP_NMFVacc_SFpopulation.other)



# Create FINAL Code Output

#Create Nationwide Full Vacc Data Frames ----
Fully_Vacc_NW.df <- rbind(data.frame(HSUvsERP_TFVacc_DHB_SFtot.df, ethnicity = "Total"),
                          data.frame(HSUvsERP_MFVacc_DHB_SFtot.df, ethnicity = "Maori"),
                          data.frame(HSUvsERP_NMFVacc_DHB_SFtot.df, ethnicity = "Non-Maori"))


# Create DHB Fully Vaccination Data Frames ----
#Create each for HSU and ERP
HSUVacc_Data.df <- rbind(HSU_TFVacc_SFnontot.df%>%
                           mutate(ethnicity = "Total"), 
                         HSU_MFVacc_SFnontot.df%>%
                           mutate(ethnicity = "Maori"), 
                         HSU_NMFVacc_SFnontot.df%>%
                           mutate(ethnicity = "Non-Maori"))

ERPVacc_Data.df <- rbind(ERP_TFVacc_SFnontot.df%>%
                           mutate(ethnicity = "Total"), 
                         ERP_MFVacc_SFnontot.df%>%
                           mutate(ethnicity = "Maori"), 
                         ERP_NMFVacc_SFnontot.df%>%
                           mutate(ethnicity = "Non-Maori"))




#Combine HSU and ERP
Fully_Vacc_DHB.df <- rbind(
  data.frame(HSUVacc_Data.df, population = "HSU"),
  data.frame(ERPVacc_Data.df, population = "ERP")
)


#Create Ratio Data =====
AllFVacc_DHB_SFpopulation.df <- rbind(
  data.frame(TFVacc_DHB_SFpopulation.df, population = "Total"),
  data.frame(MFVacc_DHB_SFpopulation.df, population = "Maori"),
  data.frame(NMFVacc_DHB_SFpopulation.df, population = "Non-Maori")
)


#Collate All Data Into a List -----
DataFemale <- list(Nationwide = Fully_Vacc_NW.df,
                  DHB = Fully_Vacc_DHB.df,
                  RatioInfo = AllFVacc_DHB_SFpopulation.df)


#Remove Everything except all data
rm(list=setdiff(ls(), c("DataTotal", "DataFemale", "DataMale")))


