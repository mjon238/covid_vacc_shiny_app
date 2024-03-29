#Code RUN!
library(readxl); 
library(magrittr); 
library(dplyr); 
library(tidyr); 
library(reshape2); 
library(patchwork);
library(ggnewscale)

#Load all Data Files -----

# weights.df <- read_excel("data/Data_Nov22/Weights.xlsx", sheet = "HSU&ERP")
# TFVacc_nontotal.df <- read_excel("data/Data_Nov22/VaccinationDataR Updated HSU2021.xlsx", sheet = "TotalFullVacc")
# MFVacc_nontotal.df <- read_excel("data/Data_Nov22/VaccinationDataR Updated HSU2021.xlsx", sheet = "MaoriFullVacc")
# NMFVacc_nontotal.df <- read_excel("data/Data_Nov22/VaccinationDataR Updated HSU2021.xlsx", sheet = "NonMaoriFullVacc")
# TFVacc_DHBtotal.df <- read_excel("data/Data_Nov22/VaccinationDataR Updated HSU2021.xlsx", sheet = "TotalFullVacc")
# MFVacc_DHBtotal.df <- read_excel("data/Data_Nov22/VaccinationDataR Updated HSU2021.xlsx", sheet = "MaoriFullVacc")
# NMFVacc_DHBtotal.df <- read_excel("data/Data_Nov22/VaccinationDataR Updated HSU2021.xlsx", sheet = "NonMaoriFullVacc")


## Creating weights for each population type ----
## creating weights df



#HSU total weights
HSU_DHB.df <- weights$HSU

#ERP total weights
ERP_DHB.df <- weights$ERP

#HSU Māori weights
HSUM_DHB.df <- weights$`HSU Maori`

#ERP Maori weights
ERPM_DHB.df <- weights$`ERP Maori`

#HSU NonMāori weights
HSUNM_DHB.df <- weights$`HSU NonMaori`

#ERP NonMaori weights
ERPNM_DHB.df <- weights$`ERP NonMaori`


## Total Fully Vaccinated Rates ----
# Total fully vaccinated
TFVacc_nontotal.df<- subset(TFVacc_nontotal.df, Group!="Male" & Group!="Female" & DHB!="Total")

TFVacc_nontotal.df <- pivot_longer(TFVacc_nontotal.df, cols = 4:20, names_to = "AgeGroup", values_to = "Count" ) #changing from rows to column
TFVacc_nontotal.df <- TFVacc_nontotal.df[,c(1,4,5,3)] #rearranging columns to match Daniel's example below


# Using HSU Weights

## Join on the standard population weights (adds in a column called "Weights")
HSU_TFVacc_nontot.df <- left_join(TFVacc_nontotal.df, HSU_DHB.df, 
                                  by = c("AgeGroup", "DHB"))


## Create a dataset for the total of each DHB (all age groups summed), doesn't include all DHB named 'Total'
HSU_TFVacc_tot.df <- HSU_TFVacc_nontot.df %>%
  group_by(DHB) %>%
  summarise(
    AgeGroup = "Total",
    Count = sum(Count), Total = sum(Weights) #sum weights as want to divide by total HSU population for Rate
  )

## Calculate the age-specific rates for each age group in each year
alpha = 0.05 #added this in for 95% CI
HSU_TFVacc_nontot.df <- HSU_TFVacc_nontot.df %>%
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
ERP_TFVacc_nontot.df <- left_join(TFVacc_nontotal.df, ERP_DHB.df, by = c("AgeGroup", "DHB"))


## Create a dataset for the total of each DHB (all age groups summed), doesn't include all DHB named 'Total'
ERP_TFVacc_tot.df <- ERP_TFVacc_nontot.df %>%
  group_by(DHB) %>%
  summarise(
    AgeGroup = "Total",
    Count = sum(Count), Total = sum(Weights) #sum weights as want to divide by total HSU population for Rate
  )

## Calculate the age-specific rates for each age group in each year
alpha = 0.05 #added this in for 95% CI
ERP_TFVacc_nontot.df <- ERP_TFVacc_nontot.df %>%
  mutate(
    Rate     = Count / Weights, #dividing by age-band population for rate
    RateMult = Rate * 100000,
    Variance = Rate / Weights, ## Var(R) = Count / Weights^2
    
    Rate_KeyfitzLwr = (pmax(0, Rate - qnorm(1 - alpha / 2) * sqrt(Variance), na.rm = TRUE))*100000,
    Rate_KeyfitzUpr = (Rate + qnorm(1 - alpha / 2) * sqrt(Variance))*100000,
    
    Rate_Gamma1Lwr  = (0.5 * qchisq(alpha / 2, 2 * Count) / Weights)*100000,
    Rate_Gamma1Upr  = (0.5 * qchisq(1 - alpha / 2, 2 * (Count + 1)) / Weights)*100000
  )


## Māori Fully Vaccinated Rates ----
# Total fully vaccinated
MFVacc_nontotal.df<- subset(MFVacc_nontotal.df, Group!="Male" & Group!="Female" & DHB!="Total")

MFVacc_nontotal.df <- pivot_longer(MFVacc_nontotal.df, cols = 4:20, names_to = "AgeGroup", values_to = "Count" ) #changing from rows to column
MFVacc_nontotal.df <- MFVacc_nontotal.df[,c(1,4,5,3)] #rearranging columns to match Daniel's example below


# Using HSU Weights

## Join on the standard population weights (adds in a column called "Weights")
HSU_MFVacc_nontot.df <- left_join(MFVacc_nontotal.df, HSUM_DHB.df, by = c("AgeGroup", "DHB"))

## Create a dataset for the total of each DHB (all age groups summed), doesn't include all DHB named 'Total'
HSU_MFVacc_tot.df <- HSU_MFVacc_nontot.df %>%
  group_by(DHB) %>%
  summarise(
    AgeGroup = "Total",
    Count = sum(Count), Total = sum(Weights) #sum weights as want to divide by total HSU population for Rate
  )

## Calculate the age-specific rates for each age group in each DHB
alpha = 0.05 #added this in for 95% CI
HSU_MFVacc_nontot.df <- HSU_MFVacc_nontot.df %>%
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
ERP_MFVacc_nontot.df <- left_join(MFVacc_nontotal.df, ERPM_DHB.df, by = c("AgeGroup", "DHB"))


## Create a dataset for the total of each DHB (all age groups summed), doesn't include all DHB named 'Total'
ERP_MFVacc_tot.df <- ERP_MFVacc_nontot.df %>%
  group_by(DHB) %>%
  summarise(
    AgeGroup = "Total",
    Count = sum(Count), Total = sum(Weights) #sum weights as want to divide by total HSU population for Rate
  )

## Calculate the age-specific rates for each age group in each DHB
alpha = 0.05 #added this in for 95% CI
ERP_MFVacc_nontot.df <- ERP_MFVacc_nontot.df %>%
  mutate(
    Rate     = Count / Weights, #dividing by age-band population for rate
    RateMult = Rate * 100000,
    Variance = Rate / Weights, ## Var(R) = Count / Weights^2
    
    Rate_KeyfitzLwr = (pmax(0, Rate - qnorm(1 - alpha / 2) * sqrt(Variance), na.rm = TRUE))*100000,
    Rate_KeyfitzUpr = (Rate + qnorm(1 - alpha / 2) * sqrt(Variance))*100000,
    
    Rate_Gamma1Lwr  = (0.5 * qchisq(alpha / 2, 2 * Count) / Weights)*100000,
    Rate_Gamma1Upr  = (0.5 * qchisq(1 - alpha / 2, 2 * (Count + 1)) / Weights)*100000
  )


## Non-Māori Fully Vaccinated Rates ----
# Total fully vaccinated
# NMFVacc_nontotal.df <- read_excel("VaccinationDataR Updated HSU2021.xlsx", sheet = "NonMaoriFullVacc")
NMFVacc_nontotal.df<- subset(NMFVacc_nontotal.df, Group!="Male" & Group!="Female" & DHB!="Total")

NMFVacc_nontotal.df <- pivot_longer(NMFVacc_nontotal.df, cols = 4:20, names_to = "AgeGroup", values_to = "Count" ) #changing from rows to column
NMFVacc_nontotal.df <- NMFVacc_nontotal.df[,c(1,4,5,3)] #rearranging columns to match Daniel's example below


# Using HSU Weights

## Join on the standard population weights (adds in a column called "Weights")
HSU_NMFVacc_nontot.df <- left_join(NMFVacc_nontotal.df, HSUNM_DHB.df, by = c("AgeGroup", "DHB"))


## Create a dataset for the total of each DHB (all age groups summed), doesn't include all DHB named 'Total'
HSU_NMFVacc_tot.df <- HSU_NMFVacc_nontot.df %>%
  group_by(DHB) %>%
  summarise(
    AgeGroup = "Total",
    Count = sum(Count), Total = sum(Weights) #sum weights as want to divide by total HSU population for Rate
  )

## Calculate the age-specific rates for each age group in each DHB
alpha = 0.05 #added this in for 95% CI
HSU_NMFVacc_nontot.df <- HSU_NMFVacc_nontot.df %>%
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
ERP_NMFVacc_nontot.df <- left_join(NMFVacc_nontotal.df, ERPNM_DHB.df, by = c("AgeGroup", "DHB"))


## Create a dataset for the total of each DHB (all age groups summed), doesn't include all DHB named 'Total'
ERP_NMFVacc_tot.df <- ERP_NMFVacc_nontot.df %>%
  group_by(DHB) %>%
  summarise(
    AgeGroup = "Total",
    Count = sum(Count), Total = sum(Weights) #sum weights as want to divide by total HSU population for Rate
  )

## Calculate the age-specific rates for each age group in each DHB
alpha = 0.05 #added this in for 95% CI
ERP_NMFVacc_nontot.df <- ERP_NMFVacc_nontot.df %>%
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
TFVacc_DHBtotal.df<- subset(TFVacc_DHBtotal.df, Group!="Male" & Group!="Female" & DHB=="Total")

TFVacc_DHBtotal.df <- pivot_longer(TFVacc_DHBtotal.df, cols = 3, values_to = "Total" ) #changing from rows to column
TFVacc_DHBtotal.df <- pivot_longer(TFVacc_DHBtotal.df, cols = 3:19, names_to = "AgeGroup", values_to = "Count" ) #changing from rows to column

TFVacc_DHBtotal.df <- TFVacc_DHBtotal.df[,c(1,5,6,4)] #rearranging columns to match Daniel's example below



# Using HSU Weights


HSU_TFVacc_DHBtot.df <- left_join(TFVacc_DHBtotal.df, HSU_DHB.df, by = c("AgeGroup", "DHB"))


## Calculate the age-specific rates for each age group for total regions
alpha = 0.05 #added this in for 95% CI
HSU_TFVacc_DHBtot.df <- HSU_TFVacc_DHBtot.df %>%
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


ERP_TFVacc_DHBtot.df <- left_join(TFVacc_DHBtotal.df, ERP_DHB.df, by = c("AgeGroup", "DHB"))


## Calculate the age-specific rates for each age group for total regions
alpha = 0.05 #added this in for 95% CI
ERP_TFVacc_DHBtot.df <- ERP_TFVacc_DHBtot.df %>%
  mutate(
    Rate     = Count / Weights,
    RateMult = Rate * 100000,
    Variance = Rate / Weights, ## Var(R) = Count / Total^2
    
    Rate_KeyfitzLwr = (pmax(0, Rate - qnorm(1 - alpha / 2) * sqrt(Variance), na.rm = TRUE))*100000,
    Rate_KeyfitzUpr = (Rate + qnorm(1 - alpha / 2) * sqrt(Variance))*100000,
    
    Rate_Gamma1Lwr  = (0.5 * qchisq(alpha / 2, 2 * Count) / Weights)*100000,
    Rate_Gamma1Upr  = (0.5 * qchisq(1 - alpha / 2, 2 * (Count + 1)) / Weights)*100000
  )


## Rates of DHB Total by Age groups (Māori pop) -----


MFVacc_DHBtotal.df<- subset(MFVacc_DHBtotal.df, Group!="Male" & Group!="Female" & DHB=="Total")

MFVacc_DHBtotal.df <- pivot_longer(MFVacc_DHBtotal.df, cols = 3, values_to = "Total" ) #changing from rows to column
MFVacc_DHBtotal.df <- pivot_longer(MFVacc_DHBtotal.df, cols = 3:19, names_to = "AgeGroup", values_to = "Count" ) #changing from rows to column

MFVacc_DHBtotal.df <- MFVacc_DHBtotal.df[,c(1,5,6,4)] #rearranging columns to match Daniel's example below



# Using HSU Weights


HSU_MFVacc_DHBtot.df <- left_join(MFVacc_DHBtotal.df, HSUM_DHB.df, by = c("AgeGroup", "DHB"))


## Calculate the age-specific rates for each age group for total regions
alpha = 0.05 #added this in for 95% CI
HSU_MFVacc_DHBtot.df <- HSU_MFVacc_DHBtot.df %>%
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


ERP_MFVacc_DHBtot.df <- left_join(MFVacc_DHBtotal.df, ERPM_DHB.df, by = c("AgeGroup", "DHB"))


## Calculate the age-specific rates for each age group for total regions
alpha = 0.05 #added this in for 95% CI
ERP_MFVacc_DHBtot.df <- ERP_MFVacc_DHBtot.df %>%
  mutate(
    Rate     = Count / Weights,
    RateMult = Rate * 100000,
    Variance = Rate / Weights, ## Var(R) = Count / Total^2
    
    Rate_KeyfitzLwr = (pmax(0, Rate - qnorm(1 - alpha / 2) * sqrt(Variance), na.rm = TRUE))*100000,
    Rate_KeyfitzUpr = (Rate + qnorm(1 - alpha / 2) * sqrt(Variance))*100000,
    
    Rate_Gamma1Lwr  = (0.5 * qchisq(alpha / 2, 2 * Count) / Weights)*100000,
    Rate_Gamma1Upr  = (0.5 * qchisq(1 - alpha / 2, 2 * (Count + 1)) / Weights)*100000
  )


## Rates of DHB Total by Age groups (Non-Māori pop) ----

# Total fully vaccinated
# NMFVacc_DHBtotal.df <- read_excel("VaccinationDataR Updated HSU2021.xlsx", sheet = "NonMaoriFullVacc")
NMFVacc_DHBtotal.df<- subset(NMFVacc_DHBtotal.df, Group!="Male" & Group!="Female" & DHB=="Total")

NMFVacc_DHBtotal.df <- pivot_longer(NMFVacc_DHBtotal.df, cols = 3, values_to = "Total" ) #changing from rows to column
NMFVacc_DHBtotal.df <- pivot_longer(NMFVacc_DHBtotal.df, cols = 3:19, names_to = "AgeGroup", values_to = "Count" ) #changing from rows to column

NMFVacc_DHBtotal.df <- NMFVacc_DHBtotal.df[,c(1,5,6,4)] #rearranging columns to match Daniel's example below



# Using HSU Weights


HSU_NMFVacc_DHBtot.df <- left_join(NMFVacc_DHBtotal.df, HSUNM_DHB.df, by = c("AgeGroup", "DHB"))


## Calculate the age-specific rates for each age group for total regions
alpha = 0.05 #added this in for 95% CI
HSU_NMFVacc_DHBtot.df <- HSU_NMFVacc_DHBtot.df %>%
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


ERP_NMFVacc_DHBtot.df <- left_join(NMFVacc_DHBtotal.df, ERPNM_DHB.df, by = c("AgeGroup", "DHB"))


## Calculate the age-specific rates for each age group for total regions
alpha = 0.05 #added this in for 95% CI
ERP_NMFVacc_DHBtot.df <- ERP_NMFVacc_DHBtot.df %>%
  mutate(
    Rate     = Count / Weights,
    RateMult = Rate * 100000,
    Variance = Rate / Weights, ## Var(R) = Count / Total^2
    
    Rate_KeyfitzLwr = (pmax(0, Rate - qnorm(1 - alpha / 2) * sqrt(Variance), na.rm = TRUE))*100000,
    Rate_KeyfitzUpr = (Rate + qnorm(1 - alpha / 2) * sqrt(Variance))*100000,
    
    Rate_Gamma1Lwr  = (0.5 * qchisq(alpha / 2, 2 * Count) / Weights)*100000,
    Rate_Gamma1Upr  = (0.5 * qchisq(1 - alpha / 2, 2 * (Count + 1)) / Weights)*100000
  )


# Plot Data THIS IS USED ----

#THIS IS USED

#Total
HSUvsERP_TFVacc_DHB.df <- rbind(
  data.frame(HSU_TFVacc_DHBtot.df, population="HSU"),
  data.frame(ERP_TFVacc_DHBtot.df, population="ERP")
)

#Maori
HSUvsERP_MFVacc_DHB.df <- rbind(
  data.frame(HSU_MFVacc_DHBtot.df, population="HSU"),
  data.frame(ERP_MFVacc_DHBtot.df, population="ERP")
)

#Non Maori
HSUvsERP_NMFVacc_DHB.df <- rbind(
  data.frame(HSU_NMFVacc_DHBtot.df, population="HSU"),
  data.frame(ERP_NMFVacc_DHBtot.df, population="ERP")
)



# Rate Ratio Code -----

## Total Vaccinations RR DHB total by agegroups-----

## code for setting HSU as baseline for Total Vaccinations

HSU_TFVacc_baseline.total <- HSU_TFVacc_DHBtot.df
ERP_TFVacc_population.total <- ERP_TFVacc_DHBtot.df

## Rename variables so we can use them after joining to the other dataset (otherwise we get Rate.x and Rate.y which is a bit confusing).
HSU_TFVacc_baseline.total <- HSU_TFVacc_baseline.total %>%
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


HSU_TFVacc_baseline.other <- HSU_TFVacc_nontot.df
ERP_TFVacc_population.other <- ERP_TFVacc_nontot.df

HSU_TFVacc_baseline.other$AgeGroup <- as.character(HSU_TFVacc_baseline.other$AgeGroup)
ERP_TFVacc_population.other$AgeGroup <- as.character(ERP_TFVacc_population.other$AgeGroup)




HSU_TFVacc_baseline.other <- HSU_TFVacc_baseline.other %>%
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
ERP_TFVacc_population.other <-ERP_TFVacc_population.other %>%
  dplyr::left_join(HSU_TFVacc_baseline.other, by = c("DHB", "AgeGroup")) %>%
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

TFVacc_DHBpopulation.df <- ERP_TFVacc_population.total %>%
  dplyr::left_join(HSU_TFVacc_baseline.total, by = c("DHB", "AgeGroup")) %>%
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
  dplyr::bind_rows(ERP_TFVacc_population.other)



## Total Maori Vaccinations RR ----

## code for setting ERP as baseline for Māori Vaccinations

HSU_MFVacc_baseline.total <- HSU_MFVacc_DHBtot.df
ERP_MFVacc_population.total <- ERP_MFVacc_DHBtot.df

## Rename variables so we can use them after joining to the other dataset (otherwise we get Rate.x and Rate.y which is a bit confusing).
HSU_MFVacc_baseline.total <- HSU_MFVacc_baseline.total %>%
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


HSU_MFVacc_baseline.other <- HSU_MFVacc_nontot.df
ERP_MFVacc_population.other <- ERP_MFVacc_nontot.df

HSU_MFVacc_baseline.other$AgeGroup <- as.character(HSU_MFVacc_baseline.other$AgeGroup)
ERP_MFVacc_population.other$AgeGroup <- as.character(ERP_MFVacc_population.other$AgeGroup)


HSU_MFVacc_baseline.other <- HSU_MFVacc_baseline.other %>%
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
ERP_MFVacc_population.other <-ERP_MFVacc_population.other %>%
  dplyr::left_join(HSU_MFVacc_baseline.other, by = c("DHB", "AgeGroup")) %>%
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

MFVacc_DHBpopulation.df <- ERP_MFVacc_population.total %>%
  dplyr::left_join(HSU_MFVacc_baseline.total, by = c("DHB", "AgeGroup")) %>%
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
  dplyr::bind_rows(ERP_MFVacc_population.other)


## Total Non-Maori Vaccinations RR----

## code for setting ERP as baseline for Non-Māori Vaccinations
HSU_NMFVacc_baseline.total <- HSU_NMFVacc_DHBtot.df
ERP_NMFVacc_population.total <- ERP_NMFVacc_DHBtot.df

## Rename variables so we can use them after joining to the other dataset (otherwise we get Rate.x and Rate.y which is a bit confusing).
HSU_NMFVacc_baseline.total <- HSU_NMFVacc_baseline.total %>%
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


HSU_NMFVacc_baseline.other <- HSU_NMFVacc_nontot.df
ERP_NMFVacc_population.other <- ERP_NMFVacc_nontot.df

HSU_NMFVacc_baseline.other$AgeGroup <- as.character(HSU_NMFVacc_baseline.other$AgeGroup)
ERP_NMFVacc_population.other$AgeGroup <- as.character(ERP_NMFVacc_population.other$AgeGroup)

HSU_NMFVacc_baseline.other <- HSU_NMFVacc_baseline.other %>%
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
ERP_NMFVacc_population.other <-ERP_NMFVacc_population.other %>%
  dplyr::left_join(HSU_NMFVacc_baseline.other, by = c("DHB", "AgeGroup")) %>%
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

NMFVacc_DHBpopulation.df <- ERP_NMFVacc_population.total %>%
  dplyr::left_join(HSU_NMFVacc_baseline.total, by = c("DHB", "AgeGroup")) %>%
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
  dplyr::bind_rows(ERP_NMFVacc_population.other)


# Create FINAL Code Output

#Create Nationwide Full Vacc Data Frames ----
Fully_Vacc_NW.df <- rbind(data.frame(HSUvsERP_TFVacc_DHB.df, ethnicity = "Total"),
                          data.frame(HSUvsERP_MFVacc_DHB.df, ethnicity = "Maori"),
                          data.frame(HSUvsERP_NMFVacc_DHB.df, ethnicity = "Non-Maori"))


# Create DHB Fully Vaccination Data Frames ----
#Create each for HSU and ERP
HSUVacc_Data.df <- rbind(HSU_TFVacc_nontot.df%>%
                           mutate(ethnicity = "Total"), 
                         HSU_MFVacc_nontot.df%>%
                           mutate(ethnicity = "Maori"), 
                         HSU_NMFVacc_nontot.df%>%
                           mutate(ethnicity = "Non-Maori"))

ERPVacc_Data.df <- rbind(ERP_TFVacc_nontot.df%>%
                           mutate(ethnicity = "Total"), 
                         ERP_MFVacc_nontot.df%>%
                           mutate(ethnicity = "Maori"), 
                         ERP_NMFVacc_nontot.df%>%
                           mutate(ethnicity = "Non-Maori"))

#Combine HSU and ERP
Fully_Vacc_DHB.df <- rbind(
  data.frame(HSUVacc_Data.df, population = "HSU"),
  data.frame(ERPVacc_Data.df, population = "ERP")
)



#Create Ratio Data =====
AllFVacc_DHBpopulation.df <- rbind(
  data.frame(TFVacc_DHBpopulation.df, population = "Total"),
  data.frame(MFVacc_DHBpopulation.df, population = "Maori"),
  data.frame(NMFVacc_DHBpopulation.df, population = "Non-Maori")
)


#Change Total to Nationwide
AllFVacc_DHBpopulation.df$DHB <- gsub("Total", "Nationwide", AllFVacc_DHBpopulation.df$DHB)



#Combine Nationwide and DHB
Fully_Vacc.df <- rbind(Fully_Vacc_NW.df, Fully_Vacc_DHB.df)

#Change Total to Nationwide
Fully_Vacc.df$DHB <- gsub("Total", "Nationwide", Fully_Vacc.df$DHB)


#ADD HSU To Ratio Data, BY First Extracting, Renaming and Merging

#1. Extract
HSUOnly <- Fully_Vacc.df%>%
  filter(population == "HSU")%>%
  select(-c(population))%>%
  rename(population = ethnicity)

colnames(HSUOnly) <- gsub(pattern = "Rate", 
                        replacement = "HSU_Rate", 
                        colnames(HSUOnly))
colnames(HSUOnly) <- gsub(pattern = "Variance",
                        replacement = "HSU_Variance",
                        colnames(HSUOnly))
colnames(HSUOnly) <- gsub(pattern = "Weights",
                        replacement = "HSU_Weights",
                        colnames(HSUOnly))

#2. Rename
colnames(AllFVacc_DHBpopulation.df) <- gsub(pattern = "Rate", 
                         replacement = "ERP_Rate", 
                         colnames(AllFVacc_DHBpopulation.df))

colnames(AllFVacc_DHBpopulation.df) <- gsub(pattern = "Variance",
                         replacement = "ERP_Variance",
                         colnames(AllFVacc_DHBpopulation.df))

colnames(AllFVacc_DHBpopulation.df) <- gsub(pattern = "Weights",
                         replacement = "ERP_Weights",
                         colnames(AllFVacc_DHBpopulation.df))


#3. Merge
AllFVacc_DHBpopulation.df <- merge(HSUOnly, AllFVacc_DHBpopulation.df, 
                                   by = c("DHB", "AgeGroup", "population",
                            "Count", "Total"))



#Collate All Data Into a List -----
DataTotal <- list(HSUvsERP = Fully_Vacc.df,
                  RatioInfo = AllFVacc_DHBpopulation.df)






