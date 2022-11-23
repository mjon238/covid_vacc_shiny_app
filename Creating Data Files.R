# Creating Data Files
# Step 1: Load New Excel File into data/NOv 22 Vacc../VaccinationDataR Updated...
# Step 2: Delete Old Data File
# Step 3: Run Vaccination Rates.Rmd (in the same R-Session)
# Step 4: Run the following code for Total Pop (Not Male/Female)

DHBData <- list(
  Total = list(
    HSU = list(
      Total = HSU_TFVacc_nontot.df,
      Maori = HSU_MFVacc_nontot.df,
      NMaori = HSU_NMFVacc_nontot.df
    ),
    ERP = list(
      Total = ERP_TFVacc_nontot.df,
      Maori = ERP_MFVacc_nontot.df,
      NMaori = ERP_NMFVacc_nontot.df
    )
  ),
  Male = ,
  Female =
  )

FullyVaccData <- list(
  Total = HSUvsERP_TFVacc_DHB,
  Maori = HSUvsERP_MFVacc_DHB
)

RatioRatesData <- list(
  Total = TFVacc_DHBpopulation,
  Maori = MFVacc_DHBpopulation,
  NMaori = NMFVacc_DHBpopulation
)
