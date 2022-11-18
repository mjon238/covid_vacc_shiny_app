library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(ggplot2)
library(plotly)
library(readxl)
library(magrittr)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(patchwork)
library(ggnewscale)

#Total Fully Vacc Tab
load("data/Full Vacc/Total Fully Vaccinated rates using HSU and ERP, by national total (DHB=total).Rda")
load("data/Full Vacc/Maori Fully Vaccinated rates using HSU and ERP, by national total (DHB=total).Rda")

#Rate Rates Tab
load("data/Rate Ratio/Maori Rate ratio for Fully Vaccinated Rates by grouped DHB with 95% CI's.Rda")
load("data/Rate Ratio/Non-Maori Rate ratio for Fully Vaccinated Rates by grouped DHB with 95% CI's.Rda")
load("data/Rate Ratio/Rate ratio for Fully Vaccinated Rates by grouped DHB with 95% CI's.Rda")

AllFVacc_DHBpopulation.df <- rbind(
  data.frame(TFVacc_DHBpopulation.df, population="Total"),
  data.frame(MFVacc_DHBpopulation.df, population="Maori"),
  data.frame(NMFVacc_DHBpopulation.df, population="Non-Maori")
)
#By DHB Data Tab

#Other Info
level_order <- c("5-11", "12-17", "18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+")

listOfDHB <- c(
  "Auckland", "Bay of Plenty", "Canterbury", "Capital & Coast",
  "Counties-Manukau", "Hawke's Bay", "Hutt Valley", "Lakes",
  "MidCentral", "Nelson-Marlborough", "Northland", "Southern",
  "South Canterbury  ", "Tairawhiti", "Taranaki", "Waikato",
  "Wairarapa", "Waitemata", "West Coast", "Whanganui"
)
