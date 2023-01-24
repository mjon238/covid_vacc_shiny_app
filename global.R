library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(plotly)
library(readxl)
library(magrittr)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(patchwork)
library(ggnewscale)
library(tidyr)
library(DT)
library(orca)
library(processx)

#Load the Data
load("data/shiny_app_data.Rmd")


# Other Info
level_order <- c("5-11", "12-17", "18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+")

listOfDHB <- c(
  "Auckland", "Bay of Plenty", "Canterbury", "Capital and Coast",
  "Counties Manukau", "Hawkes Bay", "Hutt Valley", "Lakes",
  "MidCentral", "Nelson Marlborough", "Northland", "Southern",
  "South Canterbury", "Tairawhiti", "Taranaki", "Waikato",
  "Wairarapa", "Waitemata", "West Coast", "Whanganui"
)




