rm(list=ls())
# ----------------------------------------------------------------------------------------------------
# written
# Gabriel LEMYRE
# ----------------------------------------------------------------------------------------------------
# First version : 26 avril 2020
# ----------------------------------------------------------------------------------------------------
# Last version : 26 avril 2020
# ----------------------------------------------------------------------------------------------------
username <- "VTL"
nb.days <- 30
image.width <- 3
path.images <- '~/Documents/GitHub/TrainingCharts/images_VTL'

path.output <- '~/Documents/GitHub/TrainingCharts/code/latex'

# --------------------------------------------------------
# Changement du document de travail
# --------------------------------------------------------
path.code <- '~/Documents/GitHub/TrainingCharts/code/R'
setwd(path.expand(path.code)) # Setting Sourcing path

# GENERATING CHART ----
source("chart_generator.R")
