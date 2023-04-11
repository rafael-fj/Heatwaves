library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(leaflet)
library(geobr)
library(tibble)
library(DT)
library(heatwaveR)
library(ggplot2)

#source("heatwaves/test_leafdown.R")
#source("heatwaves/heatwaves.R")
load(file = "data/mhw.RData")
helper_files = list.files(path = "helper", full.names = TRUE, pattern = "*.R")
sapply(helper_files, source, encoding = "UTF-8")


# Load Spatial Data
data_uf <- read_state(year = 2017)
data_muni <- read_municipality(code_muni = "all", year = 2017, showProgress = T)

# Load heatwave count
df_muni <- readr::read_delim("data/heatwaves_muni.csv", ",",
                             locale = locale("br", decimal_mark = ",", encoding = "LATIN1"))
df_uf <- readr::read_delim("data/heatwaves_uf.csv", ",",
                             locale = locale("br", decimal_mark = ",", encoding = "LATIN1"))
# Merge Data
data_muni <- dplyr::left_join(data_muni, df_muni, by = c("code_muni" = "ID_IBGE"))
data_uf <- dplyr::left_join(data_uf, df_uf, by = c("code_state" = "cod_uf"))
colnames(data_uf)[7] <- 'Heatwaves'

data <- data_muni

spdfs_list <- list(data_uf, data_muni)
