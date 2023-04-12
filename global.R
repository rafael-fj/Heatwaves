library(shiny)
library(shinydashboard)
library(shinyjs)
library(readr)
library(dplyr)
library(leaflet)
library(leafdown)
library(geobr)
library(tibble)
library(DT)
library(heatwaveR)
library(ggplot2)
library(sf)
library(sp)

#source("heatwaves/test_leafdown.R")
#source("heatwaves/heatwaves.R")
load(file = "data/mhw.RData")
helper_files = list.files(path = "helper", full.names = TRUE, pattern = "*.R")
sapply(helper_files, source, encoding = "UTF-8")



data_uf <- readRDS("shapes/data_uf.RDS")
data_muni <- readRDS("shapes/data_muni.RDS")
spdfs_list <- list(data_uf, data_muni)

# Load heatwave count
df_muni <- readr::read_delim("data/heatwaves_muni.csv", ",",
                             locale = locale("br", decimal_mark = ",", encoding = "LATIN1"),
                             show_col_types = FALSE)
df_uf <- readr::read_delim("data/heatwaves_uf.csv", ",",
                             locale = locale("br", decimal_mark = ",", encoding = "LATIN1"),
                           show_col_types = FALSE)

#df_all <- rbind(df_uf, df_muni)

# Merge Data
#data_muni <- dplyr::left_join(data_muni, df_muni, by = c("code_muni" = "ID_IBGE"))
#data_uf <- dplyr::left_join(data_uf, df_uf, by = c("code_state" = "cod_uf"))
#colnames(data_uf)[7] <- 'Heatwaves'

data <- data_muni


