library(shiny)
library(shinydashboard)
library(shinyjs)
library(readr)
library(dplyr)
library(leaflet)
library(leafdown)
#library(geobr)
library(tibble)
library(DT)
library(heatwaveR)
library(ggplot2)
#library(cleangeo)
#library(sf)
#library(sp)
#library(geodata) #-> simplifyGeom

#source("heatwaves/test_leafdown.R")
#source("heatwaves/heatwaves.R")
load(file = "data/mhw.RData")
helper_files = list.files(path = "helper", full.names = TRUE, pattern = "*.R")
sapply(helper_files, source, encoding = "UTF-8")

# Load Spatial Data
#data_uf <- readRDS("shapes/data_uf.RDS")
#data_muni <- readRDS("shapes/data_muni.RDS")
#spdfs_list <- list(data_uf, data_muni)
data_uf <- readRDS("shapes/estados.RDS")
data_muni <- readRDS("shapes/municipios.RDS")
#data_muni <- cleangeo::clgeo_Clean(data_muni, strategy = "POLYGONATION")
#test <- readRDS("shapes/muni_0005.RDS")
#test <- readRDS("heatwave/shapes/gadm41_BRA_2_pk.rds")

## If I need to simplify any geometry downloaded from terra and convert to sp
#test <- simplifyGeom(test, 0.01)
#test <- as(test, "SPatial")

#spdfs_list <- list(data_uf, data_muni)
spdfs_list <- list(data_uf, data_muni)

# Load heatwave count
df_muni <- readr::read_delim("data/heatwaves_muni.csv", ",",
                             locale = locale("br", decimal_mark = ",", encoding = "LATIN1"),
                             show_col_types = FALSE)
df_muni$code_muni <- as.character(df_muni$code_muni)
df_uf <- readr::read_delim("data/heatwaves_uf.csv", ",",
                             locale = locale("br", decimal_mark = ",", encoding = "LATIN1"),
                           show_col_types = FALSE)

#df_all <- rbind(df_uf, df_muni)

# Merge Data
#data_muni <- dplyr::left_join(data_muni, df_muni, by = c("code_muni" = "ID_IBGE"))
#data_uf <- dplyr::left_join(data_uf, df_uf, by = c("code_state" = "cod_uf"))
#colnames(data_uf)[7] <- 'Heatwaves'

#data <- data_muni


