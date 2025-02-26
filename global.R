#install.versions("rgeos", versions = "0.3-28")

library(shiny)
library(curl)
library(jsonlite)
library(getPass)
library(elastic)
library(rgeos)
library(bs4Dash)
#library(shinydashboard)
library(shinyjs)
library(echarts4r)
library(purrr)
library(readr)
library(dplyr)
library(tidyr)
library(leaflet)
library(leafdown)
#library(tidyr)
#library(geobr)
library(tibble)
library(DT)
library(heatwaveR)
library(ggplot2)
library(plotly)
library(calendR)
library(shinyBS)
#library(shiny.semantic)
library(tippy)
library(lubridate)
library(htmlwidgets)

# Específico:
library(getPass) # Pra gerar o token
library(httr) # Pra fazer a solicitação
library(paletteer)
library(shinycssloaders)
library(ggthemes)

#loadlibrary("elastic")
#packages <- c("curl", "jsonlite", "getPass")
#lapply(packages, loadlibrary)

#library(cleangeo)
#library(sf)
#library(sp)
#library(geodata) #-> simplifyGeom

#source("heatwaves/test_leafdown.R")
#source("heatwaves/heatwaves.R")
#load(file = "C:/Users/admin/Documents/R/fio_cruz/Shiny/v1/test.RData")

load(file = "data/polu.RData")
load(file = "data/polu_plots.RData")
load(file = "data/polu_list.RData")
load(file = "data/mhw.RData")
load(file = "data/compilado.RData")
load(file = "data/compilado_uf.RData")
load(file = "data/lolli_list.RData")
helper_files = list.files(path = "helper", full.names = TRUE, pattern = "*.R")
sapply(helper_files, source, encoding = "UTF-8")
lista_ref <- lista_ref[-c(688,3508)]
#lista_ref <- sort(lista_ref)

# Load Spatial Data
#data_uf <- readRDS("shapes/data_uf.RDS")
#data_muni <- readRDS("shapes/data_muni.RDS")
#spdfs_list <- list(data_uf, data_muni)
data_uf <- readRDS("shapes/estados.RDS")
data_muni <- readRDS("shapes/municipios.RDS")
#print(nrow(data_muni))

#index <- which(data_muni@data$NAME_2 == "Lagoa dos Patos" |
#                 data_muni@data$NAME_2 == "Lagoa Mirim" |
#                 data_muni@data$NAME_2 == "Santa Vitória do Palmar" |
#                 data_muni@data$NAME_2 == "Chuí")

#data_muni@data <- data_muni@data[-index, ]
#data_muni@polygons <- data_muni@polygons[-index]
#data_muni@plotOrder <- data_muni@plotOrder[-index]

#saveRDS(data_muni, file = "data_muni_novao.RDS")



#data_muni <- data_muni[-c(688, 3508),]
#data_muni <- data_muni[-c(4607, 4608, 4980),]
#print(nrow(data_muni))
#print(data_muni[4608])
#data_muni <- cleangeo::clgeo_Clean(data_muni, strategy = "POLYGONATION")
#test <- readRDS("shapes/muni_0005.RDS")
#test <- readRDS("heatwave/shapes/gadm41_BRA_2_pk.rds")

## If I need to simplify any geometry downloaded from terra and convert to sp
#test <- simplifyGeom(test, 0.01)
#test <- as(test, "SPatial")

#spdfs_list <- list(data_uf, data_muni)


df_uf_polu <- df_uf_polu[is.finite(df_uf_polu$PM25),]
df_muni_polu <- df_muni_polu[is.finite(df_muni_polu$PM25),]

data_uf_polu <- data_uf[data_uf$NAME_1 %in% df_uf_polu$Estado,]
data_muni_polu <- data_muni[data_muni$NAME_2 %in% df_muni_polu$Cidade,]

spdfs_list <- list(data_uf, data_muni)
spdfs_list_polu <- list(data_uf_polu,data_muni_polu)

# Load heatwave count
ano_ref <- readr::read_delim("data/ano_ref.csv")
head(ano_ref)
df_muni <- readr::read_delim("data/heatwaves_muni.csv", ",",
                             locale = locale("br", decimal_mark = ",", encoding = "LATIN1"),
                             show_col_types = FALSE)
df_muni$code_muni <- as.character(df_muni$code_muni)
df_uf <- readr::read_delim("data/heatwaves_uf.csv", ",",
                             locale = locale("br", decimal_mark = ",", encoding = "LATIN1"),
                           show_col_types = FALSE)
df_uf$cod_muni <- as.character(df_uf$cod_muni)
df_uf <- df_uf[order(df_uf$Estado), ]

#df_all <- rbind(df_uf, df_muni)

# Merge Data
#data_muni <- dplyr::left_join(data_muni, df_muni, by = c("code_muni" = "ID_IBGE"))
#data_uf <- dplyr::left_join(data_uf, df_uf, by = c("code_state" = "cod_uf"))
#colnames(data_uf)[7] <- 'Heatwaves'

#data <- data_muni


