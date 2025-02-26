get_muni <- function(muni, ano_analise){

  #print(muni, ano_analise)

  token = "6e98b629bf40347876eb8865d50381620184b8bb"
  #token = getPass()
  url_base = "https://bigdata-api.fiocruz.br"
  endpoint = paste0(url_base,"/","show_tables")
  request <- httr::POST(url = endpoint,
                        body = list("token" = token),
                        encode = "json")
  as_tibble(content(request))
  content(request)$databases

  #selected_muni <- "ID_1100015"  # Default value
  #date_start <- "2020-01-01"
  #date_end <- "2020-12-31"

  selected_muni <- paste0("ID_", df_muni[df_muni$Cidade == muni,1])
  #date_start <- paste0(ano_analise,"-01-01")
  #date_end <- paste0(ano_analise,"-12-31")

  if (ano_analise <= 2000) {
    date_start <- "1990-01-01"
    date_end <- "2000-12-31"
  } else if (ano_analise > 2000 & ano_analise <= 2010){
    date_start <- "2001-01-01"
    date_end <- "2010-12-31"
  } else if ( 2010 < ano_analise) {
    date_start <- "2010-01-01"
    date_end <- "2021-12-31"
  }

  #print(muni,date_start,date_end)
  print(selected_muni)
  print(date_start)
  print(date_end)

  params <- paste0('{
  "token": {
    "token": "', token, '"
  },
  "sql": {
    "sql": {
      "query": "SELECT date, id_municipio, temp_med FROM \\"ondas_de_calor_temp_med\\" WHERE id_municipio=\'', selected_muni, '\' AND CAST(date as DATE) >= CAST(\'', date_start, '\' AS DATE) AND CAST(date as DATE) <= CAST(\'', date_end, '\' AS DATE)",
      "fetch_size": 10000
    }
  }
}')

  endpoint <- paste0(url_base,"/","sql_query")
  request <- POST(url = endpoint,
                  body = params,
                  encode = "json")
  print("até o request foi")

  convertRequestToDF <- function(request){
    variables = unlist(content(request)$columns)
    variables = variables[names(variables) == "name"]
    column_names <- unname(variables)
    values = httr::content(request)$rows
    df <- as.data.frame(do.call(rbind,lapply(values,function(r) rbind(unlist(r)))))
    names(df) <- column_names
    return(df)
  }

  df_request <- convertRequestToDF(request)
  #print(head(df_request))
  df_request <- df_request[,c(1,3)]
  df_request$date <- substr(df_request$date,1,10)
  df_request$date <- as.Date(df_request$date, format = "%Y-%m-%d")
  df_request$temp_med <- as.numeric(df_request$temp_med)
  colnames(df_request) <- c("t", "temp")
  print(head(df_request))
  print(str(df_request))
  return(df_request)
}
