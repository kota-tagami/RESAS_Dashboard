library(tidyverse)
library(httr)
library(rlist)
library(pipeR)
library(listviewer)

endpoint <- "https://opendata.resas-portal.go.jp"
api_key <- "CcpDn8wG1XR94IUtv1A5BnRmMK84LG5YMUNXxfnz"

resas_api <- function(.api, param = NULL){
  res <- GET(
    paste0(endpoint, .api),
    add_headers("X-API-KEY" = api_key),
    query = param
  )
  
  Sys.sleep(1/5)
  
  res_content <- content(res)
  
  return(res_content)
}

pref_api <- "/api/v1/prefectures"
res_pref <- resas_api(pref_api)

prefs <- 
  res_pref %>>% 
  pluck("result") %>>% 
  list.select(prefCode, prefName) %>>%
  list.stack() %>>%
  as_tibble() %>% 
  mutate(
    prefCode = prefCode %>% as.character(),
    prefName = prefName %>% fct_inorder()
  )

cities_api <- "/api/v1/cities"
res_cities <- 
  prefs$prefCode %>% 
  map(
    ~ resas_api(cities_api, param = list("prefCode" = .)) 
  ) %>% 
  set_names(prefs$prefCode)

cities <- 
  res_cities %>% 
  map_dfr(
    ~ pluck(., "result") %>>% 
      list.select(cityCode, cityName, bigCityFlag) %>>%
      list.stack() %>>%
      as_tibble(),
    .id = "prefCode"
  ) %>% 
  mutate(
    cityName = cityName %>% fct_inorder(),
    bigCityFlagName = case_when(
      bigCityFlag == "0" ~ "一般の市区町村",
      bigCityFlag == "1" ~ "政令指定都市の区",
      bigCityFlag == "2" ~ "政令指定都市の市",
      bigCityFlag == "3" ~ "東京都23区",
    )
  )


prefs_cities <- left_join(prefs, cities, by = "prefCode")

write_rds(prefs_cities, "prefs_cities/prefs_cities.rds")
