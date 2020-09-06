library(shiny)
library(shinythemes)
library(shinybusy)
library(tidyverse, quietly = T)
library(httr, quietly = T)
library(rlist, quietly = T)
library(pipeR, quietly = T)
library(ggthemes, quietly = T)
library(ggdark, quietly = T)


##===========##
#### Input ####
##===========##
prefs_cities <- 
  read_rds("prefs_cities/prefs_cities.rds") %>% 
  mutate(across(where(is.factor), as.character))

prefs <-
  prefs_cities %>% 
  distinct(prefCode, prefName)

default_pref <- head(prefs$prefName, 1)
default_cities <- 
  prefs_cities %>% 
  filter(prefName == default_pref) %>% 
  distinct(cityName) %>% 
  pull(cityName) 



##============##
#### Server ####
##============##

endpoint <- "https://opendata.resas-portal.go.jp"
api_key <- "CcpDn8wG1XR94IUtv1A5BnRmMK84LG5YMUNXxfnz"

resas_api <- function(.api, param = NULL){
  res <- GET(
    paste0(endpoint, .api),
    add_headers("X-API-KEY" = api_key),
    query = param
  )
  Sys.sleep(1)
  res_content <- content(res)
  res_content
}


darktheme <- 
  theme_minimal(base_family = "Noto Sans JP", base_size = 15) +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "black"),
    panel.grid.minor.y = element_line(color = "black"),
    plot.background = element_rect(fill = "grey10", color = "grey10"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(color = "grey80"),
    text = element_text(color = "grey80"),
    axis.text = element_text(color = "grey80"),
    axis.ticks.x = element_line(color = "grey80")
  )


##------------##
#### People ####
##------------##

api_popu_comp <- "/api/v1/population/composition/perYear"
api_popu_change <- "/api/v1/population/sum/estimate"
api_popu_pyra <- "/api/v1/population/composition/pyramid"
api_popu_birth <- "/api/v1/population/nature"

