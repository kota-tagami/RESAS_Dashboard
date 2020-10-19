#<<  Prefectures and cities data  >>#
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

#<<  Input function  >>#
select_pref <- function(id, label){
  selectizeInput(
    id, label,
    width = "100%",
    choices = prefs$prefName,
    selected = default_pref
  )
}

select_city <- function(id, label){
  selectInput(
    id, label,
    width = "100%",
    choices = default_cities,
    selected = head(default_cities, 1)
  )
}

set_button <- function(id, i, icon_name){
  lab <- paste0("Set to Tab #", as.character(i))
  actionButton(id, lab, icon = icon(icon_name), width = "100%")
}
