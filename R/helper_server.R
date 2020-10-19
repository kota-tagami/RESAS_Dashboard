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

get_region <- function(city_name){
  prefs_cities %>% 
    filter(cityName == city_name)
}

darktheme <- 
  theme_minimal(base_family = "noto sans jp", base_size = 15) +
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
