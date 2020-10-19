##++++++++++++++++++++++++++++++++++++++++++++++++##
##>>  Sub-section 1: 人口推移  <<##
##++++++++++++++++++++++++++++++++++++++++++++++++##
fn_popu_comp <- function(pref_code, city_code){
  res_popu_comp <- 
    api_popu_comp %>% 
    resas_api(., param = list(
      "prefCode" = pref_code,
      "cityCode" = city_code
    ))
  
  stopifnot(is.list(res_popu_comp))
  
  boundaryYear <- 
    res_popu_comp %>% 
    pluck("result", "boundaryYear") 
  
  data_popu_comp <- 
    res_popu_comp %>% 
    pluck("result", "data") %>%
    list.select(label, data) %>%
    list.stack() %>% 
    as_tibble() %>% 
    mutate(
      year = data %>% 
        list.select(year) %>% 
        unlist(),
      value = data %>% 
        list.select(value) %>% 
        unlist()
    ) %>% 
    select(-data)
  
  plot_popu_comp <- 
    data_popu_comp %>% 
    ggplot(aes(x = year, y = value/10000, group = label, color = label)) +
    darktheme +
    scale_color_few(palette = "Light") +
    scale_x_continuous(breaks = seq(1980, 2050, 10)) +
    labs(x = NULL, y = "(万人)", color = NULL) +
    geom_point(
      data = . %>% 
        filter(year <= boundaryYear),
      size = 2,
      shape = 16
    ) +
    geom_point(
      data = . %>% 
        filter(year >= boundaryYear),
      size = 2,
      shape = 1
    ) +
    geom_line(
      data = . %>% 
        filter(year <= boundaryYear),
      size = 1,
      linetype = "solid"
    ) +
    geom_line(
      data = . %>% 
        filter(year >= boundaryYear),
      size = 1,
      linetype = "dotted"
    ) 
  
  plot_popu_comp
}