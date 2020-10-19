##++++++++++++++++++++++++++++++++++++++++++++++++##
##>>  Sub-section 4: 合計特殊出生率  <<##
##++++++++++++++++++++++++++++++++++++++++++++++++##
fn_popu_birth <- function(pref_code, city_code){
  res_popu_birth <- 
    api_popu_birth %>% 
    resas_api(., param = list(
      "prefCode" = pref_code,
      "cityCode" = city_code,
      "ageFrom" = "15",
      "ageTo" = "49"
    ))
  
  stopifnot(is.list(res_popu_birth))
  
  data_popu_birth <-
    res_popu_birth %>%
    pluck("result", "line", "data") %>%
    list.select(yearRange, value) %>%
    list.stack() %>%
    as_tibble() %>%
    mutate(
      yearRange = yearRange %>%
        fct_inorder(),
      type = "foo"
    )
  
  plot_popu_birth <-
    data_popu_birth %>%
    ggplot(aes(x = yearRange, y = value, group = type)) +
    darktheme +
    labs(x = NULL, y = NULL) +
    ## ggthemes_data$few$colors$Light
    geom_point(color = "#88BDE6", size = 2) +
    geom_line(color = "#88BDE6", size = 1)
  
  plot_popu_birth
}