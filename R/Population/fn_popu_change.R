##++++++++++++++++++++++++++++++++++++++++++++++++##
##>>  Sub-section 2: 人口増減  <<##
##++++++++++++++++++++++++++++++++++++++++++++++++##
fn_popu_change <- function(pref_code, city_code){
  res_popu_change <- 
    api_popu_change %>% 
    resas_api(., param = list(
      "prefCode" = pref_code,
      "cityCode" = city_code
    ))
  
  stopifnot(is.list(res_popu_change))
  
  data_popu_change <-
    res_popu_change %>%
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
        unlist(),
    ) %>%
    select(-data) %>%
    filter(label != "総人口") %>%
    mutate(
      type = case_when(
        label %in% c("死亡数", "出生数") ~ "自然増減",
        label %in% c("転出数", "転入数") ~ "社会増減"
      )
    )
  
  plot_popu_change <-
    data_popu_change %>%
    ggplot(aes(x = year, y = value/10000, group = label, color = label)) +
    darktheme +
    scale_color_few(palette = "Light") +
    scale_x_continuous(breaks = seq(1900, 2100, 5)) +
    labs(x = NULL, y = "(万人)", color = NULL) +
    facet_wrap(~ type, scales = "free_y") +
    geom_point(size = 2) +
    geom_line(size = 1)
  
  plot_popu_change
}