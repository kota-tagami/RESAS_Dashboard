##++++++++++++++++++++++++++++++++++++++++++++++++##
##>>  Sub-section 3: 人口ピラミッド  <<##
##++++++++++++++++++++++++++++++++++++++++++++++++##
fn_popu_pyra <- function(pref_code, city_code){
  res_popu_pyra <- 
    api_popu_pyra %>% 
    resas_api(., param = list(
      "prefCode" = pref_code,
      "cityCode" = city_code,
      "yearLeft" = "2015",
      "yearRight" = "2040"
    ))
  
  stopifnot(is.list(res_popu_pyra))
  
  data_popu_pyra <-
    res_popu_pyra %>%
    pluck("result") %>%
    map_dfr(~{
      year <- pluck(.x, "year")
      dat <-
        pluck(.x, "data") %>%
        list.select(class, manPercent, womanPercent) %>%
        list.stack() %>%
        as_tibble() %>%
        select(class, `男性` = manPercent, `女性` = womanPercent) %>%
        mutate(
          class = class %>%
            str_replace_all("～", "~") %>%
            str_remove_all("歳") %>%
            fct_inorder(),
          `男性` = `男性` * -1,
          year = year
        )
      dat
    }) %>%
    pivot_longer(-c(class, year)) %>%
    mutate(name = name %>% fct_relevel("男性", "女性"))
  
  y_breaks <-
    data_popu_pyra %>%
    distinct(class) %>%
    mutate(
      class = if_else(
        row_number(class) %% 2 == 1, as.character(class), ""
      )
    ) %>%
    pull(class)
  
  plot_popu_pyra <-
    data_popu_pyra %>%
    ggplot(aes(x = value, y = class, group = name, fill = name)) +
    darktheme +
    theme(
      panel.grid.major.y = element_blank(),
      axis.ticks.y = element_line(color = "grey80"),
    ) +
    scale_x_continuous(breaks = seq(-10, 10, 1), labels=abs(seq(-10, 10, 1))) +
    scale_y_discrete(labels = y_breaks) +
    scale_fill_few() +
    facet_wrap(~ year) +
    geom_col(width = 0.8) +
    labs(x = "%", y = "年齢", fill = NULL)
  
  plot_popu_pyra
}