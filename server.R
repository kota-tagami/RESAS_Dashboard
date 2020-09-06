server <- function(session, input, output){
  observeEvent(input$pref, {
    cities <- 
      prefs_cities %>% 
      filter(prefName == input$pref) %>% 
      distinct(cityName) %>% 
      pull(cityName) 
    
    updateSelectInput(session, "city", choices = cities)
  })
  
  pref <- reactive(input$pref)
  city <- reactive(input$city)
  
  outputSection <- eventReactive(input$triger1, {
      tagList(
        titlePanel(str_c(pref(), city(), sep = " ")),
        
        fluidPage(
          wellPanel(
            navlistPanel(
              widths = c(2, 10),
              well = F,
              tabPanel(
                "ひと",
                wellPanel(column(6, h3("人口推移")), 
                          column(6, h3("人口増減"))),
                wellPanel(fluidRow(column(6, plotOutput("plot1")), 
                                   column(6, plotOutput("plot2")))),
                wellPanel(column(6, h3("人口ピラミッド")), 
                          column(6, h3("合計特殊出生率"))),
                wellPanel(fluidRow(column(6, plotOutput("plot3")), 
                                   column(6, plotOutput("plot4"))))
              ),
              
              tabPanel("まち"),
              tabPanel("しごと")
              
            )
          )
        )
      )
  })
  output$outputSection <- renderUI(outputSection())
  
  
  target_region <- eventReactive(input$triger1, {
    prefs_cities %>% 
      filter(cityName == city())
  })

  plot_popu_comp <- eventReactive(input$triger1, {
    res_popu_comp <- 
      api_popu_comp %>% 
      resas_api(., param = list(
        "prefCode" = target_region()$prefCode,
        "cityCode" = target_region()$cityCode
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
  })
  output$plot1 <- renderPlot(plot_popu_comp())

  plot_popu_change <- eventReactive(input$triger1, {

    res_popu_change <- 
      api_popu_change %>% 
      resas_api(., param = list(
        "prefCode" = target_region()$prefCode,
        "cityCode" = target_region()$cityCode
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
  })
  output$plot2 <- renderPlot(plot_popu_change())

  plot_popu_pyra <- eventReactive(input$triger1, {
    
    res_popu_pyra <- 
      api_popu_pyra %>% 
      resas_api(., param = list(
        "prefCode" = target_region()$prefCode,
        "cityCode" = target_region()$cityCode,
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
  })
  output$plot3 <- renderPlot(plot_popu_pyra())

  plot_popu_birth <- eventReactive(input$triger1, {
    
    res_popu_birth <- 
      api_popu_birth %>% 
      resas_api(., param = list(
        "prefCode" = target_region()$prefCode,
        "cityCode" = target_region()$cityCode,
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
  })
  output$plot4 <- renderPlot(plot_popu_birth())

}
