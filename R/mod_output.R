outputUI <- function(id){
  tagList(
    titlePanel(textOutput(NS(id, "region"))),
    fluidPage(
      wellPanel(
        navlistPanel(
          widths = c(2, 10),
          well = F,
          tabPanel("ひと", popuUI(NS(id, "popu"))),
          tabPanel("まち", h2("Coming soon")),
          tabPanel("しごと", h2("Coming soon"))
        )
      )
    )
  )
}

outputServer <- function(id, triger, city){
  moduleServer(id, function(input, output, session){
    pref_code <- eventReactive(triger(), {
      prefs_cities %>% 
        filter(cityName == city) %>% 
        pull(prefCode)
    })
    city_code <- eventReactive(triger(), {
      prefs_cities %>% 
        filter(cityName == city) %>% 
        pull(cityCode)
    })
    
    region <- eventReactive(triger(), {
      target <- 
        prefs_cities %>% 
        filter(cityName == city)
      
      str_c(target$prefName, target$cityName, sep = " ")
    })

    output$region <- renderText(region())
    
    popuServer(
      "popu", 
      triger = triger, 
      pref_code = pref_code, 
      city_code = city_code
    )
    # townServer(foo)
    # empServer(bar)
    
  })
}