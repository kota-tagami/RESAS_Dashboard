##  title: Server

##================================================##
##>>  Section 2: Server  <<##
##================================================##
server <- function(session, input, output){
  
  #<<  Input reactivity  >>#  
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
  
  #<<  Trigers  >>#
  triger1 <- reactive(input$triger1)
  triger2 <- reactive(input$triger2)
  triger3 <- reactive(input$triger3)
  triger4 <- reactive(input$triger4)
  
  #<<  Tab 1  >>#
  outputServer("tab1", triger = triger1, city = city())
  observeEvent(triger1(), {
    updateTabsetPanel(
      session, "outputTabset",
      selected = "#1"
    )
  })

  #<<  Tab 2  >>#
  outputServer("tab2", triger = triger2, city = city())
  observeEvent(triger2(), {
    updateTabsetPanel(
      session, "outputTabset",
      selected = "#2"
    )
  })

  #<<  Tab 3  >>#
  outputServer("tab3", triger = triger3, city = city())
  observeEvent(triger3(), {
    updateTabsetPanel(
      session, "outputTabset",
      selected = "#3"
    )
  })

  #<<  Tab 4  >>#
  outputServer("tab4", triger = triger4, city = city())
  observeEvent(triger4(), {
    updateTabsetPanel(
      session, "outputTabset",
      selected = "#4"
    )
  })
  
}

##>>--------------END of Section 2--------------<<##

