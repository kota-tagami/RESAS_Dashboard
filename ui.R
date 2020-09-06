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

ui <- navbarPage(
  "RESAS Dashboard",
  theme = shinytheme("darkly"),
  collapsible = T,
  
  tabPanel(
    "Surface",
    
    titlePanel("Dashboard Surface"),
    
    fluidPage(
      add_busy_bar(color = "#FF0000"),
      wellPanel(
        fluidRow(
          column(6, select_pref("pref", "Prefecture")),
          column(6, select_city("city", "City"))
        ),
        
        fluidRow(
          column(3, set_button("triger1", 1, "cat")),
          column(3, set_button("triger2", 2, "dog")),
          column(3, set_button("triger3", 3, "crow")),
          column(3, set_button("triger4", 4, "fish"))
        )
        
      )
    ),
    
    tabsetPanel(
      tabPanel("#1", uiOutput("outputSection"), icon = icon("cat")),
      tabPanel("#2", h1(icon("dog")), icon = icon("dog")),
      tabPanel("#3", h1(icon("crow")), icon = icon("crow")),
      tabPanel("#4", h1(icon("fish")), icon = icon("fish"))
    ),


  ),
  
  
  tabPanel("Depth", titlePanel("Dashboard Depth"))
  
)
  
  
  
