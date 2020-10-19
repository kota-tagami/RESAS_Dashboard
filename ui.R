##  title: UI

##================================================##
##>>  Section 1: UI  <<##
##================================================##

##++++++++++++++++++++++++++++++++++++++++++++++++##
##>>  Sub-section 1: Surface  <<##
##++++++++++++++++++++++++++++++++++++++++++++++++##
ui_surface_input <- tagList(
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
  )
)

ui_surface_output <- tagList(
  tabsetPanel(
    id = "outputTabset",
    tabPanel("#1", icon = icon("cat"), outputUI("tab1")),
    tabPanel("#2", icon = icon("dog"), outputUI("tab2")),
    tabPanel("#3", icon = icon("crow"), outputUI("tab3")),
    tabPanel("#4", icon = icon("fish"), outputUI("tab4"))
  )
)

##++++++++++++++++++++++++++++++++++++++++++++++++##
##>>  Sub-section 2: Depth  <<##
##++++++++++++++++++++++++++++++++++++++++++++++++##


##++++++++++++++++++++++++++++++++++++++++++++++++##
##>>  Sub-section 3: Combined  <<##
##++++++++++++++++++++++++++++++++++++++++++++++++##
ui <- navbarPage(
  "RESAS Dashboard",
  header = ref,
  footer = ver,
  theme = shinytheme("darkly"),
  collapsible = T,
  
  #<<  Surface section  >>#
  tabPanel(
    "Surface",
    titlePanel("Look at the Surface"),
    ui_surface_input,
    ui_surface_output
  ),
  
  #<<  Depth section  >>#
  tabPanel(
    "Depth", 
    titlePanel("Dive into the Depth"), 
    h2("Coming soon")
  ),
  
  #<<  README section  >>#
  tabPanel("README", includeMarkdown("README.md"))
)
  
##>>--------------END of Section 1--------------<<##
