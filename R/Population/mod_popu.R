##  title: Module of Population section

##================================================##
##>>  Section 1: UI  <<##
##================================================##
popuUI <- function(id){
  tagList(
    wellPanel(
      column(6, h3("人口推移")), 
      column(6, h3("人口増減"))
    ),
    wellPanel(fluidRow(
      column(6, plotOutput(NS(id, "plot1"))), 
      column(6, plotOutput(NS(id, "plot2")))
    )),
    wellPanel(
      column(6, h3("人口ピラミッド")), 
      column(6, h3("合計特殊出生率"))
    ),
    wellPanel(fluidRow(
      column(6, plotOutput(NS(id, "plot3"))), 
      column(6, plotOutput(NS(id, "plot4")))
    ))
  )
}

##>>--------------END of Section 1--------------<<##


##================================================##
##>>  Section 2: Server  <<##
##================================================##
popuServer <- function(id, triger, pref_code, city_code){
  moduleServer(id, function(input, output, session){
    #<<  人口推移  >>#
    plot_popu_comp <- eventReactive(triger(), {
      fn_popu_comp(pref_code = pref_code(), city_code = city_code())
    })
    output$plot1 <- renderPlot(plot_popu_comp())
    
    #<<  人口増減  >>#
    plot_popu_change <- eventReactive(triger(), {
      fn_popu_change(pref_code = pref_code(), city_code = city_code())
    })
    output$plot2 <- renderPlot(plot_popu_change())

    #<<  人口ピラミッド  >>#
    plot_popu_pyra <- eventReactive(triger(), {
      fn_popu_pyra(pref_code = pref_code(), city_code = city_code())
    })
    output$plot3 <- renderPlot(plot_popu_pyra())
    
    #<<  合計特殊出生率  >>#
    plot_popu_birth <- eventReactive(triger(), {
      fn_popu_birth(pref_code = pref_code(), city_code = city_code())
    })
    output$plot4 <- renderPlot(plot_popu_birth())
    
  })
}

##>>--------------END of Section 2--------------<<##
