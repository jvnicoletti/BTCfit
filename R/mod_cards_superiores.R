#' cards_superiores UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cards_superiores_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    shinydashboard::valueBox(subtitle = 'Area (cm²)',value = textOutput(outputId = "area"),
             icon = icon("ruler-combined"),color = "blue",width = 3),
    shinydashboard::valueBox(subtitle = "Volume (cm³)", value = textOutput(outputId = "volume"),
             icon = icon("flask"),color = "blue",width = 3),
    shinydashboard::valueBox(subtitle = "Flow Rate (cm/min)", value = textOutput(outputId = "vazao"),
             icon = icon("tint"),color = "blue",width = 3),
    shinydashboard::valueBox(subtitle = 'RMSE',value = textOutput(outputId = "erro_final"),
             icon = icon("times"),color = "blue",width = 3)
    
  )
}
    
#' cards_superiores Server Functions
#'
#' @noRd 
mod_cards_superiores_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_cards_superiores_ui("cards_superiores_ui_1")
    
## To be copied in the server
# mod_cards_superiores_server("cards_superiores_ui_1")
