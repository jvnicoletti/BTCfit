#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  )
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    # Your application UI logic 
    shinydashboard::dashboardPage(
    
        
    # montando o cabeçalho ----------------------------------------------------
    shinydashboard::dashboardHeader(title = tags$li(a(href = 'http://www.leb.esalq.usp.br/gpeas/',
                                          tags$img(src = 'www/BTCFIT_LOGO_3.png',
                                                   title = "Company Home", height = "45px"),
                                          style = "padding-top:00px; padding-bottom:00px;")),
                                          titleWidth = 250,
                                 tags$li(a(href = 'https://www.esalq.usp.br/',
                                           img(src = 'www/logo2.png',
                                               title = "Company Home", height = "35px"),
                                               style = "padding-top:5px; padding-bottom:5px;"),
                                               class = "dropdown"),
                                 tags$li(a(href="https://www.linkedin.com/in/joaonicoletti",
                                               img(src = 'www/linkedin.png',
                                               title = "Company Home", height = "35px"),
                                               style = "padding-top:5px; padding-bottom:5px;"),
                                               class="dropdown")
    ),#fim do dashboard header


    # Montando o menu lateral -------------------------------------------------
    shinydashboard::dashboardSidebar(width = '250px',collapsed = F,
                                     shinydashboard::sidebarMenu(
                                       shinydashboard::menuItem('Data Input',
                                                                tabName = 'dados',
                                                                icon = shiny::icon('file-import')),
                                       
                                       shinydashboard::menuItem('Dashboard',
                                                                tabName = 'dashboard',
                                                                icon = shiny::icon('dashboard')),
                                       
                                       shinydashboard::menuItem('Infos',
                                                                tabName = 'infos',
                                                                icon = shiny::icon('info-circle'))
                                     )),#fim do dashboard sidebar
    
    # Setando tema do Corpo da Dashboard ------------------------------------------------------
    shinydashboard::dashboardBody(
      htmltools::tags$style(
        htmltools::HTML("
            .box.box-solid.box-primary>.box-header {
              color:#fff;
              background:#24769f
                                }
            .box.box-solid.box-primary{
            border-bottom-color:#24769f;
            border-left-color:#24769f;
            border-right-color:#24769f;
            border-top-color:#24769f;
            }")),
      
      htmltools::tags$head(tags$style(HTML(".info-box, .info-box-icon, .small-box{height: 100px}"))),
      dashboardthemes::shinyDashboardThemes(theme = "blue_gradient"),
      
      
      # Menu de infomações ------------------------------------------------------
      shinydashboard::tabItems(
        shinydashboard::tabItem(tabName = 'infos',
                                shiny::h1("Infos"),
                                shinydashboard::infoBox(title = 'Contact',icon = shiny::icon('envelope-square'),
                                                        subtitle = 'For further information and/or feedback please contact: jvnicoletti@usp.br'))
        ,#fim do tabitem info
        
        # menu de importação de dados ---------------------------------------------
        shinydashboard::tabItem(tabName = 'dados',
                                shiny::fluidRow(
                                  shiny::column(12,
                                                shinydashboard::box(title = span("1º - Table Upload",style = "color: white; font-size: 16px"),width = '100%',
                                                                    solidHeader = T,
                                                                    collapsible = T,
                                                                    collapsed = T,
                                                                    status="primary",
                                                                    #height = 1000,
                                                                           fileInput("arquivo", span("Upload Experimental Data:",style = "color: black; font-size: 16px"),multiple = F,accept = c(".xlsx"), width = "90%"),
                                                                           helpText("Xlsx Table, no header, 1º Column: Time Interval (Time format); 2º Column: Pore Volume; 3º Column: C/Co",width = "100%"),
                                                                    
                                                                    
                                                                           actionButton("upload",span("Upload",style = "color: black; font-size: 16px"), width = "100%")
                                                                   
                                                                    
                                                )#fim do box
                                  ),#fim da coluna
                                  shiny::column(12,
                                                shinydashboard::box(title = span("2º - Table Editing",style = "color: white; font-size: 16px"),width = '100%',
                                                                    solidHeader = T,
                                                                    collapsible = T,
                                                                    collapsed = T,
                                                                    status="primary",
                                                                    #height = 1000,
                                                                    column(12,
                                                                           
                                                                           tags$head(tags$style(HTML('.modal-lg {width: 1200px;}'))),
                                                                           br(),
                                                                           ### tags$head() is to customize the download button
                                                                           tags$head(tags$style(".butt{background-color:#230682;} .butt{color: #e6ebef;}")),
                                                                           
                                                                           
                                                                           shinyalert::useShinyalert(), # Set up shinyalert
                                                                           
                                                                           uiOutput("MainBody_trich"),
                                                                           helpText("Note: Remember to save any updates!"),
                                                                           actionButton(inputId = "Updated_trich",label = "Save"),downloadButton("Trich_csv", "Download input file", class="butt")
                                                                          #fileInput("arquivo", span("Upload dos dados experimentais:",style = "color: white; font-size: 16px"),multiple = T,accept = c(".xlsx")),
                                                                    ),
                                          )#fim do box
                                  ),#fim da coluna
                                         column(12,       
                                                shinydashboard::box(title = span("3º - Experimental data input",style = "color: white; font-size: 16px"),width = '100%',
                                                                    solidHeader = T,
                                                                    collapsible = T,
                                                                    collapsed = T,
                                                                    status="primary",
                                                                    column(6,
                                                                           textInput("diametro","Enter the column diameter (cm)","5"),
                                                                           textInput("comprimento","Enter the Column Length (cm)","20"),
                                                                           
                                                                    ),
                                                                    column(6,
                                                                           textInput("volume_frasco","Enter the volume of the collection flask (cm³)","23"),
                                                                           shinyWidgets::pickerInput(
                                                                             inputId = "escolha_metodologia",
                                                                             label = "Choose the methodology adopted", 
                                                                             choices = c("Pore Volume", "Time"),
                                                                             multiple = F
                                                                           )
                                                                    )
                                                )#fim do box
                                         ),
                                 
                                  
                                ),#fim da fluid row
        ),#fim da tabItem
        
        # Menu da dashboard -------------------------------------------------------
        shinydashboard::tabItem(tabName = 'dashboard',
                # cards superiores------------------------------------
                shiny::fluidRow(
                  mod_cards_superiores_ui("cards_superiores_ui_1")
                ),
                
                shiny::fluidRow(
                  
                  shiny::column(width = 12,
                           shinydashboard::box(title = span("Data Visualization",style = "color: white; font-size: 16px"),width = '100%',
                               solidHeader = T,
                               collapsible = F,
                               status="primary",
                               
                               column(width = 8,
                                      shinydashboard::tabBox(
                                        title = "Experimental Graphical Visualization",
                                        
                                        tags$head(tags$style("#erro{color: red;
                                              font-size: 15px;
                                              font-style: italic;
                                              }")),
                                        width = '100%',
                                        height = 600,
                                        side = "left",
                                        selected = "BTC",
                                        tabPanel("BTC",shinycssloaders::withSpinner(plotOutput("BTC",height = "500px",width = "100%"))),
                                        tabPanel("Boxplot",shinycssloaders::withSpinner(plotOutput("boxplot", width = "100%",height = 500))),
                                        tabPanel("Density",shinycssloaders::withSpinner(plotOutput("histograma", width = "100%",height = 500)))
                                        
                                      ),#fim box
                                      
                                      
                                      downloadButton("grafico_final", "Graphic Download",class = "butt"),
                                      downloadButton("tabela_final", "Download Final Report",class = "butt"),
                                      tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: white;} .butt{width: 49%;}")), # background color and font color
                                      
                                      
                               ),
                               
                               column(4,
                                      
                                      shinydashboard::infoBox(width = 12,title = "Retardation factor",
                                                              value = textOutput(outputId = "retardamento"),icon = icon("hand-holding-water")),
                                      shinydashboard::infoBox(width = 12, title = "Dispersion coeff. (cm²/min)", value = textOutput(outputId = "dispersao"),
                                                              icon = icon("hand-holding-water")),
                                      shinydashboard::infoBox(width = 12, title = "Peclet number", value = textOutput(outputId = "peclet"),
                                                              icon = icon("hand-holding-water")),
                                      numericInput("slider_umidade_saturacao", label = "Saturated Volumetric Water Content:", min = 0, 
                                                  max = 5, value = 0.42,step = 0.01),
                                      numericInput("slider_densidade_solo", label = "Soil Density (g/cm³):", min = 0, 
                                                  max = 5, value = 1.5,step = 0.01),
                                      numericInput("slider_densidade_particula", label = "Particle Density (g/cm³):", min = 0, 
                                                  max = 5, value = 2.65,step = 0.01)
                                      
                                      
                               ),#fim da coluna valores
                               column(width = 12,
                                      shinydashboard::box(
                                        title = span("Results",
                                                     style = "color: black; font-size: 16px"),
                                        width = '100%',
                                        #background = 'navy',
                                        ## gráfico de linhas
                                        DT::dataTableOutput("tabela", width = "100%"),
                                        ## texto descritivo do grafico de linhas
                                      )#fim box
                               ),
                            )#fim do box
                           )#fim da coluna
                           
                    )#fim da fluid row
                    
                )#fim da tab item dashboard
        
      )#fim tab itens
    )#fim do dashboard body

    
    )#fim do dashboard page
    
  )#fim da taglist
  
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'BTCfit'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

