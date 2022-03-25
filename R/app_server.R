#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#'
 
library(shiny)
library(shinyjs)
library(DT)
library(data.table)
library(lubridate)
library(shinyalert)

shinyalert::useShinyalert()

app_server <- function( input, output, session ) {

  
  # Your application server logic 
  options(scipen = 999)
  data_input <- data.frame()
  
  data <- reactive({
    req(input$arquivo)
    
    inFile <- input$arquivo
    
    if (is.null(inFile)) { return(NULL) }
    
    data_input = readxl::read_xlsx(inFile$datapath, sheet =  1,col_names = F) %>%
      tibble::as_tibble() 
    
    colnames(data_input) = c("Time","Pore_Volume","CCo")
    
    n_row = nrow(data_input)-1
    
    data_input <- data_input %>% 
      dplyr::mutate_all(as.numeric) %>% 
      dplyr::mutate(
        Time = lubridate::as_datetime(Time),
        minuto = lubridate::minute(Time),
        segundo = lubridate::second(Time)/60,
        min_decimal = minuto+segundo,
        Time = cumsum(min_decimal)
      ) %>% 
      dplyr::select(-c(minuto,segundo,min_decimal))
      
    
    
    
  })
  
  observeEvent(input$upload, {
  vals_trich<-reactiveValues()
  vals_trich$Data<-data()
  
  
  #### MainBody_trich is the id of DT table
  output$MainBody_trich<-renderUI({
    
    fluidPage(
      hr(),
      column(6,offset = 0,
             HTML('<div class="btn-group" role="group" aria-label="Basic example" style = "padding:10px">'),
             ### tags$head() This is to change the color of "Add a new row" button
             tags$head(tags$style(".butt2{background-color:#231651;} .butt2{color: #e6ebef;}")),
             div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "Add_row_head",label = "Add", class="butt2") ),
             tags$head(tags$style(".butt4{background-color:#4d1566;} .butt4{color: #e6ebef;}")),
             div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "mod_row_head",label = "Edit", class="butt4") ),
             tags$head(tags$style(".butt3{background-color:#590b25;} .butt3{color: #e6ebef;}")),
             div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "Del_row_head",label = "Delete", class="butt3") ),
             ### Optional: a html button 
             # HTML('<input type="submit" name="Add_row_head" value="Add">'),
             HTML('</div>') ),
      
      column(12,DT::dataTableOutput("Main_table_trich")),
      tags$script("$(document).on('click', '#Main_table_trich button', function () {
                   Shiny.onInputChange('lastClickId',this.id);
                   Shiny.onInputChange('lastClick', Math.random()) });")
      
    ) 
  })
  
  #### render DataTable part ####
  output$Main_table_trich<-DT::renderDT({
    
    DT= vals_trich$Data
    DT::datatable(DT,selection = 'single',
              escape=F) %>%
      DT::formatRound(columns=c("Time","Pore_Volume","CCo"), digits=3)
    })
  
  
  observeEvent(input$Add_row_head, {
    ### This is the pop up board for input a new row
    showModal(modalDialog(title = "Add a new row",
                          textInput(paste0("Add_Tempo", input$Add_row_head), "Time:"),
                          textInput(paste0("Add_VP", input$Add_row_head), "Pore Volume:"),
                          textInput(paste0("Add_CCO", input$Add_row_head), "C/Co:"),
                          actionButton("go", "Add item"),
                          easyClose = TRUE, footer = NULL ))
    
  })
  ### Add a new row to DT  
  observeEvent(input$go, {
    new_row = data.frame(
      Time  = as.character( input[[paste0("Add_Tempo", input$Add_row_head)]] ),
      Pore_Volume    = input[[paste0("Add_VP", input$Add_row_head)]],
      CCo = input[[paste0("Add_CCO", input$Add_row_head)]],
      
    )
    vals_trich$Data<-rbind(vals_trich$Data,new_row )
    removeModal()
  })
  
  
  
  ### save to RDS part 
  observeEvent(input$Updated_trich,{
    saveRDS(vals_trich$Data, "inst/app/note.rds")
    shinyalert::shinyalert(title = "Saved!", type = "success")
  })
  
  
  
  ### delete selected rows part
  ### this is warning messge for deleting
  observeEvent(input$Del_row_head,{
    showModal(
      if(length(input$Main_table_trich_rows_selected)>=1 ){
        modalDialog(
          title = "Warning",
          paste("Are you sure delete",length(input$Main_table_trich_rows_selected),"rows?" ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("ok", "Yes")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select row(s) that you want to delect!" ),easyClose = TRUE
        )
      }
      
    )
  })
  
  ### If user say OK, then delete the selected rows
  observeEvent(input$ok, {
    vals_trich$Data = vals_trich$Data[- input$Main_table_trich_rows_selected, ]
    removeModal()
  })
  
  ### edit button
  observeEvent(input$mod_row_head,{
    showModal(
      if(length(input$Main_table_trich_rows_selected)>=1 ){
        modalDialog(
          
          fluidPage(
            tags$style(type="text/css",
                       ".shiny-output-error { visibility: hidden; }",
                       ".shiny-output-error:before { visibility: hidden; }"
            ),
            
            h3(strong("Modification"),align="center"),
            hr(),
            DT::dataTableOutput('row_modif'),
            actionButton("save_changes","Save changes"),
            
            tags$script(HTML("$(document).on('click', '#save_changes', function () {
                             var list_value=[]
                             
                             for (i = 0; i < $( '.new_input' ).length; i++){
                             
                             list_value.push($( '.new_input' )[i].value)
                             }
                             Shiny.onInputChange('newValue', list_value) });"))),
          size="l",easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select the row that you want to edit!" ),easyClose = TRUE
        )
      }
      
      )
  })
  
  
  
  
  #### modify part
  output$row_modif<-DT::renderDataTable({
    selected_row=input$Main_table_trich_rows_selected
    old_row=vals_trich$Data[selected_row,]
    
    row_change=list()
    for (i in colnames(old_row))
    {
      if (is.numeric(vals_trich$Data[[selected_row,i]]))
      {
        row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"','  type="number" id=new_',i,' ><br>')
      } 
      if( lubridate::is.Date(vals_trich$Data[[selected_row,i]])){
        row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"',' type="date" id=new_  ',i,'  ><br>') 
      }
      else{ 
        row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"',' type="textarea"  id=new_',i,'><br>')
      }
    }
    row_change=data.table::as.data.table(row_change)
    data.table::setnames(row_change,colnames(old_row))
    DT=row_change
    DT 
    
  },escape=F,options=list(dom='t',ordering=F,scrollX = TRUE),selection="none" )
  
  
  
  ### This is to replace the modified row to existing row
  observeEvent(input$newValue,
               {
                  newValue=lapply(input$newValue, function(col) {
                   if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
                     as.numeric(as.character(col))
                   } else {
                       col
                   }
                  })
                 DF=data.frame(lapply(newValue, function(x) t(data.frame(x))),row.names = input$Main_table_trich_rows_selected)
                 
                 colnames(DF) = colnames(vals_trich$Data)
                 
                 vals_trich$Data[input$Main_table_trich_rows_selected,]<-DF
                
               }
  )
  ### This is nothing related to DT Editor but I think it is nice to have a download function in the Shiny so user 
  ### can download the table in csv
  output$Trich_csv<- downloadHandler(
    filename = function() {
      paste("Trich Project-Progress", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      xlsx::write.xlsx(data.frame(vals_trich$Data), file, row.names = F)
    }
  )
  

# iniciando evento de processar -------------------------------------------
  
  #observeEvent(input$Processar,{
  values <-  reactive({
    df <- vals_trich$Data %>% 
      tidyr::as_tibble() %>% 
      dplyr::rename(
        Tempo = Time,
        VP = Pore_Volume
      )
    #importando dados gerais
    print(df)
    
    r_inicial = 1
    d_inicial = 1
    
    volume_frasco = as.numeric(gsub(",",".",input$volume_frasco))
    comprimento_coluna = as.numeric(gsub(",",".",input$comprimento))
    diametro_coluna = as.numeric(gsub(",",".",input$diametro))
    
    
    raio_coluna = diametro_coluna/2
    area_coluna = (pi*(diametro_coluna^2))/4
    
    volume_coluna = area_coluna*comprimento_coluna #cm3
    
    if(input$escolha_metodologia == "Pore Volume"){
      escolha_metodologia = 1
    }
    
    if(input$escolha_metodologia == "Time"){
      escolha_metodologia = 2
    }
    
    print(escolha_metodologia)
    
    #importando dados da metodologia - volume de poro
    if(as.numeric(escolha_metodologia) == 1){
      #Volume de poro
      tempo = list(df$Tempo)[[1]]
      
      VP = list(df$VP)[[1]]
      
      CCo = list(df$CCo)[[1]]
      
      print(CCo)
      
      umidade_saturacao = as.numeric(input$slider_umidade_saturacao)
      
      vazao = vector(mode = "list", length = length(tempo))
      
      for (i in 1:length(tempo)) {
        vazao[[i]] = volume_frasco/(tempo[i]-tempo[i-1])#cm/min
      }
      
      vazao = mean(unlist(vazao))
      
      densidade_fluxo = (vazao/area_coluna)
      
      velocidade = (densidade_fluxo/umidade_saturacao)
      
    } 
    
    #importando dados da metodologia - tempo
    if(as.numeric(escolha_metodologia) == 2){
      #Tempo
      
      tempo = list(df$Tempo)[[1]]
      
      CCo = list(df$CCo)[[1]]
      
      print(CCo)
     
      
      densidade_solo = as.numeric(input$slider_densidade_solo)
      densidade_particulas = as.numeric(input$slider_densidade_particula)
      
      #densidade_solo = as.numeric(df[1,6][[1]])
      
      
      #densidade_particulas = as.numeric(df[1,5][[1]])
      
      
      porosidade = 1 - (densidade_solo/densidade_particulas)
      
      volume_poroso = volume_coluna*porosidade
      volume_de_poros = volume_frasco/volume_poroso
      VP = seq(from = volume_de_poros,to = length(tempo)*volume_de_poros,by=volume_de_poros)
      
      vazao = vector(mode = "list", length = length(tempo))
      
      for (i in 1:length(tempo)) {
        vazao[[i]] = volume_frasco/(tempo[i]-tempo[i-1])#cm/min
      }
      
      vazao = mean(unlist(vazao))
      print(vazao)
      densidade_fluxo = (vazao/area_coluna)
      
      velocidade = (densidade_fluxo/porosidade)
      
    }
    
    #criando dados que serão passados a função
    data = list()
    data[[1]] = VP;data[[2]] = velocidade;data[[3]] = comprimento_coluna;data[[4]] = CCo
    anterior <- as.data.frame(cbind(VP,CCo))
    colnames(anterior) <- c("VP","C/Co")
    
    # call solver (with initial value c(0, 1) and default method = "Nelder-Mead")
    result <- stats::optim(par = c(r_inicial, d_inicial), 
                    otimizacao,
                    data = data,
                    #method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN","Brent"),
                    #control = list()
    )
    print(result$par)
    ajustado <- modelo(result$par,data)
    
    
    final <- as.data.frame(cbind(VP,ajustado))
    
    colnames(final) <- c("VP","C/Co")
    
    cols <- c("Experimental Data"="#f04546","Adjusted Data"="#3591d1")
    
    adjusted_plot <- ggplot2::ggplot()  + 
      ggplot2::geom_point(data = anterior, ggplot2::aes(VP,`C/Co` ,colour = "Experimental Data")) + 
      #ggplot2::geom_line(data = anterior, ggplot2::aes(VP,`C/Co` ,colour = "Dados Experimentais")) + 
      #ggplot2::geom_point(data = final, ggplot2::aes(VP,`C/Co` ,colour = "Dados Ajustados")) + 
      ggplot2::geom_line(data = final, ggplot2::aes(VP,`C/Co` ,colour = "Adjusted Data")) + 
      ggplot2::scale_colour_manual(name="",values=cols)+
      ggplot2::ggtitle(" Breakthrough Curve (BTC)") +
      ggplot2::ylab("Relative Concentration (C/Co)") + ggplot2::xlab("Pore Volume (VP)") +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 10, vjust=-.2)) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = 10, vjust=0.3)) + 
      ggplot2::theme(plot.title = ggplot2::element_text(size = 15, vjust=0.5, hjust = 0.5))+
      ggplot2::theme(
        legend.position = c(.95, .4),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = ggplot2::margin(6, 6, 6, 6)
      )+
      ggplot2::scale_x_continuous(expand = c(0, 0))+
      ggplot2::scale_y_continuous(expand = c(0, 0),limits = c(0,1))
    
    boxplot_data <- rbind(cbind(final$`C/Co`,"Relative Concentration BTCFit"),
                          cbind(anterior$`C/Co`,"Experimental Data")) %>% 
      tidyr::as_tibble() %>% 
      dplyr::mutate(
        V1 = as.numeric(V1)
      ) %>% 
      dplyr::rename(
        "Value" = V1,
        "Group" = V2
      )
    
    boxplot_comparativo <- ggplot2::ggplot(boxplot_data, ggplot2::aes(x=Group, y=Value, color=Group)) + 
      ggplot2::geom_boxplot(alpha = 0.8) +
      ggplot2::geom_dotplot(binaxis='y', stackdir='center', dotsize=0.3)+
      ggplot2::stat_summary(fun.y=mean, geom="point", shape=23, size=4)+
      ggplot2::scale_color_brewer(palette="Dark2")+
      ggplot2::ylab("Relative Concentration (C/Co)")+
      ggplot2::xlab(ggplot2::element_blank())+
      ggplot2::theme_classic()+
      ggplot2::theme(axis.text.x=ggplot2::element_blank(),
            plot.title = ggplot2::element_text(color="black", size=14, face="bold.italic",hjust=0.5,vjust = 2),
            axis.title.y = ggplot2::element_text(color="#0836AA", size=14, face="bold", hjust=0.5)
      )
    
    #histogram
    sumarizado = boxplot_data %>% 
      dplyr::group_by(Group) %>% 
      dplyr::summarise(media = median(Value))
    
    
    densidade_comparativo <- ggplot2::ggplot(boxplot_data, ggplot2::aes(x=Value, color=Group)) +
      ggplot2::geom_density(alpha=0.4)+
      ggplot2::geom_vline(data=sumarizado, ggplot2::aes(xintercept=media, color=Group),
                 linetype="dashed")+
      ggplot2::xlab("Relative Concentration (C/Co)")+
      ggplot2::ylab("Density")+
      ggplot2::theme_classic()+
      ggplot2::theme(axis.title.x = ggplot2::element_text(color="#0836AA", size=14, face="bold", hjust=0.5),
            plot.title = ggplot2::element_text(color="black", size=14, face="bold.italic",hjust=0.5,vjust = 2),
            axis.title.y = ggplot2::element_text(color="#0836AA", size=14, face="bold", hjust=0.5)
      )
    
    
    peclet = ((velocidade*comprimento_coluna)/(result$par[2]))
    
    df_parcial <- cbind(round(as.numeric(area_coluna),digits = 2),
                        round(as.numeric(volume_coluna),digits = 2),
                        round(as.numeric(vazao),digits = 2),
                        round(as.numeric(result$value),digits = 4),
                        round(as.numeric(result$par[1]),digits = 4),
                        round(as.numeric(result$par[2]),digits = 4),
                        round(as.numeric(peclet),digits = 4),
                        round(as.numeric(velocidade),digits = 2)
    ) 
    colnames(df_parcial) = c("Area of the column (cm²)",
                             "Volume of the column (cm³)",
                             "Flow Rate (cm/min)",
                             "Root-Mean-Square Error",
                             "Retardation factor",
                             "Dispersion coefficient (cm²/min)",
                             "Peclet number",
                             "Velocity (cm/seg)"
    )
    
    final <- as.data.frame(cbind(VP,anterior$`C/Co`,ajustado))
    
    colnames(final) <- c("Pore_Volume (cm³)","Experimental C/Co","Fitted C/Co")
    
    final[,2] = round(final[,2],digits = 6) 
    final[,3] = round(final[,3],digits = 6) 
    
    
    final2 <- qpcR:::cbind.na(final,df_parcial)
    final2[is.na(final2)]<-""
    
    
    
    lista = list()
    lista[["BTC"]] = adjusted_plot
    lista[["boxplot_comparativo"]] = boxplot_comparativo
    lista[["densidade_comparativo"]] = densidade_comparativo
    lista[["final2"]] = final2
    lista[["area"]] =as.character(round(as.numeric(area_coluna),digits = 2))
    lista[["volume"]] =as.character(round(as.numeric(volume_coluna),digits = 2))
    lista[["vazao"]] =as.character(round(as.numeric(vazao),digits = 2))
    lista[["erro_final"]] =as.character(round(as.numeric(result$value),digits = 4))
    lista[["retardamento"]] =as.character(round(as.numeric( result$par[1]),digits = 4))
    lista[["dispersao"]] =as.character(round(as.numeric(result$par[2]),digits = 4))
    lista[["peclet"]] =as.character(round(as.numeric(peclet),digits = 4))
    
    
    return(lista)
    
  })
    output$BTC <- renderPlot({
      
      print(values()$BTC)
    })
    
     output$boxplot <- renderPlot({
      print(values()$boxplot_comparativo)
     })
    
      output$histograma <- renderPlot({
       print(values()$densidade_comparativo)
     })
     
    output$tabela <- DT::renderDT(server = FALSE, {

      DT::datatable(values()$final2,
                    filter = 'top',
                    extensions = 'Buttons',
                    options = list(
                      scrollX = T,
                      sScrollY = '75vh',
                      autoWidth = TRUE,
                      scrollCollapse = TRUE,
                      paging = F,
                      searching = TRUE,
                      fixedColumns = TRUE,
                      autoWidth = TRUE,
                      ordering = TRUE,
                      dom = 'tB',
                      buttons = list("copy","print",list(
                        extend = "collection",
                        buttons = list(
                          list(extend = 'csv', filename = paste0("Final_table","-",Sys.Date())),
                          list(extend = 'excel', filename = paste0("Final_table","-",Sys.Date())),
                          list(extend = 'pdf', filename = paste0("Final_table","-",Sys.Date()))),
                        text = 'Download'
                      ))
                    ),

                    class = "display"
      ) %>%
        DT::formatRound(c(1:11),digits=4)
    })


    output$tabela_final <- downloadHandler(
      filename = paste0("Final_table","-",Sys.Date(),".xlsx"),
      content = function(file) {
        openxlsx::write.xlsx(values()$final2,file,row.names = F,sep = ",")
      }
    )

    output$grafico_final <- downloadHandler(
      filename = paste0("BTC_Plot","-",Sys.Date(),".png"),
      content = function(file) {
        ggplot2::ggsave(filename = file,plot = values()$BTC, device = "png",
                        units = "in",
                        dpi = 400,
                        width=12,
                        height=7)
      }
    )

    output$area <- renderText({
      print(values()$area)
    })

    output$volume <- renderText({
      print(values()$volume)
    })

    output$vazao <- renderText({
      print(values()$vazao)
    })

    output$erro_final <- renderText({
      print(values()$erro_final)
    })

    output$retardamento <- renderText({
      print(values()$retardamento)
    })

    output$dispersao <- renderText({
      print(values()$dispersao)
    })

    output$peclet <- renderText({
      print(values()$peclet)
    })
  })#fim do processar

  
  
  
}


