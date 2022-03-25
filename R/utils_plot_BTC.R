#' Plot the fitted data
#'
#' @description Plot the performed Fit of the experimental data.
#'
#' @param  PV Pore Volume measured between each experimental observation (as exported in one of the above functions).
#' @param  CCo Relative concentration between each experimental observation (as exported in one of the above functions).
#' @param  Fitted Numerical fit of the relative concentration (as exported in one of the above functions).
#' 
#' @return Returns the plot of the BTCs adjusted by BTCfit
#' @export
#'
#' @examples MRE <- xlsx::read.xlsx("./inst/app/www/MRE.xlsx",sheetIndex = 1) %>% tidyr::as_tibble()
#' @examples fitted_MRE <- numerical_fit_PV(MRE$Pore_Volume, MRE$CCo, 0.42, 20, 5.2)
#' @examples BTC <- plot_BTC(fitted_MRE$PV,MRE$CCo,fitted_MRE$Fitted)
#'

plot_BTC <- function(PV,CCo,Fitted){

cols <- c("Dados Experimentais"="#f04546","Dados Ajustados"="#3591d1")

adjusted_plot <- ggplot2::ggplot()  + 
  ggplot2::geom_point(ggplot2::aes(PV,CCo ,colour = "Dados Experimentais")) + 
  #ggplot2::geom_line(ggplot2::aes(PV,CCo ,colour = "Dados Experimentais")) + 
  #ggplot2::geom_point(ggplot2::aes(PV,Fitted ,colour = "Dados Ajustados")) + 
  ggplot2::geom_line(ggplot2::aes(PV,Fitted ,colour = "Dados Ajustados")) + 
  ggplot2::scale_colour_manual(name="",values=cols)+
  ggplot2::ggtitle(" Breakthrough Curve (BTC)") +
  ggplot2::ylab("Concentração Relativa (C/Co)") + ggplot2::xlab("Volume de Poros (PV)") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 10, vjust=-.2)) +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 10, vjust=0.3)) + 
  ggplot2::theme(plot.title = ggplot2::element_text(size = 15, vjust=0.5, hjust = 0.5))+
  ggplot2::theme(
    legend.position = c(.95, .35),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = ggplot2::margin(5, 5, 5, 5)
  )+
  ggplot2::scale_x_continuous(expand = c(0, 0))+
  ggplot2::scale_y_continuous(expand = c(0, 0),limits = c(0,1))
  

return(plotly::ggplotly(adjusted_plot))

}


