#' Numerically fit the Breakthrough Curves - Time Methodology
#'
#' @description  Performs the numerical adjustment of the relative concentration of solutes through the time-based methodology - check inputs; and returns a dataframe with the fitted values and transport parameters.
#'
#' @param  time - Time elapsed between each experimental observation - LIST.
#' @param  CCo - Relative concentration between each experimental observation - LIST.
#' @param  flask_volume - Volume of the solute collection flask (cm³) - Numerical Input.
#' @param  column_length - total length of soil column (cm) - Numerical Input.
#' @param  column_diameter - total diameter of soil column (cm) - Numerical Input.
#' @param  soil_density - Soil density for the soil under analysis (g. cm-3) - Numerical Input.
#' @param  particle_density - Particle density for the soil under analysis (g. cm-3) - Numerical Input. 
#' 
#' @return Returns a table with the experimental information, numerical fit of the relative concentration, transport parameters, and RMSE.
#' @export
#'
#'@examples MRE <- xlsx::read.xlsx("./inst/app/www/MRE.xlsx",sheetIndex = 1) %>% tidyr::as_tibble()
#'@examples fitted_MRE <- numerical_fit_TIME(MRE$Time, MRE$CCo, 23, 20, 5.2, 1.46, 2.57)
#'

numerical_fit_TIME =    function(time,
                                 CCo,
                                 flask_volume,
                                 column_length,
                                 column_diameter,
                                 soil_density,
                                 particle_density){
  
  modelo = function(param,data,velocidade,column_length){
    CECO_erf = vector(mode = "list",length = length(data))
    for (i in seq_along(data[[1]])){
      CECO_erf[i] <- if(param[1] - data[[1]][i] > 0){
        (0.5)*(1-pracma::erf((((data[[2]][1]*data[[3]][1])/param[2])/(4*param[1]*data[[1]][i]))^(0.5)*(param[1]-data[[1]][i])))}
      else{
        (0.5)*(1+pracma::erf((((data[[2]][1]*data[[3]][1])/param[2])/(4*param[1]*data[[1]][i]))^(0.5)*-1*(param[1]-data[[1]][i])))
      }}
    
    CECO_erf = unlist(CECO_erf)
    return(CECO_erf)
  }
  
  otimizacao <- function(param,data){
    CECO_erf = vector(mode = "list",length = length(data))
    for (i in seq_along(data[[1]])){
      CECO_erf[i] <- if(param[1] - data[[1]][i] > 0){
        (0.5)*(1-pracma::erf((((data[[2]][1]*data[[3]][1])/param[2])/(4*param[1]*data[[1]][i]))^(0.5)*(param[1]-data[[1]][i])))}
      else{
        (0.5)*(1+pracma::erf((((data[[2]][1]*data[[3]][1])/param[2])/(4*param[1]*data[[1]][i]))^(0.5)*-1*(param[1]-data[[1]][i])))
      }}
    
    CECO_erf = unlist(CECO_erf)
    
    Erro = sum((data[[4]] - CECO_erf)^2)
    return(Erro)
  }
  
  time <- as.data.frame(time) %>% 
    dplyr::mutate(
      Time = lubridate::as_datetime(time),
      min = lubridate::minute(Time),
      seg = lubridate::second(Time)/60,
      min_decimal = min+seg,
      Time = cumsum(min_decimal)
    ) %>% 
    dplyr::select(Time)
  time <-  time$Time
  
  raio_coluna = column_diameter/2
  area_coluna = (pi*(column_diameter^2))/4
  
  volume_coluna = area_coluna*column_length #cm3
  
  #importando dados da metodologia - tempo
  soil_density = as.numeric(soil_density)
  particle_density = as.numeric(particle_density)
  porosidade = 1 - (soil_density/particle_density)
  
  volume_poroso = volume_coluna*porosidade
  volume_de_poros = flask_volume/volume_poroso
  PV = seq(from = volume_de_poros,to = length(time)*volume_de_poros,by=volume_de_poros)
  
  vazao = vector(mode = "list", length = length(time))
  
  for (i in 1:length(time)) {
    vazao[[i]] = flask_volume/((time[i]-time[i-1]))#cm/min
  }
  
  vazao = mean(unlist(vazao))
  
  densidade_fluxo = (vazao/area_coluna)/60
  
  velocidade = (densidade_fluxo/porosidade)*60
  
  #criando dados que serão passados a função
  data = list()
  data[[1]] = PV;data[[2]] = velocidade;data[[3]] = column_length;data[[4]] = CCo
  anterior <- as.data.frame(cbind(PV,CCo))
  colnames(anterior) <- c("PV","C/Co")
  
  # call solver (with initial value c(0, 1) and default method = "Nelder-Mead")
  result <- stats::optim(par = c(1,1), 
                         otimizacao,
                         data = data,
                         #method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN","Brent"),
                         #control = list()
  )
  
  
  
  peclet = ((velocidade*column_length)/(result$par[2]))
  
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
                           "Dispersion coefficient",
                           "Peclet number",
                           "Velocity (cm/seg)"
  )
  
  Fitted <- modelo(result$par,data)
  
  final <- as.data.frame(cbind(PV,Fitted))
  
  final2 <- qpcR:::cbind.na(final,df_parcial)
  final2[is.na(final2)]<-""
  
  final2 <- tidyr::as_tibble(final2)
  
  
  return(final2)
}


