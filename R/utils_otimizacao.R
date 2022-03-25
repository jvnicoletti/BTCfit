#' otimizacao 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#' 
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