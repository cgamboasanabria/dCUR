var_exp <- function(data,variables,estandarizar=FALSE){
  #Selección de variables

  vars <- as.list(match.call())[-1][-1]
  vars <- vars[-c(length(vars):(length(vars)-0))] #cambiar el 1 por la cantidad de argumentos después de k
  vars <- sapply(vars, function(x){
    if(length(x)>1){
      paste(paste(x)[c(2,1,3)], collapse = "")
    }else(paste(x))
  }) %>%
    paste(collapse = ", ")
  data <- eval(parse(text = paste("dplyr::select(data,", vars, ")")))
  nombres <- names(data)

  if(estandarizar){
    data <- scale(data)
  }else({})

  #Descomposición
  descomposicion <- svd(data)
  sigma <- t(descomposicion$u)%*%as.matrix(data)%*%descomposicion$v

  #Variancia explicada
  var_expl <- round(cumsum(diag(sigma)/sum(diag(sigma)))*100, 2)
  data.frame(Componente=paste("PCA", 1:length(var_expl), sep=""), Varianza=paste(sprintf("%.3f", var_expl), "%", sep=""))
}
