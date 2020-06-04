var_exp <- function(data, estandarizar=FALSE,...){
  #Selección de variables

  data <- dplyr::select(data,...)

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
