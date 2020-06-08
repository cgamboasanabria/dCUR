var_exp <- function(data, standardize=FALSE,...){
  #Selección de variables

  data <- dplyr::select(data,...)

  if(standardize){
    data <- scale(data)
  }else({})

  #Descomposición
  decomposition <- svd(data)
  sigma <- t(decomposition$u)%*%as.matrix(data)%*%decomposition$v

  #Variancia explicada
  var_expl <- round(cumsum(diag(sigma)/sum(diag(sigma)))*100, 2)
  data.frame(component=paste("PCA", 1:length(var_expl), sep=""), Variance=paste(sprintf("%.3f", var_expl), "%", sep=""))
}
