CUR <- function(data, variables, k=NULL, k_porc=NULL, filas, columnas, estandarizar=FALSE, metodo="muestral"){
  #Selección de variables

  vars <- as.list(match.call())[-1][-1][[1]]
  vars <- sapply(vars, function(x){
    if(length(x)>1){
      paste(paste(x), collapse = "")
    }else(paste(x))
  })[c(2,1,3)] %>%
    paste(collapse = "")
  data <- eval(parse(text = paste("dplyr::select(data,", vars, ")")))
  nombres <- names(data)

  if(estandarizar){
    data <- scale(data)
  }

  #Descomposición
  descomposicion <- svd(data)
  sigma <- t(descomposicion$u)%*%as.matrix(data)%*%descomposicion$v
  A_hat <- descomposicion$u%*%sigma%*%t(descomposicion$v) %>% as.data.frame()
  names(A_hat) <- nombres

  #Variancia explicada
  var_expl <- cumsum(diag(sigma)/sum(diag(sigma)))*100


  #Puntaje leverage

  ##Si solo se especifica el porcentaje para k
  if(is.null(k) & !is.null(k_porc)){
    k <- min(which((var_expl>=k_porc) == TRUE))
  }
  ##Si no se especifica k ni su porcentaje se toma el 80%
  if(is.null(k) & is.null(k_porc)){
    k <- min(which((var_expl>=80) == TRUE))
  }
  ## Si solo se define un valor entero de k
  if(!is.null(k) & is.null(k_porc)){
    k <- k
  }

  ###Leverage columnas

  if(k==1 & is.null(k_porc)){
    leverage_columnas <- descomposicion$v[, 1]^2 %>% matrix(.,nrow(descomposicion$v),1)
  }else({
    leverage_columnas <- descomposicion$v[, 1:k]^2
  })

  leverage_columnas_orden <- rowSums(leverage_columnas)/k*1000

  leverage_columnas_orden <- data.frame(leverage_columnas=leverage_columnas_orden,
                                        nombres=nombres) %>%
    arrange(desc(leverage_columnas))

  ###Leverage filas

  if(k==1 & is.null(k_porc)){
    leverage_filas <- descomposicion$u[, 1]^2%>% matrix(.,nrow(descomposicion$u),1)
  }else({
    leverage_filas <- descomposicion$u[, 1:k]^2 #No puse la ponderación
  })


  leverage_filas_orden <- rowSums(leverage_filas)/k*1000

  leverage_filas_orden <- data.frame(leverage_filas=leverage_filas_orden,
                                     nombres=1:length(leverage_filas_orden)) %>%
    arrange(desc(leverage_filas))

  ####Paso de seleccion ####
  if(metodo=="muestral"){
    columnas <- ceiling(columnas*nrow(leverage_columnas_orden))
    filas <- ceiling(filas*nrow(leverage_filas_orden))

    leverage_columnas_orden <- leverage_columnas_orden[1:columnas,]
    index_col <- leverage_columnas_orden$nombres
    leverage_filas_orden <- leverage_filas_orden[1:filas, ]
    index_fil <- leverage_filas_orden$nombres

    densidad_columnas <- NULL
    densidad_filas <- NULL
  }

  if(metodo=="mixturas"){
    #Para columnas
    densidad_columnas <- densityMclust(leverage_columnas_orden$leverage_columnas)
    valor_critico_columnas <- quantileMclust(densidad_columnas, p = c(1-columnas))
    leverage_columnas_orden <- filter(leverage_columnas_orden, leverage_columnas>=valor_critico_columnas)
    index_col <- leverage_columnas_orden$nombres

    ##Para filas
    densidad_filas <- densityMclust(leverage_filas_orden$leverage_filas)
    valor_critico_filas <- quantileMclust(densidad_filas, p = c(1-filas))
    leverage_filas_orden <- filter(leverage_filas_orden, leverage_filas>=valor_critico_filas)
    index_fil <- leverage_filas_orden$nombres
  }

  #Cálculo de CUR

  C_cur <- data[,index_col] %>% as.matrix
  R_cur <- data[index_fil, ] %>% as.matrix
  U_cur <- ginv(C_cur)%*%as.matrix(data)%*%ginv(R_cur)
  CUR <- C_cur%*%U_cur%*%R_cur

  error_abs <- norm(as.matrix(data)-CUR, type="F")
  error_rel <- error_abs/norm(as.matrix(data), type="F")

  list(#U=descomposicion$u,
    #D=descomposicion$d,
    #V=descomposicion$v,
    #sigma=sigma,
    varianza_explicada=var_expl,
    #leverage_columnas=leverage_columnas,
    #leverage_filas=leverage_filas,
    #A_hat=A_hat,
    #C_cur=C_cur,
    #R_cur=R_cur,
    #U_cur=U_cur,
    CUR=CUR,
    error_absoluto=error_abs,
    error_relativo=error_rel,
    leverage_columnas_orden=leverage_columnas_orden,
    leverage_filas_orden=leverage_filas_orden#,
    #densidad_columnas=densidad_columnas,
    #densidad_filas=densidad_filas
  )
}
