dCUR <- function(data, variables, estandarizar=FALSE, dinamico_columnas=FALSE, dinamico_filas=FALSE, paralelo=FALSE,...){
  #######Esta parte es igual al inicio de la función CUR, pero se pone aquí para ahorrar tiempo ejecutándolo una sola vez, la función CUR_d2 es similar a CUR solo que sin la parte de selección de variables.
  #Selección de variables
  argumentos <- match.call.defaults(expand.dots = TRUE)[-1] %>% as.list

  expresion <- sapply(argumentos[c("variables", "correlacion")], as.expression) %>% paste()
  correlacion <- eval(parse(text = paste("dplyr::select(data,", expresion[2], ")")))
  data <- eval(parse(text = paste("dplyr::select(data,", expresion[1], ")")))
  nombres <- names(data)
  nombre_cor <- names(correlacion)

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

  #Se define el valor de k para hacerlo dinámico
  k <- if(!("k"%in%names(argumentos))){
    ncol(data)-1
  }else(argumentos$k)

  #Puntaje leverage

  k <- if(is.null(k)){
    min(which((var_expl>=80) == TRUE))
  }else{
    if(k%%1!=0){
      min(which((var_expl>=k*100) == TRUE))
    }else(k)
  }

  #######################################Proceso dinámico######################################
  #Se definen los escenarios

  escenarios <-if(argumentos$metodo=="muestral"){
    columnas <- ncol(data)*argumentos$columnas
    filas <- nrow(data)*argumentos$filas

    if(dinamico_columnas){
      if(dinamico_filas){
        expand.grid(k=1:k, columnas=1:columnas, filas=1:filas) %>%
          arrange(k, columnas, filas)
      }else({
        expand.grid(k=1:k, columnas=1:columnas, filas=filas) %>%
          arrange(k, columnas, filas)
      })
    }else({
      if(dinamico_filas){
        expand.grid(k=1:k, columnas=columnas, filas=1:filas) %>%
          arrange(k, columnas, filas)
      }else({
        expand.grid(k=1:k, columnas=columnas, filas=filas) %>%
          arrange(k, columnas, filas)
      })
    })
  }else({
    columnas <- argumentos$columnas
    filas <- argumentos$filas
    expand.grid(k=1:k, filas=filas, columnas=columnas) %>%
      arrange(k, columnas, filas)
  })

  ##SECUENCIAL###

  mapply(CUR_d2,
         k=escenarios$k,
         filas=escenarios$filas,
         columnas=escenarios$columnas,
         MoreArgs = list(metodo=argumentos$metodo,
                         correlacion=correlacion,
                         tipo_correlacion=argumentos$tipo_correlacion,
                         descomposicion=descomposicion),
         SIMPLIFY = FALSE)

}

CUR_d2 <- function(k=NULL, filas, columnas, metodo="muestral", correlacion=NULL,tipo_correlacion=c("parcial", "semiparcial"),descomposicion,...){

  ###Leverage columnas

  if(k==1){
    leverage_columnas <- descomposicion$v[, 1]^2 %>% matrix(.,nrow(descomposicion$v),1)
  }else({
    leverage_columnas <- descomposicion$v[, 1:k]^2
  })

  ####### Correlaciones

  if(ncol(correlacion)>0){
    correlacion <- cbind(data, correlacion)
    posicion <- which(names(correlacion)==nombre_cor)

    if(tipo_correlacion=="parcial"){
      correlacion <- pcor(correlacion,...)$estimate[,posicion][-posicion]
      leverage_columnas_orden <- ((rowSums(leverage_columnas)/k)/(1-correlacion^2))*1000
    }
    if(tipo_correlacion=="semiparcial"){
      correlacion <- spcor(correlacion,...)$estimate[,posicion][-posicion]
      leverage_columnas_orden <- ((rowSums(leverage_columnas)/k)/(1-correlacion^2))*1000
    }
  }else({leverage_columnas_orden <- rowSums(leverage_columnas)/k*1000})

  leverage_columnas_orden <- data.frame(leverage_columnas=leverage_columnas_orden,
                                        nombres=nombres) %>%
    arrange(desc(leverage_columnas))

  ###Leverage filas

  if(k==1){
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
