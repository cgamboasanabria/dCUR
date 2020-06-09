dCUR <- function(data, variables, standardize=FALSE, dynamic_columns=FALSE, dynamic_rows=FALSE, parallelize=FALSE,...){
  #######Esta parte es igual al inicio de la función CUR, pero se pone aquí para ahorrar tiempo ejecutándolo una sola vez, la función CUR_d2 es similar a CUR solo que sin la parte de selección de variables.
  #Selección de variables

  require(data.table)
  require(corpcor)
  require(MASS)
  require(mclust)
  require(ppcor)
  require(stackoverflow)
  require(dplyr)

  fun_args <- stackoverflow::match.call.defaults(expand.dots = TRUE)[-1] %>% as.list

  test_fun <- sapply(fun_args[c("variables", "correlation")], as.expression) %>% paste()
  correlation <- eval(parse(text = paste("dplyr::select(data,", test_fun[2], ")")))
  data <- eval(parse(text = paste("dplyr::select(data,", test_fun[1], ")")))
  var_names <- names(data)
  cor_name <- names(correlation)

  if(standardize){
    data <- scale(data)
  }

  #Descomposición
  decomposition <- svd(data)
  sigma <- t(decomposition$u)%*%as.matrix(data)%*%decomposition$v
  A_hat <- decomposition$u%*%sigma%*%t(decomposition$v) %>% as.data.frame()
  names(A_hat) <- var_names

  #variance explicada
  var_expl <- cumsum(diag(sigma)/sum(diag(sigma)))*100

  #Se define el valor de k para hacerlo dinámico
  k <- if(!("k"%in%names(fun_args))){
    ncol(data)-1
  }else(fun_args$k)

  #Puntaje leverage

  k <- if(is.null(k)){
    min(which((var_expl>=80) == TRUE))
  }else{
    if(k%%1!=0){
      min(which((var_expl>=k*100) == TRUE))
    }else(k)
  }

  #######################################Proceso dinámico######################################
  #Se definen los stages

  stages <-if(fun_args$cur_method=="sample_cur"){
    columns <- ncol(data)*fun_args$columns
    rows <- nrow(data)*fun_args$rows

    if(dynamic_columns){
      if(dynamic_rows){
        expand.grid(k=1:k, columns=1:columns, rows=1:rows) %>%
          arrange(k, columns, rows)
      }else({
        expand.grid(k=1:k, columns=1:columns, rows=rows) %>%
          arrange(k, columns, rows)
      })
    }else({
      if(dynamic_rows){
        expand.grid(k=1:k, columns=columns, rows=1:rows) %>%
          arrange(k, columns, rows)
      }else({
        expand.grid(k=1:k, columns=columns, rows=rows) %>%
          arrange(k, columns, rows)
      })
    })
  }else({
    columns <- fun_args$columns
    rows <- fun_args$rows
    expand.grid(k=1:k, rows=rows, columns=columns) %>%
      arrange(k, columns, rows)
  })


  if(!parallelize){
    ##SECUENCIAL###
    result <- mapply(CUR_d2,
                       k=stages$k,
                       rows=stages$rows,
                       columns=stages$columns,
                       MoreArgs = list(cur_method=fun_args$cur_method,
                                       correlation=correlation,
                                       correlation_type=fun_args$correlation_type,
                                       decomposition=decomposition,
                                       sigma=sigma,
                                       A_hat=A_hat,
                                       data=data,
                                       cor_name=cor_name,
                                       var_names=var_names,
                                       variance=var_expl),
                       SIMPLIFY = FALSE)
    names(result) <- paste("k", stages$k,
                             "columns", fun_args$columns,
                             "rows", fun_args$rows, sep="_")
    result
  }else({
    require(parallel)
    clp <- makeCluster(detectCores(logical = FALSE), type = "SOCK", useXDR=FALSE)
    clusterEvalQ(clp, {
      require(data.table)
      require(corpcor)
      require(MASS)
      require(mclust)
      require(ppcor)
      require(stackoverflow)
      require(dplyr)
    })
    #parallelize
    result <- clusterMap(cl=clp, fun = CUR_d2,
                           k=stages$k,
                           rows=stages$rows,
                           columns=stages$columns,
                           MoreArgs = list(cur_method=fun_args$cur_method,
                                           correlation=correlation,
                                           correlation_type=fun_args$correlation_type,
                                           decomposition=decomposition,
                                           sigma=sigma,
                                           A_hat=A_hat,
                                           data=data,
                                           cor_name=cor_name,
                                           var_names=var_names,
                                           variance=var_expl),
                           SIMPLIFY = FALSE, .scheduling = "dynamic")
    stopCluster(clp)
    names(result) <- paste("k", stages$k,
                             "columns", fun_args$columns,
                             "rows", fun_args$rows, sep="_")
    result
  })


}

CUR_d2 <- function(data,k=NULL, rows, columns, cur_method, correlation=NULL,correlation_type,decomposition,sigma,A_hat,cor_name,var_names,variance,...){

  ###Leverage columns

  if(k==1){
    leverage_columns <- decomposition$v[, 1]^2 %>% matrix(.,nrow(decomposition$v),1)
  }else({
    leverage_columns <- decomposition$v[, 1:k]^2
  })

  ####### correlationes

  if(ncol(correlation)>0){
    correlation <- cbind(data, correlation)
    position <- which(names(correlation)==cor_name)

    if(correlation_type=="partial"){
      correlation <- pcor(correlation,...)$estimate[,position][-position]
      leverage_columns_sorted <- ((rowSums(leverage_columns)/k)/(1-correlation^2))*1000
    }
    if(correlation_type=="semipartial"){
      correlation <- spcor(correlation,...)$estimate[,position][-position]
      leverage_columns_sorted <- ((rowSums(leverage_columns)/k)/(1-correlation^2))*1000
    }
  }else({leverage_columns_sorted <- rowSums(leverage_columns)/k*1000})

  leverage_columns_sorted <- data.frame(leverage_columns=leverage_columns_sorted,
                                        var_names=var_names) %>%
    arrange(desc(leverage_columns))

  ###Leverage rows

  if(k==1){
    leverage_rows <- decomposition$u[, 1]^2%>% matrix(.,nrow(decomposition$u),1)
  }else({
    leverage_rows <- decomposition$u[, 1:k]^2 #No puse la ponderación
  })


  leverage_rows_sorted <- rowSums(leverage_rows)/k*1000

  leverage_rows_sorted <- data.frame(leverage_rows=leverage_rows_sorted,
                                     var_names=1:length(leverage_rows_sorted)) %>%
    arrange(desc(leverage_rows))

  ####Paso de seleccion ####
  if(cur_method=="sample_cur"){

    #columns <- ceiling(columns*nrow(leverage_columns_sorted))
    #rows <- ceiling(rows*nrow(leverage_rows_sorted))

    leverage_columns_sorted <- leverage_columns_sorted[1:columns,]
    index_col <- leverage_columns_sorted$var_names
    leverage_rows_sorted <- leverage_rows_sorted[1:rows, ]
    index_row <- leverage_rows_sorted$var_names

    density_columns <- NULL
    density_rows <- NULL
  }

  if(cur_method=="mixturas"){
    #Para columns
    density_columns <- densityMclust(leverage_columns_sorted$leverage_columns)
    critical_value_columns <- quantileMclust(density_columns, p = c(1-columns))
    leverage_columns_sorted <- filter(leverage_columns_sorted, leverage_columns>=critical_value_columns)
    index_col <- leverage_columns_sorted$var_names

    ##Para rows
    density_rows <- densityMclust(leverage_rows_sorted$leverage_rows)
    critical_value_rows <- quantileMclust(density_rows, p = c(1-rows))
    leverage_rows_sorted <- filter(leverage_rows_sorted, leverage_rows>=critical_value_rows)
    index_row <- leverage_rows_sorted$var_names
  }

  #Cálculo de CUR

  leverage_columns_sorted

  C_cur <- data[,index_col] %>% as.matrix
  R_cur <- data[index_row, ] %>% as.matrix
  U_cur <- ginv(C_cur)%*%as.matrix(data)%*%ginv(R_cur)
  CUR <- C_cur%*%U_cur%*%R_cur

  error_abs <- norm(as.matrix(data)-CUR, type="F")
  error_rel <- error_abs/norm(as.matrix(data), type="F")

  list(U=decomposition$u,
    D=decomposition$d,
    V=decomposition$v,
    sigma=sigma,
    variance_explained=variance,
    leverage_columns=leverage_columns,
    leverage_rows=leverage_rows,
    A_hat=A_hat,
    C_cur=C_cur,
    R_cur=R_cur,
    U_cur=U_cur,
    CUR=CUR,
    absolute_error=error_abs,
    relative_error=error_rel,
    leverage_columns_sorted=leverage_columns_sorted,
    leverage_rows_sorted=leverage_rows_sorted,
    density_columns=density_columns,
    density_rows=density_rows
  )
}
