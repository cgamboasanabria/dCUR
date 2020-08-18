#' dCUR
#'
#' @description
#' Dynamic CUR is a function that boosts the CUR decomposition varying the k, number of columns, and rows used. Its ultimate purpose is to find the stage which minimizes the relative error. The classic CUR and its extensions can be used in \code{dCUR}.
#'
#' @param data a data frame that contains the variables to use in CUR decomposition and other externals variables with which you want to correlate.
#' @param variables correspond to the variables used to compute the leverage scores in CUR analysis. The external variable’s names must not be included. dplyr package notation can be used to specify the variables (see examples).
#' @param standardize logical. If \code{TRUE} the data is standardized (by subtracting the average and dividing by the standard deviation)
#' @param dynamic_columns logical. If \code{TRUE}, an iterative process begins where leverage scores are computed for the different values from 1 to k main components, as well as from 1 to c (the proportion of columns to be selected from the data matrix).
#' @param dynamic_rows logical. If \code{TRUE}, an iterative process begins where leverage scores are computed for the different values from 1 to k main components, as well as from 1 to r (the proportion of rows to be selected from the data matrix).
#' @param skip numeric. It specifies the change ratio of columns and rows to be selected.
#' @param parallelize logical.If\code{TRUE} the CUR analysis is parallelized.
#' @param ... additional arguments to be passed to \code{\link{CUR}}.
#'
#' @details
#'
#' This function serves as a basis for selecting the best combination of k (principal components), c (number of columns) and r (number of rows), in other words, the stage that minimizes the relative error \eqn{\frac{||A-CUR||}{||A||}}, and thus optimizes the number of columns in the analysis, ensuring a percentage of explained variability of the data matrix and facilitating the interpretation of the data set by reducing the dimensionality of the original matrix.
#'
#' If \code{skip = 0.1} for each k, it is tested with a column proportion of 0, 0.1, 0.11,0.22,...; the same applies for rows. Given the above, it is recommended not to choose a tiny skip, since this implies doing the CUR analysis for more stages.
#'
#' Parallelizing the function improves its speed significantly.
#' @return
#'
#' CUR returns a list of lists, each one represents a stage, and it contains:
#'
#' \item{k}{Number of principal components with which leverages scores are computed.}
#' \item{columns}{number of columns selected.}
#' \item{rows}{number of rows selected.}
#' \item{relative_error}{relative_error obtained: \eqn{\frac{||A-CUR||}{||A||}}}
#'
#' @examples
#'
#'  results <- dCUR::dCUR(data=AASP, variables=hoessem:notabachillerato,
#'  k=15, rows=0.25, columns=0.25,skip = 0.1, standardize=TRUE,
#'  cur_method="sample_cur",
#'  parallelize =TRUE, dynamic_columns  = TRUE,
#'  dynamic_rows  = TRUE)
#'  results
#'
#'
#' @author
#'
#' Cesar Gamboa Sanabria, Stefany Matarrita-Munoz, Katherine Barquero-Mejias, Greibin Villegas-Barahona, Mercedes Sanchez-Barba and Maria Purificacion Galindo-Villardon.
#'
#' @seealso
#'
#' \code{\link{CUR}}
#' \code{\link{optimal_stage}}
#'
#' @export
dCUR <- function(data, variables, standardize=FALSE,
                 dynamic_columns=FALSE, dynamic_rows=FALSE, parallelize=FALSE,
                 skip=0.05,...){

  fun_args <- stackoverflow::match.call.defaults(expand.dots = TRUE)[-1] %>% as.list

  test_fun <- sapply(fun_args[c("variables", "correlation")], as.expression) %>% paste()
  correlation <- eval(parse(text = paste("dplyr::select(data,", test_fun[2], ")")))
  data <- eval(parse(text = paste("dplyr::select(data,", test_fun[1], ")")))
  var_names <- names(data)
  cor_name <- names(correlation)

  if(standardize){
    data <- scale(data)
  }


  decomposition <- svd(data)
  sigma <- t(decomposition$u)%*%as.matrix(data)%*%decomposition$v
  A_hat <- decomposition$u%*%sigma%*%t(decomposition$v) %>% as.data.frame()
  names(A_hat) <- var_names


  var_expl <- cumsum(diag(sigma)/sum(diag(sigma)))*100


  k <- if(!("k"%in%names(fun_args))){
    ncol(data)-1
  }else(fun_args$k)



  k <- if(is.null(k)){
    min(which((var_expl>=80) == TRUE))
  }else{
    if(k%%1!=0){
      min(which((var_expl>=k*100) == TRUE))
    }else(k)
  }


  stages <-if(fun_args$cur_method=="sample_cur"){
    columns <- fun_args$columns
    rows <- fun_args$rows

    if(dynamic_columns){
      if(dynamic_rows){
        expand.grid(k=1:k,
                    columns=unique(c(seq(.01,columns,skip), columns)),
                    rows=unique(c(seq(.01,rows,skip), rows))) %>%
          arrange(k, columns, rows)
      }else({
        expand.grid(k=1:k,
                    columns=unique(c(seq(.01,columns,skip), columns)),
                    rows=rows) %>%
          arrange(k, columns, rows)
      })
    }else({
      if(dynamic_rows){
        expand.grid(k=1:k,
                    columns=columns,
                    rows=unique(c(seq(.01,rows,skip), rows))) %>%
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

  stages <- if(fun_args$cur_method=="sample_cur"){
    stages %>%
      mutate(columns=ceiling(ncol(data)*columns),
             rows=ceiling(nrow(data)*rows))
  }else{stages}


  CUR_d2 <- function(data,k=NULL, rows, columns, cur_method, correlation=NULL,correlation_type,decomposition,cor_name,var_names,variance,...){




    if(k==1){
      leverage_columns <- decomposition$v[, 1]^2 %>% matrix(.,nrow(decomposition$v),1)
    }else({
      leverage_columns <- decomposition$v[, 1:k]^2
    })



    if(ncol(correlation)>0){
      correlation <- cbind(data, correlation)
      position <- which(names(correlation)==cor_name)

      if(correlation_type=="partial"){
        correlation <- pcor(correlation,...)$estimate[,position][-position]
        leverage_columns <- ((rowSums(leverage_columns)/k)/(1-correlation^2))*1000
      }
      if(correlation_type=="semipartial"){
        correlation <- spcor(correlation,...)$estimate[,position][-position]
        leverage_columns <- ((rowSums(leverage_columns)/k)/(1-correlation^2))*1000
      }
    }else({leverage_columns <- rowSums(leverage_columns)/k*1000})

    leverage_columns_sorted <- data.frame(leverage_columns=leverage_columns,
                                          var_names=var_names) %>%
      arrange(desc(leverage_columns))



    if(k==1){
      leverage_rows <- decomposition$u[, 1]^2%>% matrix(.,nrow(decomposition$u),1)
    }else({
      leverage_rows <- decomposition$u[, 1:k]^2
    })


    leverage_rows <- rowSums(leverage_rows)/k*1000

    leverage_rows_sorted <- data.frame(leverage_rows=leverage_rows,
                                       var_names=1:length(leverage_rows)) %>%
      arrange(desc(leverage_rows))


    if(cur_method=="sample_cur"){



      leverage_columns_sorted <- leverage_columns_sorted[1:columns,]
      index_col <- leverage_columns_sorted$var_names
      leverage_rows_sorted <- leverage_rows_sorted[1:rows, ]
      index_row <- leverage_rows_sorted$var_names

      density_columns <- NULL
      density_rows <- NULL
    }

    if(cur_method=="mixture"){

      columns <- ifelse(columns==1, .99999999, columns)
      density_columns <- densityMclust(leverage_columns_sorted$leverage_columns)
      critical_value_columns <- quantileMclust(density_columns, p = c(1-columns))
      leverage_columns_sorted <- filter(leverage_columns_sorted, leverage_columns>=critical_value_columns)
      index_col <- leverage_columns_sorted$var_names


      rows <- ifelse(rows==1, .99999999, rows)
      density_rows <- densityMclust(leverage_rows_sorted$leverage_rows)
      critical_value_rows <- quantileMclust(density_rows, p = c(1-rows))
      leverage_rows_sorted <- filter(leverage_rows_sorted, leverage_rows>=critical_value_rows)
      index_row <- leverage_rows_sorted$var_names
    }

    C_cur <- data[,as.character(index_col)] %>% as.matrix
    R_cur <- data[index_row, ] %>% as.matrix
    U_cur <- ginv(C_cur)%*%as.matrix(data)%*%ginv(R_cur)
    CUR <- C_cur%*%U_cur%*%R_cur

    error_abs <- norm(as.matrix(data)-CUR, type="F")
    error_rel <- error_abs/norm(as.matrix(data), type="F")

    lista <- list(k=k,
                  columns=columns,
                  rows=rows,
                  relative_error=error_rel)

    lista <- structure(lista, class=c("list", "dCUR"))
    lista
  }

  if(!parallelize){

    result <- mapply(CUR_d2,
                       k=stages$k,
                       rows=stages$rows,
                       columns=stages$columns,
                       MoreArgs = list(cur_method=fun_args$cur_method,
                                       correlation=correlation,
                                       correlation_type=fun_args$correlation_type,
                                       decomposition=decomposition,
                                       data=data,
                                       cor_name=cor_name,
                                       var_names=var_names,
                                       variance=var_expl),
                       SIMPLIFY = FALSE)
    names(result) <- paste("k", stages$k,
                             "columns", round(stages$columns/ncol(data), 2),
                             "rows", round(stages$rows/nrow(data), 2), sep="_")
    result <- structure(result, class=c("list", "dCUR"))
    result
  }else({

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

    result <- clusterMap(cl=clp, fun = CUR_d2,
                           k=stages$k,
                           rows=stages$rows,
                           columns=stages$columns,
                           MoreArgs = list(cur_method=fun_args$cur_method,
                                           correlation=correlation,
                                           correlation_type=fun_args$correlation_type,
                                           decomposition=decomposition,
                                           data=data,
                                           cor_name=cor_name,
                                           var_names=var_names,
                                           variance=var_expl),
                           SIMPLIFY = FALSE, .scheduling = "dynamic")
    stopCluster(clp)
    names(result) <- paste("k", stages$k,
                           "columns", round(stages$columns/ncol(data), 2),
                           "rows", round(stages$rows/nrow(data), 2), sep="_")
    result <- structure(result, class=c("list", "dCUR"))
    result
  })


}


