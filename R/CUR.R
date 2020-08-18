#' CUR
#'
#' Extension of classic CUR descomposition with top scores selection criteria.
#'
#' @description
#' This function computes the canonical CUR decomposition using top scores as selection criteria to identify the most relevant columns and rows of a given data matrix. It also provides an option to use an extension of CUR decomposition, which reconfigures leverage scores with the partial and semi partial correlations with an external variable of interest. Additionally, this function lets the user fit a probability distribution of leverage scores with Mixtures Gaussian Models.
#'
#' @param data a data frame containing the variables to be used in CUR decomposition and other externals variables with which you want to correlate.
#' @param variables correspond to the variables used to compute the leverage scores in CUR analysis. The external variable’s names must not be included. dplyr package notation can be used to specify the variables (see the example).
#' @param k corresponds to the number of principal components used to compute the leverage scores. If NULL, it is considered the number of k main components that accumulate 80\% of the variability explained. This argument can also be a proportion, in which case the function takes this value as the desired cumulative explained variance and automatically chooses the k.
#' @param rows correspond to the proportion of rows to be selected from the total number of rows in the data matrix. When all the rows are needed and \code{mixture} is used as cur_method, a proportion of 0.999 must be used.
#' @param columns correspond to the proportion of columns (variables) to be selected from the total number of variables in the data matrix.
#' @param standardize If \code{TRUE} the data is standardized (by subtracting the average and dividing by the standard deviation)
#' @param cur_method character. If \code{sample_cur}, the selection of leverage scores is made according to the top score selection criteria set out by Mahoney & Drineas (2009). If \code{mixture} method is specified, the best Mixture Gaussian Model is fitted for the leverages, and the selection of the most relevant variables is based on a tabular value given the critical area specified in rows and columns arguments.
#' @param correlation character. It specifies the name of the external variable the computation of leverage must be adjusted with.
#' @param correlation_type character. It specifies if the computation of leverage must be adjusted by the \code{semipartial} or \code{partial} correlation with an external variable.
#' @param ... additional arguments to be passed to \code{\link{pcor}} or \code{\link{spcor}}
#'
#' @details
#'
#' CUR decomposition chooses columns and rows that exhibit high leverage scores and exert a disproportionately large “influence” on the best low-rank fit of the data matrix. The main advantage of CUR Decomposition over SVD is that the original data matrix can be expressed as a reduced number of rows and columns instead of obtaining factorial axes resulting from a linear combination of all the original variables to facilitate interpretation.
#'
#' The reconfiguration of the leverage scores according to the methodology of Villegas et al. (2018). Dividing the leverage score by \eqn{(1-\rho^2)}{(1-rho^2)}. Where \eqn{\rho} rho represents the partial or semi-partial correlation that the variables used in CUR decomposition have with an external variable, its purpose is recalibrating the relative importance of the leverage scores according to an external variable of interest.
#'
#' The correlation type selection could be partial or semi-partial, according to Seongho (2015) of the package in R ppcor.
#'
#' @return
#'
#' \item{k}{Number of principal components with which leverages scores are computed.}
#' \item{CUR}{CUR matrix.}
#' \item{absolute_error}{Absolute error computed as the Frobenius norm of the original data -detnoted as A- and CUR matrix: ||A-CUR||}
#' \item{relative_error)}{Relative error \eqn{\frac{||A-CUR||}{||A||}}}
#' \item{leverage_columns_sorted}{a data frame which specifies the names of relevant columns and its leverages scores arranged downwardly.}
#' \item{leverage_rows_sorted }{a data frame which specifies the number of relevant rows and its leverages scores arranged downwardly.}
#' \item{leverage_columns }{a data frame which specifies the names of all columns and its leverages scores.}
#' \item{leverage_rows }{a data frame which specifies the number of all rows and its leverages scores.}
#'
#' @examples
#'
#'  #Classic CUR with top scores selection criteria.
#'  result <- CUR(data=AASP, variables=hoessem:notabachillerato,
#'            k=20, rows = 1, columns = .2, standardize = TRUE,
#'            cur_method = "sample_cur")
#'  result

#' #Extension of classic CUR: Recalibrating leverages scores
#' #and adjusting a mixtures Gaussian models to leverages.

#'  result <- CUR(data=AASP, variables=hoessem:notabachillerato,
#'            k=20, rows = 1, columns = .2, standardize = TRUE,
#'            cur_method = "mixture",
#'            correlation = R1, correlation_type = "partial")
#'  result
#'
#'
#' @author
#'
#' Cesar Gamboa Sanabria, Stefany Matarrita-Munoz, Katherine Barquero-Mejias, Greibin Villegas-Barahona, Mercedes Sanchez-Barba and Maria Purificacion Galindo-Villardon.
#'
#' @references
#'
#' \insertRef{Mahoney697}{dCUR}
#' \insertRef{villegas2018modelo}{dCUR}
#' \insertRef{dynamyCUR}{dCUR}
#' \insertRef{relativeE}{dCUR}
#'
#' @export

CUR <- function(data, variables, k=NULL, rows, columns, standardize=FALSE, cur_method="sample_cur", correlation=NULL,correlation_type=c("partial", "semipartial"),...){

  fun_args <- stackoverflow::match.call.defaults(expand.dots = FALSE) %>% as.list

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



  k <- if(is.null(k)){
    min(which((var_expl>=80) == TRUE))
  }else{
    if(k%%1!=0){
      min(which((var_expl>=k*100) == TRUE))
    }else(k)
  }


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
    columns <- ceiling(columns*nrow(leverage_columns_sorted))
    rows <- ceiling(rows*nrow(leverage_rows_sorted))

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

  list1 <- list(U=decomposition$u,
                D=decomposition$d,
                V=decomposition$v,
                sigma=sigma,
                k=k,
                variance_explained=var_expl,
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
  list1 <- structure(list1, class=c("list", "CUR"))
  list1

}
