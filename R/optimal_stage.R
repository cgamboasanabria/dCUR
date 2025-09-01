#' optimal_stage
#'
#' Select the optimal stage of dynamic CUR descomposition
#'
#' @description
#' \code{optimal_stage} is a function used to select the optimal k, the number of columns and rows of dynamic CUR object; it also produces a data frame and corresponding plots.
#'
#' @param data An object resulting from a call to dCUR.
#' @param limit Cumulative percentage average of relative error rate.
#'
#' @details
#'
#' The objective of CUR decomposition is to find the most relevant variables and observations within a data matrix to reduce the dimensionality. It is well known that as more columns (variables) and rows are selected, the relative error will decrease; however, this is not true for k (number of components to compute leverages). Given the above, this function seeks to find the best-balanced stage of k, the number of relevant columns, and rows that have an error very close to the minimum, but at the same time maintain the low-rank fit of the data matrix.
#'
#' @return
#'
#' \item{data}{a data frame which specifies the relative error for each stage of CUR decomposition.}
#' \item{rows_plot}{a plot where the average relative error is shown for each number of relevant rows selected.}
#' \item{columns_plot}{a plot where the average relative error is shown for each number of relevant columns selected.}
#' \item{k_plot}{a plot where the average relative error is shown for each k (number of components to compute leverage), given the optimal number of relevant columns and rows.}
#' \item{optimal}{a data frame where the average relative error is shown for optimal k (number of components to compute leverage), given the optimal number of relevant columns and rows.}
#'
#' @examples
#' \donttest{
#' results <- dCUR(data=AASP, variables=hoessem:notabachillerato,
#' k=15, rows=0.25, columns=0.25,skip = 0.1, standardize=TRUE,
#' cur_method="sample_cur",
#' parallelize =TRUE, dynamic_columns  = TRUE,
#' dynamic_rows  = TRUE)
#' result <- optimal_stage(results, limit = 80)
#' result
#' result$k_plot
#' result$columns_plot
#' result$data
#' result$optimal
#' }
#'
#' @author
#'
#' Cesar Gamboa-Sanabria, Stefany Matarrita-Munoz, Katherine Barquero-Mejias, Greibin Villegas-Barahona, Mercedes Sanchez-Barba and Maria Purificacion Galindo-Villardon.
#'
#' @seealso
#'
#' \code{\link[dCUR]{dCUR}}
#' \code{\link[dCUR]{CUR}}
#'
#' @references
#'
#' \insertRef{dynamyCUR}{dCUR}
#'
#' @export
optimal_stage <- function(data, limit=80){

  data <- lapply(data, function(x){
    data.frame(k=x$k, columns=x$columns, rows=x$rows, error=x$relative_error)
  }) %>%
    do.call(rbind, .) %>%
    mutate(stage=row.names(.)) %>%
    arrange(error, desc(k), desc(columns), desc(rows))

  if(length(unique(data$columns))>=2){
    best_columns <- data %>%
      group_by(columns) %>%
      summarise(error=mean(error)) %>%
      mutate(rate=lag(error),
             rate2=rate-error,
             rate2=rate2/rate) %>%
      filter(!is.na(rate2)) %>%
      mutate(rate2=rate2/sum(rate2)*100,
             rate2=cumsum(rate2)) %>%
      filter(rate2>=limit)

    best_columns <- best_columns[1,1]

    columns_plot <- data %>%
      group_by(columns) %>%
      summarise(error=mean(error)) %>%
      ggplot(., aes(x=columns, y=error))+
      geom_line(color="orange", size=0.8)+
      theme_bw()+
      scale_x_continuous(expand = c(0,0), limits=c(min(data$columns), max(data$columns)*1.10))+
      scale_y_continuous(expand = c(0,0))+
      geom_vline(xintercept = best_columns$columns, color="red")+
      geom_text(mapping = aes(label = paste("columns=", best_columns$columns),
                              x = best_columns$columns, y=median(error)), angle = 60, hjust = 0)+
      labs(y="Average relative error", x="Number of columns", title="Average relative error by number of columns")

  }else({
    best_columns <- data.frame(columns=unique(data$columns))
    columns_plot <- NULL
  })


  if(length(unique(data$rows))>=2){
    best_rows <- data %>%
      group_by(rows) %>%
      summarise(error=mean(error)) %>%
      mutate(rate=lag(error),
             rate2=rate-error,
             rate2=rate2/rate) %>%
      filter(!is.na(rate2)) %>%
      mutate(rate2=rate2/sum(rate2)*100,
             rate2=cumsum(rate2)) %>%
      filter(rate2>=limit)

    best_rows <- best_rows[1,1]

    rows_plot <- data %>%
      group_by(rows) %>%
      summarise(error=mean(error)) %>%
      ggplot(., aes(x=rows, y=error))+
      geom_line(color="orange", size=0.8)+
      theme_bw()+
      scale_x_continuous(expand = c(0,0), limits=c(min(data$rows), max(data$rows)*1.10))+
      scale_y_continuous(expand = c(0,0))+
      geom_vline(xintercept = best_rows$rows, color="red")+
      geom_text(mapping = aes(label = paste("rows=", best_rows$rows),
                              x = best_rows$rows, y=median(error)), angle = 60, hjust = 0)+
      labs(y="Average relative error", x="Number of rows", title="Average relative error by number of rows")

  }else({
    best_rows <- data.frame(rows=unique(data$rows))

    rows_plot <- NULL
  })


  best_k <- data %>%
    filter(columns==best_columns$columns & rows==best_rows$rows) %>%
    group_by(k) %>%
    summarise(error=mean(error)) %>%
    arrange(error)

  best_k <- best_k[1,1]

  k_plot <- data %>%
    filter(columns==best_columns$columns & rows==best_rows$rows) %>%
    group_by(k) %>%
    summarise(error=mean(error)) %>%
    ggplot(., aes(x=k, y=error))+
    geom_line(color="orange", size=0.8)+
    theme_bw()+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    geom_vline(xintercept = best_k$k, color="red")+
    geom_text(mapping = aes(label = paste("k=", best_k$k),
                            x = best_k$k, y=median(error)), angle = 60, hjust = 0)+
    labs(y="Average relative error", x="k",
         title=paste("Average relative error with", best_columns$columns, "columns and ", best_rows$rows, "rows by k", sep = " "))

  data <- arrange(data, error)
  error <- filter(data, columns==best_columns$columns & rows==best_rows$rows & k==best_k$k)[1,1]

  optimal <- data.frame(k=best_k$k,
                        columns=best_columns$columns,
                        rows=best_rows$rows,
                        error=error)

  list(data=data, columns_plot=columns_plot, rows_plot=rows_plot, k_plot=k_plot,
       optimal=optimal)
}
