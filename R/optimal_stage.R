optimal_stage <- function(data){

  data <- lapply(data, function(x){
    data.frame(k=x$k, columns=x$columns, rows=x$rows, error=x$relative_error)
  }) %>%
    do.call(rbind, .) %>%
    mutate(stage=row.names(.)) %>%
    arrange(error, desc(k), desc(columns), desc(rows))
  #Columns
  best_columns <- data %>%
    group_by(columns) %>%
    summarise(error=mean(error)) %>%
    arrange(error) %>%
    mutate(error2=lag(error),
           range=error-error2) %>%
    arrange(desc(range)) %>%
    mutate(range=lag(range))  %>%
    dplyr::select(-error2) %>%
    filter_all(complete.cases) %>%
    arrange(desc(error))
  best_columns <- best_columns[1,1]

  best_columns_plot <- data %>%
    group_by(columns) %>%
    summarise(error=mean(error)) %>%
    ggplot(., aes(x=columns, y=error))+
    geom_line(color="orange", size=0.8)+
    theme_bw()+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    geom_vline(xintercept = best_columns$columns, color="red", show.legend = "hola")+
    geom_text(mapping = aes(label = paste("columns=", best_columns$columns),
                            x = best_columns$columns, y=median(error)), angle = 60, hjust = 0)
  #Rows
  best_rows <- data %>%
    group_by(rows) %>%
    summarise(error=mean(error)) %>%
    arrange(error) %>%
    mutate(error2=lag(error),
           range=error-error2) %>%
    arrange(desc(range)) %>%
    mutate(range=lag(range))  %>%
    dplyr::select(-error2) %>%
    filter_all(complete.cases) %>%
    arrange(desc(error))
  best_rows <- best_rows[1,1]

  best_rows_plot <- data %>%
    group_by(rows) %>%
    summarise(error=mean(error)) %>%
    ggplot(., aes(x=rows, y=error))+
    geom_line(color="orange", size=0.8)+
    theme_bw()+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    geom_vline(xintercept = best_rows$rows, color="red")+
    geom_text(mapping = aes(label = paste("rows=", best_rows$rows),
                            x = best_rows$rows, y=median(error)), angle = 60, hjust = 0)

  #k

  best_k <- data %>%
    filter(columns==best_columns$columns & rows==best_rows$rows) %>%
    group_by(k) %>%
    summarise(error=mean(error)) %>%
    arrange(error)

  best_k <- best_k[1,1]

  best_k_plot <- data %>%
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
                            x = best_k$k, y=median(error)), angle = 60, hjust = 0)


  optimal <- data %>%
    filter(k==best_k$k & columns==best_columns$columns & rows==best_rows$rows)

  list(data=data, optimal=optimal, best_columns_plot=best_columns_plot, best_rows_plot=best_rows_plot, best_k_plot=best_k_plot)
}
