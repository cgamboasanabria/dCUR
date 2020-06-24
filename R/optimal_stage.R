optimal_stage <- function(data, limit=80){

  data <- lapply(data, function(x){
    data.frame(k=x$k, columns=x$columns, rows=x$rows, error=x$relative_error)
  }) %>%
    do.call(rbind, .) %>%
    mutate(stage=row.names(.)) %>%
    arrange(error, desc(k), desc(columns), desc(rows))

  best_columns <- datos %>%
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

  best_rows <- datos %>%
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

  best_k <- datos %>%
    filter(columns==best_columns$columns & rows==best_rows$rows) %>%
    group_by(k) %>%
    summarise(error=mean(error)) %>%
    arrange(error)

  best_k <- best_k[1,1]

  columns_plot <- datos %>%
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

  rows_plot <- datos %>%
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

  k_plot <- datos %>%
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

  data <- arrange(data, error)

  list(data=data, columns_plot=columns_plot, rows_plot=rows_plot, k_plot=k_plot, optimal=data.frame(best_k, best_columns, best_rows))
}
