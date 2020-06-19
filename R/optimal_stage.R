optimal_stage <- function(data){

  data <- lapply(data, function(x){
    data.frame(k=x$k, columns=x$columns, rows=x$rows, error=x$relative_error)
  }) %>%
    do.call(rbind, .) %>%
    mutate(stage=row.names(.)) %>%
    arrange(error, desc(k), desc(columns), desc(rows)) %>%
    mutate(rate=lag(error),
           rate=error-rate,
           rate=round(rate/error*100, 4)) %>%
    arrange(desc(rate))

  plot <- data %>%
    ggplot()+
    geom_raster(data = datos, aes(x=k, y=columns, fill=error), interpolate = TRUE)+
    scale_y_continuous(expand = c(0,0),
                       sec.axis = dup_axis(name = "Rows",
                                           labels = levels(as.factor(datos$rows))),
                       labels = levels(as.factor(datos$columns)))+
    scale_x_discrete(expand = c(0,0), limits = c(min(datos$k),
                                                 floor(quantile(datos$k, probs = seq(.1,.9,.1))),
                                                 max(datos$k)))+
    scale_fill_viridis(option="B")+
    theme_bw()+
    labs(y="Columns", x="K", fill="Relative error", title="", caption="")+
    theme(plot.title = element_text(hjust = 0.5, face="plain"),
          plot.caption=element_text(hjust=0, vjust=0.5,
                                    margin=margin(t=1,10,10,10)))

  data <- arrange(data, error)

  list(data=data, plot=plot)
}
