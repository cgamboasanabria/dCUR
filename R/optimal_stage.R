optimal_stage <- function(data){

  data <- lapply(data, function(x){
    data.frame(k=x$k, error=x$relative_error)
  }) %>%
    do.call(rbind, .) %>%
    mutate(stage=row.names(.))

  plot <- ggplot(datos,aes(x=k, y=error,colour="1")) +
    geom_line(colour="blue")  +
    labs(title = "RELATIVE ERROR ESTIMATION",
         x = "K",
         y = "Relative Error")+
    theme_bw()+
    theme(plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank())

  data <- arrange(data, error)

  list(data=data, plot=plot)
}
