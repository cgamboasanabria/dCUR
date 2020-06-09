relevant_variables_plot <- function(data){
  if(class(data)[2] == "dCUR"){

    lapply(data, function(x){
      suppressWarnings(relevant_variables_fun(x) )
    })
  }else(suppressWarnings(relevant_variables_fun(data)))
}

relevant_variables_fun <- function(data){
  data$leverage_columns_sorted %>%
    ggplot(., aes(x=reorder(var_names,leverage_columns),
                  y=leverage_columns, fill=leverage_columns)) +
    geom_bar(stat="identity", width=0.4)+ scale_fill_gradient(low="orange", high="purple")+
    coord_flip()+labs(x="Variables", y="Leverage", fill="Leverage range")+
    theme(axis.title.x = element_text(face="bold", vjust=-0.5, colour="orange", size=rel(1)),
          axis.title.y = element_text(face="bold", vjust=1.5, colour="blue", size=rel(1)),
          axis.text=element_text(size=rel(.5)),
          legend.title = element_text(colour="blue", size=8, face="bold"),
          legend.text = element_text(colour="blue", size = 8, face = "bold"),
          plot.title = element_text(hjust = 0.45, face="plain")) +
    scale_y_continuous(expand = c(0,0))+
    labs(title = "Relevant variables")
}
