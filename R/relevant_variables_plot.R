#' relevant_variables_plot
#'
#' Relevant Variables Plot
#'
#' @description
#' \code{relevant_variables_plot} returns a bar graph which contains the leverages of the most relevant variable of data matrix according to CUR decomposition.
#'
#' @param data An object resulting from a call to CUR.
#'
#' @examples
#' \donttest{
#' result <- CUR(data=AASP, variables=hoessem:notabachillerato,
#' k=20, rows = 1, columns = .2, standardize = TRUE,
#' cur_method = "sample_cur")
#' relevant_variables_plot(result)
#' }
#'
#' @author
#'
#' Cesar Gamboa-Sanabria, Stefany Matarrita-Munoz, Katherine Barquero-Mejias, Greibin Villegas-Barahona, Mercedes Sanchez-Barba and Maria Purificacion Galindo-Villardon.
#'
#' @seealso
#'
#' \code{\link{dCUR}}
#' \code{\link{CUR}}
#'
#' @references
#'
#' \insertRef{dynamyCUR}{dCUR}
#'
#' @export
relevant_variables_plot <- function(data){

  plot1 <- data.frame(leverage_columns=data$leverage_columns) %>%
    ggplot(., aes(x=leverage_columns)) +
    geom_histogram(aes(y=..density..),
                   binwidth=.5,
                   colour="black", fill="orange") +
    geom_density(alpha=.2, color="red", size=.8) +
    scale_x_continuous(expand=c(0,0), breaks = round(seq(min(data$leverage_columns),
                                                         max(data$leverage_columns),
                                                         length.out = 10), 2))+
    scale_y_continuous(expand=c(0,0), breaks = round(seq(min(density(data$leverage_columns)$y),
                                                         max(density(data$leverage_columns)$y+.1),
                                                         length.out = 10), 2))+
    theme_bw()+
    theme(axis.title.x = element_text(face="bold", vjust=-0.5, colour="orange", size=rel(1)),
          axis.title.y = element_text(face="bold", vjust=1.5, colour="blue", size=rel(1)),
          axis.text=element_text(size=rel(.5)),
          legend.title = element_text(colour="blue", size=8, face="bold"),
          legend.text = element_text(colour="blue", size = 8, face = "bold"),
          plot.title = element_text(hjust = 0.5, face="plain")) +
    labs(title = "Leverage Histogram", y="Density", x="Leverage")


  plot2 <- data$leverage_columns_sorted %>%
    ggplot(., aes(x=reorder(var_names,leverage_columns),
                  y=leverage_columns, fill=leverage_columns)) +
    geom_bar(stat="identity", width=0.4)+ scale_fill_gradient(low="orange", high="purple")+
    coord_flip()+labs(x="Variables", y="Leverage", fill="Leverage range")+
    theme_bw()+
    theme(axis.title.x = element_text(face="bold", vjust=-0.5, colour="orange", size=rel(1)),
          axis.title.y = element_text(face="bold", vjust=1.5, colour="blue", size=rel(1)),
          axis.text=element_text(size=rel(.5)),
          legend.title = element_text(colour="blue", size=8, face="bold"),
          legend.text = element_text(colour="blue", size = 8, face = "bold"),
          plot.title = element_text(hjust = 0.45, face="plain")) +
    scale_y_continuous(expand = c(0,0))+
    labs(title = "Relevant variables")

  list(histogram=plot1, relevant=plot2)
}
