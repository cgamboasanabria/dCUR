#' mixture_plots
#'
#' Gaussian Mixture Models Plots
#'
#' @description
#'
#' This function returns different plots associated with the fitting of leverages scores through Mixture Gaussian Models.
#'
#' @param data An object resulting from a call to CUR when "mixture" is specified as cur_method.
#'
#' @return
#' \code{mixture_plots} returns a list with the following plots:
#'
#' \item{BIC}{BIC Plot of the Bayesian Information Criterion (BIC) for each number of mixture components. E and V stands for equal variance in mixture components or variable variance, respectively.}
#' \item{density}{leverages score's density}
#' \item{Cumulative}{cumulative density of leverages scores.}
#' \item{QQPlot}{Plot the sample quantiles and controlled quantiles of the inverse of the cumulative distribution function.}
#'
#' @examples
#'
#' results <- CUR(data=AASP, variables=hoessem:notabachillerato,
#' k=20, rows = .9999999, columns = .10, standardize = TRUE,
#' cur_method = "mixture")
#' mixture_plots(results)
#'
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
#' \insertRef{Mahoney697}{dCUR}
#' \insertRef{villegas2018modelo}{dCUR}
#' \insertRef{dynamyCUR}{dCUR}
#'
#' @export
mixture_plots <- function(data){

  leverage_columns <- data$leverage_columns
  data <- data$density_columns

  df <- data.frame(matrix(data$BIC)) %>%
    mutate(n = nrow(matrix(data$BIC)),
           components = rep(1:(n/2),2),
           type = rep(c("E","V"),each = (n/2))) %>%
    rename(BIC = matrix.data.BIC. ) %>%
    dplyr::select(components,type,BIC)

  p1 <- ggplot(data = df, aes(x = components, y = BIC, colour = type)) +
    geom_line() +
    geom_point(aes(shape=type, color=type), size=2)+
    scale_shape_manual(values=c(24, 24))+
    scale_color_manual(values=c('#73108f','#E69F00'))+
    theme(legend.position="bottom")+labs(fill = "")+
    theme_classic()+
    scale_x_continuous(breaks = seq(min(df$components),max(df$components)))


  df <- data.frame(leverage_columns = leverage_columns, densityMixture=densityMclust(leverage_columns)$density)

  p2 <- ggplot(df, aes(x=leverage_columns)) +
    geom_histogram(aes(y=..density..),
                   binwidth=.5,
                   colour="black", fill="white") +
    geom_line(data=df,aes(x = leverage_columns, y = densityMixture), col = "#73108f") +
    theme_classic()


  df <- cdfMclust(data) %>%
    data.frame() %>%
    rename(`Leverage`=x,`Cumulative density function` = y)

  p3 <- ggplot(df, aes(`Leverage`,`Cumulative density function`)) +
    geom_line()+
    theme_classic()

  data_data <- as.numeric(data$data) %>% sort()
  n <- length(data_data)
  q <- quantileMclust(data, p = ppoints(n))

  df <- data.frame(data_data,q) %>% rename(`sample quantiles` = data_data, `theorical quantiles` = q)

  p4 <- ggplot(df,aes(`theorical quantiles`,`sample quantiles`))+
    geom_abline(intercept = 0, slope = 1)+
    geom_point(size = 0.2,color = "#73108f" )+
    theme_classic()
  list(BIC=p1, density=p2, Cumulative=p3, QQPlot=p4)
}
