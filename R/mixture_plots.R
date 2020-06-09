mixture_plots <- function(data){
  if(class(data)[2] == "dCUR"){

    lapply(data, function(x){
      suppressWarnings(mixture_plots_fun(x) )
    })
  }else(suppressWarnings(mixture_plots_fun(data)))
}

mixture_plots_fun <- function(data){
  #BIC plot

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

  #dataity plot

  df <- data.frame(leverage_columns = leverage_columns)

  p2 <- ggplot(df, aes(x=leverage_columns)) +
    geom_histogram(aes(y=..density..),
                   binwidth=.5,
                   colour="black", fill="white") +
    geom_density(alpha=.2, fill="#73108f") +
    theme_classic()

  #Cumulative distribution

  df <- cdfMclust(data) %>%
    data.frame() %>%
    rename(`Leverage`=x,`Cumulative density function` = y)

  p3 <- ggplot(df, aes(`Leverage`,`Cumulative density function`)) +
    geom_line()+
    theme_classic()

  #Qqplot

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
