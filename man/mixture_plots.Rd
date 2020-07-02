\name{mixture_plots}
\alias{mixture_plots}

\title{
Gaussian Mixture Models Plots
}
\description{

\code{mixture_plots} returns the results of fit the empirical distribution of the leverage scores obtained with the CUR function to a probability distribution estimated by means of Gaussian mixture models for each of the k components with which the leverage scores can be calculated, choosing columns and rows in which that probability accumulates.

}
\usage{
mixture_plots(data)
}

\arguments{
  \item{data}{

an object with the results of the CUR function estimation for a matrix data

}
}
\details{

To estimate \code{mixture_plots}, in \code{\link{CUR}} function cur_method "mixture" is required. The function returns: the graph with the size of the Bayesian information criterion according to the number of Gaussian components, the graph with the density of leverages values, the graph with cumulative density of leverages values and the graph with sample quantiles and controlled quantiles of the inverse of the cumulative distribution function.

}

\seealso{

\code{\link{dCUR}}
\code{\link{CUR}}
}
\references{
Mahoney, M. W., & Drineas, P. (2009). {CUR} matrix decompositions for improved data analysis. Proceedings of the National Academy of Sciences of the United States of America, 106(3), 697-702. http://doi.org/10.1073/pnas.0803205106

Villegas, B. G. (2018). Modelo estadistico pedagogico para la toma de decisiones administrativas y academicas con impacto en el mejoramiento continuo del rendimiento de los estudiantes universitarios, basado en los m?todos de seleccion CUR. Universidad de Salamanca. Retrieved from https://gredos.usal.es/handle/10366/139405

Villegas, G., Martin, C., Garcia-Gonzalez, N., Hernandez, S., Sanchez-Barba, M., & Galindo-Villardon, M. P. (2019). Dynamic CUR, an alternative to variable selectin in CUR decomposition. Revista Investigacion Operacional, 40(3), 391-399. Retrieved from https://rev-inv-ope.univ-paris1.fr/fileadmin/rev-inv-ope/files/40319/40319-09.pdf

}
\author{
M.Sc. Cesar Gamboa Sanabria, info@cesargamboasanabria.com, https://orcid.org/0000-0001-6733-4759; M.Sc. Stefany Matarrita Mu?oz,stefmatarrita@gmail.com, https://orcid.org/0000-0002-1222-1981;
M.Sc. Katherine Barquero Mejias, kath30.bm@gmail.com, https://orcid.org/0000-0003-1760-9026; Ph.D. Greibin Villegas Barahona,gvillegas@uned.ac.cr, https://orcid.org/0000-0002-4380-0812;
Ph.D. Mercedes Sanchez Barba,mersanbar@usal.es, https://orcid.org/000-0002-3324-5798;
Ph.D. Maria Purificacion Galindo VillardOn, pgalindo@usal.es, https://orcid.org/0000-0001-6977-7545}


\examples{

results <- CUR(data=AASP, variables=hoessem:notabachillerato,
                 k=20, rows = .9999999, columns = .10, standardize = TRUE,
                 cur_method = "mixture")

mixture_plots(results)


}

