\name{var_exp}
\alias{var_exp}

\title{
Variance Explained
}
\description{

\code{var_exp} is used to fit the fraction of variance explained by a principal component. It is the ratio between the variance of that principal component and the total variance with CUR technique proposal by Mahoney & Drineas (2009).

}

\usage{
var_exp(data, standardize, ...)
}

\arguments{
  \item{data}{
    an optional data frame}

  \item{standardize}{
     logical. If \code{TRUE} rescale an original data frame to have a mean of zero and standard deviation of one}

  \item{...}{
    Additional arguments to be passed to \code{dplyr::select}}

}
\value{
 \code{var_exp} fits a data frame with the proportion of variance explained in each principal component.
}
\references{

Mahoney, M. W., & Drineas, P. (2009). {CUR} matrix decompositions for improved data analysis. Proceedings of the National Academy of Sciences of the United States of America, 106(3), 697-702. http://doi.org/10.1073/pnas.0803205106

Villegas, B. G. (2018). Modelo estadistico pedagogico para la toma de decisiones administrativas y academicas con impacto en el mejoramiento continuo del rendimiento de los estudiantes universitarios, basado en los metodos de seleccion CUR. Universidad de Salamanca. Retrieved from https://gredos.usal.es/handle/10366/139405

Villegas, G., Martin, C., Garcia-Gonzalez, N., Hernandez, S., Sanchez-Barba, M., & Galindo-Villardon, M. P. (2019). Dynamic CUR, an alternative to variable selectin in CUR decomposition. Revista Investigacion Operacional, 40(3), 391-399. Retrieved from https://rev-inv-ope.univ-paris1.fr/fileadmin/rev-inv-ope/files/40319/40319-09.pdf
}
\author{
M.Sc. Cesar Gamboa Sanabria, info@cesargamboasanabria.com, https://orcid.org/0000-0001-6733-4759; M.Sc. Stefany Matarrita Munoz,stefmatarrita@gmail.com, https://orcid.org/0000-0002-1222-1981;
M.Sc. Katherine Barquero Mejias, kath30.bm@gmail.com, https://orcid.org/0000-0003-1760-9026; Ph.D. Greibin Villegas Barahona,gvillegas@uned.ac.cr, https://orcid.org/0000-0002-4380-0812;
Ph.D. Mercedes Sanchez Barba,mersanbar@usal.es, https://orcid.org/000-0002-3324-5798;
Ph.D. Maria Purificacion Galindo VillardOn, pgalindo@usal.es, https://orcid.org/0000-0001-6977-7545}



\seealso{

\code{\link{dCUR}}
\code{\link{CUR}}

}
\examples{

var_exp(AASP, standardize = TRUE, hoessem:notabachillerato)

}
