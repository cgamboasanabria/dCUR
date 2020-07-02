\name{CUR}
\alias{CUR}
\title{
Extension of classic CUR descomposition with top scores selection criteria.
}
\description{

This function computes the canonical CUR decomposition using top scores as selection criteria to identify the most relevant columns and rows. This function includes the reconfiguration of leverage scores with the partial and semi partial correlations with and external variable and the selection of rows and columns by adjusting the probability distribution of leverage scores with mixtures Gaussian models.
}
\usage{
 CUR(data,variables,k=NULL,rows,columns,
 standardize=FALSE,cur_method="sample_cur",
 correlation=NULL,
 correlation_type=c("partial","semipartial"),...)
}
\arguments{
  \item{data}{
    a data frame which contains the variables to use in CUR descomposition and other externals variables you want to correlate with.}
  \item{variables}{corresponds to the variables used to compute the leverage scores in CUR analysis. The external variable's names mus not be included. Dplyr notation can be used to specify the variables (see example).}
  \item{k}{corresponds to the number of main components used to compute the leverage scores. If NULL it is considered the number of k main components that accumulate 80\% of the variability explained. It can also be a proportion, in this case the function takes this value as the desired cumulative explained variance  and automaticalyy choose the k.}
  \item{rows}{corresponds to the proportion of rows to be selected from the total number of rows in the data matrix. When all the rows are needed and \code{mixture} is used as cur_method, a proportion of 0.999 must be used.}
  \item{columns}{corresponds to the proportion of columns (variables) to be selected from the total number of variables in the data matrix.}
  \item{standardize}{If \code{TRUE} the data is standardized (by subtracting the average and dividing by the standard deviation)}
  \item{cur_method}{character.If \code{sample_cur} the selection of leverage scores is made according to the top score selection criteria set out by Mahoney & Drineas (2009). If \code{mixture} method is specified, a probabilistic mixture gaussian model is fitted for the leverages, and the selection of the most relevant variables is based on a tabular value given a critical area.}
     \item{correlation}{character. It specifies the name of the external variable the computation of leverage must be adjusted with.}
  \item{correlation_type}{character. It specifies if the computation of leverage must be adjusted by the \code{semipartial} or \code{partial} correlation with an external variable.}
  \item{...}{additional arguments to be passed to \code{\link{pcor}} or \code{\link{spcor}}}
}
\details{

The CUR theoretical basis comes from the SVD of the matrix of interest to make a new factorization by selecting columns and rows from the original matrix. It is a low-rank approximation to the original matrix expressed in a small number of rows and columns, which are easier to interpret than the singular vectors of the SVD. The main advantage of CUR Decomposition over SVD is that the original data matrix can express a reduced number of rows and columns instead of obtaining factorial axes resulting from a linear combination of all the original variables.

This function calculates the traditional leverage scores according to the proposal of Mahoney & Drineas (2009). The top.score method of Mahoney and Drineas is called sample CUR. The extension to Mahoney's standard procedure corresponds to the reconfiguration of the leverage scores according to the methodology of Villegas et al. (2018). This methodology consists in dividing the leverage score by \deqn{(1-\rho^2)}{(1-rho^2)}. \deqn{\rho} rho represents the partial or semi-partial correlation that the variables used in CUR decomposition have with an external variable, and its purpose is recalibrating the relative importance of the leverage scores according to an external variable of user's interested.

The partial correlation can be explained as the association between two random variables after eliminating all other random variables. The semi-partial correlation eliminates the effect of a fraction of other random variables, for instance, removing all other random variables from just one of two interesting random variables. The rationale for the partial and semi-partial correlations is to estimate a direct relationship or association between two random variables.
The correlation type selection could be partial or semi-partial, according to Seongho (2015) of the package in R ppcor.

The extension to Mahoney's standard procedure includes the adjustment of a probability distribution of the leverage scores to select the best columns and rows that minimize the reconstruction error of the matrix approximation \deqn{|A-CUR|}{|A-CUR|}.
}

\value{

\item{k}{Number of principal components with which leverages scores are computed.}
\item{CUR}{CUR matrix.}
\item{absolute_error}{Absolute error computed as the Frobenius norm of the orginal data and CUR matrix: \\|A-CUR\\|}
\item{relative_error)}{Relative error \deqn{\frac{|A-CUR}{A}}{|A-CUR|/|A|}}
\item{leverage_columns_sorted}{ a data frame which specifies the names of relevant columns and leverages scores arranged downwardly.}
\item{leverage_rows_sorted }{a data frame which specifies the number of relevant rows and leverages scores arranged downwardly.}
\item{leverage_columns }{a data frame which specifies the names of all columns and its leverages scores.}
\item{leverage_rows }{a data frame which specifies the number of rows and its leverages scores.}
}


\author{M.Sc. Cesar Gamboa Sanabria, info@cesargamboasanabria.com, https://orcid.org/0000-0001-6733-4759; M.Sc. Stefany Matarrita Muoz,stefmatarrita@gmail.com, https://orcid.org/0000-0002-1222-1981;
M.Sc. Katherine Barquero Mejias, kath30.bm@gmail.com, https://orcid.org/0000-0003-1760-9026; Ph.D. Greibin Villegas Barahona,gvillegas@uned.ac.cr, https://orcid.org/0000-0002-4380-0812;
Ph.D. Mercedes Sanchez Barba,mersanbar@usal.es, https://orcid.org/000-0002-3324-5798;
Ph.D. Maria Purificacion Galindo Villardon, pgalindo@usal.es, https://orcid.org/0000-0001-6977-7545}

\references{
Mahoney, M. W., & Drineas, P. (2009). {CUR} matrix decompositions for improved data analysis. Proceedings of the National Academy of Sciences of the United States of America, 106(3), 697-702. http://doi.org/10.1073/pnas.0803205106

Seongho, K. (2015). ppcor: An R Package for a Fast Calculation to Semi-partial Correlation Coefficients. Commun Stat Appl Methods, 22(6), 665-674. https://doi.org/10.5351/CSAM.2015.22.6.665.ppcor

Villegas, B. G. (2018). Modelo estadistico pedagogico para la toma de decisiones administrativas y academicas con impacto en el mejoramiento continuo del rendimiento de los estudiantes universitarios, basado en los metodos de seleccion CUR. Universidad de Salamanca. Retrieved from https://gredos.usal.es/handle/10366/139405

Villegas, G., Martin, C., Garcia-Gonzalez, N., Hernandez, S., Sanchez-Barba, M., & Galindo-Villardon, M. P. (2019). Dynamic CUR, an alternative to variable selectin in CUR decomposition. Revista Investigacion Operacional, 40(3), 391-399. Retrieved from https://rev-inv-ope.univ-paris1.fr/fileadmin/rev-inv-ope/files/40319/40319-09.pdf

Bodor, A. A., Csabai, I. I. I., Mahoney, M. W., & Solymosi, N. (2012). rCUR: an R package for CUR matrix decomposition. BMC BIOINFORMATICS, 13(1), 103. https://doi.org/10.1186/1471-2105-13-103
Drineas, P., Mahoney, M. W., & Muthukrishnan, S. (2008). Relative-error cur matrix decompositions. SIAM Journal on Matrix Analysis and Applications, 30(2), 844-881. https://doi.org/10.1137/07070471X
}

\examples{

#Classic CUR with top scores selection criteria.
result <- CUR(data=AASP, variables=hoessem:notabachillerato,
                 k=20, rows = 1, columns = .2, standardize = TRUE,
                 cur_method = "sample_cur")
result

#Extension of classic CUR: Recalibrating leverages scores
#and adjusting a mixtures Gaussian models to leverages.

result <- CUR(data=AASP, variables=hoessem:notabachillerato,
                 k=20, rows = 1, columns = .2, standardize = TRUE,
                 cur_method = "mixture",
                 correlation = R1, correlation_type = "partial")
result

}

