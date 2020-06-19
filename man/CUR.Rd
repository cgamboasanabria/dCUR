\name{CUR}
\alias{CUR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
CUR cl?sico incluyendo la reconfiguraci?n de los puntajes leverages y la selecci?n de filas y columnas ajustando la distribuci?n de probabilidad de los puntajes leverage
}
\description{
Esta funci?n calcula los puntajes leverage tradicionales seg?n la propuesta metodol?gia de Mahoney & Drineas (2009), se le denomina CUR muestral. Tambi?n tiene una extensi?n al procedimiento cl?sico de Mahoney, el cu?l corresponde a la reconfiguraci?n de los puntajes leverages seg?n la metodolog?a de Villegas et al (2018). Esta metodolog?a consiste en dividir el puntaje leverage por uno menos la correlaci?n parcial o semiparcial al cuadrado que tiene la variable asociada a un puntaje leverage con el prop?sito de recalibrar la importancia relativa de los puntajes leverage en funci?n de su correlaci?n con una variable externa. Tambi?n ajusta la distribuci?n de probabilidad de los puntajes leverage para selecting the best columns and rows that minimize the reconstruction error of the matrix approximation ???A-CUR???.}
\usage{
## function (data, variables, k = NULL, rows, columns, standardize = FALSE,
##     cur_method = "sample_cur", correlation = NULL, correlation_type = c("partial",
##         "semipartial"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
    corresponde al set de datos, puede incluir o no variables externas}
  \item{variables}{
    corresponde a las variables que se utilizan en el an?lisis CUR, con las cuales     se calculan los puntajes leverage, no debe incluir variables externas}
  \item{k}{
    corresponde a la cantidad de componentes principales que se utilizan para   calcular los puntajes leverage, si es NULL se considera la cantidad de componentes principales k que acumulan el 80\% de la varaiablidad}
  \item{rows}{
    corresponde a la cantidad de filas que se desean seleccionar, obtenida esa cantidad como un porcentaje del ?rea bajo la curva de la distribuci?n de los puntajes leverage de las filas (\% *n?mero de filas)}
  \item{columns}{
    corresponde a la cantidad de columnas que se desean seleccionar, obtenida esa cantidad como un porcentaje del ?rea bajo la curva de la distribuci?n de los puntajes leverage de las columnas (\% *n?mero de columnas)}
  \item{standardize}{
     standardize logical.Si \code{TRUE} los datos se estandarizan (restando el promedio y dividiendo por la desviaci?n est?ndar)}
  \item{cur_method = "sample_cur"}{
    corresponde al m?todo muestral que consiste en el c?lculo de los puntajes leverage de manera cl?sica segun Mahoney & Drineas (2009)}
  \item{correlation}{
    corresponde a la selecci?n del cambio de m?todo de c?lculo de los puntajes leverages, seg?n la metodolog?a Villegas (2018) y Villegas, et al. (2019) puede tener dos opciones correlaci?n parcial o semiparcial. The partial correlation can be explained as the association between two random variables after eliminating the effect of all other random variables. the semi-partial correlation eliminates the effect of a fraction of other random variables, for instance, removing the effect of all other random variables from just one of two interesting random variables.  The rationale for the partial and semi-partial correlations is to estimate a direct relationship or association between two random variables.}
  \item{correlation_type}{
    corresponde a la selecci?n del tipo de correlaci?n que se utiliza, sea parcial o semiparcial seg?n Seongho (2015) del paquete en R ppcor}
}
\details{
Mahoney & Drineas (2009) identified the singular vectors of the SVD as the PCs' interpretation problem and proposed another type of matrix factorization known as CUR Decomposition (Mahoney & Drineas, 2009; Mahoney, Maggioni, & Drineas, 2008; Bodor, Csabai, Mahoney, & Solymosi, 2012). The goal of CUR Decomposition is to give a better interpretation of the matrix decomposition by means of relevant variable selection in the data matrix, in a way that yields a simplified structure. Its origins come from analysis in genetics. One example is the one showed in Mahoney & Drineas (2009), in which cancer microarrays are highlighted with the purpose of recognizing, based on 5000 variables, genetic patterns in patients with soft tissue tumors analyzed with cDNA microarrays.
The CUR theoretical basis comes from the SVD of the matrix of interest, in order to make a new factorization by selecting columns and rows from the original matrix. This is basically a low-rank approximation to the original matrix expressed in a small number of rows and/or columns, which are easier to interpret than the singular vectors of the SVD. The main advantage of CUR Decomposition over SVD is that the original data matrix can be expressed with a reduced number of rows and columns instead of obtaining factorial axes that result of a linear combination of all the original variables.
The objective of this package is to show an alternative to variable selection (columns) or individuals (rows) to the ones developed by Mahoney & Drineas (2009). The idea proposed consists of adjusting the probability distributions to the leverage scores and selecting the best columns and rows that minimize the reconstruction error of the matrix approximation ???A-CUR???}
\value{
 CUR retorna los siguientes resultadosel n?mero de componentes principales con los que se calcula los puntajes leverage, la dimensi?n de la matriz original, el error absoluto ???A-CUR???,e lerror relativo ???A-CUR ???/  ???A ???, los puntajes leverage de todas las columnas ordenadas de mayor a menor seg?n el valor del puntaje leverage,

 R,??un data frame indicando para cada componente el porcentaje de variabilidad explicada.

\item{resultado$k}{N?mero de componentes principales con los que se calcula los puntajes leverage}
\item{dim(resultado$CUR) }{Dimensi?n de la matriz CUR}
\item{resultado$absolute_error}{Error absoluto ???A-CUR???}
\item{dim(resultado$relative_error) }{Error relativo ???A-CUR???/???A??? }
\item{dim(resultado$leverage_columns_sorted) }{Nombre de las columnas y puntaje leverage columnas ordenados de mayor a menor}
\item{resultado$leverage_rows_sorted }{N?mero de las filas y puntajes leverage filas ordenados de mayor a menor}
\item{resultado$leverage_columns }{Todos los leverages de todas las columnas usando head}
\item{resultado$leverage_rows }{Todoslos leverages de todas las filas usando head}
}
\references{
}
\author{
Mahoney, M. W., & Drineas, P. (2009). {CUR} matrix decompositions for improved data analysis. Proceedings of the National Academy of Sciences of the United States of America, 106(3), 697-702. http://doi.org/10.1073/pnas.0803205106
Seongho, K. (2015). ppcor: An R Package for a Fast Calculation to Semi-partial Correlation Coefficients. Commun Stat Appl Methods, 22(6), 665-674. https://doi.org/10.5351/CSAM.2015.22.6.665.ppcor
Villegas, B. G. (2018). Modelo estad?stico pedag?gico para la toma de decisiones administrativas y acad?micas con impacto en el mejoramiento continuo del rendimiento de los estudiantes universitarios, basado en los m?todos de selecci?n CUR. Universidad de Salamanca. Retrieved from https://gredos.usal.es/handle/10366/139405
Villegas, G., Mart?n, C., Garc?a-Gonz?lez, N., Hern?ndez, S., S?nchez-Barba, M., & Galindo-Villard?n, M. P. (2019). Dynamic CUR, an alternative to variable selectin in CUR decomposition. Revista Investigaci?n Operacional, 40(3), 391-399. Retrieved from https://rev-inv-ope.univ-paris1.fr/fileadmin/rev-inv-ope/files/40319/40319-09.pdf
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Bodor, A. A., Csabai, I. I. I., Mahoney, M. W., & Solymosi, N. (2012). rCUR: an R package for CUR matrix decomposition. BMC BIOINFORMATICS, 13(1), 103. https://doi.org/10.1186/1471-2105-13-103
Drineas, P., Mahoney, M. W., & Muthukrishnan, S. (2008). Relative-error cur matrix decompositions. SIAM Journal on Matrix Analysis and Applications, 30(2), 844-881. https://doi.org/10.1137/07070471X
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

