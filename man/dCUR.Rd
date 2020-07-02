\name{dCUR}
\alias{dCUR}
\title{
Dynamic CUR
}
\description{
Dynamic CUR is a function that boosts the CUR descomposition varying the k, number of columns, and rows used, its final purposes is help to find the stage which minimizes the relative error. The classic CUR and its extensions can be used in \code{dCUR}.
}
\usage{
 dCUR(data, variables, standardize = FALSE, dynamic_columns = FALSE,
     dynamic_rows = FALSE, parallelize = FALSE,skip = 0.05, ...)
}
\arguments{
  \item{data}{
    a data frame which contains the variables to use in CUR descomposition and other externals variables you want to correlate with.}
  \item{variables}{corresponds to the variables used to compute the leverage scores in CUR analysis. The external variable's names must not be included. Dplyr notation can be used to specify the variables (see example).}
  \item{standardize}{logical. If \code{TRUE} the data is standardized (by subtracting the average and dividing by the standard deviation)}
  \item{dynamic_columns}{
     logical. If \code{TRUE} starts an iterative process in which the leverage scores are computed for the different values from 1 to k main components, as well as from 1 to c (the proportion of columns to be selected from the data matrix).}
  \item{dynamic_rows}{
     logical. If \code{TRUE} starts an iterative process in which the leverage scores are computed for the different values from 1 to k main components, as well as from 1 to c (the proportion of columns to be selected from the data matrix).}

  \item{skip}{numeric. It specifies change ratio of columns and rows to be selected.}

  \item{parallelize}{
     logical.If\code{TRUE} the CUR analysis is parallelized.}

  \item{...}{Additional arguments to be passed to "CUR"}
}
\details{

This function serves as a basis for selecting the best combination of k (main components), c (number of columns) and r (number of rows), in other word, the stage that minimizes the relative error \deqn{\frac{|A-CUR}{A}}{|A-CUR|/|A|}, and thus optimizes the number of columns in the analysis, ensuring a percentage of explained variability of the data matrix and facilitating the interpretation of the data set by reducing the dimensionality of the original matrix.


If \code{skip = 0.1} for each k, it is tested with a column proportion of 0, 0.1, 0.11,0.22,...; the same applies for rows. Given the above, it is recommended not to choose a very small skip, since this implies doing the CUR analysis for more stages.

Parallelizing the function improves significantly the speed of it.
}
\value{

 CUR returns a list of multiple list, each one represents a stage and it contains:

\item{k}{Number of principal components with which leverages scores are computed.}
\item{columns}{ number of columns selected.}
\item{rows}{number of rows selected.}
\item{relative_error)}{relative_error obtained: \deqn{\frac{|A-CUR}{A}}{|A-CUR|/|A|}}}



\author{M.Sc. Cesar Gamboa Sanabria, info@cesargamboasanabria.com, https://orcid.org/0000-0001-6733-4759; M.Sc. Stefany Matarrita Munoz,stefmatarrita@gmail.com, https://orcid.org/0000-0002-1222-1981;
M.Sc. Katherine Barquero Mejias, kath30.bm@gmail.com, https://orcid.org/0000-0003-1760-9026; Ph.D. Greibin Villegas Barahona,gvillegas@uned.ac.cr, https://orcid.org/0000-0002-4380-0812;
Ph.D. Mercedes Sanchez Barba,mersanbar@usal.es, https://orcid.org/000-0002-3324-5798;
Ph.D. Maria Purificacion Galindo Villardon, pgalindo@usal.es, https://orcid.org/0000-0001-6977-7545}

\references{
Chris, A., Scrucca, L., Brendan, T., & Fop, M. (2018). mclust: Gaussian Mixture Modelling for Model-Based Clustering, Classification, and Density Estimation. Retrieved from https://cran.r-project.org/web/packages/mclust/index.html

Seongho, K. (2015). ppcor: An R Package for a Fast Calculation to Semi-partial Correlation Coefficients. Commun Stat Appl Methods, 22(6), 665-674. https://doi.org/10.5351/CSAM.2015.22.6.665.ppcor
}

\seealso{
\code{\link{CUR}}
\code{\link{optimal_stage}}
}

\examples{

results <- dCUR::dCUR(data=AASP, variables=hoessem:notabachillerato,
               k=15, rows=0.25, columns=0.25,skip = 0.1, standardize=TRUE,
               cur_method="sample_cur",
               parallelize =TRUE, dynamic_columns  = TRUE,
               dynamic_rows  = TRUE)

results
}

