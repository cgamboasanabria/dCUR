\name{optimal_stage}
\alias{optimal_stage}

\title{
Select the optimal stage of dynamic CUR descomposition
}
\description{
\code{Optimal_stage} is a function used to select the optimal k, the number of columns and rows of dynamic CUR object, it also produces a data frame and corresponding plots.
}
\usage{
optimal_stage(x, limit = 80)
}

\arguments{
  \item{data}{dCUR results.}
  \item{limit}{Cumulative percentage average relative error rate.}
}
\details{

The objective of CUR decomposition is to find the most relevant variables and observations within a data matrix and to reduce the dimensionality. It is well known that as more columns (variables) and rows are selected the relative error will be lower, however, this is not true for k (number of components to calculate leverages). Given the above, this function seeks to find the best-balanced scenario of k, the number of relevant columns, and rows that have an error very close to the minimum, and that in turn uses a smaller amount of information.

}
\value{
\item{data}{a data frame which specifies the relative error for each stage of CUR descomposition.}
\item{rows_plot}{a plot where the average relative error is showed for each number of relevant rows selected.}
\item{columns_plot}{a plot where the average relative error is showed for each number of relevant columns selected.}
\item{k_plot}{a plot where the average relative error is showed for each k (number of components to compute leverage), given the optimal number of relevant columns and rows.}
\item{optimal}{a data frame where the average relative error is showed for optimal k (number of components to compute leverage), given the optimal number of relevant columns and rows.}
}
\references{
Villegas, G., Martin, C., Garcia-Gonzalez, N., Hernandez, S., Sanchez-Barba, M., & Galindo-Villardin, M. P. (2019). Dynamic CUR, an alternative to variable selectin in CUR decomposition. Revista Investigacion Operacional, 40(3), 391-399. Retrieved from https://rev-inv-ope.univ-paris1.fr/fileadmin/rev-inv-ope/files/40319/40319-09.pdf
}
\author{
M.Sc. Cesar Gamboa Sanabria, info@cesargamboasanabria.com, https://orcid.org/0000-0001-6733-4759; M.Sc. Stefany Matarrita Munoz, stefmatarrita@gmail.com, https://orcid.org/0000-0002-1222-1981;
M.Sc. Katherine Barquero Mejias, kath30.bm@gmail.com, https://orcid.org/0000-0003-1760-9026; Ph.D. Greibin Villegas Barahona,gvillegas@uned.ac.cr, https://orcid.org/0000-0002-4380-0812;
Ph.D. Mercedes Sanchez Barba,mersanbar@usal.es, https://orcid.org/000-0002-3324-5798;
Ph.D. Maria Purificacion Galindo Villard?n, pgalindo@usal.es, https://orcid.org/0000-0001-6977-7545}



\seealso{
\code{\link{dCUR}}
\code{\link{CUR}}
}
\examples{
data("AASP")
list <- dCUR::dCUR(data=AASP, variables=hoessem:notabachillerato,
               k=50, rows=0.5, columns=0.5,skip = 0.1, standardize=TRUE,
               cur_method="sample_cur",
               parallelize =TRUE, dynamic_columns  = TRUE,
               dynamic_rows  = TRUE)

result <- optimal_stage(prueba,limit = 85)
result
result$k_plot
result$columns_plot
result$data
result$optima}

