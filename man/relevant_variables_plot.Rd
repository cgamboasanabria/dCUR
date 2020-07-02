\name{relevant_variables_plot}
\alias{relevant_variables_plot}

\title{
Relevant Variables Plot
}
\description{

\code{relevant_variables_plot} returns a bar graph with the higher leverages values fitted with the CUR function according to the selected columns in a matrix data.

}
\usage{
relevant_variables_plot(data)
}

\arguments{
  \item{data}{

an object with the results of the CUR function estimation for a matrix data

}
}
\details{

The leverage values evaluate the relevance of the columns for explaining the extern variables. They are fitted with CUR technique proposal by Mahoney & Drineas (2009). The objective of bar graph is show the most relevant columns with better fit to the original data matrix.

}

\references{

Villegas, G., Martin, C., Garcia-Gonzalez, N., Hernandez, S., Sanchez-Barba, M., & Galindo-Villardon, M. P. (2019). Dynamic CUR, an alternative to variable selectin in CUR decomposition. Revista Investigacion Operacional, 40(3), 391-399. Retrieved from https://rev-inv-ope.univ-paris1.fr/fileadmin/rev-inv-ope/files/40319/40319-09.pdf

}
\author{

M.Sc. Cesar Gamboa Sanabria, info@cesargamboasanabria.com, https://orcid.org/0000-0001-6733-4759; M.Sc. Stefany Matarrita Mu?oz,stefmatarrita@gmail.com, https://orcid.org/0000-0002-1222-1981;
M.Sc. Katherine Barquero Mejias, kath30.bm@gmail.com, https://orcid.org/0000-0003-1760-9026; Ph.D. Greibin Villegas Barahona,gvillegas@uned.ac.cr, https://orcid.org/0000-0002-4380-0812;
Ph.D. Mercedes Sanchez Barba,mersanbar@usal.es, https://orcid.org/000-0002-3324-5798;
Ph.D. Maria Purificacion Galindo VillardOn, pgalindo@usal.es, https://orcid.org/0000-0001-6977-7545}

\seealso{

\code{\link{dCUR}}
\code{\link{CUR}}

}
\examples{
resultado <- CUR(data=AASP, variables=hoessem:notabachillerato,
                 k=20, rows = 1, columns = .2, standardize = TRUE,
                 cur_method = "sample_cur")

relevant_variables_plot(resultado)
}
