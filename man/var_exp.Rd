\name{var_exp}
\alias{var_exp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Variabilidad explicada
}
\description{
Esta función calcula la varianza explicada para cada uno de los componentes según el método de descomposición valores singulares (SVD).
}
\usage{
var_exp(data, standardize = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
    corresponde al set de datos.} 

  \item{standardize}{
     standardize logical.Si \code{TRUE} los datos se estandarizan}

}
\details{


%%  ~~ If necessary, more details than the description above ~~
}
\value{
 var_exp retorna un data frame indicando para cada componente el porcentaje de variabilidad explicada.

\item{residuals }{fldkfkdgk}
\item{rank }{dsfsfs}
    
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{


%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
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
