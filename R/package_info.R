#' dCUR Package
#'
#' Dynamic CUR is an r package that boosts the CUR decomposition varying the k, the number of columns and rows used, and its final purposes to help find the stage, which minimizes the relative error to reduce matrix dimension. Mahoney & Drineas (2009) identified the singular vectors of the SVD as the PCs' interpretation problem and proposed another type of matrix factorization known as CUR Decomposition (Mahoney & Drineas, 2009; Mahoney, Maggioni, & Drineas, 2008; Bodor, Csabai, Mahoney, & Solymosi, 2012). The goal of CUR Decomposition is to give a better interpretation of the matrix decomposition employing proper variable selection in the data matrix, in a way that yields a simplified structure. Its origins come from analysis in genetics. One example is the one showed in Mahoney & Drineas (2009), in which cancer microarrays highlighted to recognize, based on 5000 variables, genetic patterns in patients with soft tissue tumors analyzed with cDNA microarrays. The objective of this package is to show an alternative to variable selection (columns) or individuals (rows) to the ones developed by Mahoney & Drineas (2009). The idea proposed consists of adjusting the probability distributions to the leverage scores and selecting the best columns and rows that minimize the reconstruction error of the matrix approximation \|A-CUR\|. It also includes a method that recalibrates the relative importance of the leverage scores according to an external variable of the user's interest.
#'
#' @docType package
#'
#' @author
#'
#' Cesar Gamboa-Sanabria \email{info@cesargamboasanabria.com}
#'
#' @name dCUR
#' @aliases dCUR-package
#'
#' @import Rdpack
#' @importFrom mclust densityMclust quantileMclust cdfMclust
#' @importFrom dplyr select filter mutate rename arrange group_by summarise
#' @importFrom magrittr '%>%'
#' @importFrom MASS ginv
#' @importFrom ppcor pcor spcor
#' @importFrom parallel makeCluster detectCores clusterEvalQ clusterMap stopCluster
#' @importFrom ggplot2 ggplot aes element_text scale_fill_gradient rel geom_line geom_point scale_shape_manual scale_color_manual theme theme_classic scale_x_continuous geom_histogram geom_abline theme_bw scale_y_continuous geom_vline geom_text labs geom_density theme_bw geom_bar coord_flip
NULL
