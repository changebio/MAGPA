#' Genotype Data
#'
#' The data is a list with a SnpMatrix `genotypes` (2000 rows, 50 columns) and a data frame `map`.
#' It should be used in the \code{magpa} function to test multivariate correlation.
#'
#' \itemize{
#'   \item genotypes
#'   \item map
#'}
#'
#' @docType data
#' @keywords datasets
#' @name geno
#' @usage data(geno)
#' @format An object of list with a Fromal class \code{'SnpMatrix'} and a data.frame
#' @examples
#' data(geno)
#' snps <- geno$genotypes
NULL
