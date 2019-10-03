#' @title  Multivariate Analysis of Genotype–Phenotype Association
#'
#' @aliases magpa
#'
#' @description
#' the function is performed multivariate analysis of genotype–phenotype association based on CCA,
#' which calls \code{cca} in the **yacca** package to do canonical correlation analysis.
#'
#' @param geno
#' string; a file prefix name of plink output (.bed, .bim, .fam),
#' list; an object read by \code{read.plink} of the **snpStats** package,
#' or a SnpMatrix
#'
#' @param pheno
#' a matrix with n rows of sample and m columns of features
#'
#' @param pca
#' boolean. perform principal component analysis on the phenotype?
#' the default is FALSE
#'
#' @param rm.na
#' boolean. remove missing values during analysis?
#' the default is TRUE
#'
#' @return A matrix included SNP, CHR, position, MAF, canonical correlation, chisq, and pvalue.
#'
#'
#' @examples
#' data(geno)
#' data(pheno)
#' gpa<-magpa(geno,pheno,pca = TRUE)
#' head(gpa)
#'
#' @import snpStats
#' @import yacca
#' @importFrom assertthat is.string
#' @importFrom  methods as
#' @importFrom stats pchisq
#'
#' @export
#'
magpa <- function(geno, pheno, pca = FALSE, rm.na = TRUE) {
    if (is.string(geno))
        geno <- read.plink(geno)
    if (is.list(geno)) {
        CHR <- geno$map$chromosome
        position <- geno$map$position
        geno <- geno$genotypes
        MAF <- col.summary(geno)$MAF
        SNP <- colnames(geno)
        geno <- as(geno, "numeric")

    } else {
        CHR <- NULL
        MAF <- col.summary(geno)$MAF
        SNP <- colnames(geno)
        geno <- as(geno, "numeric")
    }

    if (pca)
        pheno <- pcapheno(pheno)$pheno

    alst <- apply(geno, 2, function(snp) {
        na_ind <- !is.na(snp)
        a <- cca(pheno[na_ind, ], snp[na_ind])
        return(a)
    })
    cc <- sapply(alst, function(a) a$corr)
    chisq <- sapply(alst, function(a) a$chisq)
    pvalue <- sapply(alst, function(a) pchisq(a$chisq, a$df, lower.tail = FALSE))
    if (is.null(CHR)) {
        gpa <- data.frame(SNP, MAF, cc, chisq, pvalue, row.names = NULL)
    } else {
        gpa <- data.frame(SNP, CHR, position, MAF, cc, chisq, pvalue, row.names = NULL)
    }
    gpa
}

#' @title  Principal Component Analysis with Horn's Parallel Analysis
#'
#' @aliases pcapheno
#'
#' @description
#' the function is implemented to automatically extract the principal components from
#' high-dimension data, which calls \code{paran} to performs Horn's parallel analysis
#' for evaluating the components retained in a principle component analysis.
#'
#'
#' @param pheno
#' A matrix with n rows of sample and m columns of features
#'
#'
#' @param iterations
#' sets the number of iterations with a user specified whole number representing
#' the number of random data sets to be produced in the analysis. The default is 100.
#'
#' @return a list of objects with the pca of phenotypes and the results of parallel analysis
#'
#'
#' @examples
#' data(pheno)
#' paral<- pcapheno(pheno)
#'
#' @importFrom paran paran
#' @importFrom stats prcomp
#'
#' @export
#'
pcapheno <- function(pheno, iterations = 100) {
    pla <- paran(pheno, iterations = iterations)
    pheno.pca <- prcomp(pheno)
    pheno <- pheno.pca$x[, 1:pla$Retained]
    return(list(pheno = pheno, pla = pla))
}
