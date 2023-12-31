% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functionsEnrichment.R
\name{enrichGO}
\alias{enrichGO}
\title{Enrichment analysis of GO-terms based on Ensembl IDs}
\usage{
enrichGO(
  genes,
  fdr = NULL,
  fc = NULL,
  ntop = NA,
  thr.fdr = 0.05,
  thr.fc = NA,
  db = "BP",
  genome = "org.Hs.eg.db",
  id = c("entrez", "alias", "ensembl", "symbol", "genename"),
  algorithm = "weight",
  do.sort = TRUE,
  randomFraction = 0,
  return.genes = FALSE
)
}
\arguments{
\item{genes}{character vector with list of ENSEBML IDs}

\item{fdr}{numeric vector of FDR for each gene}

\item{fc}{numeric vector of logFC for each gene}

\item{ntop}{number of first taken genes}

\item{thr.fdr}{significance threshold for FDR}

\item{thr.fc}{significance threshold for absolute logFC}

\item{db}{name of GO database: "BP","MF","CC"}

\item{genome}{R-package for genome annotation used. For human -
'org.Hs.eg.db'}

\item{id}{id}

\item{algorithm}{algorithm for `runTest()`}

\item{do.sort}{if TRUE - resulted functions sorted by p-value}

\item{randomFraction}{for testing only, the fraction of the genes to be 
randomized}

\item{return.genes}{If TRUE include genes in output. Default value is FALSE}
}
\value{
list with terms and stats
}
\description{
Enrichment analysis of GO-terms for independent components with 
Ensembl IDs based on topGO package
}
\author{
Petr V. Nazarov
}
