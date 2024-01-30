
#' Derivation Trees
#'
#' The implementation of a data type for derivation trees.
#'
#' The derivation tree operations for generating complete random subtrees and for
#' for subtree extraction and insertion are formally introduced in Geyer-Schulz (1997)
#' and used for implementing mutation and crossover operations. 
#'
#' @references  Geyer-Schulz, Andreas (1997):
#'          \emph{Fuzzy Rule-Based Expert Systems and Genetic Machine Learning},
#'                Physica, Heidelberg.
#'           (ISBN:978-3-7908-0830-X)
#'
#' @section The Architecture of the xegaX-Packages:
#' 
#' The xegaX-packages are a family of R-packages which implement 
#' eXtended Evolutionary and Genetic Algorithms (xega).  
#' The architecture has 3 layers, 
#' namely the user interface layer,
#' the population layer, and the gene layer: 
#' 
#' \itemize{
#' \item
#' The user interface layer (package \code{xega}) 
#' provides a function call interface and configuration support
#' for several algorithms: genetic algorithms (sga), 
#' permutation-based genetic algorithms (sgPerm), 
#' derivation-free algorithms as e.g. differential evolution (sgde), 
#' grammar-based genetic programming (sgp) and grammatical evolution
#' (sge). 
#'
#' \item
#' The population layer (package \code{xegaPopulation}) contains
#' population-related functionality as well as support for 
#' adaptive mechanisms which depend on population statistics. 
#' In addition, support for parallel evaluation of genes is implemented here.
#'
#' \item 
#' The gene layer is split in a representation-independent and 
#' a representation-dependent part:
#' \enumerate{
#' \item 
#'  The representation-indendent part (package \code{xegaSelectGene})
#'  is responsible for variants of selection operators, evaluation 
#'  strategies for genes, as well as profiling and timing capabilities.        
#' \item 
#'  The representation-dependent part consists of the following packages: 
#' \itemize{
#' \item \code{xegaGaGene} for binary-coded genetic algorithms.
#' \item \code{xegaPermGene} for permutation-based genetic algorithms.
#' \item \code{xegaDfGene} for derivation-free algorithms. For example, 
#'                         differential evolution.
#' \item \code{xegaGpGene} for grammar-based genetic algorithms.
#' \item \code{xegaGeGene} for grammatical evolution algorithms.
#' }
#' The packages \code{xegaDerivationTrees} and \code{xegaBNF} support
#' the last two packages:
#' \code{xegaBNF} essentially provides a grammar compiler, and 
#' \code{xegaDerivationTrees} an abstract data type for derivation trees.
#' }}
#'
#' @family Package Description
#' 
#' @name xegaDerivationTrees
#' @aliases xegaDerivationTrees
#' @docType package
#' @title Package xegaDerivationTrees
#' @author Andreas Geyer-Schulz
#' @section Copyright: (c) 2023 Andreas Geyer-Schulz
#' @section License: MIT
#' @section URL: <https://github.com/ageyerschulz/xegaDerivationTrees>
#' @section Installation: 
#' From cran with \code{install.packages("xegaDerivationTrees")}
NULL
