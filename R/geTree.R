
#' Selects k-th production rule index from a vector of production rules.
#'
#' @description \code{chooseRulek} selects the k-th production rule index 
#'      from the vector of production rule indices 
#'      in the \code{g$PT$LHS$} for a non-terminal symbol.
#'
#' @param riv  The vector of production rules indices for 
#'                    a non-terminal symbol.
#' @param k   Integer.
#'
#' @return The index of the production rule. 
#'
#' @family Choice
#'
#' @examples
#' chooseRulek(c(7, 8, 9), 9)
#' chooseRulek(as.vector(1), 9)
#' @export
chooseRulek<- function(riv, k) {return(riv[1+(k%%length(riv))])}

#' Transforms a non-terminal symbol into a 1-level derivation tree 
#' for a given k.
#'
#' @description \code{rndsubk} expands a non-terminal by a derivation
#'              specified by k and returns a 1-level derivation tree.
#'
#' @param sym      A non-terminal symbol.
#' @param k        Codon (An integer). 
#' @param PT       A production table.
#'
#' @return A 1-level derivation tree.
#'
#' @family Generate Derivation Tree
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' rndsubk(g$Start, 207, g$PT)
#'
#' @importFrom xegaBNF rules
#' @export
rndsubk<-function(sym, k, PT)
{substituteSymbol(chooseRulek(xegaBNF::rules(sym, PT$LHS), k),PT)}

#' Generates a derivation tree from an integer vector.
#'
#' @description \code{generateDerivationTree} 
#'    generates a derivation tree from an integer vector.
#'    The derivation tree may be incomplete.
#'
#' @param sym          A non-terminal symbol. 
#' @param kvec         Integer vector.
#' @param complete     Boolean. FALSE for incomplete derivation trees.
#' @param G            A grammar. 
#' @param maxdepth     The maximal depth of the derivation tree.    
#'
#' @details \code{generateDerivationTree} recursively expands 
#'         non-terminals and builds a derivation tree.
#'         See Ryan et al. (1998).         
#'
#' @return A named list l$tree, l$kvec, l$complete. 
#'
#' @references Ryan, C., Collins, J. J., and  O'Neill, M. (1998):
#'         Grammatical evolution: Evolving programs for an arbitrary language, pp. 83-96.
#'         In: 	Banzhaf, W., Poli, R., Schoenauer, M., and Fogarty, T. C. (Eds.)
#'         Genetic Programming. First European Workshop, EuroGP' 98 Paris, 
#'         France, April 14-15, 1998 Proceedings. Lecture Notes in Computer Science,
#'         Vol. 1391, Springer, Heidelberg.
#'         <doi:10.1007/BFb0055930>
#'
#' @family Generate Derivation Tree
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' a<-sample(100, 100, replace=TRUE)
#' b<-generateDerivationTree(sym=g$Start, kvec=a, G=g, maxdepth=10)
#' decodeDT(b$tree, g$ST)
#'
#' @importFrom xegaBNF isTerminal
#' @export
generateDerivationTree<-function(sym, kvec, complete=TRUE, G, maxdepth=5)
{
   if (xegaBNF::isTerminal(sym, G$ST))
   { return(list(tree=sym, kvec=kvec, complete=complete)) }

   if (length(kvec)==0)
   { return(list(tree=sym, kvec=kvec, complete=complete)) }

   tmp<-rndsubk(sym, kvec[1], G$PT)
   if (length(kvec)==1) 
   {  # cat("integers used up.\n")
      return(list(tree=tmp, kvec=vector(), complete=FALSE)) }
   nvec<-kvec[2:length(kvec)]
   symbols<-tmp[[2]]
   l<-list()
   for (i in 1:length(symbols))
   { m<-generateDerivationTree(symbols[i], nvec, complete, G, maxdepth-1)
     h<-m$tree
     nvec<-m$kvec
     complete<-m$complete
     l[[i]]<-h }
   tmp[[2]]<-l
   return(list(tree=tmp, kvec=nvec, complete=complete))
}

#' Generates \code{times} derivation trees from random 
#' integer vectors and decodes them.
#' 
#' @param times  Number of derivation trees which should be generated.
#'
#' @return Number of complete derivation trees generated.
#' 
#' @family Tests
#'
#' @examples
#' testGenerateDerivationTree(5)
#' @export 
testGenerateDerivationTree<-function(times)
{
g<-compileBNF(booleanGrammar())

cDT<-0
for (i in 1:times) 
{
a<-sample(100, 100, replace=TRUE)
b<-generateDerivationTree(sym=g$Start, kvec= a, complete=TRUE, G=g, maxdepth=10)
if (b$complete) {cDT<-cDT+1}
cat("Derivation Tree", i, "Complete:", b$complete, "\n")
cat(decodeDT(b$tree, g$ST), "\n")
}
return(cDT)
}

