#' Decodes a vector of symbols.
#'
#' @param v  Vector of symbols.
#' @param ST Symbol table.
#' 
#' @return  A program.
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' a<-randomDerivationTree(g$Start, g)
#' r<-treeRoot(a)
#' decodeSymVec(r, g$ST)
#' c<-unlist(lapply(treeChildren(a), FUN=treeRoot))
#' decodeSymVec(c, g$ST)
#'
#' @export
decodeSymVec<-function(v, ST)
{ return(Reduce(unlist(lapply(ST$Symbols[v],as.character)), f=paste0)) }

#' Print derivations.
#'
#' @description A depth-first left-to-right tree traversal
#'               without recursion. 
#'
#' @details Does not work with incomplete derivation trees.
#' 
#' @param tree     Derivation tree.
#' @param G        The context-free grammar.
#' @param verbose  If TRUE, the list of derivations is printed.
#'                 Default: FALSE.  
#'
#' @return A list of derivations.
#'
#' @family Diagnostics
#'
#' @examples
#'
#' g<-compileBNF(booleanGrammar())
#' a<-randomDerivationTree(g$Start, g)
#' l<-printDerivations(a, g, verbose=TRUE) 
#'
#' @importFrom utils tail
#' @importFrom xegaBNF isTerminal
#' @export
printDerivations<-function(tree, G, verbose=FALSE)
{ l<-list(); i<-3
  r<-treeRoot(tree)
  if (verbose) {cat("  ",decodeSymVec(r, G$ST), "\n")}
  l[[1]]<-decodeSymVec(r, G$ST)
  pt<-treeChildren(tree) 
  post<-unlist(lapply(pt, FUN=treeRoot))
  if (verbose) {cat("=>",decodeSymVec(post, G$ST), "\n")}
  l[[2]]<-decodeSymVec(post, G$ST)
  pre<-vector()
  while (!length(post)==0)
  { if (xegaBNF::isTerminal(post[1], G$ST))
       {pt<-utils::tail(pt, -1)
        pre<-unlist(c(pre,post[1]))
        post<-utils::tail(post, -1)} 
   else  # branch for non-expanded terminals is missing.
       { fst<-treeChildren(pt[[1]])
        post<-unlist(c((lapply(fst, FUN=treeRoot)), utils::tail(post,-1)))
        pt<-c(fst, utils::tail(pt, -1))
        line<-c(pre, post)
        if (verbose) {cat("=>", decodeSymVec(line, G$ST), "\n")}
        l[[i]]<-decodeSymVec(line, G$ST); i<-i+1
        } }
 return(l)  
}

