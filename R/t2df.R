
#' Returns empty data frame for vertices.
#' 
#' @description Data frame with columns 
#'              \code{id} (numerical identifier) and 
#'              \code{name} (symbol of the grammar).
#'              
#' @return Data frame.
#'
#' @example
#' df<-newV()
#'
#' @export
newV<-function()
{return(data.frame(id<-vector(), name=list()))}

#' Returns empty data frame for edges.
#'
#' @description Data frame with columns 
#'              \code{from} root (numerical identifier) and 
#'              \code{to} kid (numerical identifier).
#'              
#' @return Data frame.
#'
#' @example
#' df<-newE()
#'
#' @export
newE<-function()
{return(data.frame(from<-vector(), to<-vector()))}

#' Add a vertex to the data frame for vertices. 
#'
#' @param  df  Data frame for vertices.
#' @param  id  Integer (numerical identifier of vertex).
#' @param  name  Name of grammar symbol of of vertex.
#'
#' @return A Data frame for vertices.
#'
#' @example
#' df<-addV(newV(), 1, "<fe>")
#'
#' @export
addV<-function(df, id, name)
{return(rbind(df, data.frame(id=id, name=name)))}

#' Add an edge or a list of edges to the data frame for edges. 
#'
#' @description Adds the edges of the kids of a node to the edge list.
#'
#' @param  df  Data frame for edges.
#' @param  from  Integer (numerical identifier of vertex).
#' @param  to    Vector of integers (numerical identifiers of kids of vertex).
#'
#' @return A Data frame for edges.
#'
#' @example
#' df<-addE(newE(), 1, c(2, 3, 4))
#'
#' @export
addE<-function(df, from, to)
{fl<-rep(from, length(to)) 
 return(rbind(df, data.frame(from=fl, to=to)))}

#' Convert a tree to two dataframes.
#'
#' @description Converts a derivation tree into a list of two data frames.
#'              With the R-package \code{igraph} the data frames 
#'              can be plotted as a derivation tree.
#'
#' @details Does not work with incomplete derivation trees.
#'
#' @param tree     Derivation tree.
#' @param G        The context-free grammar.
#' @param verbose  If TRUE, print derivations on console. 
#'                 Default: FALSE.
#'
#' @return A named list with two data frames:
#'         \enumerate{
#'         \item The data frame \code{$V} of vertices with the columns
#'               \code{$V$id} (numerical identifier) and \code{$V$name}
#'               (symbol of the grammar G).
#'         \item The data frame \code{$E} of edges with the columns 
#'               \code{$E$from} and \code{$E$to$}.
#'               }
#'
#' @family Conversion
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' a<-randomDerivationTree(g$Start, g)
#' x<-treeToDataFrames(a, g, verbose=TRUE)
#' # library(igraph) 
#' # g1<-graph_from_data_frame(x$E, directed=TRUE, vertices=x$V)
#' # plot(g1, layout=layout_as_tree)
#' 
#' @importFrom utils tail
#' @importFrom xegaBNF isTerminal
#' @export
treeToDataFrames<-function(tree, G, verbose)
{ id<-1; V<-newV(); E<-newE()
  r<-treeRoot(tree)
  V<-addV(V, id, decodeSymVec(r, G$ST))
  if (verbose) {cat("  ",decodeSymVec(r, G$ST), "\n")}
  pt<-treeChildren(tree) 
  post<-unlist(lapply(pt, FUN=treeRoot))
  pid<-id+(1:length(post))
  E<-addE(E, from=id, to=pid); 
  id<-id+length(post)
  for (j in (1:length(post))) 
    {V<-addV(V, id=pid[j], name=decodeSymVec(post[j], G$ST))} 
  if (verbose) {cat("=>",decodeSymVec(post, G$ST), "\n")}
  pre<-vector()
  while (!length(post)==0)
  { if (xegaBNF::isTerminal(post[1], G$ST))
    {pt<-utils::tail(pt, -1) 
     pre<-unlist(c(pre,post[1]))
     post<-utils::tail(post, -1)
     pid<-utils::tail(pid,-1)} 
    else 
       {fst<-treeChildren(pt[[1]])
        post<-unlist(c((lapply(fst, FUN=treeRoot)), utils::tail(post,-1)))
        fstsym<-unlist((lapply(fst, FUN=treeRoot)))
        newids<-id+(1:length(fstsym))
        E<-addE(E, from=pid[1], to=newids)
        id<-id+length(newids)
        for (j in (1:length(newids))) 
           {V<-addV(V, id=newids[j], name=decodeSymVec(fstsym[j], G$ST))} 
        pid<-c(newids, utils::tail(pid, -1))
        pt<-c(fst, utils::tail(pt, -1))
        line<-c(pre, post)
        if (verbose) {cat("=>", decodeSymVec(line, G$ST), "\n")}
        } }
 return(list(V=V, E=E))  
}

