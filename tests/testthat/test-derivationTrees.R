
library(testthat)
library(xegaBNF)
library(xegaDerivationTrees)

test_that("booleanGrammar OK", 
{
           G<-booleanGrammar() 
           fn<-"~/dev/cran/xega/xegaBNF/BooleanGrammar.txt"
	   expect_identical(G$filename, fn)
	   expect_identical("BNF" %in% names(G), TRUE)
}
)

test_that("chooseRule OK", 
{
	   expect_identical(chooseRule(c(7, 8, 9)) %in% c(7, 8, 9) , TRUE)
	   expect_identical(chooseRule(as.vector(1)), 1)
	   expect_identical(chooseRule(as.vector(5)), 5)
}
)

test_that("substituteSymbol OK", 
{
	g<-compileBNF(booleanGrammar())
        t1<-substituteSymbol(3, g$PT)
	expect_identical(t1[[1]], g$PT$LHS[3])
        expect_identical(t1[[2]], g$PT$RHS[3][[1]])
}
)

test_that("rndsub OK", 
{
	g<-compileBNF(booleanGrammar())
        set.seed(4)
        t1<-rndsub(g$Start, g$PT)
        set.seed(4)
        r<-chooseRule(xegaBNF::rules(g$Start, g$PT$LHS))  
	expect_identical(t1[[1]], g$PT$LHS[r])
        expect_identical(t1[[2]], g$PT$RHS[r][[1]])
}
)

test_that("randomDerivationTree OK",          
{
        g<-compileBNF(booleanGrammar())
        set.seed(1)
        t1<-randomDerivationTree(g$Start, g)
        expect_equal(t1[[1]], g$Start)
        expect_equal(t1[[2]][[1]][[1]], 5)
        expect_equal(t1[[2]][[1]][[2]][[1]], 11)
}
)

test_that("randomDerivationTree OK",          
{
        g<-compileBNF(booleanGrammar())
        set.seed(21)
        t1<-randomDerivationTree(g$Start, g)
        set.seed(21)
        t2<-randomDerivationTree(g$Start, g)
        expect_equal(decodeCDT(t1, g$ST), decodeCDT(t2, g$ST))
}
)

test_that("randomDerivationTree OK",
{
        g<-compileBNF(booleanGrammar())
        set.seed(21)
        t1<-randomDerivationTree(g$Start, g, CompleteDT=FALSE)
        set.seed(21)
        t2<-randomDerivationTree(g$Start, g, CompleteDT=FALSE)
        expect_equal(decodeDT(t1, g$ST), decodeDT(t2, g$ST))
}
)

test_that("rndPartition k=1 OK",
{
n<-sample(2:10,1)
a1<-rndPartition(n, 1)
expect_equal(n, a1)
}       
)


test_that("rndPartition k=n OK",
{
n<-sample(2:10,1)
a1<-rndPartition(n, n)
expect_identical(rep(1,n), a1)
}
)

test_that("rndPartition (k in (2:(n-1)), n OK",
{
n<-sample(2:10,1)
k<-sample(2:9,1)
a1<-rndPartition(n, k)
expect_equal(sum(a1), n)
expect_identical(all(a1>0), TRUE)
expect_identical(all(a1<n), TRUE)
}
)

test_that("treeListDepth OK",
{
        g<-compileBNF(booleanGrammar())
        set.seed(21)
        t1<-randomDerivationTree(g$Start, g)
        set.seed(21)
        t2<-randomDerivationTree(g$Start, g)
        expect_equal(treeListDepth(t1), treeListDepth(t2))
}       
)

test_that("treeListDepth OK",
{
        t1<-list()
        expect_equal(treeListDepth(t1), 0)
}       
)

test_that("treeSize OK",
{
        g<-compileBNF(booleanGrammar())
        set.seed(21)
        t1<-randomDerivationTree(g$Start, g)
        set.seed(21)
        t2<-randomDerivationTree(g$Start, g)
        expect_equal(treeSize(t1), treeSize(t2))
}       
)

test_that("treeNodes OK",
{
        g<-compileBNF(booleanGrammar())
        set.seed(21)
        t1<-randomDerivationTree(g$Start, g)
        set.seed(21)
        t2<-randomDerivationTree(g$Start, g)
        expect_equal(treeNodes(t1, g$ST), treeNodes(t2, g$ST))
}       
)

test_that("treeLeaves OK",
{
        g<-compileBNF(booleanGrammar())
        set.seed(21)
        t1<-randomDerivationTree(g$Start, g)
        set.seed(21)
        t2<-randomDerivationTree(g$Start, g)
        expect_equal(treeLeaves(t1, g$ST), treeLeaves(t2, g$ST))
}       
)

test_that("treeRoot OK",
{
        g<-compileBNF(booleanGrammar())
        set.seed(21)
        t1<-randomDerivationTree(g$Start, g)
        expect_equal(treeRoot(t1), 8)
}       
)

test_that("treeChildren OK",
{
        g<-compileBNF(booleanGrammar())
        set.seed(21)
        t1<-randomDerivationTree(g$Start, g)
        expect_equal(length(treeChildren(t1)), 6)
}       
)

test_that("treeANL OK",
{
        g<-compileBNF(booleanGrammar())
        set.seed(21)
        t1<-randomDerivationTree(g$Start, g)
        t1anl<-treeANL(t1, g$ST)
        set.seed(21)
        t2<-randomDerivationTree(g$Start, g)
        t2anl<-treeANL(t2, g$ST)
        expect_identical(names(t1anl), c("count", "subtreedepth", "ANL"))
        expect_equal(t1anl$count, treeSize(t1))
        expect_equal(t1anl$subtreedepth, t2anl$subtreedepth)
        t1NL<-t1anl$ANL
        expect_equal(length(t1NL), treeNodes(t1, g$ST))
}
)

test_that("chooseNode OK",
{
    g<-compileBNF(booleanGrammar())
    set.seed(21)
    t1<-randomDerivationTree(g$Start, g)
    t1anl<-treeANL(t1, g$ST)
    node<-chooseNode(t1anl$ANL)
    expect_identical(names(node), 
    c("ID", "NonTerminal", "Pos", "Depth", "Rdepth", "subtreedepth", "Index"))  
}
)

test_that("filterANL OK",
{
    g<-compileBNF(booleanGrammar())
    set.seed(111)
    t1<-randomDerivationTree(g$Start, g, maxdepth=10)
    t1anl<-treeANL(t1, g$ST)
    expect_identical(names(t1anl), c("count", "subtreedepth", "ANL"))
    expect_equal(t1anl$count, 17)
    expect_equal(t1anl$subtreedepth, 5)
    t1f1<-filterANL(t1anl, minb=1, maxb=10)
    expect_identical(t1anl$ANL, t1f1$ANL)
    t1f2<-filterANL(t1anl, minb=1, maxb=3)
    expect_identical(identical(t1anl$ANL, t1f2$ANL), FALSE)
    t1f3<-filterANL(t1anl, minb=5, maxb=10)
    expect_identical(identical(t1anl$ANL, t1f3$ANL), TRUE)

}
)

test_that("filterANLid OK",
{   
    g<-compileBNF(booleanGrammar())
    set.seed(111)
    t1<-randomDerivationTree(g$Start, g, maxdepth=10)
    t1anl<-treeANL(t1, g$ST)
    t1s8<-filterANLid(t1anl, nodeID=8)
    expect_equal(length(t1s8$ANL), 4)
    t1s7<-filterANLid(t1anl, nodeID=7)
    expect_equal(length(t1s7$ANL), 1)
    t1s6<-filterANLid(t1anl, nodeID=6)
    expect_equal(length(t1s6$ANL), 1)
    t1s5<-filterANLid(t1anl, nodeID=5)
    expect_equal(length(t1s5$ANL), 2)
    t1s9<-filterANLid(t1anl, nodeID=9)
    expect_equal(length(t1s9$ANL), 0)
}
)

test_that("compatibleSubtrees Case 1: different non-terminal ids: FALSE.",
{
    g<-compileBNF(booleanGrammar())
    set.seed(26)
    t1<-randomDerivationTree(g$Start, g)
    t1anl<-treeANL(t1, g$ST)
    set.seed(1)
    node1<-chooseNode(t1anl$ANL)
    set.seed(24)
    t2<-randomDerivationTree(g$Start, g)
    t2anl<-treeANL(t2, g$ST)
    set.seed(2)
    node2<-chooseNode(t2anl$ANL)
    expect_identical(compatibleSubtrees(node1,node2),FALSE) 
}
)


test_that("compatibleSubtrees Case 2: same non-terminal ids/no bound: TRUE.",
{
    g<-compileBNF(booleanGrammar())
    set.seed(26)
    t1<-randomDerivationTree(g$Start, g)
    t1anl<-treeANL(t1, g$ST)
    set.seed(1)
    node1<-chooseNode(t1anl$ANL)
    set.seed(24)
    t2<-randomDerivationTree(g$Start, g)
    t2anl<-treeANL(t2, g$ST)
    set.seed(1)
    node2<-chooseNode(t2anl$ANL)
    expect_identical(compatibleSubtrees(node1,node2, maxdepth=10),TRUE) 
    expect_identical(compatibleSubtrees(node1,node2, DepthBounded=FALSE),TRUE) 
}
)

test_that("compatibleSubtrees Case 1 OK",
{
    g<-compileBNF(booleanGrammar())
    set.seed(1)
    t1<-randomDerivationTree(g$Start, g)
    t1anl<-treeANL(t1, g$ST)
    node1<-chooseNode(t1anl$ANL)
    node2<-chooseNode(t1anl$ANL)
    expect_identical(compatibleSubtrees(node1,node2),TRUE) 
    node3<-chooseNode(t1anl$ANL)
    node4<-chooseNode(t1anl$ANL)
    expect_identical(compatibleSubtrees(node3,node4),FALSE) 
}
)

test_that("treeExtract OK",
{
    g<-compileBNF(booleanGrammar())
    set.seed(1)
    t1<-randomDerivationTree(g$Start, g)
    t1anl<-treeANL(t1, g$ST)
    node1<-chooseNode(t1anl$ANL)
    st1<-treeExtract(t1, node1)
    set.seed(1)
    t2<-randomDerivationTree(g$Start, g)
    t2anl<-treeANL(t2, g$ST)
    node2<-chooseNode(t2anl$ANL)
    st2<-treeExtract(t2, node2)
    expect_identical(decodeCDT(st1, g$ST), decodeCDT(st2, g$ST))
}
)

test_that("treeInsert OK",
{
    g<-compileBNF(booleanGrammar())
    set.seed(21)
    t1<-randomDerivationTree(g$Start, g)
    t1anl<-treeANL(t1, g$ST)
    set.seed(21)
    node1<-chooseNode(t1anl$ANL)
    t2<-randomDerivationTree(node1$ID, g)
    t3<-treeInsert(t1, t2, node1)
    expect_identical(
      grepl(decodeCDT(t2, g$ST), decodeCDT(t3, g$ST), fixed=TRUE), TRUE) 
}
)

test_that("decodeTree OK",
{
    g<-compileBNF(booleanGrammar())
    set.seed(1)
    t1<-randomDerivationTree(g$Start, g)
    a<-decodeTree(t1, g$ST)
    expect_identical(a, c(   "<fe>", "<f0>", "D2"))
}
)

test_that("decodeCDT OK",
{
    g<-compileBNF(booleanGrammar())
    set.seed(1)
    t1<-randomDerivationTree(g$Start, g)
    a<-decodeCDT(t1, g$ST)  
    expect_identical(a, "D2")
}
)

test_that("leavesIncompleteDT OK",
{
    g<-compileBNF(booleanGrammar())
    set.seed(5)
    t1<-randomDerivationTree(g$Start, g)
    a<-leavesIncompleteDT(t1, g$ST)  
    expect_identical(all(unlist(a)==c(12, 2, 10, 3)), TRUE)
    expect_identical(all(decodeDTsym(t1, g$ST)== c("NOT", "(", "D1", ")")), TRUE)
}
)

