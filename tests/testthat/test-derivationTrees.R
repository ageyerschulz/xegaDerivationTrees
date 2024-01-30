
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
b<-"OR(OR(NOT(D1),AND(NOT(NOT(NOT(D2))),AND(D2,NOT(D2)))),OR(NOT(D1),NOT(NOT(D1))))"
        expect_equal(decodeCDT(t1, g$ST), b)
}
)

test_that("randomDerivationTree OK",
{
        g<-compileBNF(booleanGrammar())
        set.seed(21)
        t1<-randomDerivationTree(g$Start, g, CompleteDT=FALSE)
b<-"OR(OR(NOT(D1),AND(NOT(NOT(<f1>(<fe>))),AND(NOT(<f0>),NOT(<f1>(<fe>))))),D2)"
        expect_equal(decodeDT(t1, g$ST), b)
}
)

test_that("treeListDepth OK",
{
        g<-compileBNF(booleanGrammar())
        set.seed(21)
        t1<-randomDerivationTree(g$Start, g)
        expect_equal(treeListDepth(t1), 16)
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
        expect_equal(treeSize(t1), 88)
}       
)

test_that("treeNodes OK",
{
        g<-compileBNF(booleanGrammar())
        set.seed(21)
        t1<-randomDerivationTree(g$Start, g)
        expect_equal(treeNodes(t1, g$ST), 38)
}       
)

test_that("treeLeaves OK",
{
        g<-compileBNF(booleanGrammar())
        set.seed(21)
        t1<-randomDerivationTree(g$Start, g)
        expect_equal(treeLeaves(t1, g$ST), 50)
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
        expect_identical(names(t1anl), c("count", "subtreedepth", "ANL"))
        expect_equal(t1anl$count, treeSize(t1))
        expect_equal(t1anl$subtreedepth, 9)
        t1NL<-t1anl$ANL
        expect_equal(length(t1NL), treeNodes(t1, g$ST))
}
)

test_that("choodeNode OK",
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

test_that("compatibleSubtrees Cases 2 and 3 OK",
{
    g<-compileBNF(booleanGrammar())
    set.seed(21)
    t1<-randomDerivationTree(g$Start, g)
    t1anl<-treeANL(t1, g$ST)
    node1<-chooseNode(t1anl$ANL)
    t2<-randomDerivationTree(g$Start, g)
    t2anl<-treeANL(t2, g$ST)
    node2<-chooseNode(t2anl$ANL)
    expect_identical(compatibleSubtrees(node1,node2),FALSE) 
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
    expect_identical(decodeCDT(st1, g$ST), "D2")
}
)

test_that("treeInsert OK",
{
    g<-compileBNF(booleanGrammar())
    set.seed(21)
    t1<-randomDerivationTree(g$Start, g)
    t1anl<-treeANL(t1, g$ST)
    node1<-chooseNode(t1anl$ANL)
    node1<-chooseNode(t1anl$ANL)
    t2<-randomDerivationTree(node1$ID, g)
    t3<-treeInsert(t1, t2, node1)
    expect_identical(decodeCDT(t3, g$ST), 
     "OR(OR(NOT(D1),NOT(D2)),OR(NOT(D1),NOT(NOT(D1))))")
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

