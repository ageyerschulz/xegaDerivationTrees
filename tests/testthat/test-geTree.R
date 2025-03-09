
library(testthat)
library(xegaBNF)
library(xegaDerivationTrees)

test_that("chooseRulek OK", 
{
	   expect_identical(chooseRulek(c(7, 8, 9), 9), 7)
	   expect_identical(chooseRulek(as.vector(1), 5), 1)
}
)

test_that("rndsubk OK", 
{
	g<-compileBNF(booleanGrammar())
        t1<-rndsubk(g$Start, 34, g$PT)
        expect_identical(t1[[1]]==8, TRUE)
        expect_identical(all(t1[[2]]==c(6, 2, 8, 3)), TRUE)
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
        expect_equal(decodeDT(t1, g$ST), decodeDT(t2, g$ST))
        expect_equal(decodeCDT(t1, g$ST), decodeCDT(t1, g$ST))
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

test_that("testGenerateDerivationTree OK",
{
        g<-compileBNF(booleanGrammar())
        set.seed(21)
        expect_identical(
        testGenerateDerivationTree(5, g), 5)
}
)


