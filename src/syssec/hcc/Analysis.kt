package syssec.hcc

import org.neo4j.graphdb.GraphDatabaseService


fun securityAnalysisPass(graphDb: GraphDatabaseService){
    if(Mappings.patchRE){
        analyzeMainRE(graphDb)
        log("RE-1 finished.")
        analyzeDelegateCase1RE(graphDb)
        log("RE-2 finished.")
        analyzeCreateBasedRE(graphDb)
        log("RE-3 finished.")
    }
    if(Mappings.patchIO){
        analyzeIntegerBugs(graphDb)
        log("Integer Bug Analysis finished.")
        analyzeUnaryOperation(graphDb)
        log("Unary operation finished.")
        analyzeTruncationBug(graphDb)
        log("Truncation Bug Analysis finished.")
    }
}