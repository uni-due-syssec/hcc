package syssec.hcc

import org.apache.commons.lang3.StringUtils
import org.neo4j.graphdb.Direction
import org.neo4j.graphdb.Node
import java.time.LocalDateTime


fun log(msg:String, category: String = "HCC") {
    val time = LocalDateTime.now()
    val cat = StringUtils.center(category, 7)
    println("[$cat] $time => $msg")
}

fun reportIntegerFlow(exp: Node, patch: Node, keyword: String = "Overflow"){
    val expCode = emitExpressionStatementCode(exp, true)
    val patchCode = emitExpressionStatementCode(patch)
    val report = "Patched Integer $keyword in ${exp.id}:\n  ORIGINAL\n\t$expCode\n  PATCH\n\t$expCode\n\t$patchCode"
    log(report, "PATCH")
}

fun reportUnderflow(patch: Node, exp: Node){
    val patchCode = emitExpressionStatementCode(patch)
    val expCode = emitExpressionStatementCode(exp)
    val report = "Patched Integer Underflow in ${exp.id}:\n  ORIGINAL\n\t$expCode\n  PATCH\n\t$patchCode\n\t$expCode"
    log(report, "PATCH")
}

fun reportTruncation(stmt: Node, exp: Node, patch: Node) {
    val stmtCode = getStatementCode(stmt)
    val expCode = emitExpressionCode(exp)
    val patchCode = getStatementCode(patch)
    val report = """ |Patched Truncation Bug in $expCode:
    |  ORIGINAL:
    |        $stmtCode
    |  PATCHED
    |        $patchCode
    |        $stmtCode
    """.trimMargin()
    log(report, "PATCH")
}

fun reportReentrancy(re: Node, contract: Node, function: Node, stateVariable: Node, call: Node, assignment: Node, otherFunctions: List<Node>, kind: String = "Reentrancy") {
    val contractName = contract.getProperty("name") as String
    val name = function.getProperty("name") as String
    val c = call.getRelationships(Direction.OUTGOING).first { it.isType {Relationship.HAS_CALL.name } }.endNode
    val report = """ |$kind (${re.id}) in function $name of contract $contractName:
        |  Affected State Variable: ${stateVariable.getProperty("name") as String}
        |  Call Statement:          ${emitExpressionStatementCode(c)}
        |  Assignment:              ${emitAssignmentCode(assignment)}
        |  ${otherFunctions.joinToString(prefix = "Cross Functions:         => ", separator = "\n                              ") { it.getProperty("name") as String }}
        |  Code
        |${emitFunctionCode(function).split("\n").joinToString(prefix = "\t", separator = "\n\t")}
    """.trimMargin()
    log(report, "BUG")
}

fun reportReentrancy(re: Node, function: Node, call: Node, delegateCall: Node, otherFunctions: List<Node>){
    val contractName = function.getRelationships(Direction.INCOMING).first { it.isType { Relationship.HAS_FUNCTION.name } }.startNode.getProperty("name") as String
    val name = function.getProperty("name") as String
    val c = call.getRelationships(Direction.OUTGOING).first { it.isType {Relationship.HAS_CALL.name } }.endNode
    val dc = delegateCall.getRelationships(Direction.OUTGOING).first { it.isType {Relationship.HAS_CALL.name } }.endNode
    val report = """ |Delegated Reentrancy (${re.id}) in function $name of contract $contractName:
        |  Call Statement:          ${emitExpressionStatementCode(c)}
        |  Delegate Call:           ${emitExpressionStatementCode(dc)}
        |  ${otherFunctions.joinToString(prefix = "Cross Functions:         => ", separator = "\n                              ") { it.getProperty("name") as String }}
        |  Code
        |${emitFunctionCode(function).split("\n").joinToString(prefix = "\t", separator = "\n\t")}
    """.trimMargin()
    log(report, "BUG")
}

fun reportReentrancyPatch(re: Node, function: Node, call: Node, assignment: Node, lockName: String) {
    val contractName = function.getRelationships(Direction.INCOMING).first { it.isType { Relationship.HAS_FUNCTION.name } }.startNode.getProperty("name") as String
    val name = function.getProperty("name") as String
    val c = call.getRelationships(Direction.OUTGOING).first { it.isType {Relationship.HAS_CALL.name } }.endNode
    val report = """ |Reentrancy (${re.id}) in function $name of contract $contractName:
        |  Call Statement:          ${emitExpressionStatementCode(c)}
        |  Assignment:              ${emitExpressionStatementCode(assignment)}
        |  Introduced Lock:         $lockName
        |  Code [after patch application]
        |${emitFunctionCode(function).split("\n").joinToString(prefix = "\t", separator = "\n\t")}
    """.trimMargin()
    log(report, "PATCH")
}

fun reportDC1ReentrancyPatch(re: Node, function: Node, call: Node, delegateCall: Node, lockName: String) {
    val contractName = function.getRelationships(Direction.INCOMING).first { it.isType { Relationship.HAS_FUNCTION.name } }.startNode.getProperty("name") as String
    val name = function.getProperty("name") as String
    val c = call.getRelationships(Direction.OUTGOING).first { it.isType {Relationship.HAS_CALL.name } }.endNode
    val dc = delegateCall.getRelationships(Direction.OUTGOING).first { it.isType {Relationship.HAS_CALL.name } }.endNode
    val report = """ |Delegated Reentrancy (${re.id}) in function $name of contract $contractName:
        |  Call Statement:          ${emitExpressionStatementCode(c)}
        |  Delegate Call:           ${emitExpressionStatementCode(dc)}
        |  Introduced Lock:         $lockName
        |  Code [after patch application]
        |${emitFunctionCode(function).split("\n").joinToString(prefix = "\t", separator = "\n\t")}
    """.trimMargin()
    log(report, "PATCH")
}