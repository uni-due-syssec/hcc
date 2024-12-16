package syssec.hcc

import org.neo4j.graphdb.Direction
import org.neo4j.graphdb.GraphDatabaseService
import org.neo4j.graphdb.Node

val traditionalREQuery_conservative = """
match (a :AssignmentStatement)-[:LEFT_HAND_SIDE]->()-[* 0..]->(id: IdentifierExpression)-[:REFERENCES]->(sth :Variable)
where sth:StateVariable or (sth:LocalVariable and sth.storageLocation = "storage")
match (c:ContainsExternalCall)-[:LEADS | TRUE_BODY | FALSE_BODY | LOOP_BODY *]->(a)
match (cond :Statement)-[:TRUE_BODY | FALSE_BODY | LOOP_BODY | LEADS *]->(c)-[:LEADS *]->(a)
match (cond)-[:CONDITION]->()-[* 0..]-[:REFERENCES]->(sth)
match (f: Function)-[BODY]-()-[:LEADS | TRUE_BODY | FALSE_BODY | LOOP_BODY *]-(c)
return distinct a, sth, c, f
"""

val traditionalREQuery =
    """
    match (a :AssignmentStatement)-[:HAS_IDENTIFIER]->(id: IdentifierExpression)-[:REFERENCES]->(sth :Variable)
    where (sth:StateVariable or (sth:LocalVariable and sth.storageLocation = "storage"))
    match (c:ContainsExternalCall)-[:LEADS | TRUE_BODY | FALSE_BODY | LOOP_BODY *]->(a)
    match (f: Function)-[:HAS_STATEMENT]-(c)
    match (cont: Contract)-[HAS_FUNCTION]->(f)
    return distinct a, sth, c, f, cont
    """.trimIndent()

val crossFuncsQuery =
    """
    match (c)-[:HAS_FUNCTION]->(f:Function)-[:WRITES]->(sv:StateVariable)
    where Id(c) = %d and Id(sv) = %d and Id(f) <> %d and (f.visibility = "public" or f.visibility = "external")
    return distinct f
    """.trimIndent()

val delegatedCase1Query =
    """
    match (ec :ContainsExternalCall)-[:LEADS | TRUE_BODY | FALSE_BODY | LOOP_BODY *]->(d :ContainsDelegateCall)
    match (f: Function)-[:HAS_STATEMENT]->(ec)
    where ec <> d
    return distinct d, ec, f
    """.trimIndent()

val createBasedREQuery =
    """
    match (a :AssignmentStatement)-[:HAS_IDENTIFIER]->(id: IdentifierExpression)-[:REFERENCES]->(sth :Variable)
    where (sth:StateVariable or (sth:LocalVariable and sth.storageLocation = "storage"))
    match (c:ContainsConstructorCall:Statement)-[r* 0..]->(cc:ConstructorCall)-[:CALL_EXPRESSION]->(ne: NewExpression)-[:TYPE]->()<-[:DEFINES_TYPE]-(:Contract)-[:HAS_CONSTRUCTOR]->(:Constructor:ContainsExternalCall)
    where none(x in r where type(x) in ["LEADS", "LOOP_BODY", "TRUE_BODY", "FALSE_BODY"])
    match (c)-[:LEADS | TRUE_BODY | FALSE_BODY | LOOP_BODY *]->(a)
    match (f: Function)-[:HAS_STATEMENT]-(c)
    match (cont: Contract)-[HAS_FUNCTION]->(f)
    return distinct a, sth, c, f, cont
    """.trimIndent()

fun analyzeMainRE(graphDb: GraphDatabaseService) {
    val tx = graphDb.beginTx()
    val res = tx.execute(traditionalREQuery)

    res.forEach { re ->
        val assignment = re["a"] as Node
        val call = re["c"] as Node
        val sv = re["sth"] as Node
        val function = re["f"] as Node
        val contract = re["cont"] as Node
        val otherFuncs = mutableListOf<Node>()
        val oF = tx.execute(crossFuncsQuery.format(contract.id, sv.id, function.id))
        oF.forEach {
            otherFuncs += it["f"] as Node
        }

        val ren = tx.createNode()
        ren.addLabel { NodeType.Reentrancy.name }
        if (otherFuncs.isEmpty()) {
            ren.addLabel { NodeType.TraditionalReentrancy.name }
        } else {
            ren.addLabel { NodeType.CrossFunctionReentrancy.name }
            otherFuncs.forEach {
                ren.createRelationshipTo(it) { Relationship.RE_CROSS_FUNCTION.name }
            }
        }
        if (call.hasLabel { NodeType.ContainsDelegateCall.name }) {
            ren.addLabel { NodeType.DelegatedReentrancy.name }
            ren.setProperty("case", 2)
        }
        ren.addLabel { NodeType.Vulnerability.name }
        ren.createRelationshipTo(assignment) { Relationship.RE_ASSIGNMENT.name }
        ren.createRelationshipTo(call) { Relationship.RE_CALL.name }
        ren.createRelationshipTo(sv) { Relationship.RE_STATE_VARIABLE.name }
        ren.createRelationshipTo(function) { Relationship.RE_FUNCTION.name }
        reportReentrancy(ren, contract, function, sv, call, assignment, otherFuncs)
    }

    tx.commit()
}

fun analyzeDelegateCase1RE(graphDb: GraphDatabaseService) {
    val tx = graphDb.beginTx()
    val res = tx.execute(delegatedCase1Query)

    res.forEach { re ->
        val delegateCall = re["d"] as Node
        val call = re["ec"] as Node
        val function = re["f"] as Node
        val otherFuncs =
            function
                .getRelationships(Direction.INCOMING)
                .filter { it.isType { Relationship.HAS_FUNCTION.name } }
                .map { contract ->
                    contract.endNode
                        .getRelationships(Direction.OUTGOING)
                        .filter { it.isType { Relationship.HAS_FUNCTION.name } }
                        .map { it.endNode }
                }[0]
                .filter {
                    it.id != function.id &&
                        (it.getProperty("visibility") as String == "external" || it.getProperty("visibility") as String == "public")
                }

        val ren = tx.createNode()
        ren.addLabel { NodeType.Reentrancy.name }
        ren.addLabel { NodeType.DelegatedReentrancy.name }
        ren.setProperty("case", 1)
        ren.addLabel { NodeType.Vulnerability.name }
        if (otherFuncs.isNotEmpty()) {
            ren.addLabel { NodeType.CrossFunctionReentrancy.name }
            otherFuncs.forEach {
                ren.createRelationshipTo(it) { Relationship.RE_CROSS_FUNCTION.name }
            }
        }
        ren.createRelationshipTo(call) { Relationship.RE_CALL.name }
        ren.createRelationshipTo(delegateCall) { Relationship.RE_DELEGATE_CALL.name }
        ren.createRelationshipTo(function) { Relationship.RE_FUNCTION.name }
        reportReentrancy(ren, function, call, delegateCall, otherFuncs)
    }

    tx.commit()
}

fun analyzeCreateBasedRE(graphDb: GraphDatabaseService) {
    val tx = graphDb.beginTx()
    val res = tx.execute(createBasedREQuery)

    res.forEach { re ->
        val assignment = re["a"] as Node
        val sv = re["sth"] as Node
        val call = re["c"] as Node
        val contract = re["cont"] as Node
        val function = re["f"] as Node
        val oF = tx.execute(crossFuncsQuery.format(contract.id, sv.id, function.id))
        val otherFuncs = mutableListOf<Node>()
        oF.forEach {
            otherFuncs += it["f"] as Node
        }

        val ren = tx.createNode()
        ren.addLabel { NodeType.Reentrancy.name }
        if (otherFuncs.isEmpty()) {
            ren.addLabel { NodeType.TraditionalReentrancy.name }
        } else {
            ren.addLabel { NodeType.CrossFunctionReentrancy.name }
            otherFuncs.forEach {
                ren.createRelationshipTo(it) { Relationship.RE_CROSS_FUNCTION.name }
            }
        }
        ren.addLabel { NodeType.Vulnerability.name }
        ren.addLabel { NodeType.CreateBasedReentrancy.name }
        ren.createRelationshipTo(call) { Relationship.RE_CALL.name }
        ren.createRelationshipTo(assignment) { Relationship.RE_ASSIGNMENT.name }
        ren.createRelationshipTo(function) { Relationship.RE_FUNCTION.name }
        ren.createRelationshipTo(sv) { Relationship.RE_STATE_VARIABLE.name }
        reportReentrancy(ren, contract, function, sv, call, assignment, otherFuncs, "Create-Based Reentrancy")
    }

    tx.commit()
}
