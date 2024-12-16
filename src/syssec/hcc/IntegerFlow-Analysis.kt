package syssec.hcc

import org.neo4j.graphdb.Direction
import org.neo4j.graphdb.GraphDatabaseService
import org.neo4j.graphdb.Node
import org.neo4j.graphdb.Transaction


val loopBasedUnaryQuery = """
    match (t)<-[:TYPE]-(sub)<-[:SUB_EXPRESSION]-(s:UnaryOperation)
    match (s)<-[:LOOP_EXPRESSION]-(st)
    match (st)-[:LOOP_LEAD]->(body)<-[:LOOP_LEAD]-(blk)
    return s, sub, t, blk
""".trimIndent()

val normalUnaryQuery = """
    match (t)<-[:TYPE]-(sub)<-[:SUB_EXPRESSION]-(s:UnaryOperation)
    match (s)<-[:HAS_ARITHMETIC]-(stmt)
    match (st)-[:LEADS]->(stmt)
    return s, sub, t, st
""".trimIndent()

val truncationBugQuery = """
    match (t:TypeConversion)
    match (stmt:Statement)-[HAS_CALL]->(t)
    match (t)-[:CALL_EXPRESSION]->(tt)
    match (t)-[:HAS_ARGUMENT]->(expr)
    return distinct stmt, tt as targetType, expr
""".trimIndent()

val binOpQuery = """
    match (ex)-[r]->(ae: ArithmeticExpression)
    where not ex:TupleExpression and not ex:ArithmeticExpression and not ae:UnaryOperation and not type(r) in ["HAS_ARITHMETIC"]
    match (s:Statement)-[:HAS_ARITHMETIC]->(ae)
    return distinct ae as expression, s as statement
    union
    match (ex2)-[r2]->(t:TupleExpression)-[:HAS_COMPONENT]->(ae2:ArithmeticExpression)
    where not ex2:ArithmeticExpression and not ae2:UnaryOperation and not type(r2) in ["HAS_ARITHMETIC"]
    match (s2:Statement)-[:HAS_ARITHMETIC]->(ae2)
    return distinct ae2 as expression, s2 as statement
""".trimIndent()

fun analyzeUnaryOperation(graphDb: GraphDatabaseService){
    val tx = graphDb.beginTx()
    val loopRes = tx.execute(loopBasedUnaryQuery)
    loopRes.forEach {
        val expr = it["s"] as Node
        val vrb = it["sub"] as Node
        val type = it["t"] as Node
        val loopblk = it["blk"] as Node

        val ov = tx.createNode()
        ov.addLabel {NodeType.UnaryOverflow.name}
        ov.addLabel {NodeType.Vulnerability.name}
        ov.createRelationshipTo(type) {Relationship.TYPE.name}
        ov.createRelationshipTo(expr) {Relationship.SUB_EXPRESSION.name}
        ov.createRelationshipTo(vrb) {Relationship.HAS_ARGUMENT.name}
        ov.createRelationshipTo(loopblk) {Relationship.PREVIOUS_NODE.name}
    }

    val normRes = tx.execute(normalUnaryQuery)
    normRes.forEach {
        val expr = it["s"] as Node
        val vrb = it["sub"] as Node
        val type = it["t"] as Node
        val prev = it["st"] as Node

        val ov = tx.createNode()
        ov.addLabel {NodeType.UnaryOverflow.name}
        ov.addLabel {NodeType.Vulnerability.name}
        ov.createRelationshipTo(type) {Relationship.TYPE.name}
        ov.createRelationshipTo(expr) {Relationship.SUB_EXPRESSION.name}
        ov.createRelationshipTo(vrb) {Relationship.HAS_ARGUMENT.name}
        ov.createRelationshipTo(prev) {Relationship.PREVIOUS_NODE.name}
    }

    tx.commit()
}

fun analyzeIntegerBugs(graphDb: GraphDatabaseService) {
    val tx = graphDb.beginTx()
    val bugs = tx.execute(binOpQuery)
    bugs.forEach { bug ->
        val stmt = bug["statement"] as Node
        val exp = bug["expression"] as Node
        val msg = """
            Integer Bug Detected:
            Expression:     ${emitExpressionCode(exp)}
            Statement:      ${getStatementCode(stmt)}
        """.trimIndent()
        log(msg, "DEBUG")
        tx.execute("match (stmt) where Id(stmt) = ${stmt.id} match (exp) where Id(exp) = ${exp.id} merge (exp)<-[:SUB_EXPRESSION]-(ib:IntegerBug)-[:AFFECTED_STATEMENT]->(stmt)")
    }
    tx.commit()
}

fun analyzeTruncationBug(graphDb: GraphDatabaseService) {
    val tx = graphDb.beginTx()
    val truncs = tx.execute(truncationBugQuery)
    truncs.forEach { res ->
        handleTruncationBug(res, tx)
    }
    tx.commit()
}

fun resolveTargetType(node: Node) : String {
    return when {
        node.hasLabel { NodeType.ElementaryTypeExpression.name } -> node.getProperty("typeName") as String
        node.hasLabel { NodeType.IdentifierExpression.name } -> node.getProperty("name") as String
        node.hasLabel { NodeType.TupleExpression.name } -> {
            val inner = node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.HAS_COMPONENT.name } }.endNode
            resolveTargetType(inner)
        }
        node.hasLabel { NodeType.MemberAccess.name } -> getTypeOfNode(node)
        else -> error("Cannot get target type for Node ${node.id} with labels ${node.labels}")
    }
}

fun handleTruncationBug(res: MutableMap<String, Any>, tx: Transaction) {
    val t = res["targetType"] as Node
    val targetType = resolveTargetType(t)
    val statement = res["stmt"] as Node
    val expression = res["expr"] as Node
    val sourceType = when {
        expression.hasLabel { NodeType.NumberLiteral.name } -> "uint256"
        expression.hasLabel { NodeType.StringLiteral.name } -> "string"
        expression.hasLabel { NodeType.BoolLiteral.name } -> "bool"
        expression.hasLabel { NodeType.UnaryOperation.name } && expression.hasProperty("operator") && (expression.getProperty("operator") as String) == "-" -> "int256"
        else -> getTypeOfNode(expression)
    }
    try {
        val targetBits = if(targetType == "uint" || targetType == "int") 256 else targetType.removePrefix("u").removePrefix("int").toInt()
        val sourceBits = if(sourceType == "uint" || sourceType == "int") 256 else sourceType.removePrefix("u").removePrefix("int").toInt()
        if (targetBits >= sourceBits) return
        val truncation = tx.createNode()
        truncation.addLabel { NodeType.TruncationBug.name }
        truncation.addLabel { NodeType.Vulnerability.name }
        truncation.setProperty("targetType", targetType)
        truncation.setProperty("sourceType", sourceType)
        truncation.createRelationshipTo(statement) { Relationship.AFFECTED_STATEMENT.name }
        truncation.createRelationshipTo(expression) { Relationship.HAS_ARGUMENT.name }
    }
    catch (e: NumberFormatException){
        log("TypeConversion from $sourceType to $targetType is ignored. (No Truncation)", "INFO")
        return
    }
}