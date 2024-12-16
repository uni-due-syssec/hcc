package syssec.hcc

import org.neo4j.graphdb.Direction
import org.neo4j.graphdb.GraphDatabaseService
import org.neo4j.graphdb.Node
import org.neo4j.graphdb.Transaction
import java.lang.Exception

class Locks {
    companion object {
        val lockMap =  mutableMapOf<Long, MutableList<List<String>>>()
        val nameMap = mutableMapOf<String, String>()

        var counter = 0
        fun insertConstraintsForNode(f: Long, constraints: List<Node>) {
            val c = constraintNodesToConstraintStrings(constraints)
            val cons = lockMap[f]
            if(cons != null){
                cons.add(c)
                return
            }
            val l = mutableListOf<List<String>>()
            l.add(c)
            lockMap.put(f, l)
        }

        fun nodeisLocked(f: Long, constraints: List<Node>): Boolean{
            val conStrings = constraintNodesToConstraintStrings(constraints)
            val cons = lockMap[f]
            return when {
                cons == null -> {
                    false
                }
                else -> {
                    cons.any { it.zip(conStrings).all { (x, y) -> x == y } }
                }
            }
        }

        fun getLockNameForConstraints(lhs: Node, constraints: List<Node>) : String{
            val cons = constraintNodesToConstraintStrings(constraints).joinToString(separator = ";")
            if(nameMap.containsKey(cons)){
                return nameMap[cons]!!
            }
            val lockName = "${getLockName(lhs)}_lock_hcc_${Locks.counter}"
            Locks.counter += 1;
            nameMap.put(cons, lockName)
            return lockName
        }
    }
}

fun constraintNodesToConstraintStrings(nodes: List<Node>): List<String> {
    return nodes.map {
        when {
            it.hasLabel { NodeType.LiteralExpression.name } -> {
                if(it.getRelationships(Direction.INCOMING).any { it.isType{ Relationship.INDEX_EXPRESSION.name } })
                    "IAE: ${emitExpressionCode(it)}"
                else
                    "${it.getProperty("value") as String}"
            }
            else -> "IAE: ${emitExpressionCode(it)}"
        }
    }
}

fun constraintStructureMatches(original: List<Node>, crossing: List<Node>): Boolean {
    val originalCons = constraintNodesToConstraintStrings(original).map { if(it.startsWith("IAE: ")) "_" else it }
    val crossingCons = constraintNodesToConstraintStrings(crossing).map { if(it.startsWith("IAE: ")) "_" else it }
    return originalCons.zip(crossingCons).all { (x, y) -> x == y }
}

fun patchReentrancy(graphDb: GraphDatabaseService) {
    val ftx = graphDb.beginTx()
    val res = ftx.execute("match (n: Reentrancy) return n").map { r ->
        val node = r["n"] as Node
        node.id
    }.asSequence().toList()
    ftx.commit()

    res.forEach { re ->
        val tx = graphDb.beginTx()
        val node = tx.getNodeById(re)

        if (node.hasLabel { NodeType.DelegatedReentrancy.name }) {
            patchDelegatedReentrancy(node, tx)
        } else {
            patchAnyReentrancy(node, tx)
        }
        tx.commit()
    }
}

fun patchAnyReentrancy(node: Node, tx: Transaction) {
    val reVariable = node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.RE_STATE_VARIABLE.name } }.endNode
    val assignment = node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.RE_ASSIGNMENT.name } }.endNode

    val (stateVariable, access) = if(reVariable.hasLabel { NodeType.LocalVariable.name }){
        val query = """match (d:LocalVariable)-[:HAS_VALUE]->(e)
        match (e)-[r *]->(:IdentifierExpression)-[:REFERENCES]->(sv:StateVariable)
        where Id(d) = ${reVariable.id} and none(x in r where type(x) in ["REFERENCES", "LEADS", "LOOP_BODY", "TRUE_BODY", "FALSE_BODY"])
        return distinct d, e, sv"""
        val qr = tx.execute(query)
        var state: Node? = null
        var lhs: Node? = null
        qr.forEach {
            lhs = it["e"] as Node
            state = it["sv"] as Node
        }
        if(state == null || lhs == null) error("")
        Pair(state!!, lhs!!)
    } else {
        Pair(reVariable, assignment.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.LEFT_HAND_SIDE.name } }.endNode)
    }
    val originalLHS = if(reVariable.hasLabel { NodeType.LocalVariable.name }) assignment.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.LEFT_HAND_SIDE.name } }.endNode else null
    val constraints = getHashData(access, originalLHS, tx)
    val lockName = Locks.getLockNameForConstraints(access, constraints)

    val callStatement = node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.RE_CALL.name } }.endNode
    val function = node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.RE_FUNCTION.name } }.endNode
    if(!Locks.nodeisLocked(function.id, constraints)){
        val ps = createEmptyStatement(tx)
        val locking = createLocking(lockName, tx)
        val unlocking = createUnlocking(lockName, tx)
        insertAtBeginningOfFunction(function, ps)
        insertAtBeginningOfFunction(function, locking)
        insertBeforeNode(assignment, unlocking)
        Locks.insertConstraintsForNode(callStatement.id, constraints)
        val guard = addGuard(function, lockName, tx)
        insertBeforeNode(locking, guard)
        val hashCall = createHashCall(constraints, tx)
        val lockHash = createLockLocationDefinition(function, hashCall, lockName, tx)
        insertAtBeginningOfFunction(function, lockHash)
        Locks.insertConstraintsForNode(function.id, constraints)
    }

    val crossFunctions = node.getRelationships(Direction.OUTGOING).filter { it.isType { Relationship.RE_CROSS_FUNCTION.name } }.map { it.endNode }
    crossFunctions.forEach {
        crossPatchRE(it, stateVariable, constraints, tx)
    }
    reportReentrancyPatch(node, function, callStatement, assignment, lockName)
}

fun patchDelegatedReentrancy(node: Node, tx: Transaction) {
    when (node.getProperty("case") as Int) {
        1 -> {
            patchDelegatedCase1Reentrancy(node, tx)
        }
        2 -> {
            patchAnyReentrancy(node, tx)
        }
        else -> {
            tx.commit()
            error("missing case property for delegated reentrancy.")
        }
    }
}

fun patchDelegatedCase1Reentrancy(node: Node, tx: Transaction) {
    val function = node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.RE_FUNCTION.name } }.endNode
    val externalCall = node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.RE_CALL.name } }.endNode
    val delegateCall = node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.RE_DELEGATE_CALL.name } }.endNode

    // the below hexstring is keccak256("_HCC_LOCK_contract_Lock") >> 16 | 0xdead << 240
    val constraints = listOf(createNumberLiteralNode("0xdead6ba14ffe88de884ac756450cf4fc7c326ad2334e2897e0117cd4ed71faab", tx))
    val lockName = "contract_lock_hcc"
    if(!Locks.nodeisLocked(function.id, constraints)){
        val ps = createEmptyStatement(tx)
        val locking = createLocking(lockName, tx)
        val unlocking = createUnlocking(lockName, tx)
        insertAtBeginningOfFunction(function, ps)
        insertBeforeNode(externalCall, locking)
        insertAfterNode(delegateCall, unlocking)

        val guard = addGuard(function, lockName, tx)
        insertBeforeNode(ps, guard)
        Locks.insertConstraintsForNode(function.id, constraints)

        val lockHash = createMainLockLocationDefinition(function, constraints[0], lockName, tx)
        insertAtBeginningOfFunction(function, lockHash)
    }

    reportDC1ReentrancyPatch(node, function, externalCall, delegateCall, lockName)
}

fun crossPatchRE(function: Node, stateVariable: Node, originalConstraints: List<Node>, tx: Transaction) {
    val query = """
    match (a :AssignmentStatement)-[:WRITES]->(sv:StateVariable)
    match (f :Function)-[BODY]-()-[:LEADS | TRUE_BODY | FALSE_BODY | LOOP_BODY * 0..]->(a)
    where Id(f) = %d and Id(sv) = %d
    return distinct a
    """.trimIndent()
    val res = tx.execute(query.format(function.id, stateVariable.id))
    res.forEach {
        val assignment = it["a"] as Node
        val originalLHS = assignment.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.LEFT_HAND_SIDE.name } }.endNode
        val constraints = getHashData(originalLHS, null, tx) // TODO
        if(!Locks.nodeisLocked(function.id, constraints) && constraintStructureMatches(originalConstraints, constraints)){
            val ps = createEmptyStatement(tx)
            insertBeforeNode(assignment, ps)
            val lockName = Locks.getLockNameForConstraints(originalLHS, constraints)
            val guard = addGuard(function,lockName, tx)
            insertBeforeNode(ps, guard)
            val hashCall = createHashCall(constraints, tx)
            val lockHash = createLockLocationDefinition(function, hashCall, lockName, tx)
            insertBeforeNode(guard.first, lockHash)
            Locks.insertConstraintsForNode(function.id, constraints)
        }
    }
}

fun addGuard(function: Node, lockName: String, tx: Transaction) : Pair<Node,Node> {
    val t = getOrConstructBoolType(tx)
    val checkvar = createLocalVariableDecl("${lockName}_check", function, t, tx)
    val ident = createIdentifierExpressionAnon("${lockName}_check", tx)
    val cond = createLockCondition(ident, tx)
    val guard = createFunctionCall("require", tx, listOf(cond, createStringLiteralNode("[ HCC ] ${lockName} is locked!", tx)))
    guard.addLabel { NodeType.ExpressionStatement.name }
    val loader = createLoader("${lockName}_check", lockName, tx)
    checkvar.createRelationshipTo(loader) { Relationship.LEADS.name }
    loader.createRelationshipTo(guard) { Relationship.LEADS.name }
    return Pair(checkvar, guard)
}

fun createLocalVariableDecl(name: String, function: Node, type: Node, tx: Transaction): Node{
    val vds = tx.createNode()
    vds.addLabel { NodeType.VariableDeclarationStatement.name }
    val v = createLocalVariable(name, function, tx)
    v.createRelationshipTo(type) { Relationship.TYPE.name }
    val r = vds.createRelationshipTo(v) { Relationship.DECLARES.name }
    r.setProperty("declarationIndex", 0)
    return vds
}

fun createMainLockLocationDefinition(function: Node, hashValue: Node, lockName: String, tx: Transaction): Node {
    val vds = createVariableDeclarationStatement(function, lockName, tx)
    vds.createRelationshipTo(hashValue) { Relationship.RIGHT_HAND_SIDE.name }
    return vds
}

fun createVariableDeclarationStatement(function: Node, lockName: String, tx: Transaction): Node {
    val vds = tx.createNode()
    vds.addLabel { NodeType.VariableDeclarationStatement.name }
    vds.addLabel { NodeType.AssignmentStatement.name }

    val declaration = createLocalVariable(lockName, function, tx)
    val t = getOrConstructUint256Type(tx)
    declaration.createRelationshipTo(t) { Relationship.TYPE.name }
    val rel = vds.createRelationshipTo(declaration) { Relationship.LEFT_HAND_SIDE.name }
    rel.setProperty("declarationIndex", 0)
    val rel2 = vds.createRelationshipTo(declaration) { Relationship.DECLARES.name }
    rel2.setProperty("declarationIndex", 0)
    return vds
 }

fun createLockLocationDefinition(function: Node, hashCall: Node, lockName: String, tx: Transaction): Node {
    val vds = createVariableDeclarationStatement(function, lockName, tx)

    val rs16 = createNumberLiteralNode("16", tx)
    val l = wrapTuple(createBinOp(">>", hashCall, rs16, tx), tx)
    val r = createNumberLiteralNode("0xdead000000000000000000000000000000000000000000000000000000000000", tx)
    val iv = createBinOp("|", l, r, tx)
    vds.createRelationshipTo(iv) { Relationship.RIGHT_HAND_SIDE.name }
    return vds
}

fun createBinOp(operator: String, lhs: Node, rhs: Node, tx: Transaction): Node {
    val binop = tx.createNode()
    binop.addLabel { NodeType.BinaryOperation.name }
    binop.addLabel { NodeType.Expression.name }
    binop.setProperty("operator", operator)
    binop.createRelationshipTo(lhs) { Relationship.LEFT_EXPRESSION.name }
    binop.createRelationshipTo(rhs) { Relationship.RIGHT_EXPRESSION.name }
    return binop
}

fun wrapTuple(node: Node, tx: Transaction): Node {
    val t = tx.createNode()
    t.addLabel { NodeType.TupleExpression.name }
    t.createRelationshipTo(node) { Relationship.HAS_COMPONENT.name }
    t.setProperty("isInlineArray", false)
    return t
}

fun getHashData(access: Node, originalLHS: Node?, tx: Transaction): List<Node> {
    val hashData = mutableListOf(createStringLiteralNode("_HCC_LOCK", tx))
    hashData.addAll(collectAccessConstraints(access, tx))
    if(originalLHS != null) hashData.addAll(collectAccessConstraints(originalLHS, tx).drop(1))
    return hashData
}

fun createHashCall(hashData: List<Node>, tx: Transaction): Node {
    return createFunctionCall("uint256", tx, listOf(
        createFunctionCall("keccak256", tx, listOf(
            createFunctionCall("abi.encodePacked", tx, hashData)
        ))
    ))
}

fun createEmptyStatement(tx: Transaction): Node {
    val ps = tx.createNode()
    ps.addLabel { NodeType.EmptyStatement.name }
    return ps
}

fun createLocalVariable(name: String, function: Node, tx: Transaction): Node {
    val variable = tx.createNode()
    variable.addLabel { NodeType.Variable.name }
    variable.addLabel { NodeType.LocalVariable.name }
    function.createRelationshipTo(variable) { Relationship.HAS_LOCAL_VARIABLE.name }
    variable.setProperty("constant", false)
    variable.setProperty("name", name)
    variable.setProperty("visibility", "internal")
    variable.setProperty("storageLocation", "")
    return variable
}

fun createFunctionCall(func: String, tx: Transaction, params: List<Node>): Node {
    val c = tx.createNode()
    c.addLabel { NodeType.Expression.name }
    c.addLabel { NodeType.FunctionCall.name }
    c.addLabel { NodeType.Call.name }
    val ce = createIdentifierExpressionAnon(func, tx)
    c.createRelationshipTo(ce) { Relationship.CALL_EXPRESSION.name }
    params.forEachIndexed { index, it ->
        val r = c.createRelationshipTo(it) { Relationship.HAS_ARGUMENT.name }
        r.setProperty("argIndex", index)
    }
    return c
}

fun collectAccessConstraints(node: Node, tx: Transaction): List<Node> {
    return when {
        node.hasLabel { NodeType.IndexAccessExpression.name } -> {
            val base = collectAccessConstraints(node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.BASE_EXPRESSION.name } }.endNode, tx)
            val index = node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.INDEX_EXPRESSION.name } }.endNode
            val res = base.toMutableList()
            res.add(index)
            res
        }
        node.hasLabel { NodeType.MemberAccess.name } -> {
            val member = createStringLiteralNode(node.getProperty("memberName") as String, tx)
            val lhs = collectAccessConstraints(node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.LEFT_HAND_SIDE.name } }.endNode, tx).toMutableList()
            lhs.add(member)
            lhs
        }
        node.hasLabel { NodeType.IdentifierExpression.name } -> {
            listOf(createStringLiteralNode(node.getProperty("name") as String, tx))
        }
        else -> throw Exception("cannot collect Access Constraints for ${node.labels} (${node.id})")
    }
}

fun createLocking(name: String, tx: Transaction): Node{
    return createInlineAssembly("{ sstore($name, 1) }", tx)
}

fun createUnlocking(name: String, tx: Transaction): Node{
    return createInlineAssembly("{ sstore($name, 0) }", tx)
}

fun createLoader(name: String, addr: String, tx: Transaction): Node {
    return createInlineAssembly("{ ${name} := sload(${addr}) }", tx)
}

fun createInlineAssembly(evasm: String, tx: Transaction): Node{
    val ias = tx.createNode()
    ias.addLabel { NodeType.InlineAssemblyStatement.name }
    ias.setProperty("operations", evasm)
    return ias
}

fun createNumberLiteralNode(value: String, tx: Transaction): Node {
    val nl = tx.createNode()
    nl.setProperty("value", value)
    nl.addLabel { NodeType.NumberLiteral.name }
    nl.addLabel { NodeType.LiteralExpression.name }
    return nl
}

fun createStringLiteralNode(value: String, tx: Transaction): Node {
    val sl = tx.createNode()
    sl.setProperty("value", value)
    sl.addLabel { NodeType.StringLiteral.name }
    sl.addLabel { NodeType.LiteralExpression.name }
    return sl
}

fun getOrConstructBoolType(tx: Transaction): Node {
    val r = tx.execute("""match(n: BasicType {typeString: "bool"}) return n""")
    r.forEach {
        return it["n"] as Node
    }
    val boolNode = tx.createNode()
    boolNode.addLabel { NodeType.BasicType.name }
    boolNode.addLabel { NodeType.Type.name }
    boolNode.setProperty("typeString", "bool")
    return boolNode
}

fun getOrConstructUint256Type(tx: Transaction): Node {
    val r = tx.execute("""match(n: ElementaryType {typeString: "uint256"}) return n""")
    r.forEach {
        return it["n"] as Node
    }
    val boolNode = tx.createNode()
    boolNode.addLabel { NodeType.ElementaryType.name }
    boolNode.addLabel { NodeType.Type.name }
    boolNode.setProperty("typeString", "uint256")
    return boolNode
}

fun getLockName(expr: Node): String {
    return when {
        expr.hasLabel { NodeType.MemberAccess.name } -> {
            val lhs = expr.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.LEFT_HAND_SIDE.name } }.endNode
            "${getLockName(lhs)}_${expr.getProperty("memberName") as String}"
        }
        expr.hasLabel { NodeType.IdentifierExpression.name } -> {
            expr.getProperty("name") as String
        }
        expr.hasLabel { NodeType.IndexAccessExpression.name } -> {
            val baseexpr = expr.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.BASE_EXPRESSION.name } }.endNode
            getLockName(baseexpr)
        }
        else -> {
            error("Can not construct lock name for LHS of kind ${expr.labels} (${expr.id})")
        }
    }
}

fun createIdentifierExpressionAnon(name: String, tx: Transaction): Node {
    val idexpr = tx.createNode()
    idexpr.addLabel { NodeType.Expression.name }
    idexpr.addLabel { NodeType.IdentifierExpression.name }
    idexpr.setProperty("name", name)
    return idexpr
}

fun createLockCondition(lhs: Node, tx: Transaction): Node {
    val unop = tx.createNode()
    unop.addLabel { NodeType.UnaryOperation.name }
    unop.addLabel { NodeType.LogicExpression.name }
    unop.addLabel { NodeType.Expression.name }
    unop.setProperty("operator", "!")
    unop.setProperty("prefix", true)
    unop.createRelationshipTo(lhs) { Relationship.SUB_EXPRESSION.name }
    return unop
}
