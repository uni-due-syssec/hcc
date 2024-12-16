package syssec.hcc

import org.neo4j.graphdb.Direction
import org.neo4j.graphdb.GraphDatabaseService
import org.neo4j.graphdb.Node
import org.neo4j.graphdb.Transaction

fun remapLeadRelationship(
    old: Node,
    new: Node,
) {
    val rel = old.getRelationships(Direction.OUTGOING).filter { it.isType { Relationship.LEADS.name } }
    rel.forEach {
        new.createRelationshipTo(it.endNode) { Relationship.LEADS.name }
        it.delete()
    }
    old.createRelationshipTo(new) { Relationship.LEADS.name }
}

fun createZero(tx: Transaction): Node {
    val zero = tx.createNode()
    zero.addLabel { NodeType.Expression.name }
    zero.addLabel { NodeType.LiteralExpression.name }
    zero.addLabel { NodeType.NumberLiteral.name }
    zero.setProperty("value", "0")

    return zero
}

fun createOne(tx: Transaction): Node {
    val one = tx.createNode()
    one.addLabel { NodeType.Expression.name }
    one.addLabel { NodeType.LiteralExpression.name }
    one.addLabel { NodeType.NumberLiteral.name }
    one.setProperty("value", "1")

    return one
}

fun createSignedSubAssert(
    a: Node,
    b: Node,
    c: Node,
    tx: Transaction,
): Node {
    val firstComp = createSignedSubComp(a, b, c, tx)
    return createAssert(handleParameters(a, b, false, firstComp, tx), tx)
}

fun createSignedSubComp(
    a: Node,
    b: Node,
    c: Node,
    tx: Transaction,
): Node {
    val zero = createZero(tx)
    return or(
        and(
            geq(a, zero, tx),
            or(
                geq(b, zero, tx),
                geq(c, b, tx),
                tx,
            ),
            tx,
        ),
        and(
            le(a, zero, tx),
            or(
                leq(b, zero, tx),
                leq(c, b, tx),
                tx,
            ),
            tx,
        ),
        tx,
    )
}

fun createSignedAddAssert(
    a: Node,
    b: Node,
    c: Node,
    tx: Transaction,
): Node {
    val firstComp = createSignedAddComp(a, b, c, tx)
    return createAssert(handleParameters(a, b, false, firstComp, tx), tx)
}

fun createSignedAddComp(
    a: Node,
    b: Node,
    c: Node,
    tx: Transaction,
): Node {
    val zero = createZero(tx)
    return or(
        and(
            geq(a, zero, tx),
            or(
                leq(b, zero, tx),
                geq(c, b, tx),
                tx,
            ),
            tx,
        ),
        and(
            le(a, zero, tx),
            or(
                geq(b, zero, tx),
                leq(c, b, tx),
                tx,
            ),
            tx,
        ),
        tx,
    )
}

fun createMulEqualAssert(
    a: Node,
    b: Node,
    c: Node,
    unsigned: Boolean,
    tx: Transaction,
): Node {
    val firstComp = createMulEqualComp(a, b, c, tx)
    return createAssert(handleParameters(a, b, unsigned, firstComp, tx), tx)
}

fun createMulEqualComp(
    a: Node,
    b: Node,
    c: Node,
    tx: Transaction,
): Node {
    val zero = createZero(tx)
    val div = tx.createNode()
    div.addLabel { NodeType.ArithmeticExpression.name }
    div.addLabel { NodeType.BinaryOperation.name }
    div.addLabel { NodeType.Division.name }
    div.addLabel { NodeType.Expression.name }
    div.setProperty("operator", "/")
    div.createRelationshipTo(a) { Relationship.RIGHT_EXPRESSION.name }
    div.createRelationshipTo(c) { Relationship.LEFT_EXPRESSION.name }

    return or(
        eq(a, zero, tx),
        eq(div, b, tx),
        tx,
    )
}

fun createDummyComp(tx: Transaction): Node {
    val comp = createCompExpr(createOne(tx), createZero(tx), ">", tx)
    comp.addLabel { NodeType.Fallback.name }
    return comp
}

fun isDummyComp(a: Node): Boolean = a.hasLabel { NodeType.Fallback.name }

fun and(
    left: Node,
    right: Node,
    tx: Transaction,
): Node {
    if (isDummyComp(left)) {
        return right
    }
    if (isDummyComp(right)) {
        return left
    }
    return createCompExpr(left, right, "&&", tx)
}

fun or(
    left: Node,
    right: Node,
    tx: Transaction,
): Node {
    if (isDummyComp(left)) {
        return right
    }
    if (isDummyComp(right)) {
        return left
    }
    return createCompExpr(left, right, "||", tx)
}

fun eq(
    left: Node,
    right: Node,
    tx: Transaction,
): Node = createCompExpr(left, right, "==", tx)

fun le(
    left: Node,
    right: Node,
    tx: Transaction,
): Node = createCompExpr(left, right, "<", tx)

fun leq(
    left: Node,
    right: Node,
    tx: Transaction,
): Node = createCompExpr(left, right, "<=", tx)

fun geq(
    left: Node,
    right: Node,
    tx: Transaction,
): Node = createCompExpr(left, right, ">=", tx)

fun wrapCompExpression(
    node: Node,
    tx: Transaction,
): Node {
    if (!(node.hasLabel { NodeType.ComparisonOperation.name })) {
        return node
    }
    val tuple = tx.createNode()
    tuple.addLabel { NodeType.TupleExpression.name }
    tuple.addLabel { NodeType.Expression.name }
    tuple.createRelationshipTo(node) { Relationship.HAS_COMPONENT.name }

    node.removeLabel { NodeType.ComparisonOperation.name }
    node.addLabel { NodeType.LogicExpression.name }

    return tuple
}

fun createUnaryAssert(
    node: Node,
    op: String,
    tx: Transaction,
): Node = createAssert(createUnaryComp(node, op, tx), tx)

fun createUnaryComp(
    node: Node,
    op: String,
    tx: Transaction,
): Node {
    val one = createOne(tx)
    if (op == "++") {
        return createCompExpr(createAddOperation(tx, node, one), node, ">", tx)
    } else if (op == "--") {
        return createCompExpr(node, createSubOperation(tx, node, one), ">", tx)
    }
    throw Exception("Unexpected unary operation $op, only ++ and -- supported")
}

fun createSubOperation(
    tx: Transaction,
    a: Node,
    b: Node,
): Node {
    val add = tx.createNode()
    add.addLabel { NodeType.Subtraction.name }
    add.addLabel { NodeType.ArithmeticExpression.name }
    add.addLabel { NodeType.BinaryOperation.name }
    add.addLabel { NodeType.Expression.name }
    add.setProperty("operator", "-")
    add.createRelationshipTo(a) { Relationship.LEFT_EXPRESSION.name }
    add.createRelationshipTo(b) { Relationship.RIGHT_EXPRESSION.name }

    return add
}

fun createAddOperation(
    tx: Transaction,
    a: Node,
    b: Node,
): Node {
    val add = tx.createNode()
    add.addLabel { NodeType.Addition.name }
    add.addLabel { NodeType.ArithmeticExpression.name }
    add.addLabel { NodeType.BinaryOperation.name }
    add.addLabel { NodeType.Expression.name }
    add.setProperty("operator", "+")
    add.createRelationshipTo(a) { Relationship.LEFT_EXPRESSION.name }
    add.createRelationshipTo(b) { Relationship.RIGHT_EXPRESSION.name }

    return add
}

fun createCompExpr(
    left: Node,
    right: Node,
    op: String,
    tx: Transaction,
): Node {
    val one = wrapCompExpression(left, tx)
    val two = wrapCompExpression(right, tx)
    val comp = tx.createNode()
    comp.addLabel { NodeType.ComparisonOperation.name }
    comp.addLabel { NodeType.BinaryOperation.name }
    comp.addLabel { NodeType.Expression.name }
    comp.setProperty("operator", op)
    comp.createRelationshipTo(getOrConstructBoolType(tx)) { Relationship.TYPE.name }

    comp.createRelationshipTo(one) { Relationship.LEFT_EXPRESSION.name }
    comp.createRelationshipTo(two) { Relationship.RIGHT_EXPRESSION.name }

    return comp
}

fun getLeftNode(a: Node): Node = a.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.LEFT_EXPRESSION.name } }.endNode

fun getRightNode(a: Node): Node = a.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.RIGHT_EXPRESSION.name } }.endNode

fun dispatchOperator(
    node: Node,
    left: Node,
    right: Node,
    unsigned: Boolean,
    currComp: Node,
    tx: Transaction,
): Node {
    val op = node.getProperty("operator") as String
    if (op == "+") {
        return if (unsigned) {
            and(currComp, createBiggerOrEqualComp(node, left, tx), tx)
        } else {
            and(currComp, createSignedAddComp(left, right, node, tx), tx)
        }
    } else if (op == "-") {
        return if (unsigned) {
            and(currComp, createBiggerOrEqualComp(left, right, tx), tx)
        } else {
            and(currComp, createSignedSubComp(left, right, node, tx), tx)
        }
    } else if (op == "*") {
        return and(currComp, createMulEqualComp(left, right, node, tx), tx)
    } else if (op == "/") {
        return currComp
    } else if (op == "%") {
        return currComp
    } else if (op == "**") {
        return currComp
    }
    throw Exception("Unexpected arithmetic operation $op, only +,-,*,/,++ and -- supported")
}

fun handleSingleParameter(
    a: Node,
    unsigned: Boolean,
    comp: Node,
    tx: Transaction,
): Node {
    if (a.hasLabel { NodeType.ArithmeticExpression.name }) {
        val l = getLeftNode(a)
        val r = getRightNode(a)
        val newComp = dispatchOperator(a, l, r, unsigned, comp, tx)
        return handleParameters(l, r, unsigned, newComp, tx)
    }
    if (a.hasLabel { NodeType.TupleExpression.name }) {
        // it's a parenthesis
        val op =
            a
                .getRelationships(Direction.OUTGOING)
                .first { it.isType { Relationship.HAS_COMPONENT.name } }
                .endNode
        return handleSingleParameter(op, unsigned, comp, tx)
    }
    return comp
}

fun handleParameters(
    l: Node,
    r: Node,
    unsigned: Boolean,
    comp: Node,
    tx: Transaction,
): Node {
    val leftUpdate = handleSingleParameter(l, unsigned, comp, tx)
    return handleSingleParameter(r, unsigned, leftUpdate, tx)
}

fun createBiggerOrEqualComp(
    a: Node,
    b: Node,
    tx: Transaction,
): Node = createCompExpr(a, b, ">=", tx)

fun copyAndRemoveIncBG(
    nodeToCopy: Node,
    comp: Node,
    tx: Transaction,
): Pair<Node, Node> {
    if (nodeToCopy.hasLabel { NodeType.UnaryOperation.name }) {
        val variable =
            nodeToCopy
                .getRelationships(Direction.OUTGOING)
                .first { it.isType { Relationship.SUB_EXPRESSION.name } }
                .endNode
        val op = nodeToCopy.getProperty("operator") as String
        val newComp = createUnaryComp(variable, op, tx)
        return copyAndRemoveIncBG(variable, and(comp, newComp, tx), tx)
    }

    val copy = tx.createNode()
    nodeToCopy.labels.forEach {
        copy.addLabel { it.name() }
    }
    nodeToCopy.propertyKeys.forEach {
        copy.setProperty(it, nodeToCopy.getProperty(it))
    }
    if (nodeToCopy.hasLabel { NodeType.TupleExpression.name }) {
        // it's a parenthesis
        val op =
            nodeToCopy
                .getRelationships(Direction.OUTGOING)
                .first { it.isType { Relationship.HAS_COMPONENT.name } }
                .endNode
        val (copiedOp, opComp) = copyAndRemoveIncBG(op, comp, tx)
        copy.createRelationshipTo(copiedOp) { Relationship.HAS_COMPONENT.name }
        return Pair(copy, opComp)
    }
    if (nodeToCopy.hasLabel { NodeType.ArithmeticExpression.name } || nodeToCopy.hasLabel { NodeType.BinaryOperation.name }) {
        val (l, lComp) = copyAndRemoveIncBG(getLeftNode(nodeToCopy), comp, tx)
        val (r, rComp) = copyAndRemoveIncBG(getRightNode(nodeToCopy), lComp, tx)
        copy.createRelationshipTo(l) { Relationship.LEFT_EXPRESSION.name }
        copy.createRelationshipTo(r) { Relationship.RIGHT_EXPRESSION.name }
        return Pair(copy, rComp)
    }
    if (nodeToCopy.hasLabel { NodeType.IndexAccessExpression.name }) {
        val be = nodeToCopy.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.BASE_EXPRESSION.name } }.endNode
        val ie = nodeToCopy.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.INDEX_EXPRESSION.name } }.endNode
        val (bexpr, bcomp) = copyAndRemoveIncBG(be, comp, tx)
        val (iexpr, ncomp) = copyAndRemoveIncBG(ie, bcomp, tx)
        copy.createRelationshipTo(bexpr) { Relationship.BASE_EXPRESSION.name }
        copy.createRelationshipTo(iexpr) { Relationship.INDEX_EXPRESSION.name }
        return Pair(copy, ncomp)
    }
    if (nodeToCopy.hasLabel { NodeType.MemberAccess.name }) {
        val l = nodeToCopy.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.LEFT_HAND_SIDE.name } }.endNode
        val (lhs, ncomp) = copyAndRemoveIncBG(l, comp, tx)
        copy.createRelationshipTo(lhs) { Relationship.LEFT_HAND_SIDE.name }
        return Pair(copy, ncomp)
    }

    if (nodeToCopy.hasLabel { NodeType.Call.name }) {
        val c = nodeToCopy.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.CALL_EXPRESSION.name } }.endNode
        val (callexpr, ccomp) = copyAndRemoveIncBG(c, comp, tx)
        copy.createRelationshipTo(callexpr) { Relationship.CALL_EXPRESSION.name }
        var ncomp = ccomp

        val argRels = nodeToCopy.getRelationships(Direction.OUTGOING).filter { it.isType { Relationship.HAS_ARGUMENT.name } }
        argRels.forEach { arg ->
            val index = arg.getProperty("argIndex") as Int
            val argnode = arg.endNode
            val (narg, bcomp) = copyAndRemoveIncBG(argnode, ncomp, tx)
            val r = copy.createRelationshipTo(narg) { Relationship.HAS_ARGUMENT.name }
            r.setProperty("argIndex", index)
            ncomp = bcomp
        }

        val nargRels = nodeToCopy.getRelationships(Direction.OUTGOING).filter { it.isType { Relationship.HAS_NAMED_ARGUMENT.name } }
        nargRels.forEach { arg ->
            val index = arg.getProperty("argIndex") as Int
            val name = arg.getProperty("name") as String
            val argnode = arg.endNode
            val (narg, bcomp) = copyAndRemoveIncBG(argnode, ncomp, tx)
            val r = copy.createRelationshipTo(narg) { Relationship.HAS_NAMED_ARGUMENT.name }
            r.setProperty("argIndex", index)
            r.setProperty("name", name)
            ncomp = bcomp
        }

        return Pair(copy, ncomp)
    }
    if (nodeToCopy.hasLabel { NodeType.ConditionalExpression.name }) {
        val cond = nodeToCopy.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.CONDITION.name } }.endNode
        val te = nodeToCopy.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.TRUE_EXPRESSION.name } }.endNode
        val fe = nodeToCopy.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.FALSE_EXPRESSION.name } }.endNode
        val (condexpr, condcomp) = copyAndRemoveIncBG(cond, comp, tx)
        val (teexpr, tecomp) = copyAndRemoveIncBG(te, condcomp, tx)
        val (feexpr, fecomp) = copyAndRemoveIncBG(fe, tecomp, tx)
        copy.createRelationshipTo(condexpr) { Relationship.CONDITION.name }
        copy.createRelationshipTo(teexpr) { Relationship.TRUE_EXPRESSION.name }
        copy.createRelationshipTo(feexpr) { Relationship.FALSE_EXPRESSION.name }
        return Pair(copy, fecomp)
    }
    return Pair(copy, comp)
}

fun copyAndRemoveInc(
    a: Node,
    exp: Node,
    tx: Transaction,
): Node {
    val (copy, comp) = copyAndRemoveIncBG(a, createDummyComp(tx), tx)
    if (isDummyComp(comp)) {
        return copy
    }
    val assert = createAssert(comp, tx)
    insertBeforeNode(exp, assert)
    return copy
}

fun createAssert(
    comp: Node,
    tx: Transaction,
): Node {
    val statement = tx.createNode()
    val expr = tx.createNode()
    val type = tx.createNode()
    type.setProperty("typeIdentifier", "t_function_assert_pure\$_t_bool_\$returns\$__\$")
    type.setProperty("typeString", "function (bool) pure")
    expr.createRelationshipTo(type) { Relationship.TYPE.name }
    expr.addLabel { NodeType.Expression.name }
    expr.addLabel { NodeType.IdentifierExpression.name }
    expr.setProperty("name", "assert")
    statement.createRelationshipTo(expr) { Relationship.CALL_EXPRESSION.name }
    statement.addLabel { NodeType.Statement.name }
    statement.addLabel { NodeType.FunctionCall.name }
    statement.addLabel { NodeType.ExpressionStatement.name }
    statement.addLabel { NodeType.Expression.name }
    statement.addLabel { NodeType.ContainsCall.name }
    statement.addLabel { NodeType.Call.name }
    statement.createRelationshipTo(comp) { Relationship.HAS_ARGUMENT.name }
    return statement
}

fun patchUnaryOperation(graphDb: GraphDatabaseService) {
    val ftx = graphDb.beginTx()
    val res =
        ftx
            .execute("match (n: UnaryOverflow) return n")
            .map { r ->
                val node = r["n"] as Node
                node.id
            }.asSequence()
            .toList()
    ftx.commit()

    res.forEach { r ->
        val tx = graphDb.beginTx()
        val node = tx.getNodeById(r)
        val expr = node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.SUB_EXPRESSION.name } }.endNode
        val op = expr.getProperty("operator") as String
        when {
            op == "++" -> {
                patchValidUnaryOperation(node, op, tx)
            }
            op == "--" -> {
                patchValidUnaryOperation(node, op, tx)
            }
            op == "delete" -> {
            }
            else -> {
                tx.commit()
                error("Unexpected unary operation $op, only ++ and -- supported")
            }
        }
        tx.commit()
    }
}

fun patchValidUnaryOperation(
    node: Node,
    op: String,
    tx: Transaction,
) {
    val variable = node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.HAS_ARGUMENT.name } }.endNode
    val prevNode = node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.PREVIOUS_NODE.name } }.endNode
    val assert = createUnaryAssert(variable, op, tx)

    remapLeadRelationship(prevNode, assert)
    log("The report may be bugged, pls check this", "BUGHUNT")
    val statement =
        if (prevNode.hasLabel { NodeType.BlockEntry.name }) {
            prevNode
                .getRelationships(Direction.INCOMING)
                .first {
                    it.isType { Relationship.LOOP_LEAD.name } ||
                        it.isType { Relationship.TRUE_BODY.name } ||
                        it.isType { Relationship.FALSE_BODY.name } ||
                        it.isType { Relationship.LEADS.name }
                }.startNode
        } else {
            prevNode
        }
    reportIntegerFlow(statement, assert)
}

fun patchArithmeticOperation(graphDb: GraphDatabaseService) {
    val ftx = graphDb.beginTx()
    val res =
        ftx
            .execute("match (n: IntegerBug) return n")
            .map { r ->
                val node = r["n"] as Node
                node.id
            }.asSequence()
            .toList()
    ftx.commit()

    res.forEach { r ->
        val tx = graphDb.beginTx()
        val node = tx.getNodeById(r)
        val op = node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.SUB_EXPRESSION.name } }.endNode
        val stmt = node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.AFFECTED_STATEMENT.name } }.endNode
        val type = op.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.TYPE.name } }.endNode

        val fixedOp = copyAndRemoveInc(op, stmt, tx)

        val assert = createAssert(handleSingleParameter(fixedOp, getTypeString(type).contains("uint"), createDummyComp(tx), tx), tx)
        insertBeforeNode(stmt, assert)
        reportIntegerFlow(stmt, assert)
        tx.commit()
    }
}

fun copyExpression(
    node: Node,
    tx: Transaction,
): Node {
    if (node.hasLabel { NodeType.UnaryOperation.name }) {
        val variable =
            node
                .getRelationships(Direction.OUTGOING)
                .first { it.isType { Relationship.SUB_EXPRESSION.name } }
                .endNode
        val op = node.getProperty("operator") as String
        val one = createOne(tx)
        return when (op) {
            "++" -> createAddOperation(tx, variable, one)
            "--" -> createSubOperation(tx, variable, one)
            else -> error("Unexpected unary operation $op, only ++ and -- supported")
        }
    }

    val copy = tx.createNode()
    node.labels.forEach { copy.addLabel { it.name() } }
    node.propertyKeys.forEach { copy.setProperty(it, node.getProperty(it)) }

    when {
        node.hasLabel { NodeType.TupleExpression.name } -> {
            val components =
                node
                    .getRelationships(Direction.OUTGOING)
                    .filter { it.isType { Relationship.HAS_COMPONENT.name } }
                    .map { copyExpression(it.endNode, tx) }
            components.forEach { copy.createRelationshipTo(it) { Relationship.HAS_COMPONENT.name } }
        }
        node.hasLabel { NodeType.ArithmeticExpression.name } -> {
            val le =
                copyExpression(
                    node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.LEFT_EXPRESSION.name } }.endNode,
                    tx,
                )
            val re =
                copyExpression(
                    node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.RIGHT_EXPRESSION.name } }.endNode,
                    tx,
                )
            copy.createRelationshipTo(le) { Relationship.LEFT_EXPRESSION.name }
            copy.createRelationshipTo(re) { Relationship.RIGHT_EXPRESSION.name }
        }
        node.hasLabel { NodeType.IndexAccessExpression.name } -> {
            val be =
                copyExpression(
                    node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.BASE_EXPRESSION.name } }.endNode,
                    tx,
                )
            val ie =
                copyExpression(
                    node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.INDEX_EXPRESSION.name } }.endNode,
                    tx,
                )
            copy.createRelationshipTo(be) { Relationship.BASE_EXPRESSION.name }
            copy.createRelationshipTo(ie) { Relationship.INDEX_EXPRESSION.name }
        }
        node.hasLabel { NodeType.MemberAccess.name } -> {
            val l =
                copyExpression(
                    node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.LEFT_HAND_SIDE.name } }.endNode,
                    tx,
                )
            copy.createRelationshipTo(l) { Relationship.LEFT_HAND_SIDE.name }
        }
        node.hasLabel { NodeType.Call.name } -> {
            val ce =
                copyExpression(
                    node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.CALL_EXPRESSION.name } }.endNode,
                    tx,
                )
            copy.createRelationshipTo(ce) { Relationship.CALL_EXPRESSION.name }

            val argRels = node.getRelationships(Direction.OUTGOING).filter { it.isType { Relationship.HAS_ARGUMENT.name } }
            argRels.forEach { arg ->
                val index = arg.getProperty("argIndex") as Int
                val argNode = copyExpression(arg.endNode, tx)
                val r = copy.createRelationshipTo(argNode) { Relationship.HAS_ARGUMENT.name }
                r.setProperty("argIndex", index)
            }

            val nargRels = node.getRelationships(Direction.OUTGOING).filter { it.isType { Relationship.HAS_NAMED_ARGUMENT.name } }
            nargRels.forEach { arg ->
                val index = arg.getProperty("argIndex") as Int
                val name = arg.getProperty("name") as String
                val argNode = copyExpression(arg.endNode, tx)
                val r = copy.createRelationshipTo(argNode) { Relationship.HAS_NAMED_ARGUMENT.name }
                r.setProperty("argIndex", index)
                r.setProperty("name", name)
            }
        }
        node.hasLabel { NodeType.ConditionalExpression.name } -> {
            val condition =
                copyExpression(node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.CONDITION.name } }.endNode, tx)
            val te =
                copyExpression(
                    node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.TRUE_EXPRESSION.name } }.endNode,
                    tx,
                )
            val fe =
                copyExpression(
                    node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.FALSE_EXPRESSION.name } }.endNode,
                    tx,
                )
            copy.createRelationshipTo(condition) { Relationship.CONDITION.name }
            copy.createRelationshipTo(te) { Relationship.TRUE_EXPRESSION.name }
            copy.createRelationshipTo(fe) { Relationship.FALSE_EXPRESSION.name }
        }
    }
    return copy
}

fun patchTruncation(graphDb: GraphDatabaseService) {
    val ftx = graphDb.beginTx()
    val res =
        ftx
            .execute("match (n: TruncationBug) return n")
            .map { r ->
                val node = r["n"] as Node
                node.id
            }.asSequence()
            .toList()
    ftx.commit()

    res.forEach { r ->
        val tx = graphDb.beginTx()
        val node = tx.getNodeById(r)
        val sourceType = node.getProperty("sourceType") as String
        val targetType = node.getProperty("targetType") as String
        val statement = node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.AFFECTED_STATEMENT.name } }.endNode
        val argument = node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.HAS_ARGUMENT.name } }.endNode

        val copy = copyExpression(argument, tx)
        val castToTarget = createFunctionCall(targetType, tx, listOf(copy))
        val castBack = createFunctionCall(sourceType, tx, listOf(castToTarget))
        val cond = eq(castBack, copy, tx)
        val a = createAssert(cond, tx)
        insertBeforeNode(statement, a)

        reportTruncation(statement, argument, a)
        tx.commit()
    }
}
