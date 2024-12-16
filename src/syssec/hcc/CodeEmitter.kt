package syssec.hcc

import java.io.File
import org.neo4j.graphdb.Direction
import org.neo4j.graphdb.GraphDatabaseService
import org.neo4j.graphdb.Node
import org.neo4j.graphdb.RelationshipType

fun emitCode(graphDb: GraphDatabaseService, filename: String) {
    var code = ""
    val tx = graphDb.beginTx()
    val unit = tx.findNodes { NodeType.SourceUnit.name }
    unit.forEach { sourceUnit ->
        val pragmas = sourceUnit.getProperty("pragmas") as Array<String>
        pragmas.forEach {
            code += "$it\n"
        }

        val rels = sourceUnit.getRelationships(Direction.OUTGOING).sortedBy { it.getProperty("childIndex") as Int }
        rels.forEach {
            when (it.type.name()) {
                Relationship.CONTAINS_CONTRACT.name -> code += "${emitContractCode(it.endNode)}\n\n"
                else -> throw Exception("Emitter: ${it.type.name()} unknown child node of SourceUnit.")
            }
        }
    }
    tx.commit()

    val out = File(filename)
    if (out.exists()) {
        out.delete()
    }
    out.createNewFile()
    out.writeText(code)
}

fun emitContractCode(node: Node): String {
    val name = node.getProperty("name") as String
    val isAbstract = if(node.getProperty("abstract") as Boolean) "abstract " else ""
    val rels = node.getRelationships(Direction.OUTGOING)
            .filter { it.hasProperty("childIndex") }
            .sortedBy { it.getProperty("childIndex") as Int }
    val children = rels.map {
        when (it.type.name()) {
            Relationship.CONTAINS_STATE.name -> emitStateVariableCode(it.endNode)
            Relationship.DECLARES_EVENT.name -> emitEventDeclarationCode(it.endNode)
            Relationship.DECLARES_STRUCT.name -> emitStructDeclarationCode(it.endNode)
            Relationship.HAS_FUNCTION.name -> emitFunctionCode(it.endNode)
            Relationship.MODIFIER_DEFINITION.name -> emitModifierDefinitionCode(it.endNode)
            Relationship.USING_FOR.name -> emitUsingForCode(it.endNode)
            Relationship.ENUM_DEFINITION.name -> emitEnumDefinitionCode(it.endNode)
            else -> throw Exception("Contract relationship ${it.type.name()} leaked to emitter.")
        }
    }
    val childCodes = children.joinToString(separator = "\n")
    val childCode = childCodes.split("\n").joinToString(prefix = "\t", separator = "\n\t")
    val kind = when {
        node.hasLabel { NodeType.Library.name } -> "library"
        node.hasLabel { NodeType.Contract.name } -> "contract"
        node.hasLabel { NodeType.Interface.name } -> "interface"
        else -> error("Emitter is unaware of contracts of this kind. NodeId: ${node.id}")
    }
    val baseContracts = node.getRelationships(Direction.OUTGOING).filter {
        it.isType { Relationship.INHERITANCE_DECLARATION.name }
    }.sortedBy {
        it.getProperty("baseIndex") as Int
    }.map { relationship ->
        val inherit = relationship.endNode
        val baseName = inherit.getRelationships(Direction.OUTGOING).first {
            it.isType { Relationship.TYPE.name }
        }.endNode.getProperty("name") as String
        val r = inherit.getRelationships(Direction.OUTGOING).filter { it.isType { Relationship.HAS_ARGUMENT.name } }.sortedBy { it.getProperty("argIndex") as Int }.map { emitExpressionCode(it.endNode) }.joinToString(prefix = "(", postfix = ")", separator = ", ")
        val args = if(r == "()") "" else r
        "$baseName$args"
    }
    val bases = if (baseContracts.isEmpty()) " " else baseContracts.joinToString(prefix = " is ", postfix = " ", separator = ", ")
    return "$isAbstract$kind $name$bases{\n$childCode\n}"
}

fun emitEnumDefinitionCode(node: Node): String {
    val name = node.getProperty("name") as String
    val members = when (val m = node.getRelationships(Direction.OUTGOING).filter { it.isType { Relationship.HAS_MEMBER.name } }.sortedBy { it.getProperty("memberIndex") as Int }
            .joinToString(prefix = "{\n\t", separator = ",\n\t", postfix =
            "\n}") { it.endNode.getProperty("name") as String }) {
        "{\n\t\n}" -> "{}"
        else -> m
    }
    return "enum $name $members"
}

fun emitStructDeclarationCode(node: Node): String {
    val name = node.getProperty("name") as String
    val members = when (val m = node.getRelationships(Direction.OUTGOING).filter { it.isType { Relationship.HAS_MEMBER.name } }.sortedBy { it.getProperty("memberIndex") as Int }
            .joinToString(prefix = "{\n\t", separator = ";\n\t", postfix =
            ";\n}") { emitVariableCode(it.endNode) }) {
        "{\n\t\n}" -> "{}"
        else -> m
    }
    return "struct $name $members"
}

fun emitModifierDefinitionCode(node: Node): String {
    val name = node.getProperty("name") as String
    if (node.getProperty("visibility") != "internal") {
        error("non-internal modifiers are not yet implemented")
    }
    val params = emitParameterList(node)
    val code = emitBodyCode(node)
    return "modifier $name($params) $code"
}

fun emitBodyCode(node: Node): String {
    val bodRel = node.relationships.filter { it.isType { Relationship.BODY.name } }
    val code = if (bodRel.isNotEmpty()) getBodyCode(bodRel.first().endNode).split("\n").joinToString(prefix = "\t", separator = "\n\t") else ""
    return if (code.isNotEmpty())
        "{\n$code\n}"
    else
        "{}"
}

fun emitUsingForCode(node: Node): String {
    val libName = getTypeString(node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.LIBRARY_TYPE.name } }.endNode)
    val tn = node.getRelationships(Direction.OUTGOING).filter { it.isType { Relationship.FOR_TYPE.name } }.map { it.endNode }
    val targetName = if(tn.isNotEmpty()) getTypeString(tn[0]) else "*"
    return "using $libName for $targetName;"
}

fun emitEventDeclarationCode(node: Node): String {
    val name = node.getProperty("name") as String
    val params = emitParameterList(node)
    return "event $name($params);"
}

fun emitParameterList(node: Node): String {
    val paramRels = node.getRelationships(Direction.OUTGOING)
            .filter { it.isType { Relationship.HAS_PARAMETER.name } }
            .sortedBy { it.getProperty("parameterIndex") as Int }
    return if (paramRels.isEmpty()) "" else paramRels.joinToString(separator = ", ") {
        val name = it.endNode.getProperty("name") as String
        val type = getTypeOfNode(it.endNode)
        val indexed = if (it.endNode.getProperty("indexed") as Boolean) " indexed " else " "
        val location = it.endNode.getProperty("location") as String
        "$type $location$indexed$name"
    }
}

fun emitFunctionCode(node: Node): String {
    var name = if (node.hasLabel { NodeType.Fallback.name } || node.hasLabel { NodeType.Receiver.name }) "" else " ${node.getProperty("name") as String}"

    val params = emitParameterList(node)

    val visibility = if (node.hasProperty("visibility")) node.getProperty("visibility") as String else ""
    val overrides = if(node.getProperty("override") as Boolean) " override " else ""
    val virtual = if(node.getProperty("virtual") as Boolean) " virtual " else ""

    val retRels = node.getRelationships(Direction.OUTGOING)
            .filter { it.isType { Relationship.RETURNS.name } }
            .sortedBy { it.getProperty("returnIndex") as Int }
    val returns = if (retRels.isEmpty()) "" else " ${retRels.joinToString(prefix = "returns (", separator = ", ", postfix = ")") {
        "${getTypeOfNode(it.endNode)} ${it.endNode.getProperty("location") as String} ${it.endNode.getProperty("name") as String}".trim()
    }}"

    val stateMutability = when (val sm = node.getProperty("stateMutability") as String) {
        "nonpayable" -> ""
        else -> " $sm"
    }
    val implemented = node.getProperty("fullyImplemented") as Boolean
    val childCode = if (!implemented) ";" else {
        emitBodyCode(node)
    }

    val kind = when {
        node.hasLabel { NodeType.Constructor.name } ->
        {
            name = ""
            "constructor"
        }
        node.hasLabel { NodeType.Receiver.name } -> {
            name = ""
            "receive"
        }
        else -> "function"
    }

    val modifiers = node.getRelationships(Direction.OUTGOING).filter { it.isType { Relationship.INVOKES_MODIFIER.name } }
            .sortedBy { it.getProperty("modifierIndex") as Int }.joinToString(prefix = " ", separator = " ", postfix = " ") { modRel ->
                emitModifierInvocationCode(modRel.endNode)
            }
    return "$kind$name($params) $overrides$visibility$stateMutability$virtual$modifiers$returns $childCode"
}

fun emitModifierInvocationCode(node: Node) : String {
    val name = node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.MODIFIER_NAME.name } }.endNode.getProperty("name") as String
    val argRels = node.getRelationships(Direction.OUTGOING).filter { it.isType { Relationship.HAS_ARGUMENT.name } }.sortedBy { it.getProperty("argIndex") as Int }
    val args = if(argRels.isEmpty()) "" else argRels.joinToString(separator = ", ") {
        emitExpressionCode(it.endNode)
    }
    return "$name($args)"
}

fun getBodyCode(node: Node) : String {
    var code = getStatementCode(node)
    var n = node
    while (n.hasRelationship(Direction.OUTGOING, RelationshipType { Relationship.LEADS.name })) {
        n = n.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.LEADS.name } }.endNode
        code += "\n${getStatementCode(n)}"
    }
    return code
}

fun getStatementCode(node: Node): String {
    return when {
        node.hasLabel { NodeType.BlockStatement.name } -> emitBlockStatementCode(node)
        node.hasLabel { NodeType.BlockEntry.name } -> ""
        node.hasLabel { NodeType.BreakStatement.name } -> "break;"
        node.hasLabel { NodeType.ContinueStatement.name } -> "continue;"
        node.hasLabel { NodeType.DoWhileStatement.name } -> emitDoWhileStatementCode(node)
        node.hasLabel { NodeType.EmitStatement.name} -> emitEmitStatement(node)
        node.hasLabel { NodeType.EmptyStatement.name } -> ""
        node.hasLabel { NodeType.EndBlock.name } -> ""
        node.hasLabel { NodeType.EndIf.name } -> ""
        node.hasLabel { NodeType.EndLoop.name } -> ""
        node.hasLabel { NodeType.ExpressionStatement.name } -> emitExpressionStatementCode(node)
        node.hasLabel { NodeType.ForStatement.name } -> emitForStatementCode(node)
        node.hasLabel { NodeType.FunctionReturn.name } -> emitReturnStatementCode(node)
        node.hasLabel { NodeType.IfStatement.name } -> emitIfStatementCode(node)
        node.hasLabel { NodeType.InlineAssemblyStatement.name } -> emitInlineAssemblyStatement(node)
        node.hasLabel { NodeType.PlaceholderStatement.name } -> "_;"
        node.hasLabel { NodeType.ThrowStatement.name } -> "throw;"
        node.hasLabel { NodeType.VariableDeclarationStatement.name } -> emitVariableDeclarationCode(node)
        node.hasLabel { NodeType.WhileStatement.name } -> emitWhileStatementCode(node)
        else -> error("node (${node.id}) has another label: ${node.labels}")
    }
}

fun emitBlockStatementCode(node: Node): String {
    val body = node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.TRUE_BODY.name } }.endNode
    val code = getBodyCode(body).split("\n").joinToString(prefix = "\t", separator = "\n\t")
    return "{\n$code\n}"
}

fun emitEmitStatement(node: Node): String {
    val fc = node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.EVENT_CALL.name } }.endNode
    val functionCall = emitCallExpressionCode(fc)
    return "emit $functionCall;"
}

fun emitDoWhileStatementCode(node: Node): String {
    val rump = emitWhileStatementHead(node)
    val loopRel = node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.LOOP_LEAD.name } }
    val loopBody = getBodyCode(loopRel.endNode).split("\n").joinToString(prefix = "\n\t", postfix = "\n", separator = "\n\t")
    return "do {$loopBody} ${rump};"
}

fun emitWhileStatementCode(node: Node): String {
    val head = emitWhileStatementHead(node)
    val loopRel = node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.LOOP_LEAD.name } }
    val loopBody = getBodyCode(loopRel.endNode).split("\n").joinToString(prefix = "\n\t", postfix = "\n", separator = "\n\t")
    return "$head {$loopBody}"
}

fun emitWhileStatementHead(node: Node): String {
    val condRel = node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.CONDITION.name } }
    val condition = emitExpressionCode(condRel.endNode)
    return "while ($condition)"
}

fun emitForStatementCode(node: Node): String {
    val head = emitForStatementHead(node)
    val loopRel = node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.LOOP_LEAD.name } }
    val loopBody = getBodyCode(loopRel.endNode).split("\n").joinToString(prefix = "\n\t", postfix = "\n", separator = "\n\t")
    return "$head {$loopBody}"
}

fun emitForStatementHead(node: Node) : String {
    val condRel = node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.CONDITION.name } }
    val condition = emitExpressionCode(condRel.endNode)

    val loopexprRel = node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.LOOP_EXPRESSION.name } }
    val loopexpr = emitExpressionCode(loopexprRel.endNode)

    val initRel = node.getRelationships(Direction.OUTGOING).firstOrNull { it.isType { Relationship.INITIALIZATION_EXPRESSION.name } }
    val init = if (initRel != null) getStatementCode(initRel.endNode) else ";"

    return "for ($init $condition; $loopexpr)"
}

fun emitVariableDeclarationCode(node: Node): String {
    val varnodes = node.getRelationships(Direction.OUTGOING)
        .filter { it.isType { Relationship.DECLARES.name } }
        .sortedBy { it.getProperty("declarationIndex") as Int }
        .map { rel ->
            val n = rel.endNode
            if(n.hasLabel { NodeType.LocalVariable.name })
                emitVariableCode(n)
            else
                ""
        }
    val vardecl = if(varnodes.size == 1) varnodes[0] else varnodes.joinToString(prefix="(", postfix=")", separator=", ")
    val inits = node.getRelationships(Direction.OUTGOING).filter { it.isType { Relationship.RIGHT_HAND_SIDE.name } }
    val initialValue = if (inits.isEmpty()) "" else " = ${emitExpressionCode(inits.first().endNode)}"
    return "$vardecl$initialValue;"
}

fun emitReturnStatementCode(node: Node): String {
    val exprel = node.getRelationships(Direction.OUTGOING).filter { it.isType { Relationship.RETURN_EXPRESSION.name } }
    return if(exprel.any()){
        val expr = emitExpressionCode(exprel[0].endNode)
        "return $expr;"
    }
    else "return;"
}

fun emitExpressionCode(exp: Node): String {

    return when {
        exp.hasLabel { NodeType.AssignmentStatement.name } -> emitAssignmentCode(exp)
        exp.hasLabel { NodeType.BinaryOperation.name } -> emitBinaryOperationExpression(exp)
        exp.hasLabel { NodeType.Call.name } -> emitCallExpressionCode(exp)
        exp.hasLabel { NodeType.ConditionalExpression.name } -> emitConditionalExpression(exp)
        exp.hasLabel { NodeType.EmptyExpression.name } -> ""
        exp.hasLabel { NodeType.ElementaryTypeExpression.name } -> emitElementaryTypeExpression(exp)
        exp.hasLabel { NodeType.FunctionCallOptions.name } -> emitFunctionCallOptionsCode(exp)
        exp.hasLabel { NodeType.IdentifierExpression.name } -> emitIdentifierExpressionCode(exp)
        exp.hasLabel { NodeType.IndexAccessExpression.name } -> emitIndexExpressionCode(exp)
        exp.hasLabel { NodeType.IndexRangeAccessExpression.name } -> emitIndexRangeAccessExpression(exp)
        exp.hasLabel { NodeType.LiteralExpression.name } -> emitLiteralExpression(exp)
        exp.hasLabel { NodeType.LocalVariable.name } -> emitIdentifierExpressionCode(exp)
        exp.hasLabel { NodeType.MemberAccess.name } -> emitMemberAccessCode(exp)
        exp.hasLabel { NodeType.NewExpression.name } -> emitNewExpression(exp)
        exp.hasLabel { NodeType.TupleExpression.name } -> emitTupleExpression(exp)
        exp.hasLabel { NodeType.UnaryOperation.name } -> emitUnaryOperation(exp)
        else -> error("expression (${exp.id}) has another label: ${exp.labels}")
    }
}

fun emitConditionalExpression(node: Node): String {
    val c = node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.CONDITION.name } }.endNode
    val condition = emitExpressionCode(c)
    val t = node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.TRUE_EXPRESSION.name } }.endNode
    val trueExpression = emitExpressionCode(t)
    val f = node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.FALSE_EXPRESSION.name } }.endNode
    val falseExpression = emitExpressionCode(f)
    return "$condition ? $trueExpression : $falseExpression"
}

fun emitInlineAssemblyStatement(node: Node): String {
    return "assembly ${node.getProperty("operations") as String}"
}

fun emitUnaryOperation(node: Node): String {
    val op = node.getProperty("operator") as String
    val subexp = node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.SUB_EXPRESSION.name } }.endNode
    return if (node.getProperty("prefix") as Boolean) {
        val top = if(op == "delete") "$op " else op
        "$top${emitExpressionCode(subexp)}"
    }
    else
        "${emitExpressionCode(subexp)}$op"
}

fun emitTupleExpression(node: Node): String {
    val comrels = node.getRelationships(Direction.OUTGOING).filter { it.isType { Relationship.HAS_COMPONENT.name } }.sortedBy { it.getProperty("componentIndex") as Int }
    return when (node.hasProperty("isInlineArray") && node.getProperty("isInlineArray") as Boolean) {
        true -> comrels.joinToString(separator = ", ", prefix = "[", postfix = "]") { emitExpressionCode(it.endNode) }
        false -> comrels.joinToString(separator = ", ", prefix = "(", postfix = ")") { emitExpressionCode(it.endNode) }
    }
}

fun emitNewExpression(exp: Node): String {
    return "new ${getTypeOfNode(exp)}"
}

fun emitLiteralExpression(exp: Node): String {
    val v = exp.getProperty("value") as String
    val subdenomination = if (exp.hasProperty("subdenomination")) exp.getProperty("subdenomination") as String else ""
    val value = if (exp.hasLabel { NodeType.StringLiteral.name }) "\"$v\"" else v
    val pref = if (exp.hasProperty("prefix")) exp.getProperty("prefix") as String else ""
    return "$pref$value $subdenomination"
}

fun emitBinaryOperationExpression(exp: Node): String {
    val op = exp.getProperty("operator") as String
    val lhs = exp.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.LEFT_EXPRESSION.name } }.endNode
    val rhs = exp.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.RIGHT_EXPRESSION.name } }.endNode
    return "${emitExpressionCode(lhs)} $op ${emitExpressionCode(rhs)}"
}

fun emitElementaryTypeExpression(exp: Node): String {
    return exp.getProperty("typeName") as String
}

fun emitMemberAccessCode(exp: Node): String {
    val name = exp.getProperty("memberName") as String
    val lhs = emitExpressionCode(exp.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.LEFT_HAND_SIDE.name } }.endNode)
    return "$lhs.$name"
}

fun emitCallExpressionCode(call: Node): String {
    if(call.hasLabel { NodeType.FunctionCallOptions.name }) {
        return emitFunctionCallOptionsCode(call)
    }
    val expr = call.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.CALL_EXPRESSION.name } }.endNode
    val callExpression = emitExpressionCode(expr)
    val argRels = call.getRelationships(Direction.OUTGOING).filter { it.isType { Relationship.HAS_ARGUMENT.name } }.sortedBy { it.getProperty("argIndex") as Int }
    val args = if (argRels.isEmpty()) "" else argRels.joinToString(separator = ", ") {
        emitExpressionCode(it.endNode)
    }
    val nargRels = call.getRelationships(Direction.OUTGOING).filter { it.isType { Relationship.HAS_NAMED_ARGUMENT.name } }.sortedBy { it.getProperty("argIndex") as Int }
    val nargs = if (nargRels.isEmpty()) "" else nargRels.joinToString(separator = ", ", prefix = "{", postfix = "}") {
        "${it.getProperty("name") as String}: ${emitExpressionCode(it.endNode)}"
    }
    return "$callExpression($args$nargs)"
}

fun emitFunctionCallOptionsCode(call: Node): String {
    val expr = call.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.CALL_EXPRESSION.name } }.endNode
    val callExpression = emitExpressionCode(expr)

    val nargRels = call.getRelationships(Direction.OUTGOING).filter { it.isType { Relationship.HAS_NAMED_ARGUMENT.name } }.sortedBy { it.getProperty("argIndex") as Int }
    val nargs = if (nargRels.isEmpty()) "" else nargRels.joinToString(separator = ", ", prefix = "{", postfix = "}") {
        "${it.getProperty("name") as String}: ${emitExpressionCode(it.endNode)}"
    }
    return "$callExpression $nargs"
}

fun emitIdentifierExpressionCode(exp: Node): String {
    return exp.getProperty("name") as String
}

fun emitIndexExpressionCode(exp: Node): String {
    val base = exp.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.BASE_EXPRESSION.name } }.endNode
    val baseExpr = emitExpressionCode(base)
    val index = exp.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.INDEX_EXPRESSION.name } }.endNode
    val indexExpr = emitExpressionCode(index)
    return "$baseExpr[$indexExpr]"
}

fun emitIndexRangeAccessExpression(exp: Node): String {
    val base = emitExpressionCode(exp.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.BASE_EXPRESSION.name } }.endNode)

    val srel = exp.getRelationships(Direction.OUTGOING).filter { it.isType { Relationship.START_EXPRESSION.name } }.map { it.endNode }
    val start = if(srel.isEmpty()) "" else emitExpressionCode(srel[0])

    val erel = exp.getRelationships(Direction.OUTGOING).filter { it.isType { Relationship.END_EXPRESSION.name } }.map { it.endNode }
    val end = if(erel.isEmpty()) "" else emitExpressionCode(erel[0])
    return "$base[$start:$end]"
}

fun emitIfStatementCode(node: Node): String {
    val head = emitIfStatementHead(node)

    val trueRel = node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.TRUE_BODY.name } }
    val trueBody = getBodyCode(trueRel.endNode).split("\n").joinToString(prefix = "\n\t", postfix = "\n", separator = "\n\t")

    val falseRel = node.getRelationships(Direction.OUTGOING).filter { it.isType { Relationship.FALSE_BODY.name } }
    val falseBody = if (falseRel.isEmpty()) "" else " else {\n${getBodyCode(falseRel.first().endNode).split("\n").joinToString(prefix = "\n\t", postfix = "\n", separator = "\n\t")}\n}"
    return "$head {$trueBody}$falseBody"
}

fun emitIfStatementHead(node: Node): String {
    val condRel = node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.CONDITION.name } }
    val condition = emitExpressionCode(condRel.endNode)
    return "if ($condition)"
}

fun emitExpressionStatementCode(node: Node, report: Boolean = false): String {
    val code = when {
        node.hasLabel { NodeType.AssignmentStatement.name } -> emitAssignmentCode(node)
        node.hasLabel { NodeType.Call.name } -> emitCallExpressionCode(node)
        node.hasLabel { NodeType.DoWhileStatement.name } -> if(report) "${emitWhileStatementHead(node)};" else emitDoWhileStatementCode(node)
        // ! HACK, this is only valid in integer bug reporting
        node.hasLabel { NodeType.FunctionReturn.name } -> if(report) emitReturnStatementCode(node) else error("CodeEmmiter: FunctionReturn is not an Expression Statement")
        node.hasLabel { NodeType.IdentifierExpression.name } -> emitIdentifierExpressionCode(node)
        node.hasLabel { NodeType.UnaryOperation.name } -> emitUnaryOperation(node)
        node.hasLabel { NodeType.IfStatement.name } -> if(report) emitIfStatementHead(node) else emitIfStatementCode(node)
        node.hasLabel { NodeType.ForStatement.name } -> if(report) emitForStatementHead(node) else emitForStatementCode(node)
        node.hasLabel { NodeType.WhileStatement.name } -> if(report) emitWhileStatementHead(node) else emitWhileStatementCode(node)
        node.hasLabel { NodeType.EmitStatement.name } -> emitEmitStatement(node)
        else -> error("expression statement (${node.id}) has another label: ${node.labels}")
    }
    return "$code;"
}

fun emitAssignmentCode(node: Node): String {
    val lhs = emitExpressionCode(node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.LEFT_HAND_SIDE.name } }.endNode)
    val rhs = emitExpressionCode(node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.RIGHT_HAND_SIDE.name } }.endNode)
    return "$lhs = $rhs"
}

fun emitStateVariableCode(node: Node): String {
    val rels = node.getRelationships(Direction.OUTGOING).filter { it.isType { Relationship.INITIAL_VALUE.name } }
    val initialization = when {
         rels.isNotEmpty() -> {
             if (rels.size > 1) error("Code emitter does not handle multiple initialization expressions for state variables. (NodeId: ${node.id}")
             " = ${emitExpressionCode(rels[0].endNode)}"
        }
        else -> ""
    }
    return "${emitVariableCode(node)}$initialization;"
}

fun emitVariableCode(node: Node): String {
    val typeName = getTypeOfNode(node)
    val visibility = if ((node.getProperty("visibility") as String == "internal")) "" else " ${node.getProperty("visibility") as String}"
    val constant = if (node.hasProperty("constant") && node.getProperty("constant") as Boolean) "constant " else ""
    val overrides = if(node.hasProperty("override") && node.getProperty("override") as Boolean) " override " else ""
    val storageLocation = when (val loc = node.getProperty("storageLocation") as String) {
        "default" -> ""
        else -> " $loc"
    }
    val name = node.getProperty("name") as String
    return "$typeName$storageLocation$visibility$overrides $constant$name"
}

fun getTypeOfNode(node: Node): String {
    val typeNode = node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.TYPE.name } }.endNode
    return getTypeString(typeNode)
}

fun getTypeString(node: Node, mutability: Boolean = true): String {
    val type = when {
        node.hasLabel { NodeType.MappingType.name } -> getMappingTypeString(node)
        node.hasProperty("name") -> (node.getProperty("name") as String).split(".").last()
        node.hasProperty("typeString") -> (node.getProperty("typeString") as String).split(".").last()
        else -> error("Type (${node.id}) does not have a property to derive it from: ${node.propertyKeys}")
    }

    return if(mutability){
        val stateMutability = when {
            !node.hasProperty("stateMutability") -> ""
            else -> when (val sm = node.getProperty("stateMutability") as String?) {
                "nonpayable" -> ""
                else -> " $sm"
            }
        }
        "${type}$stateMutability"
    }  else type
}

fun getMappingTypeString(node: Node) : String {
    val valueType = getTypeString(node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.VALUE_TYPE.name } }.endNode, false)
    val keyType = getTypeString(node.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.KEY_TYPE.name } }.endNode, false)
    return "mapping(${keyType} => ${valueType})"
}
