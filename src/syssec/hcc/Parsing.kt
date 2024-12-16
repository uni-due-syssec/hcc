package syssec.hcc

import com.beust.klaxon.JsonObject
import org.neo4j.graphdb.GraphDatabaseService
import org.neo4j.graphdb.Node
import org.neo4j.graphdb.Transaction

enum class NodeType {
    Addition,
    AddOverflow,
    AssignmentStatement,
    ArithmeticExpression,
    BasicType,
    BinaryOperation,
    BitwiseOperation,
    BlockEntry,
    BlockStatement,
    BoolLiteral,
    BreakStatement,
    Call,
    ComparisonOperation,
    ConditionalExpression,
    Constructor,
    ContainsCall,
    ContainsDelegateCall,
    ContainsExternalCall,
    ContinueStatement,
    Contract,
    CreateBasedReentrancy,
    CrossFunctionReentrancy,
    DelegateCall,
    DelegatedReentrancy,
    Division,
    DoWhileStatement,
    ElementaryType,
    ElementaryTypeExpression,
    EmitStatement,
    EmptyExpression,
    EmptyStatement,
    EndBlock,
    EndIf,
    EndLoop,
    EnumDefinition,
    EnumValue,
    EventDeclaration,
    Exponentiation,
    Expression,
    ExpressionStatement,
    ExternalCall,
    Fallback,
    ForStatement,
    Function,
    FunctionCall,
    FunctionCallOptions,
    FunctionReturn,
    IdentifierExpression,
    IfStatement,
    IndexAccessExpression,
    IndexRangeAccessExpression,
    InheritanceSpecifier,
    InlineAssemblyStatement,
    Interface,
    Library,
    LiteralExpression,
    LogicExpression,
    LocalVariable,
    MappingType,
    MemberAccess,
    MemberVariable,
    ModifierDefinition,
    ModifierInvocation,
    Modulus,
    Multiplication,
    MulOverflow,
    UnaryOverflow,
    NewExpression,
    NumberLiteral,
    Parameter,
    PlaceholderStatement,
    Receiver,
    Reentrancy,
    ReturnParameter,
    ShiftOperation,
    SourceUnit,
    Statement,
    StateVariable,
    StringLiteral,
    StructConstructorCall,
    StructDeclaration,
    Subtraction,
    ThrowStatement,
    TraditionalReentrancy,
    TruncationBug,
    TupleExpression,
    Type,
    TypeConversion,
    UnaryOperation,
    Underflow,
    UserDefinedType,
    UsingForDefinition,
    Variable,
    VariableDeclarationStatement,
    Vulnerability,
    WhileStatement,
    WritesState,
}

enum class Relationship {
    AFFECTED_STATEMENT,
    ARRAY_BASE_TYPE,
    ARRAY_LENGTH,
    ASSEMBLY_MAY_WRITE,
    ASSEMBLY_MAY_READ,
    BASE_CONTRACT,
    BASE_EXPRESSION,
    BODY,
    CALL_EXPRESSION,
    CHECKS_LOCK,
    CONDITION,
    CONTAINS_CONTRACT,
    CONTAINS_STATE,
    DECLARES,
    DECLARES_EVENT,
    DECLARES_STRUCT,
    DEFINES_TYPE,
    END_EXPRESSION,
    ENUM_DEFINITION,
    EVENT_CALL,
    FALSE_BODY,
    FALSE_EXPRESSION,
    FOR_TYPE,
    GUARDED,
    HAS_ARGUMENT,
    HAS_CALL,
    HAS_COMPONENT,
    HAS_FUNCTION,
    HAS_LOCAL_VARIABLE,
    HAS_MEMBER,
    HAS_NAMED_ARGUMENT,
    HAS_PARAMETER,
    HAS_VALUE,
    INDEX_EXPRESSION,
    INHERITANCE_DECLARATION,
    INITIALIZATION_EXPRESSION,
    INITIAL_VALUE,
    INVOKES_MODIFIER,
    KEY_TYPE,
    LEADS,
    LEFT_EXPRESSION,
    LEFT_HAND_SIDE,
    LIBRARY_TYPE,
    LOCK_VAR,
    LOOP_EXPRESSION,
    LOOP_LEAD,
    LOOP_LEAVE,
    MODIFIER_DEFINITION,
    MODIFIER_NAME,
    PREVIOUS_NODE,
    RE_ASSIGNMENT,
    RE_CALL,
    RE_CROSS_FUNCTION,
    RE_DELEGATE_CALL,
    RE_FUNCTION,
    RE_STATE_VARIABLE,
    READS,
    REFERENCES,
    RETURN_EXPRESSION,
    RETURN_SITE,
    RETURNS,
    RIGHT_EXPRESSION,
    RIGHT_HAND_SIDE,
    START_EXPRESSION,
    SUB_EXPRESSION,
    TRUE_BODY,
    TRUE_EXPRESSION,
    TYPE,
    USING_FOR,
    VALUE_TYPE,
    WRITES,
}

class Mappings {
    companion object {
        val idMap = mutableMapOf<Int, Long>()

        fun getNodeByAstId(
            id: Int,
            tx: Transaction,
        ): Node? =
            when (val nodeId = idMap[id]) {
                null -> null
                else -> tx.getNodeById(nodeId)
            }

        var patchRE = false
        var patchIO = false

        val returnParameters = mutableMapOf<Int, MutableSet<Long>>()
        var currentFunctionId: Long? = null
        val comparisonOperators = listOf("<", "<=", "==", ">=", ">", "!=")
        val arithmethicOperators = listOf("+", "-", "*", "/", "%", "++", "--", "**")
        val logicOperators = listOf("&&", "||", "!")
        val bitwiseOperators = listOf("&", "|", "^", "~")
        val shiftOperators = listOf("<<", ">>")
        val ifEndIfHelper = mutableMapOf<Long, Long>()
        val typeMappings = mutableMapOf<String, Long>()
        val nestedCalls = mutableListOf<Long>()
        val nestedIdentifiers = mutableListOf<Long>()
        val nestedTypeConversions = mutableListOf<Long>()
        val nestedMemberAccess = mutableListOf<Long>()
        var currentCall: Long? = null
        var nestedArithmeticExpressions = mutableListOf<Long>()

        fun getCurrentFunctionNode(tx: Transaction): Node {
            if (currentFunctionId == null) error("not inside a function!")
            return tx.getNodeById(currentFunctionId!!)
        }

        fun getTypeIfExists(json: JsonObject): Long? {
            val td = json["typeDescriptions"] as JsonObject
            val typeIdentifier = (td["typeString"] as String).split(" ").last()
            val fn = typeMappings[typeIdentifier]
            return fn
        }

        val typeReferences = mutableSetOf<Pair<Long, Int>>()

        fun resolveTypeReferenceConstraints(graph: GraphDatabaseService) {
            val resolvedTypeReferences = mutableListOf<Pair<Long, Int>>()
            typeReferences.forEach {
                val tx = graph.beginTx()
                val n = idMap[it.second]
                if (n != null) {
                    val node = tx.getNodeById(n)
                    val definesTypeNode = tx.getNodeById(it.first)
                    node.createRelationshipTo(definesTypeNode) { Relationship.DEFINES_TYPE.name }
                    resolvedTypeReferences.add(it)
                }
                tx.commit()
            }
            resolvedTypeReferences.forEach {
                typeReferences.remove(it)
            }
        }

        val variableReferences = mutableMapOf<Long, Int>()

        fun collectVariableReferenceConstraint(
            nodeId: Long,
            target: Int,
            graph: GraphDatabaseService,
        ) {
            val node = idMap[target]
            if (node == null) {
                variableReferences[nodeId] = target
                return
            }
            variableReferences.remove(node)
            createRelationship(nodeId, node, Relationship.REFERENCES, graph)
        }

        fun resolveVariableReferencesConstraints(graph: GraphDatabaseService) {
            val resolvedVariableReferences = mutableListOf<Long>()
            variableReferences.keys.forEach {
                val tx = graph.beginTx()
                val n = idMap[variableReferences[it]!!]
                if (n != null) {
                    val to = tx.getNodeById(n)
                    val from = tx.getNodeById(it)
                    from.createRelationshipTo(to) { Relationship.REFERENCES.name }
                    resolvedVariableReferences.add(it)
                }
                tx.commit()
            }
            resolvedVariableReferences.forEach {
                variableReferences.remove(it)
            }
        }

        val returnReferences = mutableMapOf<Long, Int>()

        fun collectReturnRef(
            nodeId: Long,
            target: Int,
            graph: GraphDatabaseService,
        ) {
            val node = idMap[target]
            if (node == null) {
                returnReferences[nodeId] = target
                return
            }
            createRelationship(node, nodeId, Relationship.RETURN_SITE, graph)
        }

        fun resolveReturnReferencesConstraints(graph: GraphDatabaseService) {
            val resolvedReturnReferences = mutableListOf<Long>()
            returnReferences.keys.forEach { retRef ->
                val tx = graph.beginTx()
                val n = returnParameters[returnReferences[retRef]!!]
                val realRef = tx.getNodeById(retRef)
                if (n != null) {
                    n.forEach {
                        val from = tx.getNodeById(it)
                        from.createRelationshipTo(realRef) { Relationship.RETURN_SITE.name }
                    }
                    resolvedReturnReferences.add(retRef)
                }
                tx.commit()
            }
            resolvedReturnReferences.forEach {
                returnReferences.remove(it)
            }
        }
    }
}
