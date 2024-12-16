package syssec.hcc

import com.beust.klaxon.JsonArray
import com.beust.klaxon.JsonObject
import java.lang.Exception
import javax.naming.InvalidNameException
import org.neo4j.graphdb.*


fun convertToCodePropertyGraph(json: JsonObject, graph: GraphDatabaseService) {

    require(json["nodeType"] == "SourceUnit")
    val filename = (json["absolutePath"] as String).split("/").last()

    val tx = graph.beginTx()

    val srcFile = createMappedNode(json, tx)
    srcFile.addLabel() { NodeType.SourceUnit.name }
    srcFile.setProperty("filename", filename)
    val srcFileId = srcFile.id
    tx.commit()
    val nodes = json["nodes"] as JsonArray<*>
    /*
        We need to add indizes to the relationship between the source file and its contracts to assert that their order does not change when the code is emitted.
        In theory we could also rely on the node IDs given by Neo4j for this.
    */
    nodes.forEachIndexed { index, n ->
        assert(n is JsonObject)
        val node = n as JsonObject
        when (node["nodeType"]) {
            "PragmaDirective" -> addPragma(node, srcFileId, graph)
            "ContractDefinition" -> {
                val contract = createContractNode(node, graph)
                val itx = graph.beginTx()
                val contractNode = itx.getNodeById(contract)
                val srcNode = itx.getNodeById(srcFileId)
                val rel = srcNode.createRelationshipTo(contractNode) { Relationship.CONTAINS_CONTRACT.name }
                rel.setProperty("childIndex", index)
                itx.commit()
            }
            else -> throw InvalidNameException("can not match ${node["nodeType"]}")
        }
    }

    Mappings.resolveVariableReferencesConstraints(graph)
    Mappings.resolveReturnReferencesConstraints(graph)
    Mappings.resolveTypeReferenceConstraints(graph)
}

fun addPragma(json: JsonObject, srcFileId: Long, graphDb: GraphDatabaseService) {
    val literals = json["literals"] as JsonArray<String>
    val key = literals[0]
    val value = literals.slice(IntRange(1, literals.size - 1)).reduce { acc, string -> acc + string }
    val tx = graphDb.beginTx()
    val srcFile = tx.getNodeById(srcFileId)
    srcFile.setProperty("pragmas", arrayOf("pragma $key $value;"))
    tx.commit()
}

fun createContractNode(json: JsonObject, graphDb: GraphDatabaseService): Long {
    val tx = graphDb.beginTx()
    val contract = createMappedNode(json, tx)
    val name = json["name"] as String
    contract.setProperty("name", name)

    val ck = json["contractKind"]
    assert(ck is String)
    when (ck) {
        "contract" -> contract.addLabel { NodeType.Contract.name }
        "library" -> contract.addLabel { NodeType.Library.name }
        "interface" -> contract.addLabel { NodeType.Interface.name }
        else -> error("$ck is not implemented (yet).")
    }
    val fI = json["fullyImplemented"] as Boolean
    contract.setProperty("fullyImplemented", fI)
    val isAbstract = json.containsKey("abstract") && json["abstract"] != null && json["abstract"] as Boolean
    contract.setProperty("abstract", isAbstract)
    val contractId = contract.id
    tx.commit()

    val baseContracts = json["baseContracts"] as JsonArray<JsonObject>
    baseContracts.forEachIndexed { index, it ->
        val inheritanceId = addBaseContract(it, graphDb, contractId)
        createRelationshipWithIndex(contractId, inheritanceId, Relationship.INHERITANCE_DECLARATION, "baseIndex", index, graphDb)
    }

    val nodes = json["nodes"] as JsonArray<JsonObject>
    nodes.forEachIndexed { index, node ->
        when (node["nodeType"]) {
            "VariableDeclaration" -> {
                val svId = addStateVariable(node, graphDb)
                createRelationshipWithIndex(contractId, svId, Relationship.CONTAINS_STATE, "childIndex", index, graphDb)
            }
            "FunctionDefinition" -> {
                val functionId = addFunction(node, graphDb)
                createRelationshipWithIndex(contractId, functionId, Relationship.HAS_FUNCTION, "childIndex", index, graphDb)
            }
            "EventDefinition" -> {
                val eventDefinitionId = addEventDefinition(node, graphDb)
                createRelationshipWithIndex(contractId, eventDefinitionId, Relationship.DECLARES_EVENT, "childIndex", index, graphDb)
            }
            "StructDefinition" -> {
                val structDefinitionId = addStructDefinition(node, graphDb)
                createRelationshipWithIndex(contractId, structDefinitionId, Relationship.DECLARES_STRUCT, "childIndex", index, graphDb)
            }
            "UsingForDirective" -> {
                val usingForDefinitionId = addUsingForDefinition(node, graphDb)
                createRelationshipWithIndex(contractId, usingForDefinitionId, Relationship.USING_FOR, "childIndex", index, graphDb)
            }
            "ModifierDefinition" -> {
                val modifierDefinitionId = addModifierDefinition(node, graphDb)
                createRelationshipWithIndex(contractId, modifierDefinitionId, Relationship.MODIFIER_DEFINITION, "childIndex", index, graphDb)
            }
            "EnumDefinition" -> {
                val enumDefinitionId = addEnumDefinition(node, graphDb)
                createRelationshipWithIndex(contractId, enumDefinitionId, Relationship.ENUM_DEFINITION, "childIndex", index, graphDb)
            }
            else -> error("${node["nodeType"]} is not implemented (yet).")
        }
    }
    return contract.id
}

fun addEnumDefinition(json: JsonObject, graphDb: GraphDatabaseService): Long {
    val name = json["name"] as String

    val tx = graphDb.beginTx()
    val ed = createMappedNode(json, tx)
    ed.setProperty("name", name)
    ed.addLabel { NodeType.EnumDefinition.name }
    val edId = ed.id
    tx.commit()

    val members = json["members"] as JsonArray<JsonObject>
    members.forEachIndexed { index, member ->
        val m = addEnumMember(member, graphDb)
        createRelationshipWithIndex(edId, m, Relationship.HAS_MEMBER, "memberIndex", index, graphDb)
    }
    return edId
}

fun addEnumMember(json: JsonObject, graphDb: GraphDatabaseService) : Long {
    if(json["nodeType"] as String != "EnumValue") error("Enum member should be EnumValue, but is ${json["nodeType"] as String}")
    val name = json["name"] as String
    val tx = graphDb.beginTx()
    val m = createMappedNode(json, tx)
    val mId = m.id
    m.setProperty("name", name)
    m.addLabel { NodeType.EnumValue.name }
    tx.commit()
    return mId
}

fun addModifierDefinition(json: JsonObject, graphDb: GraphDatabaseService): Long {
    val tx = graphDb.beginTx()
    val md = createMappedNode(json, tx)
    val mdId = md.id
    md.addLabel { NodeType.ModifierDefinition.name }
    md.setProperty("name", json["name"] as String)
    md.setProperty("visibility", json["visibility"] as String)
    tx.commit()
    Mappings.currentFunctionId = mdId
    addParametersToNode(json["parameters"] as JsonObject, graphDb, mdId)
    val body = addBlockBody(json["body"] as JsonObject, graphDb)
    createRelationship(mdId, body.first, Relationship.BODY, graphDb)
    return mdId
}

fun addUsingForDefinition(json: JsonObject, graphDb: GraphDatabaseService): Long {
    val tx = graphDb.beginTx()

    val ufd = createMappedNode(json, tx)
    val ufdId = ufd.id
    ufd.addLabel { NodeType.UsingForDefinition.name }

    tx.commit()

    val libraryType = getType(json["libraryName"] as JsonObject, graphDb)
    createRelationship(ufdId, libraryType, Relationship.LIBRARY_TYPE, graphDb)

    val tn = json["typeName"] as JsonObject?
    if(tn != null){
        val targetType = getType(tn, graphDb)
        createRelationship(ufdId, targetType, Relationship.FOR_TYPE, graphDb)
    }
    return ufdId
}

fun createRelationshipWithIndex(fromId: Long, toId: Long, relationship: Relationship, indexProperty: String, index: Int, graph: GraphDatabaseService) {
    val tx = graph.beginTx()
    val from = tx.getNodeById(fromId)
    val to = tx.getNodeById(toId)
    val rel = from.createRelationshipTo(to) { relationship.name }
    rel.setProperty(indexProperty, index)
    tx.commit()
}

fun createRelationship(fromId: Long, toId: Long, relationship: Relationship, graph: GraphDatabaseService) {
    val tx = graph.beginTx()
    val from = tx.getNodeById(fromId)
    val to = tx.getNodeById(toId)
    from.createRelationshipTo(to) { relationship.name }
    tx.commit()
}

fun hasLabel(nodeId: Long, labels: List<NodeType>, graph: GraphDatabaseService): Boolean {
    val tx = graph.beginTx()
    val node = tx.getNodeById(nodeId)
    labels.forEach {
        if (node.hasLabel { it.name }) {
            tx.commit()
            return true
        }
    }
    tx.commit()
    return false
}

fun addStructDefinition(json: JsonObject, graph: GraphDatabaseService): Long {
    val tx = graph.beginTx()
    val structDef = createMappedNode(json, tx)
    structDef.setProperty("canonicalName", json["canonicalName"] as String)
    structDef.setProperty("name", json["name"] as String)
    structDef.setProperty("visibility", json["visibility"] as String)
    structDef.addLabel { NodeType.StructDeclaration.name }
    val structDefId = structDef.id
    tx.commit()
    val members = json["members"] as JsonArray<JsonObject>
    members.forEachIndexed { index, member ->
        val m = addVariable(member, graph)
        addLabelToNodeUsingId(m, NodeType.MemberVariable, graph)
        createRelationshipWithIndex(structDefId, m, Relationship.HAS_MEMBER, "memberIndex", index, graph)
    }
    return structDefId
}

fun createMappedNode(json: JsonObject, tx: Transaction): Node {
    val node = tx.createNode()
    val id = json["id"] as Int
    Mappings.idMap[id] = node.id
    node.setProperty("ast_id", id)
    return node
}

fun addEventDefinition(json: JsonObject, graph: GraphDatabaseService): Long {
    val tx = graph.beginTx()
    val event = createMappedNode(json, tx)
    val name = json["name"] as String
    event.setProperty("name", name)
    event.addLabel { NodeType.EventDeclaration.name }
    val eventId = event.id
    if (json["anonymous"] as Boolean) {
        val astId = event.getProperty("ast_id") as Int
        tx.commit()
        throw Exception("check the semantics of anonymous EventDefinition for AST Node $astId")
    }
    tx.commit()
    addParametersToNode(json["parameters"] as JsonObject, graph, eventId)
    return eventId
}

fun addBaseContract(json: JsonObject, graph: GraphDatabaseService, contractId: Long): Long {
    return when (json["nodeType"] as String) {
        "InheritanceSpecifier" -> addInheritanceSpecifier(json, graph, contractId)
        else -> TODO("unknown node type for base contract: ${json["nodeType"] as String}")
    }
}

fun addInheritanceSpecifier(json: JsonObject, graph: GraphDatabaseService, contractId: Long): Long {
    val basename = json["baseName"] as JsonObject
    val baseType = getType(basename, graph)
    val reference = basename["referencedDeclaration"] as Int
    if (!Mappings.idMap.containsKey(reference)) {
        TODO("base contract was not parsed properly, missing id $reference")
    }
    val baseId = Mappings.idMap[reference]!!
    createRelationship(contractId, baseId, Relationship.BASE_CONTRACT, graph)
    val tx = graph.beginTx()
    val inherited = tx.createNode()
    inherited.addLabel { NodeType.InheritanceSpecifier.name }
    val inheritedId = inherited.id
    tx.commit()
    (json["arguments"] as JsonArray<JsonObject>?)?.forEachIndexed { index, it ->
        val arg = addExpression(it, graph)
        createRelationshipWithIndex(inheritedId, arg, Relationship.HAS_ARGUMENT, "argIndex", index, graph)
    }
    createRelationship(inheritedId, baseType, Relationship.TYPE, graph)
    return inheritedId
}

fun addFunction(json: JsonObject, graph: GraphDatabaseService): Long {
    val tx = graph.beginTx()
    val node = createMappedNode(json, tx)
    val name = json["name"] as String
    node.setProperty("name", name)
    val nodeId = node.id
    EnrichmentHelper.allFunctions.add(nodeId)
    if (json.containsKey("kind")) {
        when (val kind = json["kind"] as String) {
            "fallback" -> {
                node.addLabel { NodeType.Function.name }
                node.addLabel { NodeType.Fallback.name }
            }
            "function" -> {
                node.addLabel { NodeType.Function.name }
            }
            "constructor" -> {
                node.addLabel { NodeType.Constructor.name }
                node.addLabel { NodeType.Function.name }
            }
            "receive" -> {
                node.addLabel { NodeType.Receiver.name }
            }
            else -> throw Exception("$kind is not a supported function type.")
        }
    } else {
        val ic = json["isConstructor"] as Boolean
        when {
            ic == true -> {
                node.addLabel { NodeType.Constructor.name }
            }
            name == "" -> {
                node.addLabel { NodeType.Function.name }
                node.addLabel { NodeType.Fallback.name }
            }
            else -> {
                node.addLabel { NodeType.Function.name }
            }
        }
    }
    Mappings.currentFunctionId = nodeId
    if(json.containsKey("visibility")){
        node.setProperty("visibility", json["visibility"] as String)
    }
    node.setProperty("implemented", json["implemented"] as Boolean)
    if(json.containsKey("stateMutability") && json["stateMutability"] != null){
        node.setProperty("stateMutability", json["stateMutability"] as String)
    }
    else {
        node.setProperty("stateMutability", "")
    }
    val implemented = json["implemented"] as Boolean
    node.setProperty("fullyImplemented", implemented)
    val overrides = json["overrides"] != null
    node.setProperty("override", overrides)
    val virtual = json.containsKey("virtual") && json["virtual"] != null && json["virtual"] as Boolean
    node.setProperty("virtual", virtual)
    tx.commit()
    val modifiers = json["modifiers"] as JsonArray<JsonObject>
    modifiers.forEachIndexed() { index, modifier ->
        val mod = addModifierInvocation(modifier, graph)
        createRelationshipWithIndex(nodeId, mod, Relationship.INVOKES_MODIFIER, "modifierIndex", index, graph)
    }
    addParametersToNode(json["parameters"] as JsonObject, graph, nodeId)
    addReturnsToFunction(json["returnParameters"] as JsonObject, graph, nodeId)
    if (implemented) {
        val jsonBody = json["body"] as JsonObject
        if ((jsonBody["statements"] as JsonArray<JsonObject>).isNotEmpty()) {
            val bodyId: Long = addBlockBody(jsonBody, graph).first
            createRelationship(nodeId, bodyId, Relationship.BODY, graph)
        }
    }
    Mappings.currentFunctionId = null
    return nodeId
}

fun addModifierInvocation(json: JsonObject, graph: GraphDatabaseService): Long {
    if (json["nodeType"] as String != "ModifierInvocation") error("unknown modifier type in AST node ${json["id"] as Int}")
    val tx = graph.beginTx()
    val modifierInvocation = createMappedNode(json, tx)
    modifierInvocation.addLabel { NodeType.ModifierInvocation.name }
    val mIId = modifierInvocation.id
    tx.commit()
    (json["arguments"] as JsonArray<JsonObject>?)?.forEachIndexed { index, it ->
        val arg = addExpression(it, graph)
        createRelationshipWithIndex(mIId, arg, Relationship.HAS_ARGUMENT, "argIndex", index, graph)
    }
    val modifierName = addExpression(json["modifierName"] as JsonObject, graph)
    createRelationship(mIId, modifierName, Relationship.MODIFIER_NAME, graph)
    return mIId
}

fun createBlock(json: JsonObject, graph: GraphDatabaseService): Pair<Long, Long> {
    val tx = graph.beginTx()
    val blockEnter = tx.createNode()
    blockEnter.addLabel { NodeType.BlockEntry.name }
    var lastId = blockEnter.id
    val blockEnterId = blockEnter.id
    tx.commit()
    (json["statements"] as JsonArray<JsonObject>).forEach {
        val stmt = addStatement(it, graph)
        addLabelToNodeUsingId(stmt, NodeType.Statement, graph)
        createRelationship(lastId, stmt, Relationship.LEADS, graph)
        lastId = stmt
    }
    return Pair(blockEnterId, lastId)
}

fun addStatement(json: JsonObject, graph: GraphDatabaseService): Long {
    val statement =  when (json["nodeType"] as String) {
        "Block" -> addBlockStatement(json, graph)
        "Break" -> addBreakStatement(json, graph)
        "Continue" -> addContinueStatement(json, graph)
        "DoWhileStatement" -> addDoWhileStatement(json, graph)
        "EmitStatement" -> addEmitStatement(json, graph)
        "ExpressionStatement" -> addExpressionStatement(json, graph)
        "ForStatement" -> addForStatement(json, graph)
        "IfStatement" -> addIfStatement(json, graph)
        "InlineAssembly" -> addInlineAssemblyStatement(json, graph)
        "PlaceholderStatement" -> addPlaceholderStatement(json, graph)
        "Return" -> addReturnStatement(json, graph)
        "Throw" -> addThrowStatement(json, graph)
        "VariableDeclarationStatement" -> addVariableDeclarationStatement(json, graph)
        "WhileStatement" -> addWhileStatement(json, graph)
        else -> error("${json["nodeType"] as String}: Statement type not supported.")
    }

    val stx = graph.beginTx()
    stx.execute("match (f) where Id(f) = ${Mappings.currentFunctionId} match (s) where Id(s) = $statement merge (f)-[:HAS_STATEMENT]->(s)")
    stx.commit()

    claimOwnershipNestedCalls(statement, graph)
    claimOwnershipArithmeticExpressions(statement, graph)
    return statement
}

fun addBlockStatement(json: JsonObject, graph: GraphDatabaseService) : Long {
    val tx = graph.beginTx()
    val blockEnter = createMappedNode(json, tx)
    val blockEnterId = blockEnter.id
    blockEnter.addLabel { NodeType.BlockStatement.name }
    val blockExit = tx.createNode()
    blockExit.addLabel { NodeType.EndBlock.name }
    val blockExitId = blockExit.id
    tx.commit()

    val block = createBlock(json, graph)
    createRelationship(blockEnterId, block.first, Relationship.TRUE_BODY, graph)
    createRelationship(block.second, blockExitId, Relationship.LEADS, graph)
    return blockEnterId
}

fun claimOwnershipNestedCalls(statement: Long, graph: GraphDatabaseService) {
    Mappings.nestedCalls.forEach { call ->
        val ctx = graph.beginTx()
        ctx.execute("match (s) where Id(s) = $statement match (c) where Id(c) = $call set s:ContainsCall merge (s)-[:HAS_CALL]->(c)")
        ctx.commit()
    }
    Mappings.nestedCalls.clear()
}

fun claimOwnershipArithmeticExpressions(statement: Long, graph: GraphDatabaseService){
    Mappings.nestedArithmeticExpressions.forEach { ae ->
        val ctx = graph.beginTx()
        ctx.execute("match (s) where Id(s) = $statement match (ae) where Id(ae) = $ae set s:ContainsArithmetic merge (s)-[:HAS_ARITHMETIC]->(ae)")
        ctx.commit()
    }
    Mappings.nestedArithmeticExpressions.clear()
}

fun addDoWhileStatement(json: JsonObject, graph: GraphDatabaseService): Long {
    val tx = graph.beginTx()
    val doStatement = createMappedNode(json, tx)
    doStatement.addLabel { NodeType.DoWhileStatement.name }
    val whileId = doStatement.id
    tx.commit()
    val condition = addExpression(json["condition"] as JsonObject, graph)
    createRelationship(whileId, condition, Relationship.CONDITION, graph)
    claimOwnershipNestedCalls(whileId, graph)
    claimOwnershipArithmeticExpressions(whileId, graph)
    addLoopBody(json["body"] as JsonObject, whileId, graph)
    return whileId
}

fun addContinueStatement(json: JsonObject, graph: GraphDatabaseService): Long {
    val tx = graph.beginTx()
    val cs = createMappedNode(json, tx)
    val csId = cs.id
    cs.addLabel { NodeType.ContinueStatement.name }
    tx.commit()
    return csId
}

fun addBreakStatement(json: JsonObject, graph: GraphDatabaseService): Long {
    val tx = graph.beginTx()
    val br = createMappedNode(json, tx)
    val breakId = br.id
    br.addLabel { NodeType.BreakStatement.name }
    tx.commit()
    return breakId
}

fun addThrowStatement(json: JsonObject, graph: GraphDatabaseService): Long {
    val tx = graph.beginTx()
    val th = createMappedNode(json, tx)
    val throwId = th.id
    th.addLabel { NodeType.ThrowStatement.name }
    tx.commit()
    return throwId
}

fun addPlaceholderStatement(json: JsonObject, graph: GraphDatabaseService): Long {
    val tx = graph.beginTx()
    val ps = createMappedNode(json, tx)
    val psId = ps.id
    ps.addLabel { NodeType.PlaceholderStatement.name }
    tx.commit()
    return psId
}

fun addEmitStatement(json: JsonObject, graph: GraphDatabaseService): Long {
    val tx = graph.beginTx()
    val emitStatement = createMappedNode(json, tx)
    emitStatement.addLabel { NodeType.EmitStatement.name }
    val stmtId = emitStatement.id
    tx.commit()
    val eventCall = addFunctionCall(json["eventCall"] as JsonObject, graph)
    createRelationship(stmtId, eventCall, Relationship.EVENT_CALL, graph)
    return emitStatement.id
}

fun addWhileStatement(json: JsonObject, graph: GraphDatabaseService): Long {
    val tx = graph.beginTx()
    val whileStatement = createMappedNode(json, tx)
    whileStatement.addLabel { NodeType.WhileStatement.name }
    val whileId = whileStatement.id
    tx.commit()
    val condition = addExpression(json["condition"] as JsonObject, graph)
    createRelationship(whileId, condition, Relationship.CONDITION, graph)
    claimOwnershipNestedCalls(whileId, graph)
    claimOwnershipArithmeticExpressions(whileId, graph)
    addLoopBody(json["body"] as JsonObject, whileId, graph)
    return whileId
}

fun addLoopBody(body: JsonObject, loopStatement: Long, graph: GraphDatabaseService): Long {
    val bodyId = addBlockBody(body, graph)
    createRelationship(loopStatement, bodyId.first, Relationship.LOOP_LEAD, graph)

    val tx = graph.beginTx()
    val endLoop = tx.createNode()
    val endLoopId = endLoop.id
    endLoop.addLabel { NodeType.EndLoop.name }
    tx.commit()

    Mappings.ifEndIfHelper[endLoopId] = loopStatement
    createRelationship(bodyId.second, bodyId.first, Relationship.LOOP_LEAD, graph)
    createRelationship(bodyId.second, endLoopId, Relationship.LOOP_LEAVE, graph)
    return endLoopId
}

fun addForStatement(json: JsonObject, graph: GraphDatabaseService): Long {
    val tx = graph.beginTx()
    val forStatement = createMappedNode(json, tx)
    forStatement.addLabel { NodeType.ForStatement.name }
    val forId = forStatement.id
    tx.commit()
    val ie = json["initializationExpression"] as JsonObject?
    if(ie != null ){
        val initializationExpression =  addStatement(ie, graph)
        createRelationship(forId, initializationExpression, Relationship.INITIALIZATION_EXPRESSION, graph)
    }
    val condition = addExpression(json["condition"] as JsonObject, graph)
    createRelationship(forId, condition, Relationship.CONDITION, graph)
    val les = json["loopExpression"] as JsonObject?
    val loopExpressionStatement = if(les != null) addStatement(les, graph) else addEmptyStatement(graph)
    createRelationship(forId, loopExpressionStatement, Relationship.LOOP_EXPRESSION, graph)
    claimOwnershipNestedCalls(forId, graph)
    claimOwnershipArithmeticExpressions(forId, graph)
    addLoopBody(json["body"] as JsonObject, forId, graph)
    return forId
}

fun addEmptyStatement(graph: GraphDatabaseService): Long {
    val tx = graph.beginTx()
    val es = tx.createNode()
    es.addLabel { NodeType.EmptyStatement.name }
    val id = es.id
    tx.commit()
    return id
}

fun addInlineAssemblyStatement(json: JsonObject, graph: GraphDatabaseService): Long {
    val tx = graph.beginTx()
    val ias = createMappedNode(json, tx)
    val iasId = ias.id
    ias.addLabel { NodeType.InlineAssemblyStatement.name }
    if(json["operations"] != null)
        ias.setProperty("operations", json["operations"] as String)
    else {
        ias.setProperty("operations", "{}")
        log("InlineAssembly with AST instead of operations string.", "WARN")
    }
    val externalReferences = json["externalReferences"] as JsonArray<JsonObject>
    externalReferences.forEach { ref ->
        ref.keys.forEach { varname ->
            when (ref[varname]){
                is JsonObject -> {
                    val refjson = ref[varname] as JsonObject
                    val refId = refjson["declaration"] as Int
                    val isOffset = refjson["isOffset"] as Boolean
                    val isSlot = refjson["isSlot"] as Boolean
                    val valueSize = refjson["valueSize"] as Int
                    if (!Mappings.idMap.containsKey(refId)) {
                        error("inline assembly uses unknown variables ${json["id"] as Int}")
                    }
                    val refNode = Mappings.getNodeByAstId(refId, tx)
                    val mw = ias.createRelationshipTo(refNode) { Relationship.ASSEMBLY_MAY_WRITE.name }
                    mw.setProperty("isOffset", isOffset)
                    mw.setProperty("isSlot", isSlot)
                    mw.setProperty("valueSize", valueSize)
                    val mr = ias.createRelationshipTo(refNode) { Relationship.ASSEMBLY_MAY_READ.name }
                    mr.setProperty("isOffset", isOffset)
                    mr.setProperty("isSlot", isSlot)
                    mr.setProperty("valueSize", valueSize)
                }
                else -> {
                    log("unsupported reference type for inline assembly statement", "ETL")
                }
            }

        }
    }
    tx.commit()
    return iasId
}

fun addReturnStatement(json: JsonObject, graph: GraphDatabaseService): Long {
    val tx = graph.beginTx()
    val ret = createMappedNode(json, tx)
    ret.addLabel { NodeType.FunctionReturn.name }
    val retId = ret.id
    tx.commit()
    val retparam = json["functionReturnParameters"] as Int?
    if (retparam != null) {
        Mappings.collectReturnRef(retId, retparam, graph)
    }
    if(json["expression"] != null){
        val retexpr = addExpression(json["expression"] as JsonObject, graph)
        createRelationship(retId, retexpr, Relationship.RETURN_EXPRESSION, graph)
    }
    return retId
}

fun addIfStatement(json: JsonObject, graph: GraphDatabaseService): Long {
    val tx = graph.beginTx()
    val ifs = createMappedNode(json, tx)
    val ifId = ifs.id
    ifs.addLabel { NodeType.IfStatement.name }
    val endif = tx.createNode()
    val endIfId = endif.id
    endif.addLabel { NodeType.EndIf.name }
    tx.commit()

    createRelationship(ifId, endIfId, Relationship.LEADS, graph)

    Mappings.ifEndIfHelper[ifId] = endIfId

    val condition = addExpression(json["condition"] as JsonObject, graph)

    claimOwnershipNestedCalls(ifId, graph)
    claimOwnershipArithmeticExpressions(ifId, graph)

    createRelationship(ifId, condition, Relationship.CONDITION, graph)
    val tB = json["trueBody"] as JsonObject
    val trueBody = addBlockBody(tB, graph)
    createRelationship(ifId, trueBody.first, Relationship.TRUE_BODY, graph)
    createRelationship(trueBody.second, endIfId, Relationship.LEADS, graph)
    val fB = json["falseBody"] as JsonObject?
    if (fB != null) {
        val falseBody = addBlockBody(fB, graph)
        createRelationship(ifId, falseBody.first, Relationship.FALSE_BODY, graph)
        createRelationship(falseBody.second, endIfId, Relationship.LEADS, graph)
    }
    return ifId
}

fun addVariableDeclarationStatement(json: JsonObject, graph: GraphDatabaseService): Long {
    val tx = graph.beginTx()
    val vds = createMappedNode(json, tx)
    vds.addLabel { NodeType.VariableDeclarationStatement.name }
    vds.addLabel { NodeType.AssignmentStatement.name }
    val vdsId = vds.id
    tx.commit()

    val initialValue = json["initialValue"] as JsonObject?
    var valNode: Long? = null
    if (initialValue != null) {
        valNode = addExpression(initialValue, graph)
        createRelationship(vdsId, valNode, Relationship.RIGHT_HAND_SIDE, graph)
    }

    val assignments = (json["assignments"] as JsonArray<Int?>).filter { it != null }.toMutableList()
    val declarations = (json["declarations"] as JsonArray<JsonObject?>).toList()
    declarations.forEachIndexed { index, it ->
        if(it == null){
            val declaration = addEmptyStatement(graph)
            createRelationshipWithIndex(vdsId, declaration, Relationship.DECLARES, "declarationIndex", index, graph)
        }
        else {
            val declaration = addLocalVariable(it, graph)
            createRelationshipWithIndex(vdsId, declaration, Relationship.DECLARES, "declarationIndex", index, graph)
            createRelationshipWithIndex(vdsId, declaration, Relationship.LEFT_HAND_SIDE, "declarationIndex", index, graph)
            if (assignments.contains(it["id"] as Int)) {
                assignments.remove(it["id"] as Int)
                createRelationship(vdsId, declaration, Relationship.WRITES, graph)
            }
            if (valNode != null) {
                createRelationshipWithIndex(declaration, valNode, Relationship.INITIAL_VALUE, "returnIndex", index, graph)
                createRelationshipWithIndex(declaration, valNode, Relationship.HAS_VALUE, "returnIndex", index, graph)
            }
        }
    }
    if (assignments.size > 0) {
        throw IllegalStateException("the assignments are not empty")
    }

    return vdsId
}

fun addFunctionCall(json: JsonObject, graph: GraphDatabaseService): Long {
    val tx = graph.beginTx()
    val fc = createMappedNode(json, tx)
    fc.addLabel { NodeType.Call.name }
    when (json["kind"] as String) {
        "functionCall" -> {
            fc.addLabel { NodeType.FunctionCall.name }
        }
        "typeConversion" -> {
            fc.addLabel { NodeType.TypeConversion.name }
            Mappings.nestedTypeConversions.add(fc.id)
        }
        "structConstructorCall" -> {
            fc.addLabel { NodeType.StructConstructorCall.name }
        }
        else -> error("${json["kind"] as String}: unsupported kind of FunctionCall.")
    }
    val fcId = fc.id
    tx.commit()
    val ftx = graph.beginTx()
    ftx.execute("match (f:Function) where Id(f) = ${Mappings.currentFunctionId} match (c) where Id(c) = $fcId set f:ContainsCall merge (f)-[:HAS_CALL]->(c)")
    ftx.commit()
    Mappings.nestedCalls.add(fcId)
    val outerCall = Mappings.currentCall
    Mappings.currentCall = fcId
    val names = json["names"] as JsonArray<String>
    val args = json["arguments"] as JsonArray<JsonObject>
    when {
        names.count() > 0 -> {
            if (args.count() != names.count()) error("Different count of arguments and names.")
            args.forEachIndexed { index, it ->
                val arg = addExpression(it, graph)
                createRelationshipWithIndex(fcId, arg, Relationship.HAS_NAMED_ARGUMENT, "argIndex", index, graph)
                val itx = graph.beginTx()
                val rel = itx.getNodeById(arg).getRelationships(Direction.INCOMING).first { it.isType { Relationship.HAS_NAMED_ARGUMENT.name } }
                rel.setProperty("name", names[index])
                itx.commit()
            }
        }
        else -> {
            args.forEachIndexed { index, it ->
                val arg = addExpression(it, graph)
                createRelationshipWithIndex(fcId, arg, Relationship.HAS_ARGUMENT, "argIndex", index, graph)
            }
        }
    }
    Mappings.nestedMemberAccess.clear()
    val expr = addExpression(json["expression"] as JsonObject, graph)
    Mappings.nestedMemberAccess.forEach { ma ->
        val mtx = graph.beginTx()
        mtx.execute("match (c) where Id(c) = $fcId match (ma) where Id(ma) = $ma merge (c)-[:HAS_MA]->(ma)")
        mtx.commit()
    }
    Mappings.nestedMemberAccess.clear()
    val type = getType(json, graph)
    createRelationship(fcId, expr, Relationship.CALL_EXPRESSION, graph)
    createRelationship(fcId, type, Relationship.TYPE, graph)
    Mappings.nestedTypeConversions.remove(fcId)
    Mappings.currentCall = outerCall
    return fcId
}

fun addExpressionStatement(json: JsonObject, graph: GraphDatabaseService): Long {
    val expr = addExpression(json["expression"] as JsonObject, graph)
    addLabelToNodeUsingId(expr, NodeType.ExpressionStatement, graph)
    return expr
}

fun addExpression(json: JsonObject, graph: GraphDatabaseService): Long {
    val expr: Long = when (json["nodeType"] as String) {
        "Assignment" -> addAssingmentExpression(json, graph)
        "BinaryOperation" -> addBinaryOperationExpression(json, graph)
        "Conditional" -> addConditionalExpression(json, graph)
        "ElementaryTypeNameExpression" -> addElementaryTypeNameExpression(json, graph)
        "FunctionCall" -> addFunctionCall(json, graph)
        "FunctionCallOptions" -> addFunctionCallOptions(json, graph)
        "Identifier" -> addIdentifierExpression(json, graph)
        "IndexAccess" -> addIndexAccessExpression(json, graph)
        "IndexRangeAccess" -> addIndexRangeAccessExpression(json, graph)
        "Literal" -> addLiteralExpression(json, graph)
        "MemberAccess" -> addMemberAccessExpression(json, graph)
        "NewExpression" -> addNewExpression(json, graph)
        "TupleExpression" -> addTupleExpression(json, graph)
        "UnaryOperation" -> addUnaryOperation(json, graph)
        else -> error("${json["nodeType"] as String}: Invalid Expression.")
    }
    addLabelToNodeUsingId(expr, NodeType.Expression, graph)
    return expr
}

fun addFunctionCallOptions(json: JsonObject, graph: GraphDatabaseService): Long {
    val tx = graph.beginTx()
    val fco = createMappedNode(json, tx)
    fco.addLabel { NodeType.FunctionCallOptions.name }
    fco.addLabel { NodeType.Call.name }
    val fcoId = fco.id
    tx.commit()

    val ftx = graph.beginTx()
    ftx.execute("match (f:Function) where Id(f) = ${Mappings.currentFunctionId} match (c) where Id(c) = $fcoId set f:ContainsCall merge (f)-[:HAS_CALL]->(c)")
    ftx.commit()
    Mappings.nestedCalls.add(fcoId)
    val names = json["names"] as JsonArray<String>
    val options = json["options"] as JsonArray<JsonObject>
    if(names.size != options.size){
        error("FunctionCallOptions: names and options have different lengths.")
    }
    options.forEachIndexed { index, option ->
        val arg = addExpression(option, graph)
        createRelationshipWithIndex(fcoId, arg, Relationship.HAS_NAMED_ARGUMENT, "argIndex", index, graph)
        val itx = graph.beginTx()
        val rel = itx.getNodeById(arg).getRelationships(Direction.INCOMING).first { it.isType { Relationship.HAS_NAMED_ARGUMENT.name } }
        rel.setProperty("name", names[index])
        itx.commit()
    }

    val expr = addExpression(json["expression"] as JsonObject, graph)
    val type = getType(json, graph)
    createRelationship(fcoId, expr, Relationship.CALL_EXPRESSION, graph)
    createRelationship(fcoId, type, Relationship.TYPE, graph)
    return fcoId
}

fun addEmptyExpression(graph: GraphDatabaseService) : Long {
    val tx = graph.beginTx()
    val es = tx.createNode()
    es.addLabel { NodeType.EmptyExpression.name }
    val id = es.id
    tx.commit()
    return id
}

fun addConditionalExpression(json: JsonObject, graph: GraphDatabaseService): Long {
    val tx = graph.beginTx()
    val ce = createMappedNode(json, tx)
    ce.addLabel { NodeType.ConditionalExpression.name }
    val ceId = ce.id
    tx.commit()
    val condition = addExpression(json["condition"] as JsonObject, graph)
    createRelationship(ceId, condition, Relationship.CONDITION, graph)
    val te = json["trueExpression"] as JsonObject
    val trueExpression = addExpression(te, graph)
    val fe = json["falseExpression"] as JsonObject
    val falseExpression = addExpression(fe, graph)
    createRelationship(ceId, trueExpression, Relationship.TRUE_EXPRESSION, graph)
    createRelationship(ceId, falseExpression, Relationship.FALSE_EXPRESSION, graph)
    return ceId
}

fun addUnaryOperation(json: JsonObject, graph: GraphDatabaseService): Long {
    val subexp = addExpression(json["subExpression"] as JsonObject, graph)

    val tx = graph.beginTx()
    val uo = createMappedNode(json, tx)
    val op = json["operator"] as String
    uo.setProperty("operator", op)
    val prefix = json["prefix"] as Boolean
    uo.setProperty("prefix", prefix)
    uo.addLabel { NodeType.UnaryOperation.name }
    when (op) {
        in Mappings.logicOperators -> {
            uo.addLabel { NodeType.LogicExpression.name }
        }
        in Mappings.arithmethicOperators -> {
            uo.addLabel { NodeType.ArithmeticExpression.name }
            Mappings.nestedArithmeticExpressions.add(uo.id)
        }
    }
    val uoId = uo.id
    tx.commit()
    createRelationship(uoId, subexp, Relationship.SUB_EXPRESSION, graph)
    return uoId
}

fun addTupleExpression(json: JsonObject, graph: GraphDatabaseService): Long {
    val type = getType(json, graph)
    val inlineArray = json["isInlineArray"] as Boolean
    val tx = graph.beginTx()
    val tex = createMappedNode(json, tx)
    tex.addLabel { NodeType.TupleExpression.name }
    tex.setProperty("isInlineArray", inlineArray)
    val texId = tex.id
    tx.commit()

    createRelationship(texId, type, Relationship.TYPE, graph)

    val comps = json["components"] as JsonArray<JsonObject?>
    comps.forEachIndexed { i, comp ->
        val expr = if(comp == null) addEmptyExpression(graph) else addExpression(comp, graph)
        createRelationshipWithIndex(texId, expr, Relationship.HAS_COMPONENT, "componentIndex", i, graph)
    }
    return texId
}

fun addNewExpression(json: JsonObject, graph: GraphDatabaseService): Long {
    val type = getType(json["typeName"] as JsonObject, graph)

    val tx = graph.beginTx()
    val nexpr = createMappedNode(json, tx)
    nexpr.addLabel { NodeType.NewExpression.name }
    val id = nexpr.id
    tx.commit()

    createRelationship(id, type, Relationship.TYPE, graph)
    return id
}

fun addBinaryOperationExpression(json: JsonObject, graph: GraphDatabaseService): Long {
    val op = json["operator"] as String
    val tx = graph.beginTx()
    val binop = createMappedNode(json, tx)
    binop.setProperty("operator", op)
    binop.addLabel { NodeType.BinaryOperation.name }
    when (op) {
        in Mappings.comparisonOperators -> {
            binop.addLabel { NodeType.ComparisonOperation.name }
        }
        in Mappings.arithmethicOperators -> {
            binop.addLabel { NodeType.ArithmeticExpression.name }
            Mappings.nestedArithmeticExpressions.add(binop.id)
            when (op) {
                "+" -> binop.addLabel { NodeType.Addition.name }
                "-" -> binop.addLabel { NodeType.Subtraction.name }
                "/" -> binop.addLabel { NodeType.Division.name }
                "*" -> binop.addLabel { NodeType.Multiplication.name }
                "**" -> binop.addLabel { NodeType.Exponentiation.name }
            }
        }
        in Mappings.logicOperators -> {
            binop.addLabel { NodeType.LogicExpression.name }
        }
        in Mappings.bitwiseOperators -> {
            binop.addLabel { NodeType.BitwiseOperation.name }
        }
        in Mappings.shiftOperators -> {
            binop.addLabel { NodeType.ShiftOperation.name }
        }
        else -> {
            TODO("$op is an unknown binary operation.")
        }
    }
    val opId = binop.id
    tx.commit()
    val left = addExpression(json["leftExpression"] as JsonObject, graph)
    val right = addExpression(json["rightExpression"] as JsonObject, graph)
    createRelationship(opId, left, Relationship.LEFT_EXPRESSION, graph)
    createRelationship(opId, right, Relationship.RIGHT_EXPRESSION, graph)
    val type = getType(json, graph)
    createRelationship(opId, type, Relationship.TYPE, graph)
    return opId
}

fun addIndexAccessExpression(json: JsonObject, graph: GraphDatabaseService): Long {
    val base = addExpression(json["baseExpression"] as JsonObject, graph)
    val index = addExpression(json["indexExpression"] as JsonObject, graph)
    val type = getType(json, graph)

    val tx = graph.beginTx()
    val iae = createMappedNode(json, tx)
    iae.addLabel { NodeType.IndexAccessExpression.name }
    iae.addLabel { NodeType.Expression.name }
    val iaeId = iae.id
    tx.commit()

    createRelationship(iaeId, base, Relationship.BASE_EXPRESSION, graph)
    createRelationship(iaeId, index, Relationship.INDEX_EXPRESSION, graph)
    createRelationship(iaeId, type, Relationship.TYPE, graph)
    return iaeId
}

fun addIndexRangeAccessExpression(json: JsonObject, graph: GraphDatabaseService): Long {
    val base = addExpression(json["baseExpression"] as JsonObject, graph)
    val type = getType(json, graph)

    val tx = graph.beginTx()
    val irae = createMappedNode(json, tx)
    val iraeId = irae.id
    irae.addLabel { NodeType.IndexRangeAccessExpression.name }
    irae.addLabel { NodeType.Expression.name }
    tx.commit()

    createRelationship(iraeId, base, Relationship.BASE_EXPRESSION, graph)
    createRelationship(iraeId, type, Relationship.TYPE, graph)
    if(json["startExpression"] != null){
        val start = addExpression(json["startExpression"] as JsonObject, graph)
        createRelationship(iraeId, start, Relationship.START_EXPRESSION, graph)
    }
    if(json["endExpression"] != null){
        val start = addExpression(json["endExpression"] as JsonObject, graph)
        createRelationship(iraeId, start, Relationship.END_EXPRESSION, graph)
    }

    return iraeId
}

fun addElementaryTypeNameExpression(json: JsonObject, graph: GraphDatabaseService): Long {
    val tx = graph.beginTx()
    val etne = createMappedNode(json, tx)
    val name = when (json["typeName"]) {
        is String -> (json["typeName"] as String).split(" ").last()
        is JsonObject -> {
            val it = json["typeName"] as JsonObject
            val n = it["name"] as String
            if (n != "address")
                n
            else {
                if(Mappings.currentCall != null && Mappings.nestedTypeConversions.contains(Mappings.currentCall!!)) {
                    if(it.containsKey("stateMutability") && it["stateMutability"] != null && it["stateMutability"] as String == "payable") {
                        "payable"
                    }
                    else{
                        n
                    }
                } else {
                    n
                }
            }
        }
        else -> {
            error("Unmatched ElementaryTypeName Format.")
        }
    }
    etne.setProperty("typeName", name)
    etne.addLabel { NodeType.ElementaryTypeExpression.name }
    etne.addLabel { NodeType.Expression.name }
    val etneId = etne.id
    tx.commit()
    val type = getType(json, graph)
    createRelationship(etneId, type, Relationship.TYPE, graph)
    return etneId
}

fun addMemberAccessExpression(json: JsonObject, graph: GraphDatabaseService): Long {
    val memberName = json["memberName"] as String
    val reference = json["referencedDeclaration"] as Int?
    val expr = addExpression(json["expression"] as JsonObject, graph)
    val type = getType(json, graph)

    val tx = graph.beginTx()
    val mae = createMappedNode(json, tx)
    mae.addLabel { NodeType.MemberAccess.name }
    mae.addLabel { NodeType.Expression.name }
    mae.setProperty("memberName", memberName)
    val maeId = mae.id
    tx.commit()

    createRelationship(maeId, expr, Relationship.LEFT_HAND_SIDE, graph)
    createRelationship(maeId, type, Relationship.TYPE, graph)
    if (reference != null) Mappings.collectVariableReferenceConstraint(maeId, reference, graph)
    Mappings.nestedMemberAccess.add(maeId)
    return maeId
}

fun addLiteralExpression(json: JsonObject, graph: GraphDatabaseService): Long {
    val lit = when (json["kind"] as String) {
        "number" -> addNumberLiteral(json, graph)
        "string" -> addStringLiteral(json, graph)
        "bool" -> addBoolLiteral(json, graph)
        else -> error("${json["kind"] as String}: Invalid literal.")
    }
    addLabelToNodeUsingId(lit, NodeType.LiteralExpression, graph)
    return lit
}

fun addBoolLiteral(json: JsonObject, graph: GraphDatabaseService): Long {
    val value = json["value"] as String
    val tx = graph.beginTx()
    val bl = createMappedNode(json, tx)
    bl.setProperty("value", value)
    bl.addLabel { NodeType.BoolLiteral.name }
    val id = bl.id
    tx.commit()
    return id
}

fun escapeAllEscapeSequences(v: String): String {
    var res = v
    res = res.replace("\u0019", "\\x19")
    res = res.replace("\n", "\\n")
    return res
}

fun addStringLiteral(json: JsonObject, graph: GraphDatabaseService): Long {
    val tx = graph.beginTx()
    val sl = createMappedNode(json, tx)
    val v = if(json["value"] == null){
        sl.setProperty("prefix", "hex")
        json["hexValue"] as String
    }
    else json["value"] as String
    val value = escapeAllEscapeSequences(v)
    sl.setProperty("value", value)
    sl.addLabel { NodeType.StringLiteral.name }
    val id = sl.id
    tx.commit()
    return id
}

fun addNumberLiteral(json: JsonObject, graph: GraphDatabaseService): Long {
    val value = json["value"] as String
    val sub = when (val sub = json["subdenomination"] as String?) {
        null -> ""
        else -> sub
    }
    val tx = graph.beginTx()
    val number = createMappedNode(json, tx)
    number.addLabel { NodeType.NumberLiteral.name }
    number.setProperty("value", value)
    number.setProperty("subdenomination", sub)
    val numberId = number.id
    tx.commit()
    return numberId
}

fun addIdentifierExpression(json: JsonObject, graph: GraphDatabaseService): Long {
    val name = json["name"] as String
    val type = getType(json, graph)

    val tx = graph.beginTx()
    val identexpr = createMappedNode(json, tx)
    identexpr.addLabel { NodeType.IdentifierExpression.name }
    identexpr.setProperty("name", name)
    val identExprId = identexpr.id
    tx.commit()
    createRelationship(identExprId, type, Relationship.TYPE, graph)
    val references = json["referencedDeclaration"] as Int
    Mappings.collectVariableReferenceConstraint(identExprId, references, graph)
    Mappings.nestedIdentifiers.add(identExprId)
    return identExprId
}

fun addAssingmentExpression(json: JsonObject, graph: GraphDatabaseService): Long {
    val operator = json["operator"] as String

    val tx = graph.beginTx()
    val assignment = createMappedNode(json, tx)
    val assignmentId = assignment.id
    assignment.addLabel { NodeType.AssignmentStatement.name }
    assignment.setProperty("operator", "=")
    tx.commit()

    Mappings.nestedIdentifiers.clear()
    val left = addExpression(json["leftHandSide"] as JsonObject, graph)
    Mappings.nestedIdentifiers.forEach { ident ->
        val ntx = graph.beginTx()
        ntx.execute("match (a) where Id(a) = $assignmentId match (i) where Id(i) = $ident merge (a)-[:HAS_IDENTIFIER]->(i)")
        ntx.commit()
    }
    var right = addExpression(json["rightHandSide"] as JsonObject, graph)
    Mappings.nestedIdentifiers.clear()
    val type = getType(json, graph)
    when (operator) {
        "=" -> {}
        "+=" -> {
            val rhs = right
            val itx = graph.beginTx()
            val binop = itx.createNode()
            binop.addLabel { NodeType.BinaryOperation.name }
            binop.addLabel { NodeType.Addition.name }
            binop.addLabel { NodeType.ArithmeticExpression.name }
            binop.setProperty("operator", "+")
            right = binop.id
            Mappings.nestedArithmeticExpressions.add(right)
            itx.commit()
            val lhs = addExpression(json["leftHandSide"] as JsonObject, graph)
            createRelationship(right, lhs, Relationship.LEFT_EXPRESSION, graph)
            createRelationship(right, rhs, Relationship.RIGHT_EXPRESSION, graph)
            createRelationship(right, type, Relationship.TYPE, graph)
        }
        "-=" -> {
            val rhs = right
            val itx = graph.beginTx()
            val binop = itx.createNode()
            binop.addLabel { NodeType.BinaryOperation.name }
            binop.addLabel { NodeType.Subtraction.name }
            binop.addLabel { NodeType.ArithmeticExpression.name }
            binop.setProperty("operator", "-")
            right = binop.id
            Mappings.nestedArithmeticExpressions.add(right)
            itx.commit()
            val lhs = addExpression(json["leftHandSide"] as JsonObject, graph)
            createRelationship(right, lhs, Relationship.LEFT_EXPRESSION, graph)
            createRelationship(right, rhs, Relationship.RIGHT_EXPRESSION, graph)
            createRelationship(right, type, Relationship.TYPE, graph)
        }
        "*=" -> {
            val rhs = right
            val itx = graph.beginTx()
            val binop = itx.createNode()
            binop.addLabel { NodeType.BinaryOperation.name }
            binop.addLabel { NodeType.Multiplication.name }
            binop.addLabel { NodeType.ArithmeticExpression.name }
            binop.setProperty("operator", "*")
            right = binop.id
            Mappings.nestedArithmeticExpressions.add(right)
            itx.commit()
            val lhs = addExpression(json["leftHandSide"] as JsonObject, graph)
            createRelationship(right, lhs, Relationship.LEFT_EXPRESSION, graph)
            createRelationship(right, rhs, Relationship.RIGHT_EXPRESSION, graph)
            createRelationship(right, type, Relationship.TYPE, graph)
        }
        "/=" -> {
            val rhs = right
            val itx = graph.beginTx()
            val binop = itx.createNode()
            binop.addLabel { NodeType.BinaryOperation.name }
            binop.addLabel { NodeType.Division.name }
            binop.addLabel { NodeType.ArithmeticExpression.name }
            binop.setProperty("operator", "/")
            right = binop.id
            Mappings.nestedArithmeticExpressions.add(right)
            itx.commit()
            val lhs = addExpression(json["leftHandSide"] as JsonObject, graph)
            createRelationship(right, lhs, Relationship.LEFT_EXPRESSION, graph)
            createRelationship(right, rhs, Relationship.RIGHT_EXPRESSION, graph)
            createRelationship(right, type, Relationship.TYPE, graph)
        }
        "|=" -> {
            val rhs = right
            val itx = graph.beginTx()
            val binop = itx.createNode()
            binop.addLabel { NodeType.BinaryOperation.name }
            binop.addLabel { NodeType.BitwiseOperation.name }
            binop.addLabel { NodeType.LogicExpression.name }
            binop.setProperty("operator", "|")
            right = binop.id
            itx.commit()
            val lhs = addExpression(json["leftHandSide"] as JsonObject, graph)
            createRelationship(right, lhs, Relationship.LEFT_EXPRESSION, graph)
            createRelationship(right, rhs, Relationship.RIGHT_EXPRESSION, graph)
            createRelationship(right, type, Relationship.TYPE, graph)
        }
        ">>=" -> {
            val rhs = right
            val itx = graph.beginTx()
            val binop = itx.createNode()
            binop.addLabel { NodeType.BinaryOperation.name }
            binop.addLabel { NodeType.ArithmeticExpression.name }
            binop.setProperty("operator", ">>")
            right = binop.id
            Mappings.nestedArithmeticExpressions.add(right)
            itx.commit()
            val lhs = addExpression(json["leftHandSide"] as JsonObject, graph)
            createRelationship(right, lhs, Relationship.LEFT_EXPRESSION, graph)
            createRelationship(right, rhs, Relationship.RIGHT_EXPRESSION, graph)
            createRelationship(right, type, Relationship.TYPE, graph)
        }
        "<<=" -> {
            val rhs = right
            val itx = graph.beginTx()
            val binop = itx.createNode()
            binop.addLabel { NodeType.BinaryOperation.name }
            binop.addLabel { NodeType.ArithmeticExpression.name }
            binop.setProperty("operator", "<<")
            right = binop.id
            Mappings.nestedArithmeticExpressions.add(right)
            itx.commit()
            val lhs = addExpression(json["leftHandSide"] as JsonObject, graph)
            createRelationship(right, lhs, Relationship.LEFT_EXPRESSION, graph)
            createRelationship(right, rhs, Relationship.RIGHT_EXPRESSION, graph)
            createRelationship(right, type, Relationship.TYPE, graph)
        }
        else -> error("Operator $operator unknkown.")
    }
    createRelationship(assignmentId, type, Relationship.TYPE, graph)
    createRelationship(assignmentId, left, Relationship.LEFT_HAND_SIDE, graph)
    createRelationship(assignmentId, right, Relationship.RIGHT_HAND_SIDE, graph)
    return assignmentId
}

fun addBlockBody(json: JsonObject, graph: GraphDatabaseService): Pair<Long, Long> {
    return when (json["nodeType"] as String) {
        "Block" -> createBlock(json, graph)
        "IfStatement" -> {
            val statement = addStatement(json, graph)
            addLabelToNodeUsingId(statement, NodeType.Statement, graph)
            val ifs = Mappings.ifEndIfHelper[statement]!!
            Pair<Long, Long>(statement, ifs)
        }
        else -> {
            val s = addStatement(json, graph)
            addLabelToNodeUsingId(s, NodeType.Statement, graph)
            Pair(s, s)
        }
    }
}

fun addParametersToNode(json: JsonObject, graph: GraphDatabaseService, functionId: Long) {
    for ((counter, par) in (json["parameters"] as JsonArray<JsonObject>).withIndex()) {
        val paramId = createParameter(par, graph)
        addLabelToNodeUsingId(paramId, NodeType.Parameter, graph)
        createRelationshipWithIndex(functionId, paramId, Relationship.HAS_PARAMETER, "parameterIndex", counter, graph)
    }
}

fun addReturnsToFunction(json: JsonObject, graph: GraphDatabaseService, functionId: Long) {
    val set = mutableSetOf<Long>()
    for ((counter, par) in (json["parameters"] as JsonArray<JsonObject>).withIndex()) {
        val returnparam = createParameter(par, graph)
        addLabelToNodeUsingId(returnparam, NodeType.ReturnParameter, graph)
        createRelationshipWithIndex(functionId, returnparam, Relationship.RETURNS, "returnIndex", counter, graph)
        set.add(returnparam)
    }
    Mappings.returnParameters[json["id"] as Int] = set
}

fun createParameter(json: JsonObject, graph: GraphDatabaseService): Long {
    val constant = json["constant"] as Boolean
    val name = json["name"] as String
    if(!json.containsKey("storageLocation")){
        error("Parameter does not have storageLocation key!")
    }
    val location = when (val x = json["storageLocation"] as String) {
        "default" -> ""
        else -> x
    }
    val indexed = json.containsKey("indexed") && json["indexed"] as Boolean
    val sv = json.containsKey("stateVariable") && json["stateVariable"] as Boolean
    val type = getType(json["typeName"] as JsonObject, graph)

    val tx = graph.beginTx()
    val param = createMappedNode(json, tx)
    param.setProperty("name", name)
    param.setProperty("constant", constant)
    param.setProperty("indexed", indexed)
    param.setProperty("stateVariable", sv)
    param.setProperty("location", location)
    param.addLabel { NodeType.Variable.name }
    val paramId = param.id
    tx.commit()
    createRelationship(paramId, type, Relationship.TYPE, graph)
    return paramId
}

fun addLabelToNodeUsingId(id: Long, label: NodeType, graph: GraphDatabaseService) {
    val tx = graph.beginTx()
    val node = tx.getNodeById(id)
    node.addLabel { label.name }
    tx.commit()
}

fun addStateVariable(node: JsonObject, graph: GraphDatabaseService): Long {
    val variableId = addVariable(node, graph)
    addLabelToNodeUsingId(variableId, NodeType.StateVariable, graph)

    val overrides = node["overrides"] != null
    if(overrides){
        val tx = graph.beginTx()
        val sv = tx.getNodeById(variableId)
        sv.setProperty("override", overrides)
        tx.commit()
    }

    val initialValue = node["value"] as JsonObject?
    if (initialValue != null) {
        val valNode = addExpression(initialValue, graph)
        createRelationship(variableId, valNode, Relationship.INITIAL_VALUE, graph)
        createRelationship(variableId, valNode, Relationship.HAS_VALUE, graph)
    }
    Mappings.nestedArithmeticExpressions.clear()
    return variableId
}

fun addLocalVariable(node: JsonObject, graph: GraphDatabaseService): Long {
    if (node["stateVariable"] as Boolean) {
        TODO("unexpected state variable")
    }
    val variableId = addVariable(node, graph)
    addLabelToNodeUsingId(variableId, NodeType.LocalVariable, graph)
    if (Mappings.currentFunctionId == null) {
        TODO("handling local variable, but not in a function")
    }
    val tx = graph.beginTx()
    val currentFunction = Mappings.getCurrentFunctionNode(tx)
    val variable = tx.getNodeById(variableId)
    currentFunction.createRelationshipTo(variable) { Relationship.HAS_LOCAL_VARIABLE.name }
    tx.commit()
    return variableId
}

fun addVariable(node: JsonObject, graph: GraphDatabaseService): Long {
    val tx = graph.beginTx()
    val variable = createMappedNode(node, tx)
    variable.addLabel { NodeType.Variable.name }
    addVariableData(variable, node)
    val variableId = variable.id
    tx.commit()
    val type = if(node.containsKey("typeName") && node.get("typeName") != null) getType(node["typeName"] as JsonObject, graph) else getType(node, graph)
    createRelationship(variableId, type, Relationship.TYPE, graph)
    return variableId
}

fun addVariableData(variable: Node, json: JsonObject) {
    variable.setProperty("ast_id", json["id"] as Int)
    variable.setProperty("constant", json["constant"] as Boolean)
    variable.setProperty("name", json["name"] as String)
    val visibility = json["visibility"] as String
    assert(listOf<String>("internal", "public", "external").contains(visibility))
    variable.setProperty("visibility", visibility)
    val location = json["storageLocation"] as String
    assert(listOf<String>("storage").contains(location))
    variable.setProperty("storageLocation", location)
}

fun getType(json: JsonObject, graph: GraphDatabaseService): Long {
    val maybeExists = Mappings.getTypeIfExists(json)
    var relationships = emptyList<Long>()
    if (maybeExists != null) {
        val tx = graph.beginTx()
        val maybeExistsNode = tx.getNodeById(maybeExists)
        if (maybeExistsNode.hasProperty("name")) {
            tx.commit()
            return maybeExists
        }
        val rels = maybeExistsNode.getRelationships(Direction.INCOMING)
        relationships = rels.toList().map { it.id }
        tx.commit()
    }
    val nodeId: Long = when (json["nodeType"] as String) {
        "ArrayTypeName" -> getArrayType(json, graph)
        "Assignment" -> getBasicTypeInfo(json, graph)
        "BinaryOperation" -> getBasicTypeInfo(json, graph)
        "ElementaryTypeName" -> getElementaryType(json, graph)
        "ElementaryTypeNameExpression" -> getBasicTypeInfo(json, graph)
        "FunctionCall" -> getBasicTypeInfo(json, graph)
        "FunctionCallOptions" -> getBasicTypeInfo(json, graph)
        "Identifier" -> getBasicTypeInfo(json, graph)
        "IndexAccess" -> getBasicTypeInfo(json, graph)
        "IndexRangeAccess" -> getBasicTypeInfo(json, graph)
        "Mapping" -> getMappingType(json, graph)
        "MemberAccess" -> getBasicTypeInfo(json, graph)
        "TupleExpression" -> getBasicTypeInfo(json, graph)
        "UserDefinedTypeName" -> getUserDefinedType(json, graph)
        "VariableDeclaration" -> getBasicTypeInfo(json, graph)
        else -> error("${json["id"] as Int}: ${json["nodeType"] as String} is not implemented (yet).")
    }

    val tx = graph.beginTx()
    relationships.forEach {
        val rel = tx.getRelationshipById(it)
        createRelationship(rel.startNodeId, nodeId, Relationship.TYPE, graph)
        rel.delete()
    }
    if (maybeExists != null) {
        val node = tx.getNodeById(maybeExists)
        node.delete()
    }
    tx.commit()
    return nodeId
}

fun getArrayType(json: JsonObject, graph: GraphDatabaseService): Long {
    val typeIdentifier = (json["typeDescriptions"] as JsonObject)["typeIdentifier"] as String
    val typeString = ((json["typeDescriptions"] as JsonObject)["typeString"] as String).split(" ").last()
    val baseType = getType(json["baseType"] as JsonObject, graph)

    val tx = graph.beginTx()
    val typenode = createMappedNode(json, tx)
    typenode.addLabel { NodeType.Type.name }
    val tId = typenode.id
    Mappings.typeMappings[typeString] = tId
    if (json.containsKey("stateMutability")) {
        typenode.setProperty("stateMutability", json["stateMutability"] as String)
    }
    typenode.addLabel { NodeType.Type.name }
    typenode.addLabel { NodeType.ElementaryType.name }
    typenode.setProperty("typeIdentifier", typeIdentifier)
    typenode.setProperty("typeString", typeString)
    typenode.setProperty("name", typeString)
    tx.commit()

    createRelationship(tId, baseType, Relationship.ARRAY_BASE_TYPE, graph)
    val len = json["length"] as JsonObject?
    if (len != null) {
        val length = addExpression(len, graph)
        createRelationship(tId, length, Relationship.ARRAY_LENGTH, graph)
    }
    return tId
}

fun getBasicTypeInfo(json: JsonObject, graph: GraphDatabaseService): Long {
    val typeIdentifier = (json["typeDescriptions"] as JsonObject)["typeIdentifier"] as String
    val typeString = (json["typeDescriptions"] as JsonObject)["typeString"] as String
    val tx = graph.beginTx()
    val typenode = tx.createNode()
    typenode.setProperty("typeIdentifier", typeIdentifier)
    typenode.setProperty("typeString", typeString)
    Mappings.typeMappings[typeString] = typenode.id
    typenode.addLabel { NodeType.Type.name }
    typenode.addLabel { NodeType.BasicType.name }
    val id = typenode.id
    tx.commit()
    return id
}

fun getElementaryType(json: JsonObject, graph: GraphDatabaseService): Long {
    val name = json["name"] as String
    val typeIdentifier = (json["typeDescriptions"] as JsonObject)["typeIdentifier"] as String
    val typeString = (json["typeDescriptions"] as JsonObject)["typeString"] as String

    val tx = graph.beginTx()
    val typenode = createMappedNode(json, tx)
    if (json.containsKey("stateMutability")) {
        typenode.setProperty("stateMutability", json["stateMutability"] as String)
    }
    typenode.addLabel { NodeType.Type.name }
    typenode.addLabel { NodeType.ElementaryType.name }
    typenode.setProperty("name", name)
    typenode.setProperty("typeIdentifier", typeIdentifier)
    typenode.setProperty("typeString", typeString)
    val id = typenode.id
    tx.commit()

    Mappings.typeMappings[typeString] = id
    return id
}

fun getTypeNameOrString(id: Long, graph: GraphDatabaseService): String {
    val tx = graph.beginTx()
    val node = tx.getNodeById(id)
    val res = if (node.hasProperty("name")) node.getProperty("name") as String else node.getProperty("typeString") as String
    tx.commit()
    return res
}

fun getMappingType(json: JsonObject, graph: GraphDatabaseService): Long {
    val typeIdentifier = (json["typeDescriptions"] as JsonObject)["typeIdentifier"] as String
    val keyType = getType(json["keyType"] as JsonObject, graph)
    val valueType = getType(json["valueType"] as JsonObject, graph)
    val keyTypeName = getTypeNameOrString(keyType, graph)
    val valueTypeName = getTypeNameOrString(valueType, graph)
    val typeString = "mapping($keyTypeName => $valueTypeName)"

    val tx = graph.beginTx()
    val typeNode = createMappedNode(json, tx)
    typeNode.addLabel { NodeType.Type.name }
    typeNode.addLabel { NodeType.MappingType.name }
    typeNode.setProperty("name", typeString)
    typeNode.setProperty("typeIdentifier", typeIdentifier)
    typeNode.setProperty("typeString", typeString)
    val id = typeNode.id
    tx.commit()

    Mappings.typeMappings[typeString] = id
    createRelationship(id, keyType, Relationship.KEY_TYPE, graph)
    createRelationship(id, valueType, Relationship.VALUE_TYPE, graph)
    return id
}

fun getUserDefinedType(json: JsonObject, graph: GraphDatabaseService): Long {
    val td = json["typeDescriptions"] as JsonObject
    val typeIdentifier = td["typeIdentifier"] as String
    val typeString = td["typeString"] as String
    val name = json["name"] as String
    val referencedDeclaration = json["referencedDeclaration"] as Int

    val tx = graph.beginTx()
    val typenode = createMappedNode(json, tx)
    typenode.addLabel { NodeType.Type.name }
    typenode.addLabel { NodeType.UserDefinedType.name }
    typenode.setProperty("name", name)
    typenode.setProperty("typeIdentifier", typeIdentifier)
    typenode.setProperty("typeString", typeString)
    val id = typenode.id
    tx.commit()

    Mappings.typeMappings[typeString] = id
    Mappings.typeReferences.add(Pair(id, referencedDeclaration))
    return id
}
