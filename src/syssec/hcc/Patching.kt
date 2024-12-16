package syssec.hcc

import org.neo4j.graphdb.Direction
import org.neo4j.graphdb.GraphDatabaseService
import org.neo4j.graphdb.Node

fun patchingPass(graphDb: GraphDatabaseService) {
    if (Mappings.patchIO) {
        patchArithmeticOperation(graphDb)
        patchUnaryOperation(graphDb)
        patchTruncation(graphDb)
    }
    if (Mappings.patchRE) {
        patchReentrancy(graphDb)
    }
}

fun insertBeforeNode(
    node: Node,
    pre: Node,
) {
    val incoming =
        node.getRelationships(Direction.INCOMING).filter {
            it.isType { Relationship.LEADS.name } ||
                it.isType { Relationship.LOOP_LEAD.name } ||
                it.isType { Relationship.TRUE_BODY.name } ||
                it.isType { Relationship.FALSE_BODY.name }
        }
    incoming.forEach {
        val startNode = it.startNode
        startNode.createRelationshipTo(pre) { it.type.name() }
        it.delete()
    }
    pre.createRelationshipTo(node) { Relationship.LEADS.name }
}

fun insertBeforeNode(
    node: Node,
    snippet: Pair<Node, Node>,
) {
    val start = snippet.first
    val end = snippet.second

    val incoming =
        node.getRelationships(Direction.INCOMING).filter {
            it.isType { Relationship.LEADS.name } ||
                it.isType { Relationship.LOOP_LEAD.name } ||
                it.isType { Relationship.TRUE_BODY.name } ||
                it.isType { Relationship.FALSE_BODY.name }
        }
    incoming.forEach {
        val startNode = it.startNode
        startNode.createRelationshipTo(start) { it.type.name() }
        it.delete()
    }
    end.createRelationshipTo(node) { Relationship.LEADS.name }
}

fun insertAfterNode(
    node: Node,
    after: Node,
) {
    val outgoing = node.getRelationships(Direction.OUTGOING).firstOrNull { it.isType { Relationship.LEADS.name } }
    if (outgoing != null) {
        val endNode = outgoing.endNode
        outgoing.delete()
        after.createRelationshipTo(endNode) { Relationship.LEADS.name }
        node.createRelationshipTo(after) { Relationship.LEADS.name }
    } else {
        node.createRelationshipTo(after) { Relationship.LEADS.name }
    }
}

fun insertAfterNode(
    node: Node,
    snippet: Pair<Node, Node>,
) {
    val start = snippet.first
    val end = snippet.second

    val outgoing = node.getRelationships(Direction.OUTGOING).firstOrNull { it.isType { Relationship.LEADS.name } }
    if (outgoing != null) {
        val endNode = outgoing.endNode
        outgoing.delete()
        end.createRelationshipTo(endNode) { Relationship.LEADS.name }
        node.createRelationshipTo(start) { Relationship.LEADS.name }
    } else {
        node.createRelationshipTo(start) { Relationship.LEADS.name }
    }
}

fun insertAtBeginningOfFunction(
    function: Node,
    instr: Node,
) {
    val blockEntry = function.getRelationships(Direction.OUTGOING).first { it.isType { Relationship.BODY.name } }.endNode
    insertAfterNode(blockEntry, instr)
}
