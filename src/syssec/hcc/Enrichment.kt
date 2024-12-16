package syssec.hcc

import org.neo4j.graphdb.GraphDatabaseService
import org.neo4j.graphdb.Node
import java.lang.Exception

class EnrichmentHelper {
    companion object {
        val currentTargetFunctions = mutableListOf<Long>()
        val knownFunctions = mutableListOf<Long>()
        val allFunctions = mutableListOf<Long>()
    }
}

fun cpgEnrichmentPass(graphDb: GraphDatabaseService) {
    try {
        log("Detecting calls...", "BOOST")
        detectCalls(graphDb)
        log("Call detection finished", "BOOST")
        collectGarbage()
        stateDependencyAnalysis(graphDb)
        collectGarbage()
        log("State Dependencies collected", "BOOST")
    } catch (e: Exception) {
        throw e
    }
}

fun stateDependencyAnalysis(graphDb: GraphDatabaseService) {
    log("Write-State Analysis running", "BOOST")
    writesStateAnalysis(graphDb)
}

fun getNameOfFunction(
    id: Long,
    graphDb: GraphDatabaseService,
): String {
    val tx = graphDb.beginTx()
    val res = tx.execute("match (n) where Id(n) = %d return n".format(id))
    var name = ""
    res.forEach { r ->
        name = (r["n"] as Node).getProperty("name") as String
    }
    tx.commit()
    return name
}

fun writesStateAnalysis(graphDb: GraphDatabaseService) {
    try {
        var svtx = graphDb.beginTx()
        val svids =
            svtx
                .execute("match(sv:StateVariable) return Id(sv) as sv")
                .map { it["sv"] as Long }
                .asSequence()
                .toList()
        svtx.commit()
        collectGarbage()

        val writingState = mutableListOf<Long>()
        svids.forEach { svid ->
            val atx = graphDb.beginTx()
            val funcs =
                atx
                    .execute(
                        """match (sv:StateVariable)
            where Id(sv) = %d
            match (i:IdentifierExpression)-[:REFERENCES]->(sv)
            match (a:AssignmentStatement)-[:HAS_IDENTIFIER]->(i)
            match (f:Function)-[:HAS_STATEMENT]->(a)
            set f:WritesState
            set a:WritesState
            merge (a)-[:WRITES]->(sv)
            merge (f)-[:WRITES]->(sv)
            return distinct Id(f) as func
            """.format(svid).trimIndent(),
                    ).map { it["func"] as Long }
                    .asSequence()
                    .toList()
            atx.commit()

            if (funcs.isNotEmpty()) {
                writingState.addAll(funcs)
            }
        }

        collectGarbage()
        log("WSA: Phase 1 finished.", "BOOST")

        var ws = writingState.toList()
        do {
            ws = writeStateTransitively(ws, graphDb)
        } while (ws.isNotEmpty())
    } catch (e: Exception) {
        throw e
    } catch (e: Error) {
        throw e
    }
    log("WSA: Phase 2 finished.", "BOOST")
}

fun writeStateTransitively(
    writingState: List<Long>,
    graphDb: GraphDatabaseService,
): List<Long> {
    val count = writingState.count()
    val ws = mutableListOf<Long>()
    writingState.forEachIndexed { index, id ->
        val name = getNameOfFunction(id, graphDb)
        log("WSA Phase 2: Iterative Analysis of Function ${index + 1} of $count. (~${index * 100 / count}%)   $name ($id)", "BOOST")
        do {
            val itx = graphDb.beginTx()
            val result =
                itx.execute(
                    """
                    match (w:Function:WritesState)
                    where Id(w) = %d
                    match (w)<-[:REFERENCES]-()<-[:CALL_EXPRESSION]-(c: Call)
                    match (cc:ContainsCall)-[:HAS_CALL]->(c)
                    match (f:Function)-[:HAS_STATEMENT]->(cc)
                    where not f:WritesState
                    match (w)-[:WRITES]->(sv:StateVariable)
                    set f:WritesState, c:WritesState
                    merge (f)-[:WRITES]->(sv)
                    merge (c)-[:WRITES]->(sv)
                    return Id(f) as func
                    """.trimIndent().format(id),
                )
            val funcs = result.map { it["func"] as Long }.asSequence().toList()
            itx.commit()
            if (funcs.isNotEmpty()) {
                ws.addAll(funcs)
            }
            collectGarbage()
        } while (result.queryStatistics.labelsAdded > 0)
        log("WSA Phase 2: Iterative Analysis of Function ${index + 1} of $count done. (~${(index + 1) * 100 / count}%)", "BOOST")
    }
    return ws
}

fun detectCalls(graphDb: GraphDatabaseService) {
    detectDelegateCalls(graphDb)
    log("DelegateCall detection finished", "BOOST")
    detectExternalCalls(graphDb)
    log("ExternalCall detection finished", "BOOST")
    detectConstructor(graphDb)
    log("Constructor Call detection finished", "BOOST")
}

fun detectDelegateCalls(graphDb: GraphDatabaseService) {
    val instantQuery =
        """
        match (c: Call)-[:CALL_EXPRESSION]->(m: MemberAccess)
        where m.memberName = "delegatecall"
        match (m) -[:LEFT_HAND_SIDE]->(lhs)-[:TYPE]->(t)
        where t.typeString = "address" or t.typestring = "address payable"
        set c:DelegateCall:ExternalCall
        return distinct Id(c) as call
        """.trimIndent()

    val tx = graphDb.beginTx()
    val dcs =
        tx
            .execute(instantQuery)
            .map { it["call"] as Long }
            .asSequence()
            .toList()
    tx.commit()

    delegateCallPropagation(dcs, graphDb)

    do {
        val refQuery =
            """
            match (f:Function:ContainsDelegateCall)<-[:REFERENCES]-()<-[:CALL_EXPRESSION]-(c: Call)
            where not c:DelegateCall
            set c:DelegateCall:ExternalCall
            return distinct Id(c) as call
            """.trimIndent()

        val rtx = graphDb.beginTx()
        val calls =
            rtx
                .execute(refQuery)
                .map { it["call"] as Long }
                .asSequence()
                .toList()
        rtx.commit()

        delegateCallPropagation(calls, graphDb)
    } while (calls.isNotEmpty())
}

fun delegateCallPropagation(
    calls: List<Long>,
    graphDb: GraphDatabaseService,
) {
    calls.forEach { did ->
        val statementQuery =
            """
            match (d: DelegateCall) where Id(d) = $did
            match (d)<-[:HAS_CALL]-(s:Statement)
            match (f:Function)-[:HAS_STATEMENT]->(s)
            set s:ContainsDelegateCall:ContainsExternalCall
            set f:ContainsDelegateCall:ContainsExternalCall
            """.trimIndent()

        val stx = graphDb.beginTx()
        stx.execute(statementQuery)
        stx.commit()
    }
}

fun detectExternalCalls(graphDb: GraphDatabaseService) {
    val instantQuery =
        """
        match (m:MemberAccess)-[:LEFT_HAND_SIDE]->(lhs)-[:TYPE]->(t)
        where m.memberName = "call" and (t.typeString = "address" or t.typeString = "address payable")
        match (c: Call)-[:HAS_MA]->(m)
        set c:ExternalCall
        return distinct Id(c) as call
        union
        match (af: Function)
        where af.implemented = false
        match (rc:Call)-[:CALL_EXPRESSION]->(:MemberAccess)-[:REFERENCES]->(af)
        set rc:ExternalCall
        return distinct Id(rc) as call
        union
        match (n: NewExpression)
        set n:ExternalCall
        return distinct Id(n) as call
        """.trimIndent()

    val tx = graphDb.beginTx()
    val ecs =
        tx
            .execute(instantQuery)
            .map { it["call"] as Long }
            .asSequence()
            .toList()
    tx.commit()

    log("external calls collected, propagation in progress", "BOOST")
    var nfuncs = externalCallPropagation(ecs, graphDb)

    do {
        val calls = mutableListOf<Long>()
        nfuncs.forEach { func ->
            log("external calls propagated, refining results iteratively", "BOOST")
            val refQuery =
                """
                match (f:Function:ContainsExternalCall)
                where Id(f) = $func
                match (f)<-[:REFERENCES]-()<-[:CALL_EXPRESSION]-(c: Call)
                where not c:ExternalCall
                set c:ExternalCall
                return distinct Id(c) as call
                """.trimIndent()

            val rtx = graphDb.beginTx()
            val ncalls =
                rtx
                    .execute(refQuery)
                    .map { it["call"] as Long }
                    .asSequence()
                    .toList()
            rtx.commit()
            if (ncalls.isNotEmpty()) {
                calls.addAll(ncalls)
            }
        }
        if (calls.isNotEmpty()) {
            nfuncs = externalCallPropagation(calls, graphDb)
        }
    } while (calls.isNotEmpty())
}

fun externalCallPropagation(
    calls: List<Long>,
    graphDb: GraphDatabaseService,
): List<Long> {
    val funcs = mutableListOf<Long>()
    calls.forEach { eid ->
        val statementQuery =
            """
            match (e: ExternalCall) where Id(e) = $eid
            match (e)<-[:HAS_CALL]-(s:Statement)
            match (f:Function)-[:HAS_STATEMENT]->(s)
            set s:ContainsExternalCall
            set f:ContainsExternalCall
            return distinct Id(f) as func
            """.trimIndent()

        val stx = graphDb.beginTx()
        val nfuncs =
            stx
                .execute(statementQuery)
                .map { it["func"] as Long }
                .asSequence()
                .toList()
        stx.commit()

        if (nfuncs.isNotEmpty()) {
            funcs.addAll(nfuncs)
        }
    }
    return funcs
}

fun detectConstructor(graphDb: GraphDatabaseService) {
    val constructorCallQuery =
        """
        match (n:NewExpression)-[:TYPE]->(t)<-[:DEFINES_TYPE]-(cont: Contract)
        match (c: Call)-[:CALL_EXPRESSION]->(n)
        match (cont)-[:HAS_CONSTRUCTOR]->(co:Constructor)
        set c:ConstructorCall
        remove c:ExternalCall
        return distinct Id(c) as con
        """.trimIndent()
    val tx = graphDb.beginTx()
    tx.execute("match (c:Contract)-[:HAS_FUNCTION]->(con:Constructor) merge (c)-[:HAS_CONSTRUCTOR]->(con)")
    val cons =
        tx
            .execute(constructorCallQuery)
            .map { it["con"] as Long }
            .asSequence()
            .toList()
    tx.commit()

    cons.forEach { cid ->
        val statementQuery =
            """
            match (con: ConstructorCall) where Id(con) = $cid
            match (con)<-[:HAS_CALL]-(s:Statement)
            match (f:Function)-[:HAS_STATEMENT]->(s)
            set s:ContainsConstructorCall merge (s)-[:HAS_CALL]->(con)
            set f:ContainsConstructorCall
            """.trimIndent()

        val stx = graphDb.beginTx()
        stx.execute(statementQuery)
        stx.commit()
    }
}
