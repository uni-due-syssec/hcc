package syssec.hcc

import com.beust.klaxon.JsonObject
import com.beust.klaxon.Parser
import org.neo4j.exceptions.InvalidSemanticsException
import org.neo4j.graphdb.GraphDatabaseService
import java.io.File
import java.io.FileNotFoundException

data class SourceFile(val filename: String){
    private val srcFile = File(filename)
    private val originalSourceCode: String = srcFile.readText()

    init {
        if(!srcFile.exists()){
            throw FileNotFoundException()
        }
    }

    fun loadToCPG(graph: GraphDatabaseService) {
        val jsonFile = srcFile.compileToJsonAST()
        log("Compilation finished")
        val parser = Parser.default()
        val json = parser.parse(jsonFile.bufferedReader()) as JsonObject
        if(!json.containsKey("absolutePath")) throw InvalidSemanticsException("The generated JSON has the wrong format.")
        log("Converting to CPG...")
        convertToCodePropertyGraph(json, graph)
    }
}